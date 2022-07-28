library(data.table)
library(parallel)
library(occurR)
library(zipfR)
library(text2vec)
set.seed(667)

# {{{ Import

clean_attr <- function(x) {
    x <- factor(x, exclude = "")
    levs <- sub(".*=", "", levels(x))
    levs[levs == ""] <- "__EMPTY__"
    levels(x) <- levs
    x
}

import_corpus <- function(corpus, p_attrs, s_attrs) {
    #TODO: multiple `s_attrs` don't work

    cmd <- paste("cwb-decode", corpus,
                 paste(" -S", s_attrs, collapse = ""),
                 paste(" -P", p_attrs, collapse = ""))

    lol <- fread(cmd = cmd,
          col.names = c(s_attrs, p_attrs),
          select = seq_along(c(p_attrs, s_attrs)), #sometimes trailing collumn
          sep = "\t", quote = "", na.strings = NULL
    )[, word := tolower(word)
    ][, (p_attrs) := lapply(.SD, clean_attr), .SDcols = p_attrs
    ][, #replace id strings with int = startingposition
        (s_attrs) := lapply(.SD, \(x) chmatch(x, x)), .SDcols = s_attrs
    ]
}

group_pos <- function(x, lookup) {
    for (i in seq_along(lookup))
        x[grepl(lookup[i], pos), pos_group := names(lookup[i])]

    x[, pos_group := factor(pos_group, levels = names(lookup), exclude = "")
    ][is.na(pos_group), pos_group := "other"]
}

group_suffix <- function(x, suffix) {
    regex <- paste0(".*", suffix, "$")
    x[, (suffix) := factor(
          grepl(regex, word) & grepl(regex, pos_group),
          labels = c(paste0("no_", suffix), suffix),
          exclude = ""
    )]
}

spread <- function(x, group, values, fill = NA) {
    if (length(group) == 1) return(x)

    x <- dcast(x, paste(group, collapse = "~"), value.var = values, fill = fill)

    if (length(values) == 1) {
        id_s_attr <- which(names(x) == group[1])
        setnames(x, -id_s_attr, \(y) paste0(values, "_", y))
    }
    x
}

# }}} --------------------------------------------------------------------------
# {{{ Frequency

get_frequency <- function(group, x) {
    x <- x[, .(f = .N), by = group]
    spread(x, group, "f", fill = 0)
}

# }}} --------------------------------------------------------------------------
# {{{ Attraction

ll <- function(o11, f1, f2 = sum(o11), n = sum(f1)) {
    o <- cbind(
        o11,
        o12 = f2 - o11,
        o21 = f1 - o11,
        o22 = n - f1 - f2 + o11
    )
    e <- cbind(
        e11 = f1 * f2,
        e12 = (n - f1) * f2,
        e21 = f1 * (n - f2),
        e22 = (n - f1) * (n - f2)
    ) / n
    assoc <- 2 * rowSums(o * log(o / e), na.rm = TRUE)
    repulsed <- o[, 1] < e[, 1]
    assoc[repulsed] <- -assoc[repulsed]
    assoc
}

get_assoc <- function(group, x, s_attr) {
    cat("calculating ll\n")
    x[, .(o11 = as.numeric(.N)), by = c(s_attr, group)
    ][, f := sum(o11), by = s_attr
    ][, f2 := sum(o11), by = group
    ][, ll := ll_mini(f, o11, nrow(x), f2)] |>
    # ][, ll := ll(o11, f, f2, nrow(x))] |>
        spread(c(s_attr, group), "ll")
}

# }}} --------------------------------------------------------------------------
# {{{ Dispersion

dwg <- function(cpos, f, size) { # word growth dispersion
    dd <- cpos - shift(cpos, fill = 0L) - (size / f)
    mad <- sum(abs(dd)) / f
    worst_mad <- (size - f + 1 - size / f) / (f / 2)
    dwg <- mad / worst_mad
    dwg / (2 * atan(worst_mad) / atan(mad))
}

get_dwg <- function(group, x) {
    cat("calculating dwg\n")
    x[, .(dwg = dwg(.I, .N, nrow(x))), by = group] |>
        spread(group, "dwg")
}

get_dp <- function(group, x, measure) { # TODO: implement raw corpus in "dispersions"
    cat("calculating dp\n")
    # helper to handle variable arguments to `interaction` in `[.data.table`
    ia <- \(y) interaction(lapply(y, \(i) x[[i]]), sep = "\t")

    x <- x[, .N, by = c(group, "text_id")]
    x[, .(dispersion(N, ia(group), text_id, measure))
    ][, (group) := tstrsplit(types, "\t", fixed = TRUE)
    ][f == 0, (measure) := NA # address in `dispersion`?
    ][, `:=`(types = NULL, f = NULL)] |> # TODO: do I even need this and line above?
        spread(group, measure)
}

# }}} --------------------------------------------------------------------------z
# {{{ Productivity

mtld <- function(x) {
# TODO: mtld = ?, McCarthy, Jarvis 2010 cf. Evert, Jannidis & Proisl, "Vobcabulary richness"
}

embed_kwic <- function(x, k) {
    pad <- rep(NA, k)
    x <- c(pad, x, pad) |> embed(k * 2 + 1)
    data.table(x[, -(k + 1)])
}

get_productivity <- function(group, x, kwic_names) {
    cat("calculating productivity\n")
    measures <- c("P", "Hapax", "alpha2", "Entropy")
    x[, unlist(.SD) |> na.omit() |> productivity.measures(measures),
        by = group,
        .SDcols = kwic_names
    ] |> spread(group, measures)
}

# }}} --------------------------------------------------------------------------
# {{{ Cosine Distance

train_embedding <- function(x, iter = 10, threads = 12) {
    it_train <- itoken(x$text,
        tokenizer = space_tokenizer,
        # TODO: test stopwords
        progressbar = FALSE
    )

    vectorizer <- vocab_vectorizer(create_vocabulary(it_train))
    glove <- GlobalVectors$new(rank = 50, x_max = 10)
    main <- glove$fit_transform(
        create_tcm(it_train, vectorizer, skip_grams_window = 5L),
        n_iter = iter,
        convergence_tol = 0.01,
        n_threads = threads
    )
    list(main = main, context = glove$components)
}

get_vectors <- function(x) {
    wv <- x[, .(text = paste0(word, collapse = " ")), by = text_id] |>
        train_embedding(iter = 10, threads = 12)
    wv$main + t(wv$context)
}

add_cos_sim <- function(x) {
    word_vectors <- get_vectors(x)
    x[s == "s", .(word = first(word)), by = hw
    ][, lapply(.SD, as.character)
    ][hw %chin% rownames(word_vectors) & hw != word,
        cos_sim_s := psim2(
            word_vectors[hw, ],
            word_vectors[word, ],
            method = "cosine",
            norm = "l2"
        )
    ][, `:=`(hw = as.factor(hw), word = NULL)]
}
# }}} --------------------------------------------------------------------------
# {{{ Main

add_measures <- function(x, s_attr, groups, disp_fun, context) {
    kwic_names <- c(paste0("L", 1:context), paste0("R", 1:context))
    x[, (kwic_names) := embed_kwic(hw, context)] # TODO: don't hardcode hw
    half <- seq_len(length(kwic_names) / 2)

    combs <- Map(c, s_attr, groups)
    sims <- add_cos_sim(x)
    x <- Reduce(\(x, y) x[y, on = s_attr], c( # merge
        lapply(combs, get_frequency, x),
        lapply(unlist(groups), get_assoc, x, s_attr),
        lapply(combs, get_productivity, x, kwic_names[half]),
        lapply(combs, get_productivity, x, kwic_names[-half]), # TODO: comes out as i.alpha...
        lapply(combs, get_dp, x, disp_fun),
        lapply(combs, get_dwg, x)
    )) # TODO: factor out lapply(combs, fun, ...) |> spread()
    merge(x, sims, by = "hw", all = TRUE)
}

system.time({

x <- import_corpus(
        corpus   = "BNC",
        p_attrs  = c("word", "hw", "pos"),
        s_attrs  = "text_id"
    ) |>
    group_pos(c(
        noun       = "^N",
        verb       = "^VV",
        noun_s     = "^NN2",
        verb_s     = "^VVZ",
        verb_ed    = "^VVD|^VVN",
        ambig_noun = "NN2-VVZ",
        ambig_verb = "VVZ-NN2",
        proper     = ".*NP.*"
    )) |>
    group_suffix("ed") |> group_suffix("s")

y <- add_measures(x,
        s_attr   = "hw",
        groups   = list(NULL, "s", "ed", "pos_group"),
        disp_fun = "dp.norm",
        context  = 1
    )

setcolorder(y, sort(names(y), decreasing = TRUE))

})

saveRDS(x, "BNC_data_raw")
saveRDS(y, "BNC_data_annotated")

# }}} --------------------------------------------------------------------------
# {{{
# system.time({

# x <- import_corpus(
#         corpus   = "COCA-S",
#         p_attrs  = c("word", "lemma", "pos"),
#         s_attrs  = "text_id"
#     ) |>
#     group_pos(c(
#         noun     = "^n",
#         verb     = "^vv",
#         noun_s   = "^nn2",
#         verb_s   = "^vvz",
#         verb_ed  = "^vvd|^vvn",
#         ambig    = "nn2_vvz|vvz_nn2"
#     )) |>
#     group_suffix("ed") |> group_suffix("s")

# y <- add_measures(x,
#         s_attr   = "hw",
#         groups   = list(NULL, "s", "ed", "pos_group"),
#         disp_fun = "dp.norm",
#         context  = 1
#     )

# setcolorder(y, sort(names(y), decreasing = TRUE))

# })

# saveRDS(x, "COCA_data_raw")
# saveRDS(y, "COCA_data_annotated")

# # }}} --------------------------------------------------------------------------
