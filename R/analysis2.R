library(data.table)
library(occurR)

# {{{ Import

clean_attr <- function(x) {
    x <- as.factor(x)
    levels(x) <- sub(".*=", "", levels(x))
    x
}

import_corpus <- function(corpus, p_attrs, s_attrs) {
    #TODO: multiple `s_attrs` don't work

    cmd <- paste("cwb-decode", corpus,
                 paste(" -S", s_attrs, collapse = ""),
                 paste(" -P", p_attrs, collapse = ""))

    fread(cmd = cmd,
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

    x[, pos_group := factor(pos_group, levels = names(lookup))
    ][is.na(pos_group), pos_group := "other"]
}

group_suffix <- function(x, suffix) {
    regex <- paste0(".*", suffix)
    x[, (suffix) := factor(
          grepl(regex, word) & grepl(regex, pos_group),
          labels = c(paste0("no_", suffix), suffix)
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
    assoc[o[, 1] < e[, 1]] <- -assoc
    assoc
}

get_assoc <- function(group, x, s_attr) {
    x[, .(o11 = as.double(.N)), by = c(s_attr, group)
    ][, f := sum(o11), by = s_attr
    ][, f2 := sum(o11), by = group
    ][, ll := ll(o11, f, f2, nrow(x))] |>
        spread(c(s_attr, group), "ll")
}

# }}} --------------------------------------------------------------------------
# {{{ Dispersion

dwg <- function(cpos, f, size) { # word growth dispersion
    dd <- cpos - shift(cpos, fill = 0L) - (size / f)
    mad <- sum(abs(dd)) / f
    worst_mad <- (size - f + 1 - size / f) / (f / 2)
    mad / worst_mad
}

get_dwg <- function(group, x) {
    x[, .(dwg = dwg(.I, .N, nrow(x))), by = group] |>
        spread(group, "dwg")
}

get_dp <- function(group, x, measure) { # TODO: implement raw corpus in "dispersions"
    # helper to handle variable arguments to `interaction` in `[.data.table`
    ia <- \(y) interaction(lapply(y, \(i) x[[i]]), sep = "\t")

    x <- x[, .N, by = c(group, "text_id")]
    x[, .(dispersion(N, ia(group), text_id, measure))
    ][, (group) := tstrsplit(types, "\t", fixed = TRUE)
    ][f == 0, (measure) := NA # set to NA where frequency == 0, needs fix in `dispersion`
    ][, `:=`(types = NULL, f = NULL)] |>
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

productivity <- function(x, tokens) { # TODO: reimplement using cwb + zipfR?
    counts <- tabulate(x)
    types <- length(counts)
    v <- tabulate(counts, nbins = 2)
    data.table(
        ttr = types / tokens,
        htr = v[1] / tokens,
        alpha1 = v[1] / types,
        alpha2 = 1 - (2 * (v[2] / v[1])) # Evert 2004b: 127
    )
}

get_productivity <- function(group, x, kwic_names) {
    measures <- c("ttr", "htr", "alpha1", "alpha2")
    x[, f := .N, by = eval(group[1])
    ][, productivity(unlist(.SD), first(f)), .SDcols = kwic_names, by = group] |>
        spread(group, measures)
}

# }}} --------------------------------------------------------------------------
# {{{ main

add_measures <- function(x, s_attr, groups, disp_fun, context) {
    kwic_names <- c(paste0("L", 1:context), paste0("R", 1:context))
    x[, (kwic_names) := embed_kwic(hw, context)] # TODO: don't hardcode hw

    combs <- Map(c, s_attr, groups)
    Reduce(\(x, y) x[y, on = s_attr], c( # merge
        lapply(combs, get_frequency, x),
        lapply(unlist(groups), get_assoc, x, s_attr),
        lapply(combs, get_productivity, x, kwic_names),
        lapply(combs, get_dp, x, disp_fun),
        lapply(combs, get_dwg, x)
    )) # TODO: factor out lapply(combs, fun, ...) |> spread()
}

system.time({

x <- import_corpus(
        corpus   = "BNC-BABY",
        p_attrs  = c("word", "hw", "pos"),
        s_attrs  = "text_id"
    ) |>
    group_pos(c(
        noun     = "^N",
        verb     = "^VV",
        noun_s   = "^NN2",
        verb_s   = "^VVZ",
        verb_ed  = "^VVD|^VVN",
        ambig    = "NN2-VVZ|VVZ-NN2"
    )) |>
    group_suffix("ed") |>
    group_suffix("s")

y <- add_measures(x,
        s_attr   = "hw",
        groups   = list(NULL, "s", "ed", "pos_group"),
        disp_fun = "dp.norm",
        context  = 2
    )

setcolorder(y, sort(names(y), decreasing = TRUE))

})

names(y)

saveRDS(x, "BNC_08032000")

t(y[hw == "test", !"hw"])
t(y[hw == "the", !"hw"])
hist(y[f > 1000, ttr])

plot(y[f > 100, .(log_f = log(f), ttr, htr, alpha1, alpha2, dp.norm, dwg)], pch = ".")
plot(y[f > 10, .(rank(ll_ambig), log(f_noun_s), log(f_verb_s))])

y[order(ll_ambig, decreasing = TRUE), .(hw, ll_ambig)][!is.na(ll_ambig)] |>
print(100)

names(y)

z <- y[!f_other / f == 1]
z[, noun := f_noun > 0]
z[, verb := f_verb > 0]
z[, NOUN := f_noun > f_verb]
z[, conv := (noun & verb) | f_ambig > 0]
z[, ratio_s := f_s / f]
z[, ratio_noun_s := f_noun_s / f]
z[, ratio_verb_s := f_verb_s / f]
z[, entropy := f_noun / f * log(f_noun / f / (f_verb / f))]

library(ggplot2)

z[f > 100 & f_s > 0] |>
ggplot(aes(log(f_s), log(f), color = conv)) +
    geom_density2d()

z[f > 100 & f_s > 0] |>
ggplot(aes(-log10(f_verb_s / f), color = conv)) +
    geom_density() + geom_rug()

library(gamlss)

names(z)
training <- z[f > 50 & entropy != Inf, .(entropy, ratio_s, ratio_noun_s, conv, alpha1, dwg, dp.norm, htr, f, f_noun, f_verb)]
# TODO: conv is too restrictive as predictor for ratio_s
lol <- gamlss(ratio_s ~ I(f_noun / f) * I(f_verb / f) + dwg + dp.norm, family = BEINF, data = na.omit(training))
summary(lol)
plot(lol)
coef(lol)

plot(z[f > 50, .(htr, dwg, log(f), ratio_noun_s)])
# vim:shiftwidth=4:
