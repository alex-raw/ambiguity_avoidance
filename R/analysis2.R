library(data.table)
library(occurR)
library(ggplot2)

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

spread <- function(x, group, values) {
    if (length(group) == 1) return(x)

    x <- dcast(x, paste(group, collapse = "~"), value.var = values, fill = 0L)

    if (length(values) == 1) {
        id_s_attr <- which(names(x) == group[1])
        setnames(x, -id_s_attr, \(y) paste0(values, "_", y))
    }
    x
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
    x <- x[, .(
        f = .N,
        dwg = dwg(.I, .N, nrow(x))
        ), by = group]
    spread(x, group, list("f", "dwg"))
}

get_dp <- function(group, x, measure) { # TODO: implement raw corpus in "dispersions"
    # helper to handle variable arguments to `interaction` in `[.data.table`
    ia <- \(y) interaction(lapply(y, \(i) x[[i]]), sep = "\t")

    x <- x[, .N, by = c(group, "text_id")]
    x <- x[, .(dispersion(N, ia(group), text_id, measure))
         ][, (group) := tstrsplit(types, "\t", fixed = TRUE)
         # set to NA where frequency == 0 ,needs fix in `dispersion`
         ][f == 0, (measure) := NA
         ][, `:=`(types = NULL, f = NULL)]

    spread(x, group, measure)
}

# }}} --------------------------------------------------------------------------
# {{{ Productivity

embed_kwic <- function(x, k) {
    pad <- rep(NA, k)
    x <- c(pad, x, pad) |> embed(k * 2 + 1)
    data.table(x[, -(k + 1)])
}

everts_alpha <- function(x) { # Evert 2004b: 127
    v <- tabulate(tabulate(x), nbins = 2)
    1 - 2 * v[2] / v[1]
}

get_productivity <- function(group, x, w) {
    outnames <- c(paste0("L", 1:w), paste0("R", 1:w))
    x <- x[, (outnames) := embed_kwic(word, w)
         ][, .(alpha2 = everts_alpha(unlist(.SD))), .SDcols = outnames, by = group]
    spread(x, group, "alpha2")
}

# }}} --------------------------------------------------------------------------
# {{{ Attraction

get_assoc <- function() {
}

# }}} --------------------------------------------------------------------------
# {{{ main

add_measures <- function(x, s_attr, groups, disp_fun, context) {
  groups <- Map(c, s_attr, groups)
  Reduce(\(x, y) x[y, on = s_attr], c( # merge
      lapply(groups, get_dwg, x),
      lapply(groups, get_dp, x, disp_fun),
      lapply(groups, get_productivity, x, context)
  ))
}

system.time({
x <- import_corpus(
        corpus   = "BNC",
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
    group_suffix("s") |>
    add_measures(
        s_attr   = "hw",
        groups   = list(NULL, "s", "ed", "pos_group"),
        disp_fun = "dp.norm",
        context  = 3
    )

setcolorder(x, sort(names(x), decreasing = TRUE))
})

t(x[hw == "test", -1])
t(x[hw == "en", -1])

# vim:shiftwidth=4:
