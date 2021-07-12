library(ggplot2)
library(data.table)

clean_attr <- function(x) {
  x <- as.factor(x)
  levels(x) <- sub(".*=", "", levels(x))
  x
}

import_corpus <- function(corpus, p_attrs, s_attrs) {
  lemma <- if (grepl("^BNC", corpus)) "hw" else "lemma"
  if (missing(p_attrs)) p_attrs <- c("word", lemma, "pos")
  if (missing(s_attrs)) s_attrs <- "text_id" # TODO: doesn't work

  cmd <- paste("cwb-decode", corpus,
               paste("-S", s_attrs, collapse = " "),
               paste("-P", p_attrs, collapse = " "))

  # corpus in raw format with distances for positional attributes
  fread(cmd = cmd, col.names = c(s_attrs, p_attrs),
        select = seq_along(c(p_attrs, s_attrs)),
        sep = "\t", quote = "", na.strings = NULL
    )[, word := tolower(word)
    ][, (p_attrs) := lapply(.SD, clean_attr), .SDcols = p_attrs
    ][, (s_attrs) := lapply(.SD, \(x) chmatch(x, x)), .SDcols = s_attrs]
}

x <- import_corpus("BNC-BABY")
saveRDS(x, "ja_lol_data")
y <- readRDS("ja_lol_data")

x[, d := diff(c(0, .I)), by = word]

lookup <- c(NULL
  , noun   = "^N"
  , verb   = "^V"
  , noun_s = "^NN2"
  , verb_s = "^VVZ"
  , ed     = "^VVD|^VVN"
  , ambig  = "NN2-VVZ|VVZ-NN2"
)

for (i in seq_along(lookup))
  x[grepl(lookup[i], pos), pos2 := names(lookup[i])]

x[is.na(pos2), pos2 := "other"]

x[, pos3 := fifelse(word %like% "*s" & pos2 %like% "*s", "inflected", "base")]
x[!(pos %like% "^N" | pos %like% "^V"), pos3 := "other"]

ReadingSkills
y <- x[, .N, by = .(hw, pos3)] |>
  dcast(hw ~ pos3, fun = sum)
y <- y[!(base == 0 & inflected == 0)
  ][,
  f := base + inflected + other
  ]

y <- y[, .(hw, f, ratio = inflected / f)]
# correction recommended by Zeilis blabla
model_data <- y[, ratio := (ratio * (nrow(y) - 1) + 0.5) / nrow(y)]

ggplot(model_data[f > 100], aes(ratio)) + geom_histogram()

# y <- dcast(x, lemma ~ pos2, value.var = "V1", fun = sum)

# # preserve corpus positions in list column
# # to calculate dispersions for different combos (with unite/intersect)
# x[, .(cpos = list(.I), ids = list(text_id)), by = word]
