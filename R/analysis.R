library(data.table)
library(ggplot2)
library(occurR)

x <- fread(cmd = "cwb-scan-corpus BNC word hw pos text_id",
  sep = "\t", quote = "", na.strings = NULL,
  col.names = c("f", "word", "lemma", "pos", "id"),
  key = "word"
)[, sum(f), by = .(word = tolower(word), lemma, pos)]

rm_prefix <- \(x) `levels<-`(x, sub(".*=", "", levels(x)))

x2 <- fread(cmd = "cwb-decode FROWN -P word -P lemma -P pos -S text_id",
    sep = "\t", quote = "", na.strings = NULL, select = 1:4,
    col.names = c("word", "lemma", "pos", "id"),
    stringsAsFactors = TRUE
  )[, 1:3 := lapply(.SD, rm_prefix), .SDcols = 1:3
  ][, id := as.integer(id)
]

lookup <- c(
    noun   = "^N"
  , verb   = "^V"
  , noun_s = "^NN2"
  , verb_s = "^VVZ"
  , ed     = "^VVD|^VVN"
  , ambig  = "NN2-VVZ|VVZ-NN2"
)

for (i in seq_along(lookup))
  x[grepl(lookup[i], pos), pos2 := names(lookup[i])]

# x[, pos3 := fifelse(pos2 %like% "*_s", "inflected", "other")]

y <- dcast(x, lemma ~ pos2, value.var = "V1", fun = sum)
y[, f := rowSums(y[, -1])]

z <- y[rowSums(y[, -(1:2)]) > 50, ]
z[, `:=`(s = noun_s + verb_s, f = rowSums(z[, -1]))]

# y[, ll_noun_s := ll_mini(f, noun_s)]
# y[, ll_verb_s := ll_mini(f, verb_s)]
# y[, ll := ll_mini(f, verb_s + noun_s)]
# y[, ll_ed := ll_mini(f, ed)]
# setorder(y, -ll_ed)
# head(y, 100)

ggplot(y, aes(-log1p((verb_s + noun_s) / f), log(f), color = conversion)) + geom_point()
ggplot(y, aes(-log1p(ed / f), log(f), color = conversion)) + geom_point()

log1p(0.1)

plot(log(y$ll_noun_s), log(y$ll_verb_s))
plot(y$ll, log(y$verb / y$noun))
plot(y$ll_ed, log(y$verb / y$noun))

