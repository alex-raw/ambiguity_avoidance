library(zipfR)
library(gamlss)
library(ggplot2)
library(text2vec)
set.seed(667)

plot(y[f > 50, .(log_f = log(f), P, Hapax, alpha2, Entropy, dwg, dp.norm)], pch = ".")
y[f > 50, hist(alpha2)]

# Collostruction analysis
y[order(ll_ambig, decreasing = TRUE), .(hw, ll_ambig)][!is.na(ll_ambig)] |> print(50)
# TODO: do text2vec and compare most similar words in this top 10 or so

names(y)

z <- y[!f_other / f == 1]
z[, noun := f_noun > 0]
z[, verb := f_verb > 0]
z[, NOUN := f_noun > f_verb]
z[, ratio_s := f_s / f]

z[, conv := f_noun > 10 & f_verb > 10 | f_ambig > 0]

saveRDS(z, "BABY_1")
z <- readRDS("BABY_1")

z[f > 100] |>
ggplot(aes(log(f_s), log(f), color = conv)) +
    geom_density_2d()# + geom_point()

z[f > 100] |>
ggplot(aes(log(f_s), color = conv)) +
    geom_density() + geom_rug()

names(z)
training <- z[f > 100, .(f, f_s, Hapax, alpha2, P, dp.norm, dwg, Entropy, NOUN, ratio_s)]
# TODO: conv is not a good predictor for ratio_s
lol <- gamlss(ratio_s ~ NOUN + Hapax + alpha2 + P + dp.norm + dwg + Entropy, family = BEINF, data = na.omit(training))
library(DirichletReg)
dir_response <- DR_data(training$ratio_s)
plot(dir_response)
dir_mod <- DirichReg(dir_response ~ NOUN + Hapax + alpha2 + P + dp.norm + dwg + Entropy, data = training)
summary(dir_mod)
plot(dir_mod)
anova(dir_mod)
term.plot(lol)

plot(z[f > 50, .(htr, dwg, log(f), ratio_noun_s)])
# vim:shiftwidth=4:


library(colorspace)
plot(ALake$Y, cex=.5, a2d=list(colored=FALSE, c.grid=FALSE))
plot(dir_response, cex=.5, a2d=list(colored=FALSE, c.grid=FALSE))
training
plot(rep(dir_response, 3),
    as.numeric(dir_response),
    ylim = 0:1,
    pch = 21,
    bg = rep(rainbow_hcl(3), each = 39),
    xlab = "Depth (m)",
    ylab = "Proportion"
)
