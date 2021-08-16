library(data.table)
library(parallel)
library(mgcv)
library(gamlss)
set.seed(667)
x <- readRDS("BNC_data_annotated")

system.time({

y <- x[(f_other - f) != 0, f2 := f - f_other
][, `:=`(
    f_noun = (f_noun + f_noun_s + f_ambig),
    f_verb = (f_verb + f_verb_s + f_ed + f_ambig),
    f_noun_s_rel = f_noun_s / f2,
    f_verb_s_rel = f_verb_s / f2,
    f_s_rel = f_s / f2,
    f_ed_rel = f_ed / f2
    )
][f2 > 70, conv := ifelse(f_noun > f_verb, f_verb / f_noun, f_noun / f_verb)
]#[hw != "gim"]

# Modelling
training <- y[, .(
    hw, f2, conv, f_noun, f_verb, f_s_rel, f_s, f_noun_s_rel, f_verb_s_rel,
    f_ed, f_ed_rel, dwg, dp.norm, Hapax, i.Hapax
    )
] |> na.omit()

nouns <- training[f_noun >= f_verb]
verbs <- training[f_verb >= f_noun]

indep <- "~ pb(conv, control = pb.control(inter = 15)) + pb(dwg) + pb(dp.norm) + pb(Hapax) + pb(i.Hapax)"

m_verb <- as.formula(paste("f_verb_s_rel", indep)) |>
    gamlss(nu.formula = as.formula(indep),
           # tau.formula = as.formula(indep),
           data = verbs, family = BEINF)

m_noun <- as.formula(paste("f_noun_s_rel", indep)) |>
    gamlss(nu.formula = as.formula(indep),
           tau.formula = as.formula(indep),
           data = nouns, family = BEINF)

m_noun_ed <- as.formula(paste("f_ed_rel", indep)) |>
    gamlss(nu.formula = as.formula(indep),
           tau.formula = as.formula(indep),
           data = nouns, family = BEINF)

r_squareds <- list(
    Rqs_m_verb = Rsq(m_verb),
    Rqs_m_noun = Rsq(m_noun),
    Rqs_m_noun_ed = Rsq(m_noun_ed)
)

saveRDS(list(m_verb, m_noun, m_noun_ed, r_squareds), "ambig_models")
models <- readRDS("ambig_models")
models$r_squareds

})

library(ggplot2)
library(ggstatsplot)

to_plot <- m_verb
summary(to_plot)
plot(to_plot)
par(mfrow = c(2, 3))
term.plot(to_plot, ylim = "free", rug = TRUE)
term.plot(to_plot, ylim = "free", rug = TRUE, what = "nu")
term.plot(to_plot, ylim = "free", rug = TRUE, what = "tau")
abline(h = 0)

plot_coefs <- function(x, xlims, ...)
    ggcoefstats(x, exclude.intercept = TRUE, ...) + xlim(xlims)

plot1 <- plot_coefs(m_verb, c(-10, 16), title = "lal")
plot2 <- plot_coefs(m_noun, c(-7, 10), title = "lel")
plot3 <- plot_coefs(m_noun_ed, c(-10, 10), title = "lol")

tables <- lapply(list(m_verb, m_noun, m_noun_ed), ggcoefstats, output = "tidy")
tables[[1]]$term

# plot(x[f > 50, .(
#     log_f = log(f),
#     Hapax,
#     # alpha2, i.alpha2, Entropy, i.Entropy, P,
#     dwg,
#     dp.norm,
#     cos_sim_s
# )], pch = ".")

# # Collostruction analysis
# collustr <- x[!is.na(ll_ambig)
# ][order(ll_ambig, decreasing = TRUE), .(hw, ll_ambig, cos_sim_s)
# ]
# print(collustr, 15)
# ggplot(na.omit(collustr), aes(cos_sim_s, log(ll_ambig))) +
#     geom_point() + geom_smooth() + ylim(c(0, 10)) + theme_minimal()

# vim:shiftwidth=4:
