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

