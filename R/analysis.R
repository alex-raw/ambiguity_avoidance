library(data.table)
library(parallel)
library(gamlss)
set.seed(667)

# }}} --------------------------------------------------------------------------
# {{{ Modelling

system.time({

x <- readRDS("../data/BNC_data_annotated")
y <- x[(f_other / f) %between% c(0, .9)
     ][, `:=`(
     f2 = f - (f_other + f_proper),
     f_noun = (f_noun + f_noun_s + f_ambig_noun),
     f_verb = (f_verb + f_verb_s + f_ed + f_ambig_verb)
     )][,
     conversion := fifelse(f_noun > f_verb, 1 - (f_verb / f_noun), (f_noun / f_verb) - 1)
     ][
     f_proper < (f_noun + f_verb)
     ]

training <- y[, .(
    hw, f, f2, f_noun, f_verb, f_noun_s, f_verb_s, f_s, f_ed,
    f_ambig_noun, f_ambig_verb,
    conversion, dwg, cos_sim_s, dp.norm,
    alpha1_left = Hapax,
    alpha1_right = i.Hapax
    )][
    f2 > 50 & alpha1_left > .35 & alpha1_right > .35
    ]

nouns <- copy(training)[, noun_s_rel := (f_noun_s + f_ambig_noun) / f_noun
    ] |> na.omit()
verbs <- copy(training)[, verb_s_rel := (f_verb_s + f_ambig_verb) / f_verb
    ] |> na.omit()
eds <- copy(training)[, ed_rel := f_ed / (f_verb + f_ambig_verb)
    ] |> na.omit()

verbs[verb_s_rel == 1, .N]
nouns[noun_s_rel == 1, .N]

form  <- ~ pb(conversion, df = 5) + pb(dwg) + pb(alpha1_right) + pb(alpha1_left) + pb(cos_sim_s)
form2 <- ~ pb(conversion, df = 5) + pb(dwg) + pb(alpha1_right) + pb(alpha1_left)

m_verb <- gamlss(verb_s_rel ~
    pb(conversion) + pb(dwg) + pb(cos_sim_s) + pb(alpha1_right) + pb(alpha1_left),
    sigma.formula = form, nu.formula = form, tau.formula = form,
    control = gamlss.control(n.cyc = 30),
    data = verbs, family = BEINF)

m_noun <- gamlss(noun_s_rel ~
    pb(conversion) + pb(dwg) + pb(cos_sim_s) + pb(alpha1_right) + pb(alpha1_left),
    sigma.formula = form, nu.formula = form, tau.formula = form,
    control = gamlss.control(n.cyc = 30),
    data = nouns, family = BEINF)

m_ed <- gamlss(ed_rel ~
    pb(conversion) + pb(dwg) + pb(alpha1_right) + pb(alpha1_left),
    sigma.formula = form2, nu.formula = form2, tau.formula = form2,
    control = gamlss.control(n.cyc = 30),
    data = eds, family = BEINF)

# }}} --------------------------------------------------------------------------
# {{{ Plots and tables

create_coef_plots <- function(x) {
    name <- deparse(substitute(x))
    for (parameter in c("mu", "sigma", "nu", "tau")) {
        # yikes, there must be a better way to access variables in the model
        term_strings <- as.character(attr(terms(x, parameter), "variables")[-1:-2])
        for (term in term_strings) {
            filename <- gsub("[()]|pb|m_|,.*", "", term)
            jpeg(paste0("../figures/", name, "_", parameter, "_", filename, ".jpg"),
                 width = 720, height = 720)
            par(cex = 2)
            term.plot(x, term = term, what = parameter,
                      ylim = "free", rug = TRUE)
            abline(h = 0, lty = "dashed")
            dev.off()
        }
    }
}

create_diagnostic_plot <- function(x, name) {
    jpeg(paste0("../figures/", name, "_diagnostic.jpg"),
         width = 1280, height = 800)
    plot(x)
    dev.off()
}

clean_plot_gamlss <- function(x) {
    colname <- deparse(substitute(x))
    x <- capture.output(create_diagnostic_plot(x, colname)) # side effect!! creates plot jpg
    strsplit(x, " = ") |>
        Filter(f = \(y) length(y) == 2) |>
        sapply(trimws) |>
        t() |> data.table() |>
        setNames(c("coefficient", colname))
}

# }}} --------------------------------------------------------------------------
# {{{ Export

create_coef_plots(m_verb)
create_coef_plots(m_noun)
create_coef_plots(m_ed)

Reduce(\(x, y) merge(x, y, sort = FALSE), list(
    # side effect!! creates plot jpg
    clean_plot_gamlss(m_verb),
    clean_plot_gamlss(m_noun),
    clean_plot_gamlss(m_ed))
) |> saveRDS("../data/resid_table")

verbs[conversion %between% c(.49, .5), hw]

coef_tables <- lapply(
    list(m_verb = m_verb, m_noun = m_noun, m_ed = m_ed),
    parameters::model_parameters
) |> saveRDS("../data/coef_tables")

snip_gamlss_summary <- function(x)
    gsub("\\*", "-", tail(capture.output(x), 10))

list(m_verb = snip_gamlss_summary(summary(m_verb)),
     m_noun = snip_gamlss_summary(summary(m_noun)),
     m_ed   = snip_gamlss_summary(summary(m_ed))
) |> saveRDS("../data/summaries")

list(m_verb = Rsq(m_verb),
     m_noun = Rsq(m_noun),
     m_ed = Rsq(m_ed)
) |> saveRDS("../data/Rsq_vals")

})

collostr <- x[f_noun > f_verb, wclass := "noun"
            ][f_verb > f_noun, wclass := "verb"
            ][, `:=`(
            conversion = fifelse(f_noun > f_verb, 1 - (f_verb / f_noun), (f_noun / f_verb) - 1),
            f2 = f - (f_other + f_proper),
            ratio_s = f_s / f
            )][order(ll_verb, decreasing = TRUE)]# |> na.omit()

library(ggplot2)
library(patchwork)
options(scipen = 1, digits = 2)

uncanny_verbs <- collostr[f > 70] |>
ggplot(aes(conversion, ll_verb, alpha = 1 - dp.norm, color = ratio_s)) +
    geom_point(aes(size = f)) +
    scale_size_continuous(trans = "log10", range = c(1, 3)) +
    scale_color_continuous(type = "viridis", trans = "sqrt") +
    scale_y_log10() +
    # geom_text(aes(label = hw)) +
    theme_minimal() +
    ylab("association verbal pos tag (LLR)")

uncanny_nouns <- collostr[f > 50] |>
ggplot(aes(conversion, ll_noun, alpha = 1 - dp.norm, color = ratio_s)) +
    geom_point(aes(size = f)) +
    scale_size_continuous(trans = "log10", range = c(1, 3)) +
    scale_color_continuous(type = "viridis", trans = "sqrt") +
    theme(legend.position = "none") +
    scale_y_log10() +
    # geom_text(aes(label = hw)) +
    theme_minimal() +
    guides(color = "none", size = "none", alpha = "none") +
    ylab("association nominal pos tag (LLR)")

ggsave("../figures/continuum.jpg", uncanny_nouns + uncanny_verbs, "jpg", scale = .5)

s_noun <- nouns |>
ggplot(aes(conversion, f_noun_s, alpha = 1 - dp.norm, color = cos_sim_s)) +
    geom_point(aes(size = f)) +
    # geom_text(aes(label = hw)) +
    scale_color_continuous(type = "viridis") +
    scale_size_continuous(trans = "log10", range = c(1, 4)) +
    scale_y_log10() +
    theme_minimal() +
    guides(color = "none", size = "none", alpha = "none") +
    ylab("proportion of verbal -s")

s_verb <- verbs |>
ggplot(aes(conversion, f_verb_s, alpha = 1 - dp.norm, color = cos_sim_s)) +
    geom_point(aes(size = f)) +
    # geom_text(aes(label = hw)) +
    scale_color_continuous(type = "viridis") +
    scale_size_continuous(trans = "log10", range = c(1, 4)) +
    scale_y_log10() +
    theme_minimal() +
    ylab("proportion of nominal -s")

ggsave("../figures/s_continuum.jpg", s_noun + s_verb, "jpg", scale = .5)

s_noun + s_verb

# vim:shiftwidth=4:
