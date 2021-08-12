library(text2vec)
library(data.table)

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
        train_embedding(iter = 10, threads = 2)
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

# group_suffix <- function(x, suffix) {
#     regex <- paste0(".*", suffix, "$")
#     x[, (suffix) := factor(
#           grepl(regex, word) & grepl(regex, pos_group),
#           labels = c(paste0("no_", suffix), suffix),
#           exclude = ""
#     )]
# }

# x <- readRDS("BNC_BABY_data_raw") |>
#     group_suffix("ed") |>
#     group_suffix("s")

system.time(sims <- add_cos_sim(x))

y <- readRDS("BNC_BABY_data_annotated")
sims[, hw := as.factor(hw)]
y <- y[sims, on = "hw"]
saveRDS(y, "BNC_BABY_data_annotated_2")
"cos_sim_s" %in% names(y)

sims[order(cos_sim_s, na.last = NA)] |> print(100)
