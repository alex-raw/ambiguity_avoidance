library(text2vec)
library(data.table)
data("movie_review")
setDT(movie_review)
setkey(movie_review, id)
set.seed(2016L)

all_ids <- movie_review$id
train_ids <- sample(all_ids, 4000)
test_ids <- setdiff(all_ids, train_ids)
train <- movie_review[J(train_ids)]

it_train <- itoken(train$review,
  preprocessor = tolower,
  tokenizer = word_tokenizer,
  ids = train$id,
  progressbar = FALSE
)

vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(it_train, vectorizer, skip_grams_window = 5L)
glove <- GlobalVectors$new(rank = 50, x_max = 10)

wv_main <- glove$fit_transform(tcm,
  n_iter = 10,
  convergence_tol = 0.01,
  n_threads = 8
)

wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)

ing <- word_vectors["go", , drop = FALSE]

cos_sim <- sim2(word_vectors, ing, method = "cosine", norm = "l2")
plot(sort(cos_sim[, 1], decreasing = TRUE))

dtm_train <- create_dtm(it_train, vectorizer)

lol <- system2("cwb-decode", "-C BROWN-C -P word", stdout = TRUE)
