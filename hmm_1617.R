library(HMM)

parse_text_from_url <- function(url){
  text <- paste(readLines(url, encoding = "UTF-8"), collapse = "\n")
  text <- c(gsub("[[:punct:]]", "", text))
  text <- c(gsub("[[:digit:]]", "", text))
  text <- c(gsub("é", "e", text))
  text <- c(gsub("à", "a", text))
  text <- c(gsub("æ", "ae", text))
  text <- c(gsub("\\s+", " ", text))
  text
}

get_chars_from_text <- function(text){
  tolower(as.vector(unlist(strsplit(text, ""))))
}

get_suf_from_text <- function(text, n=3){
  words <- tolower(as.vector(unlist(strsplit(text, " "))))
  sapply(words, function(word){
    substring(word, nchar(word) - n)
  })
}

get_HMM <- function(symbols, no_hidden_states){

  normalize_vector <- function(x){
    x / sum(x)
  }

  no_symbols <- length(symbols)
  my_transition_probs <- t(
    apply(matrix(rnorm(no_hidden_states ** 2, 0.5, 0.01), no_hidden_states),
      1, normalize_vector
    ))
  my_emission_probs <- t(apply(
    matrix(rnorm(no_hidden_states * no_symbols, 0.5, 0.01), no_hidden_states),
    1, normalize_vector))

  hmm <- initHMM(paste("State", 1:no_hidden_states),
    symbols,
    transProbs = my_transition_probs,
    emissionProbs = my_emission_probs)

  hmm
}

# Get text
tadeusz_url <- "https://wolnelektury.pl/media/book/txt/pan-tadeusz.txt"
oda_url <- "https://wolnelektury.pl/media/book/txt/oda-do-mlodosci.txt"

tadeusz <- parse_text_from_url(tadeusz_url)
tadeusz_chars <- get_chars_from_text(tadeusz)
# get sufixes of length 1, 2 and 3
tadeusz_sufs <- sapply(0:2, function(n) get_suf_from_text(tadeusz, n))

oda <- parse_text_from_url(oda_url)
oda_chars <-  get_chars_from_text(oda)
oda_sufs <-  sapply(0:2, function(n) get_suf_from_text(oda, n))

# create HMM for chars
hmm_chars <- get_HMM(unique(tadeusz_chars), 2)
# train the HMM for different numbers of observations
no_chars_test <- c(500, 1000, 2500, 5000, 7500)
output <- lapply(no_chars_test, function(char.no){
  baumWelch(hmm_chars, observation = tadeusz_chars[1:char.no], maxIterations = 250)
})

# create HMM for sufixes
hmm_sufs <- get_HMM(unique(tadeusz_sufs[, 2]), 5)

# train the HMM for sufixes of legth 2
hmm_sufs_BW <- baumWelch(
  hmm_sufs,
  observation = tadeusz_sufs[1:5000, 2],
  maxIterations = 50
)


est.probabilities <- output[[5]]$emissionProbs[2, ]
my.ord <- order(-est.probabilities)

# prepare vectors of colors to distinguish vowels
samogloski <- c("a", "e", "i", "o", "u", "y", "ą", "ę", "ó")
my.symbols <- unique(tadeusz_chars)
no.symbols <- length(my.symbols)
my.colors <- rep("black", no.symbols)
my.colors[which(my.symbols[my.ord] %in% samogloski)] <- "red"
my.colors

plot(est.probabilities[my.ord],
  main = "Emission probabilities of 2nd state",
  xlab = "Symbols",
  ylab = "Probability",
  type = "h", xaxt = "n")
text(1:no.symbols,
  est.probabilities[my.ord],
  labels = my.symbols[my.ord],
  cex = 0.7, pos = 3, col = my.colors)


##POSTERIOR analysis
oda_char_posterior <- posterior(hmm_char_BW$hmm, oda_chars[1:1000])
oda_char_posterior
oda_vowel <- oda_znaki[1:1000] %in% samogloski
oda_vowel_posterior <- oda_char_posterior[2, ] > oda_char_posterior[1, ]

TP <- sum(oda_vowel_posterior == oda_vowel & oda_vowel == T)
TN <- sum(oda_vowel_posterior == oda_vowel & oda_vowel == F)
FN <- sum(oda_vowel_posterior != oda_vowel & oda_vowel == T)
FP <- sum(oda_vowel_posterior != oda_vowel & oda_vowel == F)

predictions <- matrix(c(TP, FP, FN, TN), 2)
rownames(predictions) <- c("r_vowel", "r_consonant")
colnames(predictions) <- c("p_vowel", "p_consonant")
predictions

oda_suf_posterior <- posterior(hmm_sufs_BW$hmm, oda_sufs[1:1000])
