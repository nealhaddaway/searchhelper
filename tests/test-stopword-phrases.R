source("R/term_mining.R")

stop <- english_stopwords()
stopifnot("of" %in% stop)
stopifnot("really" %in% stop)
stopifnot("however" %in% stop)

terms <- candidate_tokens(
  "of environmental really important environmental effects highly relevant",
  include_bigrams = TRUE
)

stopifnot(!"of environmental" %in% terms)
stopifnot(!"really important" %in% terms)
stopifnot("environmental effects" %in% terms)

# No retained bigram may contain a stopword.
bigrams <- terms[grepl(" ", terms, fixed = TRUE)]
if (length(bigrams)) {
  parts <- strsplit(bigrams, "\\s+")
  stopifnot(all(vapply(parts, function(x) !any(x %in% stop), logical(1))))
}

cat("Stopword phrase filtering regression test passed.\n")
