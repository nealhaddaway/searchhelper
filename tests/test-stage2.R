source("R/boolean_match.R")
source("R/term_mining.R")

records <- data.frame(
  title = c(
    "Atlantic salmon aquaculture in Norway",
    "Rainbow trout production in freshwater",
    "Wild salmon ecology"
  ),
  abstract = c(
    "A field study of farmed Salmo salar.",
    "Oncorhynchus mykiss was reared in tanks.",
    "Migration patterns in wild fish."
  ),
  keywords = c(
    "salmon; aquaculture; field study",
    "rainbow trout; aquaculture",
    "salmon; migration"
  ),
  stringsAsFactors = FALSE
)

q1 <- '(salmon* OR "rainbow trout") AND aquacultur*'
m1 <- match_search_records(records, q1)
stopifnot(identical(unname(m1), c(TRUE, TRUE, FALSE)))

q2 <- 'salmon* AND NOT wild'
m2 <- match_search_records(records, q2)
stopifnot(identical(unname(m2), c(TRUE, FALSE, FALSE)))

q3 <- '"field study" AND farm*'
m3 <- match_search_records(records, q3)
stopifnot(identical(unname(m3), c(TRUE, FALSE, FALSE)))

candidates <- mine_candidate_terms(records[3, , drop = FALSE], query = "salmon*")
stopifnot(!any(candidates$candidate == "salmon"))

prox_error <- try(boolean_to_rpn("salmon NEAR/5 farm*"), silent = TRUE)
stopifnot(inherits(prox_error, "try-error"))

stop <- english_stopwords()
stopifnot("of" %in% stop)
stopifnot("really" %in% stop)

phrase_terms <- candidate_tokens(
  "of environmental really important environmental effects quality of life impact on salmon climate change in",
  include_bigrams = TRUE,
  include_trigrams = TRUE
)

stopifnot(!"of environmental" %in% phrase_terms)
stopifnot(!"really important" %in% phrase_terms)
stopifnot("environmental effects" %in% phrase_terms)
stopifnot("quality of life" %in% phrase_terms)
stopifnot("impact on salmon" %in% phrase_terms)
stopifnot(!"climate change in" %in% phrase_terms)

bigrams <- phrase_terms[vapply(strsplit(phrase_terms, "\\s+"), length, integer(1)) == 2L]
if (length(bigrams)) {
  bigram_parts <- strsplit(bigrams, "\\s+")
  stopifnot(all(vapply(bigram_parts, function(x) !any(x %in% stop), logical(1))))
}

trigrams <- phrase_terms[vapply(strsplit(phrase_terms, "\\s+"), length, integer(1)) == 3L]
if (length(trigrams)) {
  trigram_parts <- strsplit(trigrams, "\\s+")
  stopifnot(all(vapply(trigram_parts, function(x) {
    !x[1] %in% stop && !x[3] %in% stop
  }, logical(1))))
}

cat("Stage 2 Boolean matching and candidate-term tests passed.\n")
