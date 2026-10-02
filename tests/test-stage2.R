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

cat("Stage 2 Boolean matching and candidate-term tests passed.\n")
