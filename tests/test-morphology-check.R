source("R/boolean_match.R")
source("R/morphology_check.R")

x <- search_morphology_checks("salmon AND (farm OR farming)")
stopifnot(nrow(x) == 1L)
stopifnot(x$family[[1]] == "farm")
stopifnot(grepl("farms", x$missing[[1]], fixed = TRUE))
stopifnot(grepl("farmed", x$missing[[1]], fixed = TRUE))
stopifnot(x$suggestion[[1]] == "farm*")

x2 <- search_morphology_checks("salmon AND farm*")
stopifnot(nrow(x2) == 0L)

x3 <- search_morphology_checks('salmon AND "fish farming"')
stopifnot(nrow(x3) == 0L)

cat("Morphology and truncation checks passed.\n")
