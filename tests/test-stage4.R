source("R/boolean_match.R")
source("R/lens_api.R")

q <- '(salmon* OR "rainbow trout") AND (aquacultur* OR farm*)'
translated <- lens_translate_canonical_query(q)

stopifnot(is.list(translated$query_string))
stopifnot(identical(translated$query_string$query, q))
stopifnot(identical(translated$query_string$fields, c("title", "abstract", "keyword")))
stopifnot(identical(translated$query_string$default_operator, "and"))

bad <- try(lens_translate_canonical_query("salmon* NEAR/5 farm*"), silent = TRUE)
stopifnot(inherits(bad, "try-error"))

empty <- try(lens_translate_canonical_query(""), silent = TRUE)
stopifnot(inherits(empty, "try-error"))

cat("Stage 4 Lens query translation tests passed.\n")
