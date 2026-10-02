parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl('route_mode <- reactiveVal(NULL)', app_text, fixed = TRUE))
stopifnot(grepl('Start with a naive search', app_text, fixed = TRUE))
stopifnot(grepl('Upload benchmark records', app_text, fixed = TRUE))
stopifnot(grepl('Reset and choose another route', app_text, fixed = TRUE))
stopifnot(grepl('Use included records for citation chasing', app_text, fixed = TRUE))
stopifnot(!grepl('Benchmark target', app_text, fixed = TRUE))
stopifnot(!grepl('DTOutput("concept_results")', app_text, fixed = TRUE))
stopifnot(grepl('screen them one at a time', app_text, fixed = TRUE))
stopifnot(grepl('run_citation_chase()', app_text, fixed = TRUE))
stopifnot(grepl('if (is.null(citation_set())) return(NULL)', app_text, fixed = TRUE))

cat("Route and one-by-one screening flow tests passed.\n")
