parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl('Candidate terms for your search', app_text, fixed = TRUE))
stopifnot(grepl('Identifying candidate terms', app_text, fixed = TRUE))
stopifnot(grepl('run_analysis(query, refresh_blocks = TRUE)', app_text, fixed = TRUE))
stopifnot(grepl('Reidentify candidate terms', app_text, fixed = TRUE))
stopifnot(!grepl('accordion_panel("View citation-chasing records"', app_text, fixed = TRUE))
stopifnot(!grepl('accordion_panel("View missed records"', app_text, fixed = TRUE))
stopifnot(grepl('These terms are mined from citation-chasing records that your current search missed.', app_text, fixed = TRUE))

cat("Candidate-first post-citation flow tests passed.\n")
