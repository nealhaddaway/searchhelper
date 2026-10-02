parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl("last_add_result <- reactiveVal(NULL)", app_text, fixed = TRUE))
stopifnot(grepl('uiOutput("candidate_add_feedback")', app_text, fixed = TRUE))
stopifnot(grepl("Citation-set coverage changed from", app_text, fixed = TRUE))
stopifnot(grepl("citation record%s remain missed", app_text, fixed = TRUE))
stopifnot(grepl("Updated substring:", app_text, fixed = TRUE))
stopifnot(grepl("Updated full search:", app_text, fixed = TRUE))
stopifnot(grepl("last_add_result(list(", app_text, fixed = TRUE))

cat("Candidate-add feedback regression test passed.\n")
