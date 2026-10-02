parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl('class = "candidate-table-section border rounded p-3 mb-3"', app_text, fixed = TRUE))
stopifnot(grepl('class = "candidate-action-section"', app_text, fixed = TRUE))

candidate_start <- regexpr('tags$div(\n        class = "candidate-table-section', app_text, fixed = TRUE)[1]
candidate_action <- regexpr('uiOutput("candidate_action")', app_text, fixed = TRUE)[1]
candidate_end <- regexpr('class = "candidate-action-section"', app_text, fixed = TRUE)[1]

stopifnot(candidate_start > 0, candidate_end > candidate_start, candidate_action > candidate_end)
stopifnot(grepl('.candidate-action-section { position: relative; display: block; clear: both;', app_text, fixed = TRUE))

cat("Candidate action layout regression test passed.\n")
