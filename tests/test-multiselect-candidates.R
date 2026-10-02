parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl('selection = "multiple"', app_text, fixed = TRUE))
stopifnot(grepl('Select one or more rows in the table.', app_text, fixed = TRUE))
stopifnot(grepl('Add selected terms to substring', app_text, fixed = TRUE))
stopifnot(grepl('Selected terms are added with OR.', app_text, fixed = TRUE))
stopifnot(grepl('validate(need(length(selected) >= 1L', app_text, fixed = TRUE))
stopifnot(grepl('for (addition in additions)', app_text, fixed = TRUE))
stopifnot(grepl('Rechecking search after adding selected terms', app_text, fixed = TRUE))
stopifnot(!grepl('selection = "single"', app_text, fixed = TRUE))

cat("Multi-select candidate-term flow tests passed.\n")
