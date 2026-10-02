parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

observer_start <- regexpr("observeEvent(input$add_candidate", app_text, fixed = TRUE)[1]
observer_end <- regexpr("observeEvent(input$choose_naive", app_text, fixed = TRUE)[1]
stopifnot(observer_start > 0, observer_end > observer_start)

observer_text <- substr(app_text, observer_start, observer_end - 1)

assign_pos <- regexpr("after_metrics <- coverage_metrics(analysed)", observer_text, fixed = TRUE)[1]
use_pos <- regexpr("after_captured = after_metrics$captured", observer_text, fixed = TRUE)[1]

stopifnot(assign_pos > 0, use_pos > assign_pos)
stopifnot(grepl("after <- after_metrics$proportion", observer_text, fixed = TRUE))

cat("Candidate feedback after_metrics scope regression test passed.\n")
