parse(file = "app.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

run_start <- regexpr("run_analysis <- function", app_text, fixed = TRUE)[1]
run_end <- regexpr("observeEvent(input$concept_add_block", app_text, fixed = TRUE)[1]
stopifnot(run_start > 0, run_end > run_start)
run_text <- substr(app_text, run_start, run_end - 1)

stopifnot(!grepl("best_candidate_gain(", run_text, fixed = TRUE))
stopifnot(grepl("missed_gain", run_text, fixed = TRUE))
stopifnot(grepl("score_candidate_blocks(", app_text, fixed = TRUE))

cat("Lazy candidate scoring regression test passed.\n")
