app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

start <- regexpr("observeEvent\\(input\\$add_external_suggestions", app_text)
stopifnot(start[[1]] > 0L)

tail_text <- substring(app_text, start[[1]])
end <- regexpr("output\\$candidate_terms_status", tail_text)
stopifnot(end[[1]] > 0L)

handler <- substring(tail_text, 1L, end[[1]] - 1L)
stopifnot(grepl("before_metrics <- coverage_metrics", handler, fixed = TRUE))
stopifnot(grepl("after_metrics <- coverage_metrics", handler, fixed = TRUE))
stopifnot(grepl("last_add_result(list(", handler, fixed = TRUE))
stopifnot(grepl('change_type = "External lexical suggestion added"', handler, fixed = TRUE))

stopifnot(grepl('card_header("Morphology and truncation check")', app_text, fixed = TRUE))
stopifnot(grepl('uiOutput("morphology_check")', app_text, fixed = TRUE))
stopifnot(grepl('source("R/morphology_check.R")', app_text, fixed = TRUE))

cat("External suggestion feedback refresh and morphology UI checks passed.\n")
