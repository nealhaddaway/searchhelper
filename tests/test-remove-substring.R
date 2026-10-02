app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

stopifnot(grepl('actionButton("add_block", "Add empty substring"', app_text, fixed = TRUE))
stopifnot(grepl('uiOutput("remove_block_control")', app_text, fixed = TRUE))
stopifnot(grepl('actionButton(\n        "remove_block"', app_text))
stopifnot(grepl('observeEvent(input$remove_block', app_text, fixed = TRUE))
stopifnot(grepl('change_type = "Substring removed"', app_text, fixed = TRUE))
stopifnot(grepl('nrow(b) > 1L', app_text, fixed = TRUE))
stopifnot(grepl('run_analysis(query, refresh_blocks = FALSE)', app_text, fixed = TRUE))

cat("Removable substring UI and behaviour checks passed.\n")
