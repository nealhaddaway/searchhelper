parse(file = "app.R")
source("R/audit.R")

app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
stopifnot(grepl("Your improved search string", app_text, fixed = TRUE))
stopifnot(grepl("Start with concepts", app_text, fixed = TRUE))
stopifnot(grepl("Start with benchmark records", app_text, fixed = TRUE))
stopifnot(!grepl("Stage 4 ·", app_text, fixed = TRUE))
stopifnot(!grepl("Stage 1 ·", app_text, fixed = TRUE))

deploy_text <- paste(readLines("DEPLOYMENT.md", warn = FALSE), collapse = "\n")
stopifnot(grepl("LENS_API_TOKEN", deploy_text, fixed = TRUE))
stopifnot(grepl("manifest.json", deploy_text, fixed = TRUE))

cat("Stage 8 UI and deployment-preparation tests passed.\n")


app_text <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
stopifnot(grepl('class = "search-block border rounded p-3 mb-3"', app_text, fixed = TRUE))
stopifnot(!grepl('card_header(sprintf("Substring %d", i))', app_text, fixed = TRUE))
stopifnot(grepl('Check citation coverage', app_text, fixed = TRUE))
stopifnot(grepl('Citation coverage becomes available', app_text, fixed = TRUE))
stopifnot(grepl('citation-chased', app_text, fixed = TRUE))
stopifnot(grepl('class = "search-output"', app_text, fixed = TRUE))
cat("Connect Cloud UI hotfix tests passed.\n")
