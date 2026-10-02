source("R/ris.R")

tmp <- tempfile(fileext = ".ris")
writeLines(c(
  "TY  - JOUR",
  "TI  - Example benchmark article",
  "AU  - Smith, Jane",
  "PY  - 2024",
  "DO  - https://doi.org/10.1234/EXAMPLE.1",
  "ER  - ",
  "",
  "TY  - JOUR",
  "T1  - Benchmark without DOI",
  "AU  - Jones, Alex",
  "Y1  - 2023/01/01",
  "ER  - "
), tmp)

x <- parse_ris(tmp)
stopifnot(nrow(x) == 2L)
stopifnot(x$title[1] == "Example benchmark article")
stopifnot(x$doi[1] == "10.1234/example.1")
stopifnot(is.na(x$doi[2]))
stopifnot(x$title[2] == "Benchmark without DOI")

parse(file = "app.R")
parse(file = "R/lens_api.R")

cat("Stage 1 parser and syntax tests passed.\n")
