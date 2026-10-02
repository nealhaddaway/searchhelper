parse_ris <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\\r$", "", lines)
  records <- list()
  current <- list()

  flush_record <- function(rec) {
    if (!length(rec)) return(NULL)
    get1 <- function(tag) {
      x <- rec[[tag]]
      if (is.null(x) || !length(x)) return(NA_character_)
      paste(x, collapse = "; ")
    }
    year <- get1("PY")
    if (is.na(year)) year <- get1("Y1")
    data.frame(
      ris_id = length(records) + 1L,
      title = coalesce_chr(get1("TI"), get1("T1")),
      authors = get1("AU"),
      year = year,
      doi = normalise_doi(coalesce_chr(get1("DO"), get1("M3"))),
      pmid = extract_pmid(rec),
      stringsAsFactors = FALSE
    )
  }

  for (line in lines) {
    m <- regexec("^([A-Z0-9]{2})  - ?(.*)$", line)
    hit <- regmatches(line, m)[[1]]
    if (!length(hit)) next
    tag <- hit[2]
    val <- trimws(hit[3])

    if (tag == "TY" && length(current)) {
      records[[length(records) + 1L]] <- flush_record(current)
      current <- list()
    }
    current[[tag]] <- c(current[[tag]], val)
    if (tag == "ER") {
      records[[length(records) + 1L]] <- flush_record(current)
      current <- list()
    }
  }
  if (length(current)) records[[length(records) + 1L]] <- flush_record(current)
  records <- Filter(Negate(is.null), records)
  if (!length(records)) stop("No RIS records could be parsed.")
  out <- do.call(rbind, records)
  out$ris_id <- seq_len(nrow(out))
  out
}

coalesce_chr <- function(a, b) if (!is.na(a) && nzchar(a)) a else b

normalise_doi <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- tolower(trimws(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x <- sub("[[:space:][:punct:]]+$", "", x)
  if (!grepl("^10\\.[0-9]{4,9}/", x)) return(NA_character_)
  x
}

extract_pmid <- function(rec) {
  vals <- unlist(rec[c("AN", "N1", "UR")], use.names = FALSE)
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NA_character_)
  txt <- paste(vals, collapse = " ")
  m <- regexpr("(?i)(PMID[: ]+|pubmed\\.ncbi\\.nlm\\.nih\\.gov/)([0-9]{5,9})", txt, perl = TRUE)
  if (m[1] < 0) return(NA_character_)
  hit <- regmatches(txt, m)
  sub(".*?([0-9]{5,9}).*", "\\1", hit)
}
