morphology_inflections <- function(base) {
  base <- tolower(trimws(base))
  if (!grepl("^[a-z]{3,}$", base)) return(character())

  plural <- if (grepl("(s|x|z|ch|sh)$", base)) {
    paste0(base, "es")
  } else if (grepl("[^aeiou]y$", base)) {
    paste0(substr(base, 1, nchar(base) - 1L), "ies")
  } else {
    paste0(base, "s")
  }

  past <- if (grepl("e$", base)) {
    paste0(base, "d")
  } else if (grepl("[^aeiou]y$", base)) {
    paste0(substr(base, 1, nchar(base) - 1L), "ied")
  } else {
    paste0(base, "ed")
  }

  gerund <- if (grepl("e$", base) && !grepl("ee$", base)) {
    paste0(substr(base, 1, nchar(base) - 1L), "ing")
  } else {
    paste0(base, "ing")
  }

  unique(c(base, plural, past, gerund))
}

search_morphology_checks <- function(query) {
  tokens <- tokenise_boolean(query)
  tokens <- tokens[
    !vapply(tokens, is_operator, logical(1)) &
      !vapply(tokens, is_proximity_operator, logical(1)) &
      !tokens %in% c("(", ")")
  ]

  tokens <- tolower(trimws(gsub('^"|"$', "", tokens)))
  single <- tokens[nzchar(tokens) & !grepl("\\s", tokens)]
  if (!length(single)) return(data.frame())

  wildcard_tokens <- single[grepl("[*?]", single)]
  plain <- unique(gsub("[*?].*$", "", single[!grepl("[*?]", single)]))
  plain <- plain[grepl("^[a-z]{3,}$", plain)]
  if (!length(plain)) return(data.frame())

  stems <- SnowballC::wordStem(plain, language = "english")
  groups <- split(plain, stems)
  rows <- list()

  for (stem in names(groups)) {
    present <- unique(groups[[stem]])

    # Only make deterministic family suggestions when the stem itself is
    # explicitly present, or two or more explicit forms share the stem.
    if (!(stem %in% present) && length(present) < 2L) next

    wildcard_present <- any(startsWith(wildcard_tokens, paste0(stem, "*"))) ||
      any(grepl(paste0("^", stem, "\\*$"), wildcard_tokens))
    if (wildcard_present) next

    forms <- morphology_inflections(stem)
    if (!length(forms)) next

    missing <- setdiff(forms, present)
    if (!length(missing)) next

    rows[[length(rows) + 1L]] <- data.frame(
      family = stem,
      present = paste(sort(present), collapse = ", "),
      missing = paste(sort(missing), collapse = ", "),
      suggestion = paste0(stem, "*"),
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
