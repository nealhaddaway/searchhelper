combine_searchable_text <- function(records) {
  fields <- intersect(c("title", "abstract", "keywords"), names(records))
  if (!length(fields) || !nrow(records)) return(character(nrow(records)))

  apply(records[, fields, drop = FALSE], 1, function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    paste(x, collapse = " ")
  })
}

candidate_occurs <- function(records, candidate, type = "term") {
  text <- combine_searchable_text(records)
  token <- if (identical(type, "phrase")) paste0('"', candidate, '"') else candidate
  vapply(text, function(x) leaf_match(token, normalise_doc_tokens(x)), logical(1))
}

score_candidate_blocks <- function(records, blocks, candidate, type = "term") {
  if (is.null(blocks) || !nrow(blocks) || !nrow(records)) return(data.frame())

  occurs <- candidate_occurs(records, candidate, type)
  idx <- which(occurs)
  if (!length(idx)) return(data.frame())

  subset <- records[idx, , drop = FALSE]
  matches <- matrix(NA, nrow = nrow(subset), ncol = nrow(blocks))

  for (j in seq_len(nrow(blocks))) {
    matches[, j] <- tryCatch(
      match_search_records(subset, blocks$expression[j]),
      error = function(e) rep(NA, nrow(subset))
    )
  }

  failures <- !matches
  evaluable <- rowSums(!is.na(matches)) == ncol(matches)
  sole_failure <- rep(FALSE, nrow(matches))
  sole_failure[evaluable] <- rowSums(failures[evaluable, , drop = FALSE]) == 1L

  data.frame(
    block_id = blocks$block_id,
    label = blocks$label,
    candidate_records = length(idx),
    fail_rate = vapply(seq_len(ncol(matches)), function(j) {
      x <- failures[, j]
      if (all(is.na(x))) return(NA_real_)
      mean(x, na.rm = TRUE)
    }, numeric(1)),
    sole_failure_support = vapply(seq_len(ncol(matches)), function(j) {
      sum(sole_failure & failures[, j], na.rm = TRUE)
    }, integer(1)),
    stringsAsFactors = FALSE
  )
}

suggest_block_id <- function(scores) {
  if (is.null(scores) || !nrow(scores)) return(NA_integer_)

  if (any(scores$sole_failure_support > 0, na.rm = TRUE)) {
    best <- which(scores$sole_failure_support == max(scores$sole_failure_support, na.rm = TRUE))
    return(scores$block_id[best[1]])
  }

  if (all(is.na(scores$fail_rate))) return(NA_integer_)
  best <- which(scores$fail_rate == max(scores$fail_rate, na.rm = TRUE))
  scores$block_id[best[1]]
}

suggest_wildcard <- function(term) {
  if (grepl(" ", term, fixed = TRUE)) return(NA_character_)
  if (!requireNamespace("SnowballC", quietly = TRUE)) return(NA_character_)

  stem <- SnowballC::wordStem(tolower(term), language = "en")
  if (is.na(stem) || !nzchar(stem) || nchar(stem) < 5L || identical(stem, tolower(term))) {
    return(NA_character_)
  }

  paste0(stem, "*")
}

candidate_forms <- function(candidate, type = "term") {
  if (identical(type, "phrase")) {
    words <- unlist(strsplit(candidate, "\\s+"), use.names = FALSE)
    out <- c(
      "Exact phrase" = paste0('"', candidate, '"'),
      "Words joined with AND" = paste0("(", paste(words, collapse = " AND "), ")")
    )
    return(out)
  }

  out <- c("Literal term" = candidate)
  wild <- suggest_wildcard(candidate)
  if (!is.na(wild)) {
    out <- c(out, "Wildcard stem candidate" = wild)
  }
  out
}

proximity_advice <- function(candidate, type = "term", distance = 3L) {
  if (!identical(type, "phrase")) return(NULL)
  words <- unlist(strsplit(candidate, "\\s+"), use.names = FALSE)
  if (length(words) != 2L) return(NULL)

  paste0(
    "A proximity formulation may be worth testing where the target database supports it, e.g. ",
    words[1], " NEAR/", as.integer(distance), " ", words[2],
    ". Proximity syntax and directionality must be translated per database."
  )
}


candidate_query_token <- function(candidate, type = "term") {
  if (identical(type, "phrase")) paste0('"', candidate, '"') else candidate
}

candidate_incremental_gain <- function(records, blocks, candidate, type = "term") {
  if (is.null(records) || !nrow(records) || is.null(blocks) || !nrow(blocks)) {
    return(data.frame())
  }

  token <- candidate_query_token(candidate, type)
  rows <- lapply(seq_len(nrow(blocks)), function(i) {
    trial <- blocks
    trial$expression[i] <- add_or_to_block(trial$expression[i], token)
    query <- rebuild_search_from_blocks(trial)

    matched <- tryCatch(
      match_search_records(records, query),
      error = function(e) rep(FALSE, nrow(records))
    )

    data.frame(
      block_id = blocks$block_id[i],
      label = blocks$label[i],
      incremental_gain = sum(matched, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[order(-out$incremental_gain, out$block_id), , drop = FALSE]
}

best_candidate_gain <- function(records, blocks, candidate, type = "term") {
  gains <- candidate_incremental_gain(records, blocks, candidate, type)
  if (!nrow(gains)) {
    return(list(block_id = NA_integer_, label = NA_character_, gain = 0L))
  }

  best <- gains[1, , drop = FALSE]
  list(
    block_id = best$block_id[[1]],
    label = best$label[[1]],
    gain = as.integer(best$incremental_gain[[1]])
  )
}
