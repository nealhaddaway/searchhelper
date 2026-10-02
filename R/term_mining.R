english_stopwords <- function() {
  core <- c(
    "a","about","above","after","again","against","all","am","an","and","any","are","as","at",
    "be","because","been","before","being","below","between","both","but","by","can","could",
    "did","do","does","doing","down","during","each","few","for","from","further","had","has",
    "have","having","he","her","here","hers","herself","him","himself","his","how","i","if",
    "in","into","is","it","its","itself","just","may","me","might","more","most","my","myself",
    "no","nor","not","now","of","off","on","once","only","or","other","our","ours","ourselves",
    "out","over","own","same","she","should","so","some","such","than","that","the","their",
    "theirs","them","themselves","then","there","these","they","this","those","through","to",
    "too","under","until","up","very","was","we","were","what","when","where","which","while",
    "who","whom","why","will","with","would","you","your","yours","yourself","yourselves"
  )

  # Broader low-information English terms commonly covered by SMART/Snowball-style
  # stopword lexicons. Kept explicit here to avoid adding a runtime package dependency.
  extended <- c(
    "almost","already","also","although","always","among","amongst","another","around",
    "became","become","becomes","becoming","beside","besides","beyond","cannot",
    "concerning","consequently","considering","despite","else","elsewhere","enough",
    "especially","etc","ever","every","everybody","everyone","everything","everywhere",
    "except","however","indeed","instead","later","least","less","many","meanwhile",
    "moreover","mostly","much","neither","never","nevertheless","next","often","otherwise",
    "perhaps","quite","rather","really","several","since","sometimes","still","thereafter",
    "thereby","therefore","though","throughout","thus","together","toward","towards",
    "unless","upon","via","whatever","whenever","whereas","whereby","wherever","whether",
    "within","without","yet"
  )

  unique(c(core, extended))
}

plain_search_terms <- function(query) {
  tokens <- tokenise_boolean(query)
  tokens <- tokens[
    !vapply(tokens, is_operator, logical(1)) &
      !vapply(tokens, is_proximity_operator, logical(1)) &
      !tokens %in% c("(", ")")
  ]

  tokens <- gsub('^"|"$', "", tokens)
  tokens <- unlist(strsplit(tokens, "\\s+"), use.names = FALSE)
  tokens <- tolower(tokens)
  tokens <- gsub("[*?]+$", "", tokens)
  tokens <- gsub("[^[:alnum:]_-]+", "", tokens)
  unique(tokens[nzchar(tokens)])
}

candidate_tokens <- function(text, include_bigrams = TRUE) {
  if (is.na(text) || !nzchar(text)) return(character())

  text <- tolower(text)
  text <- gsub("[^[:alnum:]-]+", " ", text)
  raw <- unlist(strsplit(text, "\\s+"), use.names = FALSE)
  raw <- raw[nzchar(raw)]

  stop <- english_stopwords()
  content_keep <- nchar(raw) >= 3 &
    !raw %in% stop &
    !grepl("^[0-9]+$", raw)
  unigrams <- raw[content_keep]

  if (!include_bigrams || length(raw) < 2L) return(unigrams)

  left <- head(raw, -1)
  right <- tail(raw, -1)
  valid_bigram <- nchar(left) >= 2 &
    nchar(right) >= 2 &
    !grepl("^[0-9]+$", left) &
    !grepl("^[0-9]+$", right) &
    !left %in% stop &
    !right %in% stop

  bigrams <- paste(left[valid_bigram], right[valid_bigram])
  c(unigrams, bigrams)
}

is_already_represented <- function(candidate, search_terms) {
  if (!length(search_terms)) return(FALSE)
  words <- unlist(strsplit(candidate, "\\s+"), use.names = FALSE)
  any(vapply(words, function(word) {
    any(vapply(search_terms, function(term) {
      identical(word, term) || startsWith(word, term)
    }, logical(1)))
  }, logical(1)))
}

mine_candidate_terms <- function(records, query = "", top_n = 200L) {
  if (!nrow(records)) return(data.frame())

  search_terms <- plain_search_terms(query)
  pieces <- list()
  fields <- intersect(c("title", "abstract", "keywords"), names(records))

  for (i in seq_len(nrow(records))) {
    for (field in fields) {
      value <- records[[field]][i]
      terms <- candidate_tokens(value, include_bigrams = TRUE)
      if (!length(terms)) next

      tab <- table(terms)
      piece <- data.frame(
        candidate = names(tab),
        occurrences = as.integer(tab),
        record_id = i,
        field = field,
        stringsAsFactors = FALSE
      )
      pieces[[length(pieces) + 1L]] <- piece
    }
  }

  if (!length(pieces)) return(data.frame())
  all_terms <- do.call(rbind, pieces)

  if (length(search_terms)) {
    represented <- vapply(
      all_terms$candidate,
      is_already_represented,
      logical(1),
      search_terms = search_terms
    )
    all_terms <- all_terms[!represented, , drop = FALSE]
  }

  if (!nrow(all_terms)) return(data.frame())

  occ <- aggregate(occurrences ~ candidate, all_terms, sum)
  docs <- aggregate(record_id ~ candidate, all_terms, function(x) length(unique(x)))
  names(docs)[2] <- "n_records"

  kw <- all_terms[all_terms$field == "keywords", , drop = FALSE]
  if (nrow(kw)) {
    kw_docs <- aggregate(record_id ~ candidate, kw, function(x) length(unique(x)))
    names(kw_docs)[2] <- "keyword_records"
  } else {
    kw_docs <- data.frame(candidate = character(), keyword_records = integer())
  }

  out <- merge(occ, docs, by = "candidate", all = TRUE)
  out <- merge(out, kw_docs, by = "candidate", all.x = TRUE)
  out$keyword_records[is.na(out$keyword_records)] <- 0L
  out$type <- ifelse(grepl(" ", out$candidate, fixed = TRUE), "phrase", "term")

  out <- out[order(-out$n_records, -out$keyword_records, -out$occurrences, out$candidate), ]
  rownames(out) <- NULL
  head(out, as.integer(top_n))
}


candidate_present_in_records <- function(records, candidate, type = NULL) {
  if (is.null(records) || !nrow(records)) return(logical())

  if (is.null(type)) {
    type <- if (grepl(" ", candidate, fixed = TRUE)) "phrase" else "term"
  }

  fields <- intersect(c("title", "abstract", "keywords"), names(records))
  if (!length(fields)) return(rep(FALSE, nrow(records)))

  text <- apply(records[, fields, drop = FALSE], 1, function(x) {
    x <- x[!is.na(x) & nzchar(x)]
    paste(x, collapse = " ")
  })

  token <- if (identical(type, "phrase")) paste0('"', candidate, '"') else candidate
  vapply(
    text,
    function(x) leaf_match(token, normalise_doc_tokens(x)),
    logical(1)
  )
}

rank_discriminative_candidates <- function(
  missed_records,
  included_records,
  excluded_records,
  query = "",
  top_n = 200L,
  prior = 0.5
) {
  base <- mine_candidate_terms(missed_records, query = query, top_n = max(as.integer(top_n), 500L))
  if (!nrow(base)) return(base)

  n_inc <- if (is.null(included_records)) 0L else nrow(included_records)
  n_exc <- if (is.null(excluded_records)) 0L else nrow(excluded_records)

  stats <- lapply(seq_len(nrow(base)), function(i) {
    candidate <- base$candidate[i]
    type <- base$type[i]

    inc_present <- candidate_present_in_records(included_records, candidate, type)
    exc_present <- candidate_present_in_records(excluded_records, candidate, type)

    inc_n <- sum(inc_present, na.rm = TRUE)
    exc_n <- sum(exc_present, na.rm = TRUE)

    inc_prev <- if (n_inc > 0) inc_n / n_inc else NA_real_
    exc_prev <- if (n_exc > 0) exc_n / n_exc else NA_real_

    # Jeffreys-style smoothing avoids infinite ratios when a term is absent
    # from one group while remaining easy to interpret.
    inc_smoothed <- if (n_inc > 0) (inc_n + prior) / (n_inc + 2 * prior) else NA_real_
    exc_smoothed <- if (n_exc > 0) (exc_n + prior) / (n_exc + 2 * prior) else NA_real_

    enrichment <- if (is.finite(inc_smoothed) && is.finite(exc_smoothed)) {
      log2(inc_smoothed / exc_smoothed)
    } else {
      NA_real_
    }

    data.frame(
      candidate = candidate,
      included_records = inc_n,
      included_prevalence = inc_prev,
      excluded_records = exc_n,
      excluded_prevalence = exc_prev,
      log2_enrichment = enrichment,
      stringsAsFactors = FALSE
    )
  })

  stats <- do.call(rbind, stats)
  out <- merge(base, stats, by = "candidate", all.x = TRUE, sort = FALSE)

  # Keep the original missed-record coverage as the explicit incremental gain.
  out$missed_gain <- out$n_records
  out$discrimination_available <- n_inc > 0L && n_exc > 0L

  if (n_inc > 0L && n_exc > 0L) {
    out <- out[
      order(
        -out$log2_enrichment,
        -out$missed_gain,
        -out$keyword_records,
        -out$occurrences,
        out$candidate,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
  } else {
    out <- out[
      order(
        -out$missed_gain,
        -out$keyword_records,
        -out$occurrences,
        out$candidate
      ),
      ,
      drop = FALSE
    ]
  }

  rownames(out) <- NULL
  head(out, as.integer(top_n))
}
