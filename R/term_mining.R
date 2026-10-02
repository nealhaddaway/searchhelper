english_stopwords <- function() {
  c(
    "a","about","above","after","again","against","all","am","an","and","any","are","as","at",
    "be","because","been","before","being","below","between","both","but","by","can","could",
    "did","do","does","doing","down","during","each","few","for","from","further","had","has",
    "have","having","he","her","here","hers","herself","him","himself","his","how","i","if",
    "in","into","is","it","its","itself","just","may","me","might","more","most","my","myself",
    "no","nor","not","now","of","off","on","once","only","or","other","our","ours","ourselves",
    "out","over","own","same","she","should","so","some","such","than","that","the","their",
    "theirs","them","themselves","then","there","these","they","this","those","through","to",
    "too","under","until","up","very","was","we","were","what","when","where","which","while",
    "who","whom","why","will","with","would","you","your","yours","yourself","yourselves",
    "study","studies","result","results","method","methods","using","used","use","effect",
    "effects","data","analysis","based","research","paper","article"
  )
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
  x <- unlist(strsplit(text, "\\s+"), use.names = FALSE)
  stop <- english_stopwords()
  keep <- nzchar(x) &
    nchar(x) >= 3 &
    !x %in% stop &
    !grepl("^[0-9]+$", x)
  x <- x[keep]

  if (!include_bigrams || length(x) < 2L) return(x)
  bigrams <- paste(head(x, -1), tail(x, -1))
  c(x, bigrams)
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
