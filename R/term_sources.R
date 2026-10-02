search_seed_terms <- function(query) {
  tokens <- tokenise_boolean(query)
  tokens <- tokens[
    !vapply(tokens, is_operator, logical(1)) &
      !vapply(tokens, is_proximity_operator, logical(1)) &
      !tokens %in% c("(", ")")
  ]

  tokens <- gsub('^"|"$', "", tokens)
  tokens <- trimws(tokens)
  tokens <- gsub("[*?]+$", "", tokens)
  tokens <- tolower(tokens)
  unique(tokens[nzchar(tokens)])
}

datamuse_words <- function(params, max_results = 50L) {
  req <- httr2::request("https://api.datamuse.com/words")
  params$max <- as.integer(max_results)
  req <- do.call(httr2::req_url_query, c(list(req), params))
  resp <- httr2::req_perform(
    httr2::req_retry(req, max_tries = 3)
  )

  if (httr2::resp_status(resp) >= 400L) return(data.frame())

  x <- jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyDataFrame = TRUE
  )
  if (is.null(x) || !nrow(x) || !"word" %in% names(x)) return(data.frame())

  x
}

datamuse_morphological_variants <- function(seed, max_results = 30L) {
  seed <- tolower(trimws(seed))
  if (!nzchar(seed) || grepl("\\s", seed)) return(character())

  stem <- SnowballC::wordStem(seed, language = "english")
  if (!nzchar(stem)) return(character())

  x <- datamuse_words(
    list(sp = paste0(stem, "*")),
    max_results = max(100L, as.integer(max_results))
  )
  if (!nrow(x)) return(character())

  words <- tolower(trimws(as.character(x$word)))
  words <- words[
    nzchar(words) &
      !grepl("\\s", words) &
      !words %in% english_stopwords()
  ]
  if (!length(words)) return(character())

  same_stem <- SnowballC::wordStem(words, language = "english") == stem
  words <- unique(words[same_stem & words != seed])
  head(words, as.integer(max_results))
}

datamuse_synonyms <- function(seed, max_results = 30L) {
  seed <- tolower(trimws(seed))
  if (!nzchar(seed)) return(character())

  x <- datamuse_words(
    list(rel_syn = seed),
    max_results = max_results
  )
  if (!nrow(x)) return(character())

  words <- tolower(trimws(as.character(x$word)))
  words <- words[nzchar(words) & words != seed]
  unique(words)
}

expand_search_terms <- function(query, max_per_seed = 20L) {
  seeds <- search_seed_terms(query)
  if (!length(seeds)) return(data.frame())

  rows <- list()
  for (seed in seeds) {
    morph <- tryCatch(
      datamuse_morphological_variants(seed, max_results = max_per_seed),
      error = function(e) character()
    )
    if (length(morph)) {
      rows[[length(rows) + 1L]] <- data.frame(
        candidate = morph,
        type = ifelse(grepl(" ", morph, fixed = TRUE), "phrase", "term"),
        seed = seed,
        relation = "morphological variant",
        provider = "Datamuse",
        stringsAsFactors = FALSE
      )
    }

    syn <- tryCatch(
      datamuse_synonyms(seed, max_results = max_per_seed),
      error = function(e) character()
    )
    if (length(syn)) {
      rows[[length(rows) + 1L]] <- data.frame(
        candidate = syn,
        type = ifelse(grepl(" ", syn, fixed = TRUE), "phrase", "term"),
        seed = seed,
        relation = "synonym",
        provider = "Datamuse/WordNet",
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) return(data.frame())

  out <- do.call(rbind, rows)
  out <- out[
    !vapply(
      out$candidate,
      is_already_represented,
      logical(1),
      search_terms = plain_search_terms(query)
    ),
    ,
    drop = FALSE
  ]
  if (!nrow(out)) return(out)

  key <- paste(out$candidate, out$relation, out$provider, out$seed, sep = "\r")
  out <- out[!duplicated(key), , drop = FALSE]
  rownames(out) <- NULL
  out
}

collapse_external_sources <- function(external_terms) {
  if (is.null(external_terms) || !nrow(external_terms)) {
    return(data.frame(
      candidate = character(),
      type = character(),
      external_sources = character(),
      external_seeds = character(),
      stringsAsFactors = FALSE
    ))
  }

  groups <- split(external_terms, external_terms$candidate)
  rows <- lapply(groups, function(g) {
    data.frame(
      candidate = g$candidate[1],
      type = g$type[1],
      external_sources = paste(
        sort(unique(paste(g$relation, g$provider, sep = ": "))),
        collapse = " | "
      ),
      external_seeds = paste(sort(unique(g$seed)), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
