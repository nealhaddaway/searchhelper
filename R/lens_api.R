lens_post <- function(body, token) {
  url <- "https://api.lens.org/scholarly/search"
  resp <- httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    httr2::req_perform()

  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

lens_records <- function(response) {
  x <- response$data
  if (is.null(x)) x <- response$results
  if (is.null(x)) x <- response$scholarly
  if (is.null(x)) return(list())
  x
}

extract_external_id <- function(record, type) {
  ids <- record$external_ids
  if (is.null(ids)) return(NA_character_)
  for (x in ids) {
    if (!is.null(x$type) && tolower(x$type) == tolower(type)) {
      return(as.character(x$value %||% x$id %||% NA_character_))
    }
  }
  NA_character_
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

record_row <- function(r) {
  authors <- r$authors
  author_names <- character()
  if (length(authors)) {
    author_names <- vapply(authors, function(a) {
      as.character(a$display_name %||% paste(na.omit(c(a$first_name, a$last_name)), collapse = " "))
    }, character(1))
  }
  data.frame(
    lens_id = as.character(r$lens_id %||% NA_character_),
    title = as.character(r$title %||% NA_character_),
    year = as.character(r$year_published %||% NA_character_),
    authors = paste(author_names[nzchar(author_names)], collapse = "; "),
    doi = extract_external_id(r, "doi"),
    pmid = extract_external_id(r, "pmid"),
    abstract = as.character(r$abstract %||% NA_character_),
    keywords = paste(as.character(unlist(r$keywords %||% character(), use.names = FALSE)), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

lens_search <- function(query, token, size = 100L, include = NULL) {
  body <- list(query = query, size = as.integer(size))
  if (!is.null(include)) body$include <- include
  lens_records(lens_post(body, token))
}

lens_resolve_benchmarks <- function(ris, token) {
  out <- ris
  out$lens_id <- NA_character_
  out$match_method <- NA_character_

  doi_idx <- which(!is.na(ris$doi) & nzchar(ris$doi))
  if (length(doi_idx)) {
    chunks <- split(ris$doi[doi_idx], ceiling(seq_along(doi_idx) / 100))
    for (chunk in chunks) {
      recs <- lens_search(
        list(terms = list(doi = unname(chunk))), token,
        size = length(chunk),
        include = c("lens_id", "title", "year_published", "external_ids")
      )
      for (r in recs) {
        rr <- record_row(r)
        j <- which(ris$doi == normalise_doi(rr$doi))
        if (length(j)) {
          out$lens_id[j] <- rr$lens_id
          out$match_method[j] <- "doi"
        }
      }
    }
  }

  unresolved <- which(is.na(out$lens_id) & !is.na(ris$pmid) & nzchar(ris$pmid))
  if (length(unresolved)) {
    chunks <- split(ris$pmid[unresolved], ceiling(seq_along(unresolved) / 100))
    for (chunk in chunks) {
      recs <- lens_search(
        list(terms = list(pmid = unname(chunk))), token,
        size = length(chunk),
        include = c("lens_id", "title", "year_published", "external_ids")
      )
      for (r in recs) {
        rr <- record_row(r)
        j <- which(ris$pmid == rr$pmid)
        if (length(j)) {
          out$lens_id[j] <- rr$lens_id
          out$match_method[j] <- "pmid"
        }
      }
    }
  }

  unresolved <- which(is.na(out$lens_id) & !is.na(ris$title) & nzchar(ris$title))
  for (i in unresolved) {
    recs <- lens_search(
      list(match_phrase = list(title = ris$title[i])), token,
      size = 5,
      include = c("lens_id", "title", "year_published", "external_ids")
    )
    if (!length(recs)) next
    candidates <- do.call(rbind, lapply(recs, record_row))
    exact <- which(tolower(trimws(candidates$title)) == tolower(trimws(ris$title[i])))
    if (length(exact)) {
      k <- exact[1]
      if (!is.na(ris$year[i]) && nzchar(ris$year[i])) {
        same_year <- exact[candidates$year[exact] == substr(ris$year[i], 1, 4)]
        if (length(same_year)) k <- same_year[1]
      }
      out$lens_id[i] <- candidates$lens_id[k]
      out$match_method[i] <- "exact title"
    }
  }
  out
}

extract_link_ids <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  unique(unlist(lapply(x, function(z) {
    if (is.character(z)) return(z)
    as.character(z$lens_id %||% NA_character_)
  }), use.names = FALSE))
}

lens_get_citation_links <- function(lens_ids, token) {
  rows <- list()
  chunks <- split(lens_ids, ceiling(seq_along(lens_ids) / 100))
  for (chunk in chunks) {
    recs <- lens_search(
      list(terms = list(lens_id = unname(chunk))), token,
      size = length(chunk),
      include = c("lens_id", "references", "scholarly_citations")
    )
    for (r in recs) {
      source_id <- as.character(r$lens_id)
      back <- extract_link_ids(r$references)
      fwd <- extract_link_ids(r$scholarly_citations)
      if (length(back)) rows[[length(rows) + 1L]] <- data.frame(
        benchmark_lens_id = source_id, cited_lens_id = back, direction = "backward",
        stringsAsFactors = FALSE
      )
      if (length(fwd)) rows[[length(rows) + 1L]] <- data.frame(
        benchmark_lens_id = source_id, cited_lens_id = fwd, direction = "forward",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame(
    benchmark_lens_id = character(), cited_lens_id = character(), direction = character()
  ))
  do.call(rbind, rows)
}

lens_fetch_records <- function(lens_ids, token) {
  lens_ids <- unique(na.omit(lens_ids))
  if (!length(lens_ids)) return(data.frame())
  rows <- list()
  chunks <- split(lens_ids, ceiling(seq_along(lens_ids) / 100))
  for (chunk in chunks) {
    recs <- lens_search(
      list(terms = list(lens_id = unname(chunk))), token,
      size = length(chunk),
      include = c("lens_id", "title", "abstract", "keywords", "authors", "year_published", "external_ids")
    )
    rows <- c(rows, lapply(recs, record_row))
  }
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

merge_citation_metadata <- function(links, meta) {
  if (!nrow(links)) return(data.frame())

  groups <- split(links, links$cited_lens_id)
  agg <- do.call(rbind, lapply(names(groups), function(id) {
    g <- groups[[id]]
    directions <- unique(g$direction)
    data.frame(
      lens_id = id,
      direction = if (length(directions) > 1L) "both" else directions[[1]],
      benchmark_count = length(unique(g$benchmark_lens_id)),
      benchmark_sources = paste(sort(unique(g$benchmark_lens_id)), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))
  rownames(agg) <- NULL

  if (!nrow(meta)) return(agg)
  merge(agg, meta, by = "lens_id", all.x = TRUE, sort = FALSE)
}


lens_translate_canonical_query <- function(query) {
  query <- trimws(query)
  if (!nzchar(query)) stop("Search string is empty.")

  # Reuse the local parser as a syntax gate. This deliberately rejects
  # proximity operators because their semantics are database-specific.
  boolean_to_rpn(query)

  list(
    query_string = list(
      query = query,
      fields = c("title", "abstract", "keyword"),
      default_operator = "and"
    )
  )
}

lens_ranked_search <- function(query, token, size = 500L) {
  size <- as.integer(size)
  if (is.na(size) || size < 1L) stop("size must be a positive integer.")
  size <- min(size, 1000L)

  body <- list(
    query = lens_translate_canonical_query(query),
    size = size,
    sort = list(list(relevance = "desc")),
    stemming = FALSE,
    include = c(
      "lens_id",
      "title",
      "abstract",
      "keywords",
      "authors",
      "year_published",
      "external_ids",
      "scholarly_citations_count",
      "reference_count"
    )
  )

  response <- lens_post(body, token)
  recs <- lens_records(response)

  if (!length(recs)) {
    return(data.frame(
      rank = integer(),
      lens_id = character(),
      title = character(),
      year = character(),
      authors = character(),
      doi = character(),
      pmid = character(),
      abstract = character(),
      keywords = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, lapply(recs, record_row))
  out$rank <- seq_len(nrow(out))
  out <- out[, c("rank", setdiff(names(out), "rank")), drop = FALSE]
  rownames(out) <- NULL
  out
}
