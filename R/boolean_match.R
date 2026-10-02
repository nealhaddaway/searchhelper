tokenise_boolean <- function(query) {
  query <- trimws(query)
  if (!nzchar(query)) return(character())

  pattern <- '"(?:[^"\\\\]|\\\\.)*"|\\(|\\)|(?i:\\bAND\\b|\\bOR\\b|\\bNOT\\b)|(?i:\\b(?:NEAR|W|ADJ)/[0-9]+\\b)|[^[:space:]()]+'
  m <- gregexpr(pattern, query, perl = TRUE)
  tokens <- regmatches(query, m)[[1]]
  tokens[nzchar(tokens)]
}

is_operator <- function(x) toupper(x) %in% c("AND", "OR", "NOT")

is_proximity_operator <- function(x) {
  grepl("^(NEAR|W|ADJ)/[0-9]+$", toupper(x), perl = TRUE)
}

boolean_to_rpn <- function(query) {
  tokens <- tokenise_boolean(query)
  if (!length(tokens)) stop("Search string is empty.")

  prox <- tokens[vapply(tokens, is_proximity_operator, logical(1))]
  if (length(prox)) {
    stop(
      "Local coverage checking does not yet evaluate proximity operators (",
      paste(unique(prox), collapse = ", "),
      "). Replace these temporarily with AND or a quoted phrase for coverage analysis."
    )
  }

  precedence <- c("OR" = 1L, "AND" = 2L, "NOT" = 3L)
  output <- character()
  ops <- character()

  for (tok in tokens) {
    up <- toupper(tok)

    if (!is_operator(tok) && tok != "(" && tok != ")") {
      output <- c(output, tok)
      next
    }

    if (is_operator(tok)) {
      while (length(ops)) {
        top <- tail(ops, 1)
        if (!is_operator(top)) break
        p_top <- precedence[[toupper(top)]]
        p_tok <- precedence[[up]]
        left_assoc <- up != "NOT"
        if (p_top > p_tok || (left_assoc && p_top == p_tok)) {
          output <- c(output, top)
          ops <- head(ops, -1)
        } else {
          break
        }
      }
      ops <- c(ops, up)
      next
    }

    if (tok == "(") {
      ops <- c(ops, tok)
      next
    }

    if (tok == ")") {
      found_open <- FALSE
      while (length(ops)) {
        top <- tail(ops, 1)
        ops <- head(ops, -1)
        if (top == "(") {
          found_open <- TRUE
          break
        }
        output <- c(output, top)
      }
      if (!found_open) stop("Unbalanced closing parenthesis in search string.")
    }
  }

  if (any(ops %in% c("(", ")"))) stop("Unbalanced opening parenthesis in search string.")
  c(output, rev(ops))
}

escape_regex <- function(x) {
  gsub("([][{}()+.^$|\\\\])", "\\\\\\1", x, perl = TRUE)
}

wildcard_regex <- function(x) {
  x <- tolower(x)
  x <- escape_regex(x)
  x <- gsub("\\*", ".*", x)
  x <- gsub("\\?", ".", x)
  paste0("^", x, "$")
}

normalise_doc_tokens <- function(text) {
  if (length(text) == 0 || is.na(text) || !nzchar(text)) return(character())
  text <- tolower(text)
  text <- gsub("[^[:alnum:]_-]+", " ", text)
  x <- unlist(strsplit(text, "\\s+"), use.names = FALSE)
  x[nzchar(x)]
}

leaf_match <- function(token, doc_tokens) {
  if (!length(doc_tokens)) return(FALSE)

  quoted <- nchar(token) >= 2 &&
    substr(token, 1, 1) == '"' &&
    substr(token, nchar(token), nchar(token)) == '"'

  if (!quoted) {
    return(any(grepl(wildcard_regex(token), doc_tokens, perl = TRUE)))
  }

  phrase <- substr(token, 2, nchar(token) - 1)
  parts <- unlist(strsplit(tolower(trimws(phrase)), "\\s+"), use.names = FALSE)
  parts <- parts[nzchar(parts)]
  if (!length(parts) || length(doc_tokens) < length(parts)) return(FALSE)

  regexes <- vapply(parts, wildcard_regex, character(1))
  width <- length(parts)
  starts <- seq_len(length(doc_tokens) - width + 1L)

  any(vapply(starts, function(i) {
    segment <- doc_tokens[i:(i + width - 1L)]
    all(mapply(function(rx, value) grepl(rx, value, perl = TRUE), regexes, segment))
  }, logical(1)))
}

evaluate_rpn <- function(rpn, text) {
  doc_tokens <- normalise_doc_tokens(text)
  stack <- logical()

  for (tok in rpn) {
    up <- toupper(tok)

    if (!is_operator(tok)) {
      stack <- c(stack, leaf_match(tok, doc_tokens))
      next
    }

    if (up == "NOT") {
      if (length(stack) < 1L) stop("NOT is missing an operand.")
      value <- tail(stack, 1)
      stack <- head(stack, -1)
      stack <- c(stack, !value)
      next
    }

    if (length(stack) < 2L) stop(up, " is missing an operand.")
    right <- tail(stack, 1)
    stack <- head(stack, -1)
    left <- tail(stack, 1)
    stack <- head(stack, -1)

    stack <- c(
      stack,
      if (up == "AND") left && right else left || right
    )
  }

  if (length(stack) != 1L) {
    stop("Could not parse search string. Check that terms are joined by explicit AND/OR operators.")
  }

  stack[[1]]
}

match_search_records <- function(records, query) {
  rpn <- boolean_to_rpn(query)

  combine <- function(x) {
    vals <- c(x[["title"]], x[["abstract"]], x[["keywords"]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    paste(vals, collapse = " ")
  }

  text <- apply(records[, intersect(c("title", "abstract", "keywords"), names(records)), drop = FALSE],
                1, combine)

  vapply(text, function(x) evaluate_rpn(rpn, x), logical(1))
}
