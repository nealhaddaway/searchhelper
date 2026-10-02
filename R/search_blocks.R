rpn_to_ast <- function(rpn) {
  stack <- list()

  for (tok in rpn) {
    up <- toupper(tok)

    if (!is_operator(tok)) {
      stack[[length(stack) + 1L]] <- list(type = "LEAF", value = tok)
      next
    }

    if (up == "NOT") {
      if (length(stack) < 1L) stop("NOT is missing an operand.")
      child <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      stack[[length(stack) + 1L]] <- list(type = "NOT", child = child)
      next
    }

    if (length(stack) < 2L) stop(up, " is missing an operand.")
    right <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    left <- stack[[length(stack)]]
    stack <- stack[-length(stack)]
    stack[[length(stack) + 1L]] <- list(type = up, left = left, right = right)
  }

  if (length(stack) != 1L) stop("Could not build a Boolean expression tree.")
  stack[[1]]
}

ast_to_string <- function(node) {
  if (node$type == "LEAF") return(node$value)
  if (node$type == "NOT") return(paste0("NOT (", ast_to_string(node$child), ")"))
  paste0("(", ast_to_string(node$left), " ", node$type, " ", ast_to_string(node$right), ")")
}

flatten_root_and <- function(node) {
  if (identical(node$type, "AND")) {
    return(c(flatten_root_and(node$left), flatten_root_and(node$right)))
  }
  list(node)
}

split_search_blocks <- function(query) {
  rpn <- boolean_to_rpn(query)
  tree <- rpn_to_ast(rpn)
  nodes <- flatten_root_and(tree)

  data.frame(
    block_id = seq_along(nodes),
    label = paste("Concept", seq_along(nodes)),
    expression = vapply(nodes, ast_to_string, character(1)),
    stringsAsFactors = FALSE
  )
}

rebuild_search_from_blocks <- function(blocks) {
  if (is.null(blocks) || !nrow(blocks)) return("")
  expr <- trimws(blocks$expression)
  expr <- expr[nzchar(expr)]
  if (!length(expr)) return("")
  paste(sprintf("(%s)", expr), collapse = " AND ")
}

strip_redundant_outer_parentheses <- function(expression) {
  x <- trimws(expression)
  if (!nzchar(x)) return(x)

  repeat {
    if (nchar(x) < 2L || substr(x, 1L, 1L) != "(" || substr(x, nchar(x), nchar(x)) != ")") {
      break
    }

    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    depth <- 0L
    in_quote <- FALSE
    escaped <- FALSE
    encloses_all <- TRUE

    for (i in seq_along(chars)) {
      ch <- chars[[i]]

      if (escaped) {
        escaped <- FALSE
        next
      }
      if (ch == "\\" && in_quote) {
        escaped <- TRUE
        next
      }
      if (ch == '"') {
        in_quote <- !in_quote
        next
      }
      if (in_quote) next

      if (ch == "(") depth <- depth + 1L
      if (ch == ")") depth <- depth - 1L

      if (depth == 0L && i < length(chars)) {
        encloses_all <- FALSE
        break
      }
      if (depth < 0L) {
        encloses_all <- FALSE
        break
      }
    }

    if (!encloses_all || depth != 0L || in_quote) break
    x <- trimws(substr(x, 2L, nchar(x) - 1L))
  }

  x
}

add_or_to_block <- function(expression, term) {
  expression <- strip_redundant_outer_parentheses(expression)
  term <- strip_redundant_outer_parentheses(term)

  if (!nzchar(term)) return(expression)
  if (!nzchar(expression)) return(term)

  paste(expression, term, sep = " OR ")
}
