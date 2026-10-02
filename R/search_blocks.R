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

add_or_to_block <- function(expression, term) {
  expression <- trimws(expression)
  term <- trimws(term)
  if (!nzchar(term)) return(expression)
  if (!nzchar(expression)) return(term)
  paste0("(", expression, " OR ", term, ")")
}
