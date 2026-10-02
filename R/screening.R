init_screening <- function(records) {
  if (is.null(records) || !nrow(records)) return(data.frame())
  out <- records
  out$decision <- NA_character_
  out
}

set_screen_decision <- function(screening, index, decision) {
  stopifnot(decision %in% c("include", "exclude", "unsure"))
  if (is.null(screening) || !nrow(screening)) return(screening)
  if (index < 1L || index > nrow(screening)) stop("Screening index is out of range.")
  screening$decision[index] <- decision
  screening
}

next_unscreened_index <- function(screening, after = 0L) {
  if (is.null(screening) || !nrow(screening)) return(NA_integer_)
  idx <- which(is.na(screening$decision))
  if (!length(idx)) return(NA_integer_)
  later <- idx[idx > after]
  if (length(later)) return(later[1])
  idx[1]
}

screening_summary <- function(screening, target) {
  if (is.null(screening) || !nrow(screening)) {
    return(list(include = 0L, exclude = 0L, unsure = 0L, remaining = 0L, target = target, target_met = FALSE))
  }
  inc <- sum(screening$decision == "include", na.rm = TRUE)
  exc <- sum(screening$decision == "exclude", na.rm = TRUE)
  uns <- sum(screening$decision == "unsure", na.rm = TRUE)
  rem <- sum(is.na(screening$decision))
  list(
    include = inc,
    exclude = exc,
    unsure = uns,
    remaining = rem,
    target = as.integer(target),
    target_met = inc >= as.integer(target)
  )
}

included_benchmarks <- function(screening) {
  if (is.null(screening) || !nrow(screening)) return(data.frame())
  screening[screening$decision == "include", , drop = FALSE]
}
