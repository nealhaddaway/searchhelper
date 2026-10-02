source("R/screening.R")

x <- data.frame(
  lens_id = c("A", "B", "C"),
  title = c("One", "Two", "Three"),
  stringsAsFactors = FALSE
)

s <- init_screening(x)
stopifnot(all(is.na(s$decision)))

s <- set_screen_decision(s, 1, "include")
s <- set_screen_decision(s, 2, "exclude")

summary <- screening_summary(s, target = 1)
stopifnot(summary$include == 1L)
stopifnot(summary$exclude == 1L)
stopifnot(summary$remaining == 1L)
stopifnot(summary$target_met)

stopifnot(next_unscreened_index(s, after = 1L) == 3L)

inc <- included_benchmarks(s)
stopifnot(nrow(inc) == 1L)
stopifnot(inc$lens_id == "A")

cat("Stage 5 screening-state tests passed.\n")
