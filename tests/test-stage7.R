source("R/audit.R")

events <- empty_audit_events()
events <- append_audit_event(
  events,
  change_type = "Candidate term added",
  search_after = "(salmon* OR mariculture) AND aquacultur*",
  term = "mariculture",
  target_substring = "Population",
  syntax = "mariculture",
  incremental_recovery = 2,
  included_prevalence = 0.7,
  excluded_prevalence = 0.1,
  log2_enrichment = 2.5,
  coverage_before = 0.5,
  coverage_after = 0.7
)

stopifnot(nrow(events) == 1L)
stopifnot(events$revision == 1L)
stopifnot(events$incremental_recovery == 2L)

analysed <- data.frame(
  is_benchmark = c(TRUE, FALSE, FALSE, FALSE),
  search_match = c(TRUE, TRUE, FALSE, TRUE)
)
m <- coverage_metrics(analysed)
stopifnot(m$captured == 2L)
stopifnot(m$total == 3L)
stopifnot(abs(m$proportion - 2 / 3) < 1e-10)

html <- render_audit_html(
  starting_search = "salmon* AND aquacultur*",
  final_search = "(salmon* OR mariculture) AND aquacultur*",
  benchmark_source = "Concept-first screening",
  benchmark_count = 20,
  backward_count = 50,
  forward_count = 25,
  baseline_coverage = 0.5,
  final_coverage = 0.7,
  events = events
)

stopifnot(grepl("Final search string", html, fixed = TRUE))
stopifnot(grepl("mariculture", html, fixed = TRUE))
stopifnot(grepl("70.0%", html, fixed = TRUE))

cat("Stage 7 audit tests passed.\n")
