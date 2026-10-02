source("R/boolean_match.R")
source("R/term_mining.R")

missed <- data.frame(
  title = c(
    "Atlantic salmon mariculture field trial",
    "Salmon mariculture production",
    "Rainbow trout aquaculture field study"
  ),
  abstract = c("", "", ""),
  keywords = c("mariculture; salmon", "mariculture", "aquaculture"),
  stringsAsFactors = FALSE
)

included <- data.frame(
  title = c(
    "Atlantic salmon mariculture",
    "Salmon mariculture systems",
    "Marine salmon farming"
  ),
  abstract = c("", "", ""),
  keywords = c("mariculture", "mariculture", "salmon"),
  stringsAsFactors = FALSE
)

excluded <- data.frame(
  title = c(
    "Wild salmon migration",
    "Freshwater ecology",
    "Fish population genetics"
  ),
  abstract = c("", "", ""),
  keywords = c("migration", "ecology", "genetics"),
  stringsAsFactors = FALSE
)

ranked <- rank_discriminative_candidates(
  missed_records = missed,
  included_records = included,
  excluded_records = excluded,
  query = "salmon*"
)

stopifnot(nrow(ranked) > 0)
stopifnot(all(c(
  "included_prevalence",
  "excluded_prevalence",
  "log2_enrichment",
  "missed_gain"
) %in% names(ranked)))

mariculture <- ranked[ranked$candidate == "mariculture", , drop = FALSE]
stopifnot(nrow(mariculture) == 1L)
stopifnot(mariculture$included_records == 2L)
stopifnot(mariculture$excluded_records == 0L)
stopifnot(mariculture$missed_gain == 2L)
stopifnot(mariculture$log2_enrichment > 0)

fallback <- rank_discriminative_candidates(
  missed_records = missed,
  included_records = included,
  excluded_records = excluded[0, , drop = FALSE],
  query = "salmon*"
)
stopifnot(nrow(fallback) > 0)
stopifnot(!fallback$discrimination_available[1])

cat("Stage 6 discriminative ranking tests passed.\n")
