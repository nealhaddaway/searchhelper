source("R/boolean_match.R")
source("R/search_blocks.R")
source("R/suggestions.R")

blocks <- split_search_blocks(
  '(salmon* OR "rainbow trout") AND (aquacultur* OR farm*) AND (field OR greenhouse)'
)
stopifnot(nrow(blocks) == 3L)

# A root-level OR must not be incorrectly split at an internal AND.
one_block <- split_search_blocks('(salmon* AND farm*) OR aquacultur*')
stopifnot(nrow(one_block) == 1L)

rebuilt <- rebuild_search_from_blocks(blocks)
records <- data.frame(
  title = c(
    "Salmon aquaculture field experiment",
    "Rainbow trout farm greenhouse study"
  ),
  abstract = c("", ""),
  keywords = c("", ""),
  stringsAsFactors = FALSE
)
stopifnot(all(match_search_records(records, rebuilt)))

added <- add_or_to_block("(aquacultur* OR farm*)", "maricultur*")
stopifnot(grepl("maricultur\\*", added))

placement_blocks <- data.frame(
  block_id = 1:2,
  label = c("Population", "Intervention or exposure"),
  expression = c("salmon*", "aquacultur*"),
  stringsAsFactors = FALSE
)

missed <- data.frame(
  title = c(
    "Salmon mariculture systems",
    "Atlantic salmon mariculture production",
    "Salmon aquaculture systems"
  ),
  abstract = c("", "", ""),
  keywords = c("", "", ""),
  stringsAsFactors = FALSE
)

scores <- score_candidate_blocks(
  missed,
  placement_blocks,
  candidate = "mariculture",
  type = "term"
)
stopifnot(suggest_block_id(scores) == 2L)

forms <- candidate_forms("field study", "phrase")
stopifnot(any(grepl('"field study"', forms, fixed = TRUE)))
stopifnot(any(grepl("field AND study", forms, fixed = TRUE)))

advice <- proximity_advice("field study", "phrase")
stopifnot(!is.null(advice))
stopifnot(grepl("NEAR/3", advice, fixed = TRUE))

cat("Stage 3 search-block and candidate-placement tests passed.\n")
