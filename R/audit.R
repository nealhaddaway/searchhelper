empty_audit_events <- function() {
  data.frame(
    revision = integer(),
    change_type = character(),
    term = character(),
    target_substring = character(),
    syntax = character(),
    incremental_recovery = integer(),
    included_prevalence = numeric(),
    excluded_prevalence = numeric(),
    log2_enrichment = numeric(),
    coverage_before = numeric(),
    coverage_after = numeric(),
    search_after = character(),
    stringsAsFactors = FALSE
  )
}

coverage_metrics <- function(analysed) {
  if (is.null(analysed) || !nrow(analysed)) {
    return(list(captured = 0L, total = 0L, proportion = NA_real_))
  }

  eligible <- !analysed$is_benchmark
  total <- sum(eligible, na.rm = TRUE)
  captured <- sum(analysed$search_match & eligible, na.rm = TRUE)

  list(
    captured = as.integer(captured),
    total = as.integer(total),
    proportion = if (total > 0) captured / total else NA_real_
  )
}

append_audit_event <- function(
  events,
  change_type,
  search_after,
  term = "",
  target_substring = "",
  syntax = "",
  incremental_recovery = NA_integer_,
  included_prevalence = NA_real_,
  excluded_prevalence = NA_real_,
  log2_enrichment = NA_real_,
  coverage_before = NA_real_,
  coverage_after = NA_real_
) {
  if (is.null(events) || !nrow(events)) events <- empty_audit_events()

  row <- data.frame(
    revision = nrow(events) + 1L,
    change_type = as.character(change_type),
    term = as.character(term),
    target_substring = as.character(target_substring),
    syntax = as.character(syntax),
    incremental_recovery = as.integer(incremental_recovery),
    included_prevalence = as.numeric(included_prevalence),
    excluded_prevalence = as.numeric(excluded_prevalence),
    log2_enrichment = as.numeric(log2_enrichment),
    coverage_before = as.numeric(coverage_before),
    coverage_after = as.numeric(coverage_after),
    search_after = as.character(search_after),
    stringsAsFactors = FALSE
  )

  rbind(events, row)
}

html_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

fmt_pct <- function(x) {
  ifelse(is.na(x), "Not available", sprintf("%.1f%%", 100 * x))
}

render_audit_html <- function(
  starting_search,
  final_search,
  benchmark_source,
  benchmark_count,
  backward_count,
  forward_count,
  baseline_coverage,
  final_coverage,
  events
) {
  starting_search <- html_escape(starting_search)
  final_search <- html_escape(final_search)
  benchmark_source <- html_escape(benchmark_source)

  event_html <- if (is.null(events) || !nrow(events)) {
    "<p>No accepted search revisions were recorded after the baseline search.</p>"
  } else {
    rows <- vapply(seq_len(nrow(events)), function(i) {
      e <- events[i, , drop = FALSE]

      detail <- if (identical(e$change_type, "Candidate term added")) {
        paste0(
          "<strong>", html_escape(e$term), "</strong> added to ",
          html_escape(e$target_substring),
          if (nzchar(e$syntax)) paste0(" as <code>", html_escape(e$syntax), "</code>") else "",
          "."
        )
      } else {
        html_escape(e$change_type)
      }

      evidence <- character()
      if (!is.na(e$incremental_recovery)) {
        evidence <- c(evidence, paste0("Incremental citation-set recovery: ", e$incremental_recovery, " record(s)"))
      }
      if (!is.na(e$included_prevalence)) {
        evidence <- c(evidence, paste0("Included-record prevalence: ", fmt_pct(e$included_prevalence)))
      }
      if (!is.na(e$excluded_prevalence)) {
        evidence <- c(evidence, paste0("Excluded-record prevalence: ", fmt_pct(e$excluded_prevalence)))
      }
      if (!is.na(e$log2_enrichment)) {
        evidence <- c(evidence, paste0("log2 enrichment: ", sprintf("%.2f", e$log2_enrichment)))
      }
      if (!is.na(e$coverage_before) && !is.na(e$coverage_after)) {
        evidence <- c(
          evidence,
          paste0(
            "Citation-set coverage: ", fmt_pct(e$coverage_before),
            " → ", fmt_pct(e$coverage_after)
          )
        )
      }

      paste0(
        "<li><p>", detail, "</p>",
        if (length(evidence)) paste0("<p class='muted'>", paste(evidence, collapse = " · "), "</p>") else "",
        "<details><summary>Search after this revision</summary><pre>",
        html_escape(e$search_after),
        "</pre></details></li>"
      )
    }, character(1))

    paste0("<ol>", paste(rows, collapse = "\n"), "</ol>")
  }

  paste0(
'<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Search Helper audit</title>
<style>
body{font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;max-width:960px;margin:40px auto;padding:0 24px;line-height:1.55;color:#1f2937}
h1,h2{line-height:1.2}
pre{white-space:pre-wrap;word-break:break-word;background:#f3f4f6;padding:16px;border-radius:8px;border:1px solid #e5e7eb}
code{background:#f3f4f6;padding:2px 5px;border-radius:4px}
.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:12px;margin:16px 0 28px}
.kpi{border:1px solid #e5e7eb;border-radius:8px;padding:12px}
.kpi strong{display:block;font-size:1.35rem}
.muted{color:#6b7280;font-size:.95rem}
li{margin-bottom:20px}
details{margin-top:8px}
</style>
</head>
<body>
<h1>Search Helper audit</h1>
<p class="muted">Human-readable record of the search-development process.</p>

<h2>Final search string</h2>
<pre>', final_search, '</pre>

<h2>Summary</h2>
<div class="grid">
<div class="kpi"><strong>', benchmark_count, '</strong>benchmark records</div>
<div class="kpi"><strong>', backward_count, '</strong>backward references</div>
<div class="kpi"><strong>', forward_count, '</strong>forward citations</div>
<div class="kpi"><strong>', fmt_pct(baseline_coverage), '</strong>baseline citation-set coverage</div>
<div class="kpi"><strong>', fmt_pct(final_coverage), '</strong>final citation-set coverage</div>
</div>
<p><strong>Benchmark source:</strong> ', benchmark_source, '</p>

<h2>Starting search string</h2>
<pre>', starting_search, '</pre>

<h2>Accepted revisions</h2>
', event_html, '

<h2>Interpretation</h2>
<p>This audit documents how the search string was developed using benchmark records, citation chasing and user-accepted revisions. Coverage figures refer to the non-benchmark citation-chasing records available during this session and should not be interpreted as proof of complete literature retrieval.</p>
</body>
</html>'
  )
}
