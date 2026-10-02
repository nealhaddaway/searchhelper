library(shiny)
library(bslib)
library(DT)

source("R/ris.R")
source("R/lens_api.R")
source("R/boolean_match.R")
source("R/term_mining.R")
source("R/search_blocks.R")
source("R/suggestions.R")
source("R/screening.R")
source("R/audit.R")

ui <- page_sidebar(
  title = "Search Helper",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML("
    textarea.form-control { min-height: 130px; resize: vertical; font-size: 1rem; line-height: 1.45; }
    .search-block { background: var(--bs-body-bg); }
    .search-output { white-space: pre-wrap; overflow-wrap: anywhere; font-size: 1rem; line-height: 1.5; background: var(--bs-tertiary-bg); border: 1px solid var(--bs-border-color); border-radius: .5rem; padding: 1rem; }
    .help-note { font-size: .9rem; color: var(--bs-secondary-color); margin-top: .5rem; }
  "))),
  sidebar = sidebar(
    title = "Current search",
    textAreaInput(
      "search_string",
      "Search string",
      rows = 12,
      placeholder = '(concept A OR synonym*) AND ("concept B" OR term)'
    ),
    actionButton("analyse_search", "Check citation coverage", disabled = TRUE),
    uiOutput("search_check_status"),
    hr(),
    tags$p(class = "text-muted small", "Already have known relevant papers? Upload them below in Start with benchmark records.")
  ),

  card(
    card_header("Your improved search string"),
    p("This is the main output. Continue refining it below, or download it when you are satisfied."),
    uiOutput("final_search_display"),
    layout_columns(
      col_widths = c(6, 6),
      downloadButton("download_final_search", "Download search string"),
      downloadButton("download_audit", "Download audit (HTML)")
    ),
    uiOutput("final_search_summary")
  ),

  card(
    card_header("Start with concepts"),
    p("Use this route if you do not already have benchmark papers. Build one or more search substrings, retrieve a relevance-ranked Lens sample, then screen records to create your benchmark set."),
    uiOutput("concept_block_editor"),
    layout_columns(
      col_widths = c(4, 4, 4),
      actionButton("concept_add_block", "Add substring"),
      numericInput("concept_n", "Records to retrieve", value = 500, min = 20, max = 500, step = 20),
      actionButton("concept_search", "Search Lens", class = "btn-primary")
    ),
    uiOutput("concept_query_preview"),
    uiOutput("concept_search_summary"),
    DTOutput("concept_results"),
    tags$hr(),
    layout_columns(
      col_widths = c(4, 8),
      numericInput("benchmark_target", "Benchmark target", value = 20, min = 1, max = 500, step = 1),
      uiOutput("screening_progress")
    ),
    uiOutput("screening_record"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      actionButton("screen_include", "Include", class = "btn-success"),
      actionButton("screen_exclude", "Exclude", class = "btn-danger"),
      actionButton("screen_unsure", "Unsure"),
      actionButton("promote_benchmarks", "Use included as benchmarks", class = "btn-primary")
    )
  ),

  card(
    card_header("Start with benchmark records"),
    p("Use this route if you already have known relevant papers. Upload them as RIS, resolve them in Lens, then use citation chasing to test and improve your search."),
    fileInput("ris", "Benchmark records (RIS)", accept = c(".ris", ".txt")),
    actionButton("resolve", "Resolve benchmarks in Lens", class = "btn-primary"),
    actionButton("chase", "Run citation chasing", disabled = TRUE),
    uiOutput("status"),
    accordion(
      accordion_panel("View benchmark records", DTOutput("benchmarks")),
      open = FALSE
    )
  ),

  card(
    card_header("Citation-chasing coverage"),
    p("The app checks both backward references and forward citations from your benchmark set."),
    uiOutput("citation_summary"),
    accordion(
      accordion_panel("View citation-chasing records", DTOutput("citations")),
      open = FALSE
    )
  ),

  card(
    card_header("Edit search structure"),
    p("Each top-level AND component is treated as a separate substring. Labels are descriptive only, so several substrings can share the same label."),
    uiOutput("block_editor"),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("apply_blocks", "Apply block edits"),
      actionButton("add_block", "Add empty substring")
    )
  ),

  card(
    card_header("What is the current search missing?"),
    uiOutput("coverage_summary"),
    accordion(
      accordion_panel("View missed records", DTOutput("missed_records")),
      open = FALSE
    )
  ),

  card(
    card_header("Suggested improvements"),
    p("Candidates are drawn from missed citation records and, where screening data are available, ranked using their prevalence in included versus excluded records. Select a candidate to inspect where it may fit."),
    DTOutput("candidate_terms"),
    uiOutput("candidate_action")
  )
)

server <- function(input, output, session) {
  token <- Sys.getenv("LENS_API_TOKEN", unset = "")

  parsed <- reactiveVal(NULL)
  resolved <- reactiveVal(NULL)
  citation_set <- reactiveVal(NULL)
  analysed_set <- reactiveVal(NULL)
  candidates <- reactiveVal(NULL)
  blocks <- reactiveVal(NULL)
  concept_blocks <- reactiveVal(data.frame(
    block_id = 1L,
    label = "Concept 1",
    expression = "",
    stringsAsFactors = FALSE
  ))
  concept_results <- reactiveVal(NULL)
  screening <- reactiveVal(NULL)
  screening_index <- reactiveVal(NA_integer_)
  starting_search <- reactiveVal(NULL)
  baseline_coverage <- reactiveVal(NA_real_)
  benchmark_source <- reactiveVal("Not specified")
  audit_events <- reactiveVal(empty_audit_events())

  collect_concept_blocks <- function() {
    b <- concept_blocks()
    if (is.null(b) || !nrow(b)) return(b)

    for (i in seq_len(nrow(b))) {
      label_id <- paste0("concept_label_", i)
      expr_id <- paste0("concept_expr_", i)

      if (!is.null(input[[label_id]])) b$label[i] <- input[[label_id]]
      if (!is.null(input[[expr_id]])) b$expression[i] <- input[[expr_id]]
    }

    b
  }

  collect_blocks <- function() {
    b <- blocks()
    if (is.null(b) || !nrow(b)) return(b)

    for (i in seq_len(nrow(b))) {
      label_id <- paste0("block_label_", i)
      expr_id <- paste0("block_expr_", i)

      if (!is.null(input[[label_id]])) b$label[i] <- input[[label_id]]
      if (!is.null(input[[expr_id]])) b$expression[i] <- input[[expr_id]]
    }

    b
  }

  run_analysis <- function(query, refresh_blocks = FALSE) {
    req(citation_set())
    validate(need(nzchar(trimws(query)), "Enter a draft Boolean search string first."))

    x <- citation_set()
    matches <- tryCatch(
      match_search_records(x, query),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
        NULL
      }
    )
    req(!is.null(matches))

    x$search_match <- matches
    benchmark_ids <- unique(na.omit(resolved()$lens_id))
    x$is_benchmark <- x$lens_id %in% benchmark_ids
    x$candidate_source <- !x$search_match & !x$is_benchmark
    analysed_set(x)

    missed <- x[x$candidate_source, , drop = FALSE]

    s <- screening()
    included <- if (!is.null(s) && nrow(s)) {
      s[!is.na(s$decision) & s$decision == "include", , drop = FALSE]
    } else {
      data.frame()
    }
    excluded <- if (!is.null(s) && nrow(s)) {
      s[!is.na(s$decision) & s$decision == "exclude", , drop = FALSE]
    } else {
      data.frame()
    }

    ranked <- rank_discriminative_candidates(
      missed_records = missed,
      included_records = included,
      excluded_records = excluded,
      query = query,
      top_n = 250L
    )

    current_blocks <- if (!is.null(blocks()) && nrow(blocks())) {
      blocks()
    } else {
      tryCatch(split_search_blocks(query), error = function(e) NULL)
    }

    if (nrow(ranked) && !is.null(current_blocks) && nrow(current_blocks)) {
      gains <- lapply(seq_len(nrow(ranked)), function(i) {
        best_candidate_gain(
          records = missed,
          blocks = current_blocks,
          candidate = ranked$candidate[i],
          type = ranked$type[i]
        )
      })

      ranked$best_block_id <- vapply(gains, function(g) g$block_id, integer(1))
      ranked$best_block_label <- vapply(gains, function(g) {
        if (is.na(g$label)) "" else g$label
      }, character(1))
      ranked$incremental_recovery <- vapply(gains, function(g) g$gain, integer(1))

      if (all(ranked$discrimination_available)) {
        ranked <- ranked[
          order(
            -ranked$log2_enrichment,
            -ranked$incremental_recovery,
            -ranked$keyword_records,
            ranked$candidate,
            na.last = TRUE
          ),
          ,
          drop = FALSE
        ]
      } else {
        ranked <- ranked[
          order(
            -ranked$incremental_recovery,
            -ranked$keyword_records,
            ranked$candidate
          ),
          ,
          drop = FALSE
        ]
      }
      rownames(ranked) <- NULL
    }

    candidates(ranked)

    if (refresh_blocks || is.null(blocks())) {
      parsed_blocks <- tryCatch(
        split_search_blocks(query),
        error = function(e) {
          showNotification(
            paste("Could not initialise search blocks:", conditionMessage(e)),
            type = "warning",
            duration = NULL
          )
          NULL
        }
      )
      if (!is.null(parsed_blocks)) blocks(parsed_blocks)
    }

    invisible(x)
  }

  observeEvent(input$concept_add_block, {
    b <- collect_concept_blocks()

    if (is.null(b) || !nrow(b)) {
      b <- data.frame(
        block_id = 1L,
        label = "Concept 1",
        expression = "",
        stringsAsFactors = FALSE
      )
    } else {
      new_id <- max(b$block_id) + 1L
      b <- rbind(
        b,
        data.frame(
          block_id = new_id,
          label = paste("Concept", new_id),
          expression = "",
          stringsAsFactors = FALSE
        )
      )
    }

    concept_blocks(b)
  })

  observeEvent(input$concept_search, {
    validate(need(nzchar(token), "LENS_API_TOKEN is not available in the app environment."))

    b <- collect_concept_blocks()
    validate(need(!is.null(b) && nrow(b) > 0, "Add at least one substring."))

    query <- rebuild_search_from_blocks(b)
    validate(need(nzchar(query), "Enter at least one search term."))

    withProgress(message = "Searching Lens by relevance…", value = 0.15, {
      result <- tryCatch(
        lens_ranked_search(query, token, size = input$concept_n),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      )
      req(!is.null(result))
      incProgress(0.75)

      concept_blocks(b)
      concept_results(result)
      if (is.null(starting_search())) starting_search(query)
      benchmark_source("Concept-first screening")
      screening(init_screening(result))
      screening_index(if (nrow(result)) 1L else NA_integer_)
      blocks(b)
      updateTextAreaInput(session, "search_string", value = query)
      incProgress(0.10)
    })
  })


  record_screen_decision <- function(decision) {
    s <- screening()
    idx <- screening_index()
    req(!is.null(s), nrow(s) > 0, !is.na(idx))

    s <- set_screen_decision(s, idx, decision)
    screening(s)

    next_idx <- next_unscreened_index(s, after = idx)
    screening_index(next_idx)
  }

  observeEvent(input$screen_include, {
    record_screen_decision("include")
  })

  observeEvent(input$screen_exclude, {
    record_screen_decision("exclude")
  })

  observeEvent(input$screen_unsure, {
    record_screen_decision("unsure")
  })

  observeEvent(input$promote_benchmarks, {
    s <- screening()
    req(!is.null(s), nrow(s) > 0)

    inc <- included_benchmarks(s)
    validate(need(nrow(inc) > 0, "Include at least one record first."))

    summary <- screening_summary(s, input$benchmark_target)
    validate(
      need(
        summary$target_met,
        sprintf(
          "Your benchmark target is %d; %d records are currently included.",
          summary$target,
          summary$include
        )
      )
    )

    inc$ris_id <- seq_len(nrow(inc))
    inc$match_method <- "concept screening"

    parsed(inc)
    resolved(inc)
    benchmark_source("Concept-first screening")
    citation_set(NULL)
    analysed_set(NULL)
    candidates(NULL)

    shinyjs::enable("chase")

    showNotification(
      sprintf("%d included records promoted to benchmarks.", nrow(inc)),
      type = "message"
    )
  })

  observeEvent(input$ris, {
    req(input$ris$datapath)
    x <- parse_ris(input$ris$datapath)
    parsed(x)
    benchmark_source("Uploaded RIS")
    starting_search(NULL)
    baseline_coverage(NA_real_)
    audit_events(empty_audit_events())
    resolved(NULL)
    citation_set(NULL)
    analysed_set(NULL)
    candidates(NULL)
    blocks(NULL)
    shinyjs::disable("chase")
    shinyjs::disable("analyse_search")
  })

  observeEvent(input$resolve, {
    req(parsed())
    validate(need(nzchar(token), "LENS_API_TOKEN is not available in the app environment."))

    withProgress(message = "Resolving benchmark records in Lens…", value = 0.2, {
      ans <- lens_resolve_benchmarks(parsed(), token)
      resolved(ans)
      incProgress(0.8)
    })

    shinyjs::enable("chase")
  })

  observeEvent(input$chase, {
    req(resolved())
    validate(need(nzchar(token), "LENS_API_TOKEN is not available in the app environment."))

    ids <- unique(na.omit(resolved()$lens_id))
    validate(need(length(ids) > 0, "No benchmark records were resolved to Lens IDs."))

    withProgress(message = "Retrieving forward and backward citation links…", value = 0.1, {
      links <- lens_get_citation_links(ids, token)
      incProgress(0.45)

      meta <- lens_fetch_records(unique(links$cited_lens_id), token)
      incProgress(0.45)

      citation_set(merge_citation_metadata(links, meta))
      analysed_set(NULL)
      candidates(NULL)
      blocks(NULL)
      shinyjs::enable("analyse_search")
    })
  })

  observeEvent(input$analyse_search, {
    withProgress(message = "Checking draft-search coverage…", value = 0.2, {
      analysed <- run_analysis(input$search_string, refresh_blocks = TRUE)
      if (is.null(starting_search())) starting_search(input$search_string)
      if (is.na(baseline_coverage())) {
        baseline_coverage(coverage_metrics(analysed)$proportion)
      }
      incProgress(0.8)
    })
  })

  observeEvent(input$apply_blocks, {
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    before <- coverage_metrics(analysed_set())$proportion
    blocks(b)
    query <- rebuild_search_from_blocks(b)
    updateTextAreaInput(session, "search_string", value = query)

    withProgress(message = "Rechecking edited search…", value = 0.2, {
      analysed <- run_analysis(query, refresh_blocks = FALSE)
      after <- coverage_metrics(analysed)$proportion

      audit_events(
        append_audit_event(
          audit_events(),
          change_type = "Manual substring edit",
          search_after = query,
          coverage_before = before,
          coverage_after = after
        )
      )

      incProgress(0.8)
    })
  })

  observeEvent(input$add_block, {
    b <- collect_blocks()

    if (is.null(b) || !nrow(b)) {
      b <- data.frame(
        block_id = 1L,
        label = "Concept 1",
        expression = "",
        stringsAsFactors = FALSE
      )
    } else {
      new_id <- max(b$block_id) + 1L
      b <- rbind(
        b,
        data.frame(
          block_id = new_id,
          label = paste("Concept", new_id),
          expression = "",
          stringsAsFactors = FALSE
        )
      )
    }

    blocks(b)
  })

  observeEvent(input$add_candidate, {
    req(candidates(), analysed_set())

    selected <- input$candidate_terms_rows_selected
    validate(need(length(selected) == 1L, "Select one candidate term first."))

    cand <- candidates()[selected, , drop = FALSE]
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    target <- suppressWarnings(as.integer(input$candidate_block))
    validate(need(!is.na(target) && target %in% b$block_id, "Choose a target substring."))

    addition <- input$candidate_form
    validate(need(!is.null(addition) && nzchar(addition), "Choose how to add the candidate."))

    before <- coverage_metrics(analysed_set())$proportion
    idx <- match(target, b$block_id)
    target_label <- b$label[idx]
    b$expression[idx] <- add_or_to_block(b$expression[idx], addition)
    blocks(b)

    query <- rebuild_search_from_blocks(b)
    updateTextAreaInput(session, "search_string", value = query)

    withProgress(message = "Rechecking search after adding candidate…", value = 0.2, {
      analysed <- run_analysis(query, refresh_blocks = FALSE)
      after <- coverage_metrics(analysed)$proportion

      get_value <- function(name, default = NA) {
        if (name %in% names(cand)) cand[[name]][[1]] else default
      }

      audit_events(
        append_audit_event(
          audit_events(),
          change_type = "Candidate term added",
          search_after = query,
          term = cand$candidate[[1]],
          target_substring = target_label,
          syntax = addition,
          incremental_recovery = get_value("incremental_recovery", NA_integer_),
          included_prevalence = get_value("included_prevalence", NA_real_),
          excluded_prevalence = get_value("excluded_prevalence", NA_real_),
          log2_enrichment = get_value("log2_enrichment", NA_real_),
          coverage_before = before,
          coverage_after = after
        )
      )

      incProgress(0.8)
    })
  })



  output$final_search_display <- renderUI({
    query <- input$search_string
    if (is.null(query) || !nzchar(trimws(query))) {
      return(tags$div(
        class = "search-output text-muted",
        "Your improved search string will appear here."
      ))
    }

    tags$pre(class = "search-output", query)
  })

  output$search_check_status <- renderUI({
    if (is.null(citation_set())) {
      return(tags$div(
        class = "help-note",
        "Citation coverage becomes available after benchmark records have been citation-chased."
      ))
    }

    tags$div(
      class = "help-note",
      "Ready to compare this search against the citation-chasing set."
    )
  })

  output$final_search_summary <- renderUI({
    x <- analysed_set()
    if (is.null(x)) {
      return(tags$span(class = "text-muted", "Citation-set coverage has not yet been measured."))
    }

    m <- coverage_metrics(x)
    tags$span(
      sprintf(
        "Current citation-set coverage: %d of %d non-benchmark records%s.",
        m$captured,
        m$total,
        if (is.finite(m$proportion)) sprintf(" (%.1f%%)", 100 * m$proportion) else ""
      )
    )
  })

  output$download_final_search <- downloadHandler(
    filename = function() paste0("searchhelper-final-search-", Sys.Date(), ".txt"),
    content = function(file) {
      writeLines(input$search_string, file, useBytes = TRUE)
    }
  )

  output$download_audit <- downloadHandler(
    filename = function() paste0("searchhelper-audit-", Sys.Date(), ".html"),
    content = function(file) {
      req(resolved())

      citations <- citation_set()
      backward <- if (is.null(citations)) 0L else sum(citations$direction %in% c("backward", "both"))
      forward <- if (is.null(citations)) 0L else sum(citations$direction %in% c("forward", "both"))

      final_cov <- coverage_metrics(analysed_set())$proportion
      start <- starting_search()
      if (is.null(start) || !nzchar(start)) start <- input$search_string

      html <- render_audit_html(
        starting_search = start,
        final_search = input$search_string,
        benchmark_source = benchmark_source(),
        benchmark_count = sum(!is.na(resolved()$lens_id)),
        backward_count = backward,
        forward_count = forward,
        baseline_coverage = baseline_coverage(),
        final_coverage = final_cov,
        events = audit_events()
      )

      writeLines(html, file, useBytes = TRUE)
    }
  )

  output$concept_block_editor <- renderUI({
    b <- concept_blocks()

    preset_labels <- c(
      "Population",
      "Intervention or exposure",
      "Outcome",
      "Study design",
      "Context"
    )

    tagList(lapply(seq_len(nrow(b)), function(i) {
      tags$div(
        class = "search-block border rounded p-3 mb-3",
        tags$h5(sprintf("Substring %d", i)),
        selectizeInput(
          paste0("concept_label_", i),
          "Label",
          choices = unique(c(preset_labels, b$label[i])),
          selected = b$label[i],
          options = list(create = TRUE)
        ),
        textAreaInput(
          paste0("concept_expr_", i),
          "Terms / Boolean expression",
          value = b$expression[i],
          rows = 5,
          width = "100%",
          placeholder = 'e.g. salmon* OR "rainbow trout"'
        )
      )
    }))
  })

  output$concept_query_preview <- renderUI({
    b <- collect_concept_blocks()
    if (is.null(b) || !nrow(b)) return(NULL)

    query <- rebuild_search_from_blocks(b)
    if (!nzchar(query)) {
      return(tags$span(class = "text-muted", "The canonical Boolean search will appear here."))
    }

    tags$div(
      tags$strong("Canonical Boolean: "),
      tags$code(query)
    )
  })

  output$concept_search_summary <- renderUI({
    x <- concept_results()
    if (is.null(x)) {
      return(tags$span(class = "text-muted", "No Lens sample retrieved yet."))
    }

    tags$strong(sprintf("%d relevance-ranked Lens records retrieved.", nrow(x)))
  })

  output$concept_results <- renderDT({
    req(concept_results())

    x <- concept_results()
    keep <- intersect(
      c("rank", "lens_id", "title", "year", "authors", "doi", "keywords", "abstract"),
      names(x)
    )

    datatable(
      x[, keep, drop = FALSE],
      rownames = FALSE,
      selection = "none",
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })


  output$screening_progress <- renderUI({
    s <- screening()
    if (is.null(s)) {
      return(tags$span(class = "text-muted", "Search Lens to begin screening."))
    }

    x <- screening_summary(s, input$benchmark_target)

    tagList(
      tags$strong(
        sprintf(
          "%d included · %d excluded · %d unsure · %d remaining",
          x$include, x$exclude, x$unsure, x$remaining
        )
      ),
      tags$br(),
      if (x$target_met) {
        tags$span(
          class = "text-success",
          sprintf("Benchmark target reached (%d/%d).", x$include, x$target)
        )
      } else {
        tags$span(
          class = "text-muted",
          sprintf("%d more include decision(s) needed to reach the target.", x$target - x$include)
        )
      }
    )
  })

  output$screening_record <- renderUI({
    s <- screening()
    idx <- screening_index()

    if (is.null(s) || !nrow(s)) {
      return(tags$span(class = "text-muted", "No records available for screening."))
    }

    if (is.na(idx)) {
      return(tags$div(
        class = "alert alert-success",
        "All retrieved records have been screened."
      ))
    }

    r <- s[idx, , drop = FALSE]

    tags$div(
      class = "border rounded p-3 mb-3",
      tags$div(
        class = "text-muted",
        sprintf("Record %d of %d · Lens relevance rank %s", idx, nrow(s), r$rank)
      ),
      tags$h4(r$title),
      tags$p(tags$strong("Authors: "), ifelse(is.na(r$authors), "", r$authors)),
      tags$p(tags$strong("Year: "), ifelse(is.na(r$year), "", r$year)),
      if (!is.na(r$keywords) && nzchar(r$keywords)) {
        tags$p(tags$strong("Keywords: "), r$keywords)
      },
      tags$hr(),
      tags$p(ifelse(is.na(r$abstract) || !nzchar(r$abstract), "No abstract available.", r$abstract))
    )
  })

  output$status <- renderUI({
    x <- parsed()
    if (is.null(x)) return(tags$span(class = "text-muted", "No RIS file uploaded."))

    r <- resolved()
    if (is.null(r)) return(tags$span(sprintf("%d benchmark records parsed from RIS.", nrow(x))))

    ok <- sum(!is.na(r$lens_id))
    tags$div(
      tags$strong(sprintf("%d / %d benchmarks resolved in Lens.", ok, nrow(r))),
      if (ok < nrow(r)) {
        tags$span(class = "text-warning", sprintf(" %d unresolved.", nrow(r) - ok))
      }
    )
  })

  output$benchmarks <- renderDT({
    x <- resolved()
    if (is.null(x)) x <- parsed()
    req(x)

    datatable(
      x,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$citation_summary <- renderUI({
    x <- citation_set()
    if (is.null(x)) {
      return(tags$span(class = "text-muted", "Citation chasing has not yet been run."))
    }

    tags$div(
      tags$strong(sprintf("%d unique citation-chasing records.", nrow(x))),
      tags$span(
        sprintf(
          " %d backward references; %d forward citations.",
          sum(x$direction %in% c("backward", "both")),
          sum(x$direction %in% c("forward", "both"))
        )
      )
    )
  })

  output$citations <- renderDT({
    req(citation_set())

    datatable(
      citation_set(),
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$block_editor <- renderUI({
    b <- blocks()

    if (is.null(b) || !nrow(b)) {
      return(tags$span(
        class = "text-muted",
        "Analyse a draft search to initialise its top-level substrings."
      ))
    }

    preset_labels <- c(
      "Population",
      "Intervention or exposure",
      "Outcome",
      "Study design",
      "Context"
    )

    tagList(lapply(seq_len(nrow(b)), function(i) {
      tags$div(
        class = "search-block border rounded p-3 mb-3",
        tags$h5(sprintf("Substring %d", i)),
        selectizeInput(
          paste0("block_label_", i),
          "Label",
          choices = unique(c(preset_labels, b$label[i])),
          selected = b$label[i],
          options = list(create = TRUE)
        ),
        textAreaInput(
          paste0("block_expr_", i),
          "Boolean expression",
          value = b$expression[i],
          rows = 5,
          width = "100%"
        )
      )
    }))
  })

  output$coverage_summary <- renderUI({
    x <- analysed_set()

    if (is.null(x)) {
      return(tags$span(
        class = "text-muted",
        "Run citation chasing, then analyse the draft search."
      ))
    }

    eligible <- !x$is_benchmark
    total <- sum(eligible)
    captured <- sum(x$search_match & eligible)
    missed <- sum(x$candidate_source)
    pct <- if (total > 0) 100 * captured / total else NA_real_

    tags$div(
      tags$strong(
        sprintf("%d of %d non-benchmark citation records matched locally", captured, total)
      ),
      if (is.finite(pct)) tags$span(sprintf(" (%.1f%%).", pct)),
      tags$span(sprintf(" %d records remain for candidate-term discovery.", missed))
    )
  })

  output$missed_records <- renderDT({
    req(analysed_set())

    x <- analysed_set()
    x <- x[x$candidate_source, , drop = FALSE]

    keep <- intersect(
      c(
        "lens_id",
        "title",
        "year",
        "authors",
        "doi",
        "keywords",
        "direction",
        "benchmark_count",
        "benchmark_sources"
      ),
      names(x)
    )

    datatable(
      x[, keep, drop = FALSE],
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$candidate_terms <- renderDT({
    req(candidates())

    x <- candidates()

    keep <- intersect(
      c(
        "candidate",
        "type",
        "incremental_recovery",
        "best_block_label",
        "included_records",
        "included_prevalence",
        "excluded_records",
        "excluded_prevalence",
        "log2_enrichment",
        "keyword_records",
        "occurrences"
      ),
      names(x)
    )

    shown <- x[, keep, drop = FALSE]

    if ("included_prevalence" %in% names(shown)) {
      shown$included_prevalence <- round(shown$included_prevalence, 3)
    }
    if ("excluded_prevalence" %in% names(shown)) {
      shown$excluded_prevalence <- round(shown$excluded_prevalence, 3)
    }
    if ("log2_enrichment" %in% names(shown)) {
      shown$log2_enrichment <- round(shown$log2_enrichment, 2)
    }

    datatable(
      shown,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$candidate_action <- renderUI({
    req(candidates(), analysed_set())

    selected <- input$candidate_terms_rows_selected
    if (length(selected) != 1L) {
      return(tags$span(
        class = "text-muted",
        "Select a candidate row to inspect placement and syntax options."
      ))
    }

    cand <- candidates()[selected, , drop = FALSE]
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    missed <- analysed_set()
    missed <- missed[missed$candidate_source, , drop = FALSE]

    scores <- score_candidate_blocks(
      missed,
      b,
      cand$candidate,
      cand$type
    )
    suggested <- suggest_block_id(scores)

    block_choices <- setNames(
      b$block_id,
      paste0("Substring ", seq_len(nrow(b)), " · ", b$label)
    )

    forms <- candidate_forms(cand$candidate, cand$type)
    prox <- proximity_advice(cand$candidate, cand$type)

    placement_note <- NULL
    if (!is.na(suggested) && nrow(scores)) {
      s <- scores[scores$block_id == suggested, , drop = FALSE]
      placement_note <- tags$p(
        tags$strong("Suggested placement: "),
        paste0(
          "Substring ", match(suggested, b$block_id), " · ", s$label,
          ". In candidate-containing missed records, this substring failed in ",
          sprintf("%.0f%%", 100 * s$fail_rate),
          " of evaluable cases; it was the sole failed substring in ",
          s$sole_failure_support,
          " record(s)."
        )
      )
    }

    tagList(
      tags$hr(),
      tags$h5(cand$candidate),
      placement_note,
      selectInput(
        "candidate_block",
        "Add to substring",
        choices = block_choices,
        selected = if (!is.na(suggested)) suggested else b$block_id[1]
      ),
      selectInput(
        "candidate_form",
        "Add as",
        choices = forms,
        selected = unname(forms[1])
      ),
      if (!is.null(prox)) tags$p(class = "text-muted", prox),
      tags$p(
        class = "text-muted",
        "Wildcard stems are suggestions only and should be checked for unintended retrieval before use."
      ),
      actionButton("add_candidate", "Add candidate to search", class = "btn-primary")
    )
  })

  output$download_citations <- downloadHandler(
    filename = function() {
      paste0("searchhelper-citation-set-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(citation_set())
      write.csv(citation_set(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
