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
  fillable = FALSE,
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML("
    textarea.form-control { min-height: 130px; resize: vertical; font-size: 1rem; line-height: 1.45; }
    .search-block { background: var(--bs-body-bg); }
    .search-output { white-space: pre-wrap; overflow-wrap: anywhere; font-size: 1rem; line-height: 1.5; background: var(--bs-tertiary-bg); border: 1px solid var(--bs-border-color); border-radius: .5rem; padding: 1rem; }
    .help-note { font-size: .9rem; color: var(--bs-secondary-color); margin-top: .5rem; }
    .candidate-table-section { position: relative; display: block; width: 100%; overflow: visible; }
    .candidate-table-section .dataTables_wrapper { position: relative; display: block; width: 100%; margin-bottom: 1rem; }
    .candidate-action-section { position: relative; display: block; clear: both; width: 100%; margin-top: 1.5rem; z-index: 0; }
  "))),
  sidebar = sidebar(
    title = "Search",
    uiOutput("sidebar_ui")
  ),

  uiOutput("route_selector"),
  uiOutput("route_ui"),
  uiOutput("downstream_ui")
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
  last_add_result <- reactiveVal(NULL)
  route_mode <- reactiveVal(NULL)

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

    if (nrow(ranked)) {
      if (all(ranked$discrimination_available)) {
        ranked <- ranked[
          order(
            -ranked$log2_enrichment,
            -ranked$missed_gain,
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
            -ranked$missed_gain,
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
    validate(need(identical(route_mode(), "naive"), "Choose the naive-search route first."))
    validate(need(nzchar(token), "LENS_API_TOKEN is not available in the app environment."))
    validate(need(nzchar(trimws(input$search_string)), "Enter a naive Boolean search string first."))

    query <- trimws(input$search_string)

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

      concept_results(result)
      if (is.null(starting_search())) starting_search(query)
      benchmark_source("Naive-search screening")
      screening(init_screening(result))
      screening_index(if (nrow(result)) 1L else NA_integer_)

      parsed_blocks <- tryCatch(split_search_blocks(query), error = function(e) NULL)
      if (!is.null(parsed_blocks)) blocks(parsed_blocks)

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

    inc$ris_id <- seq_len(nrow(inc))
    inc$match_method <- "naive-search screening"

    parsed(inc)
    resolved(inc)
    benchmark_source("Naive-search screening")
    citation_set(NULL)
    analysed_set(NULL)
    candidates(NULL)
    last_add_result(NULL)

    showNotification(
      sprintf("%d included records selected as benchmarks. Starting citation chasing.", nrow(inc)),
      type = "message"
    )

    run_citation_chase()
  })

  observeEvent(input$ris, {
    validate(need(identical(route_mode(), "upload"), "Choose the benchmark-upload route first."))
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
    last_add_result(NULL)
    blocks(NULL)
    shinyjs::disable("chase")
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

  run_citation_chase <- function() {
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

      query <- trimws(input$search_string %||% "")
      if (!nzchar(query) && !is.null(starting_search())) {
        query <- trimws(starting_search())
      }

      if (nzchar(query)) {
        incProgress(0.05, detail = "Identifying candidate terms…")
        analysed <- run_analysis(query, refresh_blocks = TRUE)
        if (is.na(baseline_coverage())) {
          baseline_coverage(coverage_metrics(analysed)$proportion)
        }
      }
    })
  }

  observeEvent(input$chase, {
    run_citation_chase()
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
      after_metrics <- coverage_metrics(analysed)
      after <- after_metrics$proportion

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
    validate(need(length(selected) >= 1L, "Select at least one candidate term first."))

    cand <- candidates()[selected, , drop = FALSE]
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    target <- suppressWarnings(as.integer(input$candidate_block))
    validate(need(!is.na(target) && target %in% b$block_id, "Choose a target substring."))

    additions <- vapply(seq_len(nrow(cand)), function(i) {
      if (identical(cand$type[i], "phrase")) {
        paste0('"', cand$candidate[i], '"')
      } else {
        cand$candidate[i]
      }
    }, character(1))

    before_metrics <- coverage_metrics(analysed_set())
    before <- before_metrics$proportion
    idx <- match(target, b$block_id)
    target_label <- b$label[idx]

    for (addition in additions) {
      b$expression[idx] <- add_or_to_block(b$expression[idx], addition)
    }
    blocks(b)

    query <- rebuild_search_from_blocks(b)
    updateTextAreaInput(session, "search_string", value = query)

    withProgress(message = "Rechecking search after adding selected terms…", value = 0.2, {
      analysed <- run_analysis(query, refresh_blocks = FALSE)
      after <- coverage_metrics(analysed)$proportion

      events <- audit_events()
      for (i in seq_len(nrow(cand))) {
        get_value <- function(name, default = NA) {
          if (name %in% names(cand)) cand[[name]][[i]] else default
        }

        events <- append_audit_event(
          events,
          change_type = "Candidate term added",
          search_after = query,
          term = cand$candidate[[i]],
          target_substring = target_label,
          syntax = additions[[i]],
          incremental_recovery = NA_integer_,
          included_prevalence = get_value("included_prevalence", NA_real_),
          excluded_prevalence = get_value("excluded_prevalence", NA_real_),
          log2_enrichment = get_value("log2_enrichment", NA_real_),
          coverage_before = before,
          coverage_after = after
        )
      }
      audit_events(events)

      last_add_result(list(
        n_terms = length(additions),
        additions = additions,
        target_label = target_label,
        target_index = idx,
        target_expression = b$expression[idx],
        query = query,
        before_captured = before_metrics$captured,
        before_total = before_metrics$total,
        before_proportion = before_metrics$proportion,
        after_captured = after_metrics$captured,
        after_total = after_metrics$total,
        after_proportion = after_metrics$proportion,
        remaining_missed = sum(analysed$candidate_source, na.rm = TRUE)
      ))

      incProgress(0.8)
    })
  })


  observeEvent(input$choose_naive, {
    route_mode("naive")
  })

  observeEvent(input$choose_upload, {
    route_mode("upload")
  })

  reset_workflow <- function() {
    route_mode(NULL)
    parsed(NULL)
    resolved(NULL)
    citation_set(NULL)
    analysed_set(NULL)
    candidates(NULL)
    blocks(NULL)
    concept_results(NULL)
    screening(NULL)
    screening_index(NA_integer_)
    starting_search(NULL)
    baseline_coverage(NA_real_)
    benchmark_source("Not specified")
    audit_events(empty_audit_events())
    last_add_result(NULL)
    updateTextAreaInput(session, "search_string", value = "")
  }

  observeEvent(input$reset_workflow, {
    reset_workflow()
  })

  output$sidebar_ui <- renderUI({
    mode <- route_mode()

    if (is.null(mode)) {
      return(tags$p(class = "text-muted", "Choose a starting route in the main panel."))
    }

    if (identical(mode, "naive")) {
      return(tagList(
        textAreaInput(
          "search_string",
          "Naive search string",
          value = starting_search() %||% "",
          rows = 12,
          placeholder = 'e.g. salmon AND farming'
        ),
        if (!is.null(citation_set())) {
          tagList(
            actionButton("analyse_search", "Reidentify candidate terms", class = "btn-primary"),
            uiOutput("search_check_status")
          )
        }
      ))
    }

    if (is.null(citation_set())) {
      return(tags$p(
        class = "text-muted",
        "Upload and citation-chase your benchmark records first. Then enter the search string you want to assess."
      ))
    }

    tagList(
      textAreaInput(
        "search_string",
        "Search string to assess",
        rows = 12,
        placeholder = '(concept A OR synonym*) AND ("concept B" OR term)'
      ),
      actionButton("analyse_search", "Reidentify candidate terms", class = "btn-primary"),
      uiOutput("search_check_status")
    )
  })

  output$route_selector <- renderUI({
    mode <- route_mode()

    if (is.null(mode)) {
      return(card(
        card_header("How do you want to start?"),
        p("Choose one route. The other route will stay hidden until you reset the workflow."),
        layout_columns(
          col_widths = c(6, 6),
          actionButton("choose_naive", "Start with a naive search", class = "btn-primary btn-lg w-100"),
          actionButton("choose_upload", "Upload benchmark records", class = "btn-outline-primary btn-lg w-100")
        )
      ))
    }

    tags$div(
      class = "mb-3",
      actionButton("reset_workflow", "Reset and choose another route", class = "btn-outline-secondary")
    )
  })

  output$route_ui <- renderUI({
    mode <- route_mode()
    if (is.null(mode)) return(NULL)

    if (identical(mode, "naive")) {
      return(card(
        card_header("Naive search"),
        p("Enter a simple starting search in the Current search box, retrieve relevance-ranked Lens candidate benchmark records, then screen them one at a time."),
        layout_columns(
          col_widths = c(4, 8),
          numericInput("concept_n", "Records to retrieve", value = 500, min = 20, max = 500, step = 20),
          actionButton("concept_search", "Search Lens", class = "btn-primary")
        ),
        uiOutput("concept_search_summary"),
        uiOutput("naive_screening_panel")
      ))
    }

    card(
      card_header("Upload benchmark records"),
      p("Upload known relevant records as RIS, resolve them in Lens, then run citation chasing."),
      fileInput("ris", "Benchmark records (RIS)", accept = c(".ris", ".txt")),
      actionButton("resolve", "Resolve benchmarks in Lens", class = "btn-primary"),
      actionButton("chase", "Run citation chasing", disabled = TRUE),
      uiOutput("status"),
      accordion(
        accordion_panel("View benchmark records", DTOutput("benchmarks")),
        open = FALSE
      )
    )
  })

  output$naive_screening_panel <- renderUI({
    if (is.null(screening())) return(NULL)

    tagList(
      tags$hr(),
      tags$h4("Screen candidate benchmark records"),
      uiOutput("screening_progress"),
      uiOutput("screening_record"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        actionButton("screen_include", "Include", class = "btn-success btn-lg"),
        actionButton("screen_exclude", "Exclude", class = "btn-danger btn-lg"),
        actionButton("screen_unsure", "Unsure", class = "btn-lg"),
        actionButton("promote_benchmarks", "Use included records for citation chasing", class = "btn-primary btn-lg")
      )
    )
  })

  output$downstream_ui <- renderUI({
    if (is.null(citation_set())) return(NULL)

    tagList(
      tags$div(
        class = "candidate-table-section border rounded p-3 mb-3",
        tags$h3("Candidate terms for your search", class = "h5"),
        p("After citation chasing, the app removes citation records already retrieved by your current search. The remaining records are treated as missed records, and candidate terms are mined from those missed records. Select one or more candidate terms to add to a search substring."),
        uiOutput("candidate_terms_status"),
        DTOutput("candidate_terms")
      ),
      tags$div(
        class = "candidate-action-section",
        uiOutput("candidate_add_feedback"),
        uiOutput("candidate_action")
      ),
      card(
        card_header("Search coverage"),
        uiOutput("coverage_summary")
      ),
      card(
        card_header("Edit search structure"),
        p("Each top-level AND component is treated as a separate substring."),
        uiOutput("block_editor"),
        layout_columns(
          col_widths = c(6, 6),
          actionButton("apply_blocks", "Apply block edits"),
          actionButton("add_block", "Add empty substring")
        )
      ),
      card(
        card_header("Your improved search string"),
        p("Continue refining the search above, or download it when you are satisfied."),
        uiOutput("final_search_display"),
        layout_columns(
          col_widths = c(6, 6),
          downloadButton("download_final_search", "Download search string"),
          downloadButton("download_audit", "Download audit (HTML)")
        ),
        uiOutput("final_search_summary")
      )
    )
  })

  output$candidate_terms_status <- renderUI({
    if (is.null(analysed_set())) {
      return(tags$div(
        class = "alert alert-info",
        "Enter the search string you want to assess in the sidebar, then identify candidate terms."
      ))
    }

    x <- analysed_set()
    missed <- sum(x$candidate_source)
    total <- sum(!x$is_benchmark)
    n_candidates <- if (is.null(candidates())) 0L else nrow(candidates())

    tags$div(
      tags$strong(sprintf("%d candidate terms identified.", n_candidates)),
      tags$span(sprintf(
        " They are derived from %d missed records identified only after citation chasing and comparison with the current search, out of %d non-benchmark citation records assessed.",
        missed, total
      ))
    )
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
      "Ready to reanalyse the citation-chasing set and update candidate terms."
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

    tags$strong(sprintf("%d relevance-ranked Lens candidate benchmark records retrieved. Screen them below one at a time.", nrow(x)))
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
      return(tags$span(class = "text-muted", "Run the Lens search to begin screening."))
    }

    include_n <- sum(!is.na(s$decision) & s$decision == "include")
    exclude_n <- sum(!is.na(s$decision) & s$decision == "exclude")
    unsure_n <- sum(!is.na(s$decision) & s$decision == "unsure")
    remaining_n <- sum(is.na(s$decision))

    tagList(
      tags$strong(
        sprintf(
          "%d included · %d excluded · %d unsure · %d remaining",
          include_n, exclude_n, unsure_n, remaining_n
        )
      ),
      tags$br(),
      tags$span(
        class = if (include_n > 0) "text-success" else "text-muted",
        if (include_n > 0) {
          "When you feel you have enough relevant records, use the included set for citation chasing."
        } else {
          "Include at least one relevant record before citation chasing."
        }
      )
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
        sprintf("Candidate benchmark record %d of %d · Lens relevance rank %s", idx, nrow(s), r$rank)
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
      selection = "multiple",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        select = list(style = "multi")
      )
    )
  })

  output$candidate_add_feedback <- renderUI({
    x <- last_add_result()
    if (is.null(x)) return(NULL)

    before_pct <- if (is.finite(x$before_proportion)) sprintf("%.1f%%", 100 * x$before_proportion) else "not available"
    after_pct <- if (is.finite(x$after_proportion)) sprintf("%.1f%%", 100 * x$after_proportion) else "not available"

    tags$div(
      class = "alert alert-success",
      tags$h5(sprintf(
        "%d term%s added to Substring %d · %s",
        x$n_terms,
        if (x$n_terms == 1L) "" else "s",
        x$target_index,
        x$target_label
      )),
      tags$p(
        sprintf(
          "Citation-set coverage changed from %d/%d (%s) to %d/%d (%s). %d citation record%s remain missed.",
          x$before_captured,
          x$before_total,
          before_pct,
          x$after_captured,
          x$after_total,
          after_pct,
          x$remaining_missed,
          if (x$remaining_missed == 1L) "" else "s"
        )
      ),
      tags$p(tags$strong("Updated substring:")),
      tags$pre(class = "search-output", x$target_expression),
      tags$p(tags$strong("Updated full search:")),
      tags$pre(class = "search-output mb-0", x$query)
    )
  })

  output$candidate_action <- renderUI({
    req(candidates(), analysed_set())

    selected <- input$candidate_terms_rows_selected
    if (!length(selected)) {
      return(tags$div(
        class = "border rounded p-3 mt-3",
        tags$strong("Add candidate terms"),
        tags$p(
          class = "text-muted mb-0",
          "Select one or more rows in the table. The selected terms will appear here for addition to a search substring."
        )
      ))
    }

    cand <- candidates()[selected, , drop = FALSE]
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    missed <- analysed_set()
    missed <- missed[missed$candidate_source, , drop = FALSE]

    suggested_ids <- vapply(seq_len(nrow(cand)), function(i) {
      scores <- score_candidate_blocks(
        missed,
        b,
        cand$candidate[i],
        cand$type[i]
      )
      suggest_block_id(scores)
    }, integer(1))

    valid_suggestions <- suggested_ids[
      !is.na(suggested_ids) & suggested_ids %in% b$block_id
    ]

    suggested <- if (length(valid_suggestions)) {
      tab <- sort(table(valid_suggestions), decreasing = TRUE)
      as.integer(names(tab)[1])
    } else {
      b$block_id[1]
    }

    block_choices <- setNames(
      b$block_id,
      paste0("Substring ", seq_len(nrow(b)), " · ", b$label)
    )

    selected_items <- Map(function(term, type) {
      tags$li(
        tags$code(if (identical(type, "phrase")) paste0('"', term, '"') else term)
      )
    }, cand$candidate, cand$type)

    tagList(
      tags$div(
        class = "border rounded p-3 mt-3",
        tags$h5(sprintf("%d candidate term%s selected", nrow(cand), if (nrow(cand) == 1L) "" else "s")),
        tags$ul(selected_items),
        selectInput(
          "candidate_block",
          "Add selected terms to substring",
          choices = block_choices,
          selected = suggested
        ),
        tags$p(
          class = "text-muted",
          "Selected terms are added with OR. Single terms are added literally; multi-word phrase candidates are added as exact quoted phrases."
        ),
        actionButton(
          "add_candidate",
          sprintf("Add %d selected term%s", nrow(cand), if (nrow(cand) == 1L) "" else "s"),
          class = "btn-primary"
        )
      )
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
