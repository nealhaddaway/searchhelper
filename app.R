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

ui <- page_sidebar(
  title = "Search Helper",
  shinyjs::useShinyjs(),
  sidebar = sidebar(
    fileInput("ris", "Benchmark records (RIS)", accept = c(".ris", ".txt")),
    textAreaInput(
      "search_string",
      "Draft Boolean search string",
      rows = 8,
      placeholder = '(concept A OR synonym*) AND ("concept B" OR term)'
    ),
    actionButton("resolve", "Resolve benchmarks in Lens", class = "btn-primary"),
    actionButton("chase", "Run citation chasing", disabled = TRUE),
    actionButton("analyse_search", "Analyse draft search", disabled = TRUE),
    hr(),
    downloadButton("download_citations", "Download citation set (CSV)")
  ),

  card(
    card_header("Stage 4 · Start from concepts"),
    p("Build one or more optional search substrings, then retrieve a relevance-ranked Lens sample from title, abstract and author keyword fields."),
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
    card_header("Stage 1 · Benchmark initialisation"),
    p("Upload known relevant records, resolve them in Lens, then retrieve backward references and forward citations."),
    uiOutput("status"),
    DTOutput("benchmarks")
  ),

  card(
    card_header("Citation chasing"),
    uiOutput("citation_summary"),
    DTOutput("citations")
  ),

  card(
    card_header("Search structure"),
    p("Top-level AND components are treated as separate substrings. Labels are descriptive only: multiple substrings may share the same label."),
    uiOutput("block_editor"),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("apply_blocks", "Apply block edits"),
      actionButton("add_block", "Add empty substring")
    )
  ),

  card(
    card_header("Draft-search coverage"),
    uiOutput("coverage_summary"),
    DTOutput("missed_records")
  ),

  card(
    card_header("Candidate terms from missed records"),
    p("Select one candidate to inspect where it may fit. Existing search terms and common English stop words are excluded."),
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
    candidates(mine_candidate_terms(missed, query, top_n = 250L))

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
      run_analysis(input$search_string, refresh_blocks = TRUE)
      incProgress(0.8)
    })
  })

  observeEvent(input$apply_blocks, {
    b <- collect_blocks()
    req(!is.null(b), nrow(b) > 0)

    blocks(b)
    query <- rebuild_search_from_blocks(b)
    updateTextAreaInput(session, "search_string", value = query)

    withProgress(message = "Rechecking edited search…", value = 0.2, {
      run_analysis(query, refresh_blocks = FALSE)
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

    idx <- match(target, b$block_id)
    b$expression[idx] <- add_or_to_block(b$expression[idx], addition)
    blocks(b)

    query <- rebuild_search_from_blocks(b)
    updateTextAreaInput(session, "search_string", value = query)

    withProgress(message = "Rechecking search after adding candidate…", value = 0.2, {
      run_analysis(query, refresh_blocks = FALSE)
      incProgress(0.8)
    })
  })


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
      card(
        card_header(sprintf("Substring %d", i)),
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
          rows = 3,
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
      card(
        card_header(sprintf("Substring %d", i)),
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
          rows = 3
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

    datatable(
      candidates(),
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
