library(shiny)
library(bslib)
library(DT)

source("R/ris.R")
source("R/lens_api.R")
source("R/boolean_match.R")
source("R/term_mining.R")

ui <- page_sidebar(
  title = "Search Helper",
  shinyjs::useShinyjs(),
  sidebar = sidebar(
    fileInput("ris", "Benchmark records (RIS)", accept = c(".ris", ".txt")),
    textAreaInput("search_string", "Draft Boolean search string", rows = 7,
                  placeholder = '(concept A OR synonym*) AND ("concept B" OR term)'),
    actionButton("resolve", "Resolve benchmarks in Lens", class = "btn-primary"),
    actionButton("chase", "Run citation chasing", disabled = TRUE),
    actionButton("analyse_search", "Analyse draft search", disabled = TRUE),
    hr(),
    downloadButton("download_citations", "Download citation set (CSV)")
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
    card_header("Draft-search coverage"),
    uiOutput("coverage_summary"),
    DTOutput("missed_records")
  ),
  card(
    card_header("Candidate terms from missed records"),
    p("Ranked by the number of missed records containing each term or phrase. Existing search terms and common English stop words are excluded."),
    DTOutput("candidate_terms")
  )
)

server <- function(input, output, session) {
  token <- Sys.getenv("LENS_API_TOKEN", unset = "")
  parsed <- reactiveVal(NULL)
  resolved <- reactiveVal(NULL)
  citation_set <- reactiveVal(NULL)
  analysed_set <- reactiveVal(NULL)
  candidates <- reactiveVal(NULL)

  observeEvent(input$ris, {
    req(input$ris$datapath)
    x <- parse_ris(input$ris$datapath)
    parsed(x)
    resolved(NULL)
    citation_set(NULL)
    analysed_set(NULL)
    candidates(NULL)
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
      shinyjs::enable("analyse_search")
    })
  })

  observeEvent(input$analyse_search, {
    req(citation_set())
    validate(need(nzchar(trimws(input$search_string)), "Enter a draft Boolean search string first."))

    withProgress(message = "Checking draft-search coverage…", value = 0.2, {
      x <- citation_set()
      matches <- tryCatch(
        match_search_records(x, input$search_string),
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

      incProgress(0.4)
      missed <- x[x$candidate_source, , drop = FALSE]
      candidates(mine_candidate_terms(missed, input$search_string, top_n = 250L))
      incProgress(0.4)
    })
  })

  output$status <- renderUI({
    x <- parsed()
    if (is.null(x)) return(tags$span(class = "text-muted", "No RIS file uploaded."))
    r <- resolved()
    if (is.null(r)) return(tags$span(sprintf("%d benchmark records parsed from RIS.", nrow(x))))
    ok <- sum(!is.na(r$lens_id))
    tags$div(
      tags$strong(sprintf("%d / %d benchmarks resolved in Lens.", ok, nrow(r))),
      if (ok < nrow(r)) tags$span(class = "text-warning", sprintf(" %d unresolved.", nrow(r) - ok))
    )
  })

  output$benchmarks <- renderDT({
    x <- resolved()
    if (is.null(x)) x <- parsed()
    req(x)
    datatable(x, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$citation_summary <- renderUI({
    x <- citation_set()
    if (is.null(x)) return(tags$span(class = "text-muted", "Citation chasing has not yet been run."))
    tags$div(
      tags$strong(sprintf("%d unique citation-chasing records.", nrow(x))),
      tags$span(sprintf(" %d backward references; %d forward citations.",
                        sum(x$direction %in% c("backward", "both")),
                        sum(x$direction %in% c("forward", "both"))))
    )
  })

  output$citations <- renderDT({
    req(citation_set())
    datatable(citation_set(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$coverage_summary <- renderUI({
    x <- analysed_set()
    if (is.null(x)) {
      return(tags$span(class = "text-muted", "Run citation chasing, then analyse the draft search."))
    }

    eligible <- !x$is_benchmark
    total <- sum(eligible)
    captured <- sum(x$search_match & eligible)
    missed <- sum(x$candidate_source)
    pct <- if (total > 0) 100 * captured / total else NA_real_

    tags$div(
      tags$strong(sprintf("%d of %d non-benchmark citation records matched locally", captured, total)),
      if (is.finite(pct)) tags$span(sprintf(" (%.1f%%).", pct)),
      tags$span(sprintf(" %d records remain for candidate-term discovery.", missed))
    )
  })

  output$missed_records <- renderDT({
    req(analysed_set())
    x <- analysed_set()
    x <- x[x$candidate_source, , drop = FALSE]
    keep <- intersect(c("lens_id", "title", "year", "authors", "doi", "keywords", "direction"), names(x))
    datatable(x[, keep, drop = FALSE], rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$candidate_terms <- renderDT({
    req(candidates())
    datatable(candidates(), rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })

  output$download_citations <- downloadHandler(
    filename = function() paste0("searchhelper-citation-set-", Sys.Date(), ".csv"),
    content = function(file) {
      req(citation_set())
      write.csv(citation_set(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
