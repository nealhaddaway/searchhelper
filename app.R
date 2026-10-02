library(shiny)
library(bslib)
library(DT)

source("R/ris.R")
source("R/lens_api.R")

ui <- page_sidebar(
  title = "Search Helper",
  sidebar = sidebar(
    fileInput("ris", "Benchmark records (RIS)", accept = c(".ris", ".txt")),
    textAreaInput("search_string", "Draft Boolean search string", rows = 7,
                  placeholder = '(concept A OR synonym*) AND ("concept B" OR term)'),
    actionButton("resolve", "Resolve benchmarks in Lens", class = "btn-primary"),
    actionButton("chase", "Run citation chasing", disabled = TRUE),
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
  )
)

server <- function(input, output, session) {
  token <- Sys.getenv("LENS_API_TOKEN", unset = "")
  parsed <- reactiveVal(NULL)
  resolved <- reactiveVal(NULL)
  citation_set <- reactiveVal(NULL)

  observeEvent(input$ris, {
    req(input$ris$datapath)
    x <- parse_ris(input$ris$datapath)
    parsed(x)
    resolved(NULL)
    citation_set(NULL)
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
      meta <- lens_fetch_records(unique(c(links$backward_lens_id, links$forward_lens_id)), token)
      incProgress(0.45)
      citation_set(merge_citation_metadata(links, meta))
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

  output$download_citations <- downloadHandler(
    filename = function() paste0("searchhelper-citation-set-", Sys.Date(), ".csv"),
    content = function(file) {
      req(citation_set())
      write.csv(citation_set(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
