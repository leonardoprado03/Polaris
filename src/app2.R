library(shiny)
library(plotly)
library(dplyr)

# ==========================================================
# 0. WORKER LOGIC
# ==========================================================
args <- commandArgs(trailingOnly = TRUE)
if ("--worker" %in% args) {
  params_path <- args[which(args == "--worker") + 1]
  params <- readRDS(params_path)
  
  # Load functions from the external file
  if(file.exists("search_functions2.R")) source("search_functions2.R")
  
  # Internal function to report progress to the user interface
  report_progress <- function(src, query, n, status, message = "") {
    log_file <- file.path(params$results_dir, "live_monitor_log.csv")
    new_entry <- data.frame(
      timestamp = format(Sys.time(), "%H:%M:%S"),
      source = src,
      query = query,
      n_results = n,
      status = status,
      message = ifelse(is.na(message), "", message),
      stringsAsFactors = FALSE
    )
    write.table(new_entry, log_file, sep = ",", row.names = FALSE,
                col.names = !file.exists(log_file), append = file.exists(log_file))
  }
  # REAL SEARCHING
  for (db in params$dbs) {
    for (q in params$queries) {
      tryCatch({
        # CALL search_functions2.R
        res <- run_search_for_source(
          src = db, queries = q,
          results_dir = params$results_dir,
          api_key = params$elsevier_api_key,
          inst_token = params$elsevier_inst_key,
          skip_done = params$skip_done,
          fetch_abstracts_scopus = params$fetch_abs_scopus
        )
        
        report_progress(
          db, q,
          res$n_found %||% 0,
          res$status %||% "Success",
          res$message %||% ""
        )
        
      }, error = function(e) {
        report_progress(db, q, 0, "Error")
      })
    }
  }
  quit(save = "no")
}

# ==========================================================
# 1. SETTINGS 
# ==========================================================
APP_ROOT    <- Sys.getenv("APP_ROOT", unset = normalizePath(getwd()))
RESULTS_DIR <- file.path(APP_ROOT, "results")
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

if(file.exists(file.path(APP_ROOT, "search_functions2.R"))) {
  source(file.path(APP_ROOT, "search_functions2.R"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
DB_CHOICES <- c("PubMed"="pubmed", "Scopus"="scopus", "ScienceDirect"="scidir", "Semantic Scholar"="semantic_scholar")
ALL_DB_CODES <- unname(DB_CHOICES)

empty_log_df <- function() {
  data.frame(
    timestamp=character(),
    source=character(),
    query=character(),
    n_results=numeric(),
    status=character(),
    message=character(),
    stringsAsFactors=FALSE
  )
}

safe_read_log <- function(path) {
  if (!file.exists(path)) return(empty_log_df())
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) empty_log_df())
}

# ==========================================================
# 2. INTERFACE (UI)
# ==========================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #8C1D40 !important; font-family: 'Segoe UI', sans-serif; }
      .container-fluid { background: transparent !important; }
      #splash-screen {
        position: fixed; inset: 0; z-index: 20000;
        display: flex; align-items: center; justify-content: center;
        background: linear-gradient(135deg, rgba(108,14,42,0.96) 0%, rgba(140,29,64,0.98) 55%, rgba(58,10,25,1) 100%);
      }
      .splash-card {
        width: min(760px, 92vw); background: rgba(255,255,255,0.08);
        border: 1px solid rgba(255,255,255,0.18); border-radius: 28px;
        padding: 42px 34px; text-align: center; color: white;
        box-shadow: 0 18px 50px rgba(0,0,0,0.28);
      }
      .splash-logo img { max-width: 200px; height: auto; margin-bottom: 22px; }
      .splash-subtitle { font-size: 20px; color: #F6D365; margin-bottom: 14px; }
      .splash-btn { 
        background: #FFC627 !important; color: #000 !important; border: none !important; 
        font-weight: 700 !important; border-radius: 12px !important; padding: 12px 26px !important;
      }
      .white-card { background: #FFFFFF; border-radius: 12px; box-shadow: 0 8px 20px rgba(0,0,0,0.15); padding: 20px; margin-bottom: 20px; }
      .asu-highlight { background: #FFC627; color: #000; padding: 5px 12px; border-radius: 6px; display: inline-block; font-weight: bold; margin-bottom: 12px; }
      .main-title { font-size: 20px !important; padding: 8px 18px !important; }
      .nav-tabs { border-bottom: none !important; }
      .nav-tabs > li > a { 
        background: #FFFFFF !important; color: #8C1D40 !important; font-weight: bold; 
        border-radius: 30px !important; margin-right: 10px; border: none !important; 
        padding: 10px 25px !important;
      }
      .nav-tabs > li.active > a { background: #FFC627 !important; color: #000 !important; }
      .metric-container { display: flex; gap: 15px; margin-bottom: 20px; }
      .metric-box { background: #FFFFFF; flex: 1; padding: 15px; border-radius: 10px; text-align: center; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }
      .metric-title { font-size: 11px; color: #777; font-weight: 600; text-transform: uppercase; }
      .metric-value { font-size: 26px; font-weight: 800; color: #8C1D40; }
      pre { background-color: #f8f9fa; border-radius: 8px; font-size: 13px; border: 1px solid #ddd; padding: 10px; }
    ")),
    tags$script(HTML("Shiny.addCustomMessageHandler('hideSplash', function(m){ $('#splash-screen').fadeOut(800); });"))
  ),
  
  tags$div(id = "splash-screen",
           tags$div(class = "splash-card",
                    tags$div(class = "splash-logo", tags$img(src = "polaris_logo.png")),
                    tags$div(class = "splash-subtitle", "An Automated Literature Retrieval System"),
                    actionButton("enter_app", "Enter System", class = "splash-btn"),
                    tags$div(class = "splash-footer", "Developed by Leonardo Prado")
           )
  ),
  
  div(style="display: flex; align-items: center; justify-content: space-between; padding: 10px 0;",
      div(class="asu-highlight main-title", "An Automated Literature Retrieval System"),
      tags$img(src = "polaris_logo.png", height="50px")
  ),
  
  tabsetPanel(id = "main_tabs",
              tabPanel("Search",
                       br(),
                       fluidRow(
                         column(4,
                                div(class="white-card",
                                    h4("Load queries"),
                                    radioButtons("query_mode", "Load queries from", choices = c("Upload file" = "upload", "File path" = "path"), inline = TRUE),
                                    conditionalPanel("input.query_mode == 'upload'", fileInput("queries_file", "Upload .txt", width = "100%")),
                                    conditionalPanel("input.query_mode == 'path'", textInput("queries_path", "Full path", width = "100%")),
                                    
                                    h4("Databases"),
                                    selectizeInput("sources", "Select one or more", choices = DB_CHOICES, selected = ALL_DB_CODES, multiple = TRUE, width = "100%"),
                                    fluidRow(
                                      column(6, actionButton("select_all", "Select all", style="width:100%; font-size:11px; background:#8C1D40; color:white; border-radius:15px;")),
                                      column(6, actionButton("clear_all", "Clear", style="width:100%; font-size:11px; border-radius:15px;"))
                                    ),
                                    
                                    h4("Elsevier Keys"),
                                    passwordInput("elsevier_api_key", "API Key", width = "100%"),
                                    passwordInput("elsevier_inst_key", "Institutional Token", width = "100%"),
                                    
                                    checkboxInput("skip_done", "Skip successful searches", TRUE),
                                    checkboxInput("fetch_abs_scopus", "Fetch Scopus abstracts", FALSE),
                                    br(),
                                    actionButton("run_btn", "RUN SEARCHES", style="width:100%; background:#8C1D40; color:white; font-weight:bold; border-radius:20px; height:40px;")
                                )
                         ),
                         column(8,
                                div(class="white-card",
                                    div(class="asu-highlight", "Run preview"),
                                    verbatimTextOutput("preview")
                                ),
                                div(class="white-card",
                                    div(class="asu-highlight", "Tip"),
                                    tags$ul(
                                      tags$li("Queries should be in txt file."),
                                      tags$li("Queries file should have one entry per line."),
                                      tags$li("Check if the Elsevier API key and institutional token are correctly provided."),
                                      tags$li("Search results will be logged in the Monitoring tab.")
                                    )
                                )
                         )
                       )
              ),
              
              tabPanel("Monitoring",
                       br(),
                       uiOutput("api_notice"),
                       div(class = "white-card", 
                           div(class="asu-highlight", "Execution progress"),
                           uiOutput("progress_ui"),
                           verbatimTextOutput("live_status")
                       ),
                       
                       div(class = "metric-container",
                           div(class="metric-box", div(class="metric-title", "Total queries"), uiOutput("m_total")),
                           div(class="metric-box", div(class="metric-title", "Completed"), uiOutput("m_done")),
                           div(class="metric-box", div(class="metric-title", "Total results"), uiOutput("m_res")),
                           div(class="metric-box", div(class="metric-title", "Empty queries"), uiOutput("m_empty"))
                       ),
                       
                       fluidRow(
                         column(6, div(class="white-card", div(class="asu-highlight", "Results by source"), plotlyOutput("plot_by_source", height = "300px"))),
                         column(6, div(class="white-card", div(class="asu-highlight", "Query status"), plotlyOutput("plot_status", height = "300px")))
                       ),
                       div(class="white-card", div(class="asu-highlight", "Results"), tableOutput("recent_queries_table"))
              )
  )
)

# ==========================================================
# 3. SERVER 
# ==========================================================
server <- function(input, output, session) {
  rv <- reactiveValues(
    running = FALSE, 
    total_queries = 0, 
    start_time = NULL, 
    elapsed = "0s", 
    live_log_path = file.path(RESULTS_DIR, "live_monitor_log.csv")
  )
  
  observeEvent(input$enter_app, { session$sendCustomMessage("hideSplash", list()) })
  observeEvent(input$select_all, { updateSelectizeInput(session, "sources", selected = ALL_DB_CODES) })
  observeEvent(input$clear_all, { updateSelectizeInput(session, "sources", selected = character(0)) })
  
  live_log_data <- reactive({ 
    invalidateLater(1500, session)
    safe_read_log(rv$live_log_path) 
  })
  
  output$preview <- renderPrint({
    list(
      app_root               = APP_ROOT,
      results_directory      = RESULTS_DIR,
      selected_databases     = input$sources,
      query_input_mode       = input$query_mode,
      uploaded_filename      = if (!is.null(input$queries_file)) input$queries_file$name else NA,
      manual_path            = input$queries_path,
      skip_done_searches     = input$skip_done,
      scopus_fetch_abstracts = input$fetch_abs_scopus,
      elsevier_api_key       = if (nzchar(input$elsevier_api_key %||% "")) "provided" else "not provided"
    )
  })
  
  output$live_status <- renderText({
    df <- live_log_data()
    last_q <- if(nrow(df) > 0) tail(df, 1) else list(query="None", source="None", n_results=0)
    
    paste0(
      "Status: ", if(rv$running) "Running" else "Idle", "\n",
      "Current database: ", if(rv$running) last_q$source else "None", "\n",
      "Current query: \"", if(rv$running) last_q$query else "None", "\"\n",
      "Completed: ", nrow(df), " / ", rv$total_queries, "\n",
      "Elapsed time: ", rv$elapsed
    )
  })
  
  output$m_total <- renderUI(div(class="metric-value", rv$total_queries))
  output$m_done  <- renderUI(div(class="metric-value", nrow(live_log_data())))
  output$m_res   <- renderUI(div(class="metric-value", sum(live_log_data()$n_results, na.rm=TRUE)))
  output$m_empty <- renderUI(div(class="metric-value", sum(live_log_data()$n_results == 0, na.rm=TRUE)))
  
  output$progress_ui <- renderUI({
    done <- nrow(live_log_data()); total <- max(rv$total_queries, 1)
    pct <- min(round(100 * done / total), 100)
    div(style="width: 100%; background: #eee; border-radius: 10px; height: 20px; margin: 10px 0; overflow: hidden;",
        div(style = paste0("width:", pct, "%; background: #FFC627; height: 100%; transition: width 0.5s; text-align: center; font-weight: bold; color: #000;"), paste0(pct, "%")))
  })
  output$api_notice <- renderUI({
    df <- live_log_data()
    if (nrow(df) == 0) return(NULL)
    
    bad <- df[df$status != "Success" & nzchar(df$message), , drop = FALSE]
    if (nrow(bad) == 0) return(NULL)
    
    last_bad <- tail(bad, 1)
    
    div(
      style = "background:#fff4f4;border-left:6px solid #b00020;padding:14px 16px;border-radius:10px;margin-bottom:15px;",
      strong("API warning: "),
      span(last_bad$message),
      br(),
      tags$small(paste0(
        "Source: ", last_bad$source,
        " | Query: ", last_bad$query
      ))
    )
  })
  output$plot_by_source <- renderPlotly({
    df <- live_log_data(); if(nrow(df)==0) return(NULL)
    plot_ly(df %>% group_by(source) %>% summarise(total=sum(n_results)), x=~source, y=~total, type="bar", marker=list(color='#8C1D40'))
  })
  
  output$plot_status <- renderPlotly({
    df <- live_log_data(); if(nrow(df)==0) return(NULL)
    plot_ly(df %>% count(status), labels=~status, values=~n, type="pie", marker=list(colors=c('#FFC627','#8C1D40')))
  })
  
  output$recent_queries_table <- renderTable({ tail(live_log_data()[,c("timestamp","source","n_results","status", "query")], 10) }, width = "100%")
  
  observeEvent(input$run_btn, {
    if (rv$running) return()
    
    queries <- tryCatch({
      read_queries_user(input$query_mode, input$queries_file, input$queries_path)
    }, error = function(e) NULL)
    
    if(is.null(queries) || length(queries) == 0) {
      showNotification("Queries not found!", type = "error")
      return()
    }
    
    rv$running <- TRUE
    rv$total_queries <- length(input$sources) * length(queries)
    rv$start_time <- Sys.time()
    
    if(file.exists(rv$live_log_path)) file.remove(rv$live_log_path)
    
    params <- list(
      queries = queries, dbs = input$sources, results_dir = RESULTS_DIR, 
      skip_done = input$skip_done, fetch_abs_scopus = input$fetch_abs_scopus, 
      elsevier_api_key = input$elsevier_api_key, elsevier_inst_key = input$elsevier_inst_key
    )
    params_path <- file.path(RESULTS_DIR, "worker_params.rds")
    saveRDS(params, params_path)
    
    tryCatch({
      this_script <- rstudioapi::getSourceEditorContext()$path
      if (is.null(this_script) || this_script == "") this_script <- file.path(getwd(), "app2.R")
      this_script <- normalizePath(this_script, mustWork = FALSE)
      
      r_path <- file.path(R.home("bin"), "Rscript")
      system2(r_path, args = c(shQuote(this_script), "--worker", shQuote(params_path)), wait = FALSE)
      
      showNotification("Worker started!", type = "message")
      updateTabsetPanel(session, "main_tabs", selected = "Monitoring")
    }, error = function(e) {
      rv$running <- FALSE
      showNotification(paste("Launch Error:", e$message), type = "error")
    })
  })
  
  observe({
    if (rv$running) {
      invalidateLater(1000)
      rv$elapsed <- paste0(round(as.numeric(difftime(Sys.time(), rv$start_time, units="secs"))), "s")
      if (nrow(live_log_data()) >= rv$total_queries && rv$total_queries > 0) {
        rv$running <- FALSE
        showNotification("All searches completed!", type = "message")
      }
    }
  })
}

shinyApp(ui, server)