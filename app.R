library(shiny)
library(jsonlite)
library(paws.common)
library(ellmer)
library(shinychat)

options(shiny.maxRequestSize = 250 * 1024^2)

slim_har_for_llm <- function(har) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  slim_headers <- function(headers) {
    if (is.null(headers) || !is.list(headers)) return(list())
    keep_names <- c("server", "content-type", "cache-control", "pragma", "expires",
                    "x-frame-options", "x-content-type-options", "content-encoding",
                    "transfer-encoding", "upgrade", "accept", "sec-websocket-")
    headers[grepl(paste(keep_names, collapse = "|"), tolower(sapply(headers, function(h) h$name %||% "")))]
  }
  
  cleaned_entries <- lapply(har$log$entries, function(entry) {
    if (is.null(entry$request) || is.null(entry$response)) return(NULL)
    
    list(
      startedDateTime = entry$startedDateTime %||% NULL,
      time = entry$time %||% NULL,
      request = list(
        method = entry$request$method %||% NULL,
        url = entry$request$url %||% NULL,
        httpVersion = entry$request$httpVersion %||% NULL,
        headers = slim_headers(entry$request$headers),
        headersSize = entry$request$headersSize %||% NULL,
        bodySize = entry$request$bodySize %||% NULL
      ),
      response = list(
        status = entry$response$status %||% NULL,
        statusText = entry$response$statusText %||% NULL,
        httpVersion = entry$response$httpVersion %||% NULL,
        headers = slim_headers(entry$response$headers),
        redirectURL = entry$response$redirectURL %||% NULL,
        headersSize = entry$response$headersSize %||% NULL,
        bodySize = entry$response$bodySize %||% NULL,
        content = list(
          size = entry$response$content$size %||% NULL,
          mimeType = entry$response$content$mimeType %||% NULL
        )
      ),
      cache = entry$cache %||% list(),
      timings = entry$timings %||% NULL,
      serverIPAddress = entry$serverIPAddress %||% NULL,
      connection = entry$connection %||% NULL,
      pageref = entry$pageref %||% NULL
    )
  })
  
  har$log$entries <- Filter(Negate(is.null), cleaned_entries)
  return(har)
}

ui <- bslib::page_fluid(
  titlePanel("HAR Performance Analyzer (Posit Connect)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("harfile", "Upload HAR File", accept = ".har"),
      downloadButton("download_cleaned", "Download Cleaned HAR"),
      hr(),
      helpText("After uploading a HAR file, the summary will appear and Claude will start the chat automatically.")
    ),
    mainPanel(
      h4("HAR File Summary"),
      verbatimTextOutput("har_summary"),
      hr(),
      h4("Claude Chat (Bedrock)"),
      chat_ui("chat")
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    har_clean = NULL,
    har_summary = NULL,
    chat = NULL,
    summary_ready_at = NULL,
    chat_started = FALSE
  )
  
  observeEvent(input$harfile, {
    req(input$harfile)
    
    values$chat <- NULL
    values$chat_started <- FALSE
    values$summary_ready_at <- NULL
    chat_clear("chat")
    
    har <- fromJSON(input$harfile$datapath, simplifyVector = FALSE)
    har_slim <- slim_har_for_llm(har)
    values$har_clean <- har_slim
    
    original_size <- file.info(input$harfile$datapath)$size
    temp_path <- tempfile(fileext = ".har")
    write(toJSON(har_slim, auto_unbox = TRUE, pretty = TRUE), temp_path)
    cleaned_size <- file.info(temp_path)$size
    unlink(temp_path)
    
    reduction_pct <- round(100 * (1 - (cleaned_size / original_size)), 1)
    entries_count <- length(har_slim$log$entries)
    
    values$har_summary <- paste0(
      "✅ HAR stripping complete.\n",
      "• Original size: ", formatC(original_size / 1024^2, digits = 2, format = "f"), " MB\n",
      "• Cleaned size:  ", formatC(cleaned_size / 1024^2, digits = 2, format = "f"), " MB\n",
      "• Reduction:     ", reduction_pct, "% smaller\n",
      "• Entries:       ", entries_count, "\n"
    )
  })
  
  output$har_summary <- renderText({
    req(values$har_summary)
    isolate({ values$summary_ready_at <- Sys.time() })
    values$har_summary
  })
  
  output$download_cleaned <- downloadHandler(
    filename = function() {
      paste0("cleaned-", tools::file_path_sans_ext(input$harfile$name), ".har")
    },
    content = function(file) {
      req(values$har_clean)
      write(toJSON(values$har_clean, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )
  
  observeEvent(values$summary_ready_at, {
    req(!values$chat_started, values$har_clean)
    session$onFlushed(function() {
      isolate({
        if (is.null(values$chat)) {
          values$chat_started <- TRUE
          
          har_json <- toJSON(values$har_clean, auto_unbox = TRUE, pretty = TRUE)
          truncated_json <- substr(har_json, 1, 800000)
          
          initial_prompt <- paste(
            "This HAR file has been stripped of body content, but retains headers, URLs, status codes, timings, and server info.",
            "Your task is to analyze this HAR for signs of unexpected delays, and backend, protocol, or transport-related issues.",
            "\n\nFocus on identifying and calling out:\n",
            "1. Any excessively long calls. (e.g., Over 20 seconds).\n",
            "2. Any failed or incomplete transport protocols (e.g., WebSocket failed, XHR streaming failed).\n",
            "3. Protocol fallback patterns — such as WebSocket → XHR Streaming → XHR Send (polling).\n",
            "   - Note: `xhr_send` typically indicates the final fallback to XHR polling.\n",
            "   - Note: `wss` (websockets) should work and if they fail that is a major issue to focus on and correct.\n",
            "4. Requests that are significantly slower than others. (Provide time in seconds instead of ms)\n",
            "5. Proxy or load balancer involvement — infer from `server` headers (e.g., nginx, Cloudflare).\n",
            "\nDo NOT comment on CSS/JS asset sizes or frontend optimizations — focus entirely on backend, protocol, and network-layer analysis.\n",
            "\n\nHere is the HAR content (truncated):\n\n",
            truncated_json
          )
          
          values$chat <- chat_bedrock(
            model = "us.anthropic.claude-3-5-sonnet-20241022-v2:0",
            #model = "us.anthropic.claude-3-7-sonnet-20250219-v1:0",
            system_prompt = "You're a backend and transport-level network analyst. Help diagnose performance, proxy behavior, and protocol fallbacks using stripped HAR files."
          )
          
          stream <- values$chat$stream_async(initial_prompt)
          chat_append("chat", stream)
        }
      })
    }, once = TRUE)
  })
  
  observeEvent(input$chat_user_input, {
    req(values$chat)
    stream <- values$chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)
