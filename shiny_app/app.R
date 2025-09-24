# Set up =======================================================================

# Load libraries
require(ASySD)
require(bslib)
require(dplyr)
require(DT)
require(ggplot2)
require(htmlwidgets)
require(knitr)
require(networkD3)
library(plotly)
require(progressr)
require(RCurl)
require(readr)
require(rsconnect)
require(shiny)
require(shinyalert)
require(shinycssloaders)
require(shinyhelper)
require(shinythemes)
require(shinyWidgets)
require(stringr)
require(XML)
require(openxlsx)

# Source functions
source("process_data.R")
source("summary_table.R")
source("load_studies.R")
source("get_first_active_year.R")
source("exports.R")

# To use development build, set to TRUE
is_dev_build <- FALSE
# TO DO: use environment variable to avoid committing dev build to GitHub
# is_dev_build <- Sys.getenv("SHINY_ENV", unset = "prod") == "dev"  # doesn't work?
if (is_dev_build) {
  require(digest)  # to generate hash for caching
}

# UI Code ======================================================================

# Define UI for application
ui <- fluidPage(
#
#   theme = bs_theme(preset = "morph", primary = "#5F4C83", secondary = "#81D4FA",
#                            success = "#E3F2FD", warning = "#AE9ECC", font_scale = NULL),
  theme = bs_theme(
    preset = "minty",
    primary = "#00695C",   # dark teal
    secondary = "#80CBC4", # muted teal
    success = "#43A047",   # green
    warning = "#FFA000",   # amber
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),

  # Application title
  titlePanel(if (is_dev_build) "DREAMR (dev)" else "DREAMR"),

  # Sidebar with file upload options
  sidebarLayout(
    sidebarPanel(
      h4("File upload ", icon("upload")),
      br(),

      shinyWidgets::prettyRadioButtons(
        inputId = "fileType",
        label = "Select file type:",
        inline = FALSE,
        choices = c("Endnote Export (XML)",
                    # "Zotero Export (CSV)",
                    "Comma Separated Value (CSV)", 
                    # "Tab Delimited (TXT)",
                    # "Bibliographic (BIB)", 
                    # "Research Information Systems (RIS)",
                    "Excel spreadsheet (XLSX)"),
        status = "primary"),
      br(),

      # Input: select a file to upload
      fileInput("uploadfile", "Choose file to upload:",
                multiple = FALSE,
                placeholder = "No file selected",
                options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000)),
      width = 3),

    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of Uploaded Studies",
                 br(),
                 h4("Citation Summary"),
                 tableOutput("upload_summary"),
        ),
        tabPanel("Retrieved metadata",
                 br(),
                 h4("Data completeness"),
                 DT::dataTableOutput("missing_data_table") %>% withSpinner(color="#754E9B", type=7),
        ),
        tabPanel("Overview",
                 br(),
                 # fluidRow(
                 #   column(6, plotlyOutput("oa_plot")),   # 6-column width (half of the row)
                 #   column(6, plotlyOutput("funder_plot")) # 6-column width (other half)
                 # )
        ),

  tabPanel("Summary table",
     br(),
     uiOutput("summary_status"),
     br(),
     DTOutput("summary_table") %>% withSpinner(color="#00695C", type=7)
  ),

       tabPanel("Downloads",
                br(),
                h4("Download Data"),
                uiOutput("download_ready"),
                br(),
                uiOutput("download_button_raw")
        )
      )
    )
  )
)

# Server Code ==================================================================

# Define server logic
server <- function(input, output) {

  # Reactive values for storage
  rv <- reactiveValues()
  rv$refdata <- NULL
  rv$citation_summary <- data.frame()
  rv$pub_metadata <- NULL
  rv$authors <- NULL
  rv$institutions <- NULL
  rv$chardata <- NULL

  # File upload
  observeEvent(input$uploadfile, {

    shiny::validate(need(input$uploadfile != "", "Select your citation file to upload..."))

    if (is.null(input$uploadfile)) return(NULL)

    isolate({
      # Load citations based on file type
      method <- switch(input$fileType,
                       "Endnote Export (XML)" = "endnote",
                       # "Zotero Export (CSV)" = "zotero_csv",
                       "Comma Separated Value (CSV)" = "csv",
                       # "Research Information Systems (RIS)" = "ris",
                       # "Bibliographic (BIB)" = "bib",
                       "Excel spreadsheet (XLSX)" = "xlsx",
                       stop("Unknown file type"))

      citations <- load_studies(input$uploadfile$datapath,
                                                input$uploadfile$name,
                                                method = method)

      # Ensure unique record_id
      if(length(unique(citations$record_id)) != nrow(citations)){
        citations <- citations %>% mutate(record_id = as.character(row_number() + 1000))
      }

      rv$refdata <- citations

      # Citation summary
      rv$citation_summary <- citations %>%
        group_by(file_name) %>%
        summarise(`Number of articles` = n(), .groups = "drop")
    })
  })


  # Run DREAMR extraction when file uploaded
  observeEvent(rv$refdata, {

    shiny::validate(need(rv$refdata, "No reference data available"))

    rv$loading <- TRUE
    # Progress feedback
    withProgress(message = "Retrieving & processing metadata", value = 0, {
      progress_fun <- function(detail, amount) {
        incProgress(amount, detail = detail)
      }
      if (is_dev_build) 
        oa_data <- dreamr_extract_cached(rv$refdata, progress = progress_fun)
      else
        oa_data <- dreamr_extract(rv$refdata, progress = progress_fun)
    })

    rv$authors <- oa_data$authors
    rv$institutions <- oa_data$institutions
    rv$oa_data <- oa_data
    rv$loading <- FALSE
  })


  # Output: Summary table - ----
  output$upload_summary <- renderTable({
    req(rv$citation_summary)
    rv$citation_summary
  })


  # Output: Data completeness table----
  output$missing_data_table <- renderDT({

    shiny::validate(
      shiny::need(!is.null(rv$oa_data), "No OpenAlex data yet")
    )

    # Generate field completeness table using the new function
    missing_table <- oa_field_completeness_table(rv$oa_data)

    # Color coding based on Percent_missing
    DT::datatable(missing_table,
                  options = list(
                    searching = FALSE,
                    lengthChange = FALSE,
                    pageLength = 20,
                    scrollX = TRUE
                  ),
                  rownames = FALSE
    ) %>%
      formatStyle(
        "Percent_missing",
        target = "cell",
        backgroundColor = styleInterval(
          c(0, 25, 50, 75),
          c("#A5D6A7",  # light green (good)
            "#81C784",  # medium green
            "#FFF59D",  # soft yellow
            "#FFB74D",  # muted orange
            "#E57373"   # soft red
          )),
        color = "white",
        fontWeight = "bold"
      ) %>%
      formatStyle(
        c("Field","Retrieved_records"),
        fontWeight = "bold",
        color = "black"
      )
  })

# Output --- summary table
  output$summary_table <- renderDT({
    shiny::validate(shiny::need(!is.null(rv$oa_data), "Summary not ready yet"))
    total_citations <- if (!is.null(rv$refdata)) nrow(rv$refdata) else NA_integer_
    summary_df <- generate_summary_table(rv$oa_data, total_citations = total_citations)
    datatable(
      summary_df,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })

  # Summary status indicator
  output$summary_status <- renderUI({
    if (isTRUE(rv$loading)) {
      span(icon("spinner", class = "fa-spin"), " Building summary table...", class = "text-warning")
    } else if (!is.null(rv$oa_data)) {
      span(icon("check-circle"), " Summary table ready", class = "text-success fw-bold")
    } else {
      span(icon("hourglass-half"), " Waiting for metadata...", class = "text-muted")
    }
  })

  # Download readiness indicator
  output$download_ready <- renderUI({
    if (isTRUE(rv$loading)) {
      span(icon("spinner", class = "fa-spin"), " Preparing data exports...", class = "text-warning")
    } else if (!is.null(rv$oa_data)) {
      span(icon("check-circle"), " Exports ready (raw + summary)", class = "text-success fw-bold")
    } else {
      span(icon("hourglass-half"), " Upload and process data to enable downloads", class = "text-muted")
    }
  })

  # Raw download button (disabled while loading / not ready)
  output$download_button_raw <- renderUI({
    disabled <- isTRUE(rv$loading) || is.null(rv$oa_data)
    btn <- downloadButton("download_raw_table", "Download Raw Table as CSV")
    if (disabled) {
      btn$children[[1]]$attribs$disabled <- "disabled"
      btn$children[[1]]$attribs$class <- paste(btn$children[[1]]$attribs$class, "disabled")
    }
    btn
  })

  # Output: Download multiple files ----
  setup_download_handlers(output, rv)
}

# Run the Application ==========================================================

shinyApp(ui = ui, server = server)
