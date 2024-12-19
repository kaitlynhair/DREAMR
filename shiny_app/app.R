# Set up =======================================================================

# Load libraries
require(ASySD)
require(bslib)
require(dplyr)
require(DT)
require(htmlwidgets)
require(knitr)
require(networkD3)
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

# Source functions
source("process_data.R")
source("load_multi_search_pmid_pmcid.R")

# UI Code ======================================================================

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("DREAMR"),
  
  # Sidebar with file upload options
  sidebarLayout(
    sidebarPanel(
      h4("File upload ", icon("upload")),
      br(),
      
      shinyWidgets::prettyRadioButtons(
        inputId = "fileType",
        label = "Select file type:",
        inline = FALSE,
        choices = c("Endnote Export (XML)", "Zotero Export (CSV)",
                    "Comma Separated Value (CSV)", "Tab Delimited (TXT)",
                    "Bibliographic (BIB)", "Research Information Systems (RIS)"),
        status = "primary"),
      br(),
      
      # Input: select a file to upload
      fileInput("uploadfile", "Choose file to upload:",
                multiple = FALSE,
                placeholder = "No file selected")),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of Uploaded Studies",
                 br(),
                 h4("Citation Summary"),
                 tableOutput("summaryTable"),
                 br(),
                 h4("Percentage of DOI and PMID Presence"),
                 tableOutput("percentageTable")
        ),
        tabPanel("Transparency",
                 br(),
                 h4("Author Distribution"),
                 p("Author Distribution Visualization Placeholder") # Content placeholder
        ),
        tabPanel("Author Institutions",
                 br(),
                 h4("Author Distribution"),
                 p("Author Distribution Visualization Placeholder") # Content placeholder
        ),
        tabPanel("Author Location",
                 br(),
                 h4("Author Distribution"),
                 p("Author Distribution Visualization Placeholder") # Content placeholder
        )
      )
    )
  )
)

# Server Code ==================================================================

# Define server logic
server <- function(input, output) {
  
  # Reactive values for storage
  rv <- shiny::reactiveValues()
  rv$refdata <- NULL
  rv$citation_summary <- data.frame()
  rv$oa_data <- NULL
  
  # File upload event
  shiny::observeEvent(input$uploadfile, {
    shiny::validate(need(input$uploadfile != "", "Select your citation file to upload..."))
    
    if (is.null(input$uploadfile)) {
      return(NULL)
    } else {
      isolate(
        if(input$fileType == "Endnote Export (XML)" & all(grepl(".xml$", input$uploadfile$name))){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "endnote")
        } else if(input$fileType == "Comma Separated Value (CSV)" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "csv")
        } else if(input$fileType == "Zotero Export (CSV)" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "zotero_csv")
        } else if(input$fileType == "Research Information Systems (RIS)" & all(grepl(".txt|.ris$", input$uploadfile$name))){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "ris")
        } else if(input$fileType == "Bibliographic (BIB)" & all(grepl(".bib$", input$uploadfile$name))){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "bib")
        } else if(input$fileType == "Tab Delimited (TXT)"){
          citations <- load_multi_search_pmid_pmcid(input$uploadfile$datapath, input$uploadfile$name, method = "txt")
        } else {
          shinyalert("Wrong file type selected",
                     "The file extension doesn't match the selected upload format", type = "warning")
          shinyjs::reset(id = "uploadfile", asis = FALSE)
          return()
        }
      )
      
      # Ensure unique record_id
      if (length(unique(citations$record_id)) != nrow(citations)) {
        shinyalert("Unique identifier generated!",
                   "The record_id column within uploaded citations was not unique. ASySD has generated a unique record_id for each citation. You can preview this in the table.", type = "info")
        
        citations <- citations %>%
          mutate(record_id = as.character(row_number() + 1000))
      }
      rv$refdata <- citations
      
      # Summary calculations
      citation_summary <- rv$refdata %>%
        select(source, label, file_name) %>%
        group_by(file_name) %>%
        add_count(name = "number_of_citations") %>%
        group_by(file_name, number_of_citations) %>%
        distinct() %>%
        summarise(source = ifelse(all(is.na(source)), "", paste(ifelse(!is.na(source), source, NA), collapse = ", ")),
                  label = ifelse(all(is.na(label)), "", paste(ifelse(!is.na(label), label, NA), collapse = ", "))) %>%
        mutate(source = ifelse(str_length(source) > 10, "multiple", source),
               label = ifelse(str_length(label) > 10, "multiple", label)) %>%
        select(file_name, number_of_citations, label, source)
      
      rv$citation_summary <- citation_summary
      
      # Summary calculations
      
      oa_data <- process_oa_data(rv$refdata, global_south_country_codes)
      rv_oa_data_count <- length(oa_data$id)
      rv$oa_data <- oa_data
      
    }
  })
  
  # Output: Summary table
  output$summaryTable <- renderTable({
    rv$citation_summary
    rv$oa_data_count
  })
  
  # Output: Percentage table (DOI and PMID presence)
  output$percentageTable <- renderTable({
    req(rv$refdata)
    refdata <- rv$refdata
    
    total_records <- nrow(refdata)
    
    # Calculate DOI and PMID percentages
    doi_present <- sum(!is.na(refdata$doi) & refdata$doi != "") / total_records * 100
    pmid_present <- if ("pmid" %in% colnames(refdata)) {
      sum(!is.na(refdata$pmid) & refdata$pmid != "") / total_records * 100
    } else {
      NA
    }
    
    percentage_table <- data.frame(
      Metric = c("% DOI Present", "% PMID Present"),
      Percentage = c(round(doi_present, 2), round(pmid_present, 2))
    )
    
    percentage_table
  })
  
}

# Run the Application ==========================================================

shinyApp(ui = ui, server = server)