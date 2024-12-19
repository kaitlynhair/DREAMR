source("process_data.R")
require(shiny)
require(readr)
require(DT)
library(shinyhelper)
require(stringr)
require(dplyr)
library(shinyalert)
library(progressr)
library(networkD3)
library(rsconnect)
library(RCurl)
library(shiny)
library(ASySD)
library(shinythemes)
library(knitr)
library(shinycssloaders)
library(htmlwidgets)
library(shinyWidgets)
library(bslib)

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("DREAMR"),

  # Sidebar with file upload options
  sidebarLayout(
    sidebarPanel(
      h4("Upload citations file ", icon("upload")),

      shinyWidgets::prettyRadioButtons(
        inputId = "fileType",
        label = "Choose a file type to upload",
        inline = TRUE,
        choices = c("Endnote XML",
                    "CSV", "BIB", "RIS", "Zotero CSV", "Tab delimited"),
        status = "primary"),
      br(),

      # Input: select a file to upload
      fileInput("uploadfile", "Choose file(s) to upload",
                multiple = TRUE,
                placeholder = "No file selected"),
    br(),
    prettyRadioButtons(
      inputId = "identifierType",
      label = "Select Identifier to Process",
      choices = c("doi", "pmid", "pmcid"),
      inline = TRUE,
      status = "success"
    )),

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
        if(input$fileType == "Endnote XML" & all(grepl(".xml$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "endnote")
        } else if(input$fileType == "CSV" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "csv")
        } else if(input$fileType == "Zotero CSV" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "zotero_csv")
        } else if(input$fileType == "RIS" & all(grepl(".txt|.ris$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "ris")
        } else if(input$fileType == "BIB" & all(grepl(".bib$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "bib")
        } else if(input$fileType == "Tab delimited"){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "txt")
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

      oa_data <- process_oa_data(rv$refdata, global_south_country_codes, oa_identifier=input$identifierType)
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

# Run the application
shinyApp(ui = ui, server = server)
