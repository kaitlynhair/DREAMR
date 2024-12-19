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

# Source functions
source("process_data.R")

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
                 h4("Data completeness"),
                 DT::dataTableOutput("missing_data_table") %>% withSpinner(color="#754E9B", type=7),
        ),
        tabPanel("Transparency",
                 br(),
                 plotlyOutput("oa_plot"),
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
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "endnote")
        } else if(input$fileType == "Comma Separated Value (CSV)" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "csv")
        } else if(input$fileType == "Zotero Export (CSV)" & all(grepl(".csv$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "zotero_csv")
        } else if(input$fileType == "Research Information Systems (RIS)" & all(grepl(".txt|.ris$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "ris")
        } else if(input$fileType == "Bibliographic (BIB)" & all(grepl(".bib$", input$uploadfile$name))){
          citations <- load_multi_search(input$uploadfile$datapath, input$uploadfile$name, method = "bib")
        } else if(input$fileType == "Tab Delimited (TXT)"){
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

      # Number of studies uploaded
      citation_summary <- rv$refdata %>%
        select(file_name) %>%
        group_by(file_name) %>%
        add_count(name = "number_of_citations") %>%
        group_by(file_name, number_of_citations) %>%
        distinct() %>%
        select(file_name, number_of_citations)

      rv$citation_summary <- citation_summary

      # Summary calculations
      oa_data <- process_oa_data(rv$refdata, global_south_country_codes, oa_identifier=input$identifierType)
      rv_oa_data_count <- length(oa_data$id)
      rv$oa_data <- oa_data

    }
  })

  # Output: Summary table - how many studies uploaded----
  output$summaryTable <- renderTable({

    shiny::validate(
      shiny::need(!is.null(rv$citation_summary), "")
    )

    rv$citation_summary
  })

  # Output: Data completeness table----
  output$missing_data_table <- renderDT({

    shiny::validate(
      shiny::need(!is.null(rv$oa_data), "")
    )

    # original citations file, keeping identifier columns only
    citations <- rv$refdata %>%
      select(any_of(c("doi", "pmcid", "pmid", "title")))#

    # join back to OG citations file to see how many are in open alex
    combined_data <- right_join(citations, rv$oa_data)

    check_cols <- c(names(rv$oa_data))
    subset_data <- subset(combined_data, select = check_cols)

    # Define what constitutes missing data
    is_missing <- function(x) is.na(x) | x == "Unknown"

    # Calculate missing counts and percentages
    missing_counts <- colSums(apply(subset_data, 2, is_missing))
    total_rows <- nrow(subset_data)
    missing_percentages <- (missing_counts / total_rows) * 100

    # Create a data frame with the missing counts and percentages for each column
    missing_table <- data.frame(
      field = names(missing_counts),
      count_missing = missing_counts,
      percentage_missing = round(missing_percentages, 1)
    )

    DT::datatable(missing_table,
                  options = list(
                    searching = FALSE,
                    lengthChange = FALSE,
                    pageLength = 20
                  ),
                  rownames = FALSE
    ) %>%
      formatStyle(
        "percentage_missing",
        target = "cell",
        backgroundColor = styleInterval(
          c(0, 25, 50, 75),
          c("yellowgreen", "yellowgreen", "orange", "orange", "red")
        ),
        color = "white",
        fontWeight = "bold",
        borderRadius = "2px",
        border = "2px solid",
        borderColor = styleInterval(
          c(0, 25, 50, 75),
          c("yellowgreen", "yellowgreen", "orange", "orange", "red")
        ),
        padding = "3px",
        display = "flex"
      )
  })

  # Output: Bar chart of open access status----
  output$oa_plot <- renderPlotly({
    # Assume `df` is your dataset with `id` and `is_oa` columns

    df <- rv$oa_data  # Replace with your actual data

    # Preprocess data
    oa_summary <- df %>%
      mutate(is_oa = case_when(
        is.na(is_oa) ~ "Unknown",
        is_oa == "Unknown" ~ "Unknown",
        TRUE ~ as.character(is_oa)
      )) %>%
      group_by(is_oa) %>%
      summarise(count = n()) %>%
      mutate(percentage = round((count / sum(count)) * 100, 1))

    # Plot the data
   p <- ggplot(oa_summary, aes(x = is_oa, y = count, fill = is_oa)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +
      scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red", "Unknown" = "gray")) +
      labs(
        title = "Open Access Status",
        x = "Open Access Status",
        y = "Number of Papers",
        fill = "OA Status"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(face = "bold")
      )

    # Convert to ggplotly for interactivity
    ggplotly(p) %>%
      layout(
        hovermode = "closest",
        hoverlabel = list(font = list(size = 12)),
        xaxis = list(title = "Open Access Status"),
        yaxis = list(title = "Number of Papers")
      )
  })

  # # Output: Percentage table (DOI and PMID presence)
  # output$percentageTable <- renderTable({
  #   req(rv$refdata)
  #   refdata <- rv$refdata
  #
  #   total_records <- nrow(refdata)
  #
  #   # Calculate DOI and PMID percentages
  #   doi_present <- sum(!is.na(refdata$doi) & refdata$doi != "") / total_records * 100
  #   pmid_present <- if ("pmid" %in% colnames(refdata)) {
  #     sum(!is.na(refdata$pmid) & refdata$pmid != "") / total_records * 100
  #   } else {
  #     NA
  #   }
  #
  #   percentage_table <- data.frame(
  #     Metric = c("% DOI Present", "% PMID Present"),
  #     Percentage = c(round(doi_present, 2), round(pmid_present, 2))
  #   )
  #
  #   percentage_table
  # })

}

# Run the Application ==========================================================

shinyApp(ui = ui, server = server)
