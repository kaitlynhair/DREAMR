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

# Source functions
source("process_data.R")
source("bar_plot.R")
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
                placeholder = "No file selected",
                options(shiny.maxRequestSize=1000*1024^2, timeout = 40000000))),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Summary of Uploaded Studies",
                 br(),
                 h4("Citation Summary"),
                 tableOutput("upload_summary"),
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
                 plotlyOutput("gs_plot")
        ),
        tabPanel("Raw table",
                 br(),
                 tableOutput("raw_table"),
        ),
        tabPanel("Summary table",
                 br(),
                 uiOutput("summary_table")
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
        select(file_name) %>%
        group_by(file_name) %>%
        add_count(name = "number_of_citations") %>%
        group_by(file_name, number_of_citations) %>%
        distinct() %>%
        select(file_name, number_of_citations)

      rv$citation_summary <- citation_summary
      
      # Summary calculations      
      oa_data <- process_oa_data(rv$refdata, global_south_country_codes)

      rv_oa_data_count <- length(oa_data$id)
      rv$oa_data <- oa_data
      
    }
  })

  # Output: Summary table - how many studies uploaded----
  output$upload_summary <- renderTable({

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
    req(rv$oa_data)  # Ensure column is selected
    bar_plot(rv$oa_data, "is_oa", "% Open Access")
  })

  # Output: Bar chart of open access status----
output$gs_plot <- renderPlotly({
  req(rv$oa_data)  # Ensure column is selected
  bar_plot(rv$oa_data, "first_author_global_south", "first_auht% first authors from Global South")
})

  # Output: Raw data table - how many studies uploaded----
  output$raw_table <- renderTable({

    shiny::validate(
      shiny::need(!is.null(rv$citation_summary), "")
    )

    rv$oa_result
  })

  output$summary_table <- renderUI({

    year <- rv$oa_data %>%
      group_by(publication_year) %>%
      count(name="Number of papers") %>%
      rename(variable = publication_year) %>%
      ungroup() %>%
      mutate(variable = as.character(variable))

    oa_status <-  rv$oa_data %>%
      group_by(is_oa) %>%
      count(name="Number of papers") %>%
      rename(variable = is_oa)%>%
      ungroup() %>%
      mutate(variable = as.character(variable))

    first_author_country <- rv$oa_data %>%
      group_by(is_oa) %>%
      count(name="Number of papers") %>%
      rename(variable = is_oa)%>%
      ungroup() %>%
      mutate(variable = as.character(variable))

    citations <-  rv$oa_data %>%
      group_by(id) %>%
      summarise("mean_citations" = mean(cited_by_count))

    languages <-  rv$oa_data %>%
      group_by(language) %>%
      count(name="Number of papers") %>%
      rename(variable = language)%>%
      ungroup()

    gs_first <-  rv$oa_data %>%
      group_by(first_author_global_south) %>%
      count(name="Number of papers") %>%
      rename(variable = first_author_global_south)%>%
      ungroup() %>%
      mutate(variable = as.character(variable)) %>%
      mutate(variable = ifelse(is.na(variable), "Unknown", variable))

    gs_last <-  rv$oa_data %>%
      group_by(last_author_global_south) %>%
      count(name="Number of papers") %>%
      rename(variable = last_author_global_south)%>%
      ungroup() %>%
      mutate(variable = as.character(variable)) %>%
      mutate(variable = ifelse(is.na(variable), "Unknown", variable))

    first_auth_country <-  rv$oa_data %>%
      group_by(first_author_country) %>%
      count(name = "Number of papers") %>%
      mutate(
        # Replace countries with NA as "Unknown"
        first_author_country = ifelse(is.na(first_author_country), "Unknown", first_author_country),

        # Replace countries with <= 5 papers with "Other"
        first_author_country = ifelse(`Number of papers` > 1, first_author_country, "Other")
      ) %>%
      group_by(first_author_country) %>%  # Group by country or "Other" or "Unknown"
      summarize(Number_of_papers = sum(`Number of papers`)) %>%  # Sum the papers in each group
      arrange(desc(Number_of_papers)) %>%
      ungroup() %>%
      rename(variable = first_author_country) %>%
      rename(`Number of papers` = Number_of_papers)

    last_auth_country <-  rv$oa_data %>%
      group_by(last_author_country) %>%
      count(name = "Number of papers") %>%
      mutate(
        # Replace countries with NA as "Unknown"
        last_author_country = ifelse(is.na(last_author_country), "Unknown", last_author_country),

        # Replace countries with <= 5 papers with "Other"
        last_author_country = ifelse(`Number of papers` > 1, last_author_country, "Other")
      ) %>%
      group_by(last_author_country) %>%  # Group by country or "Other" or "Unknown"
      summarize(Number_of_papers = sum(`Number of papers`)) %>%  # Sum the papers in each group
      arrange(desc(Number_of_papers)) %>%
      ungroup() %>%
      rename(variable = last_author_country) %>%
      rename(`Number of papers` = Number_of_papers)

    # Create a list of the data frames to be added
    dfs <- list(year, oa_status, languages, first_auth_country, gs_first, last_auth_country, gs_last)

    # Calculate start and end rows
    start_row <- cumsum(c(1, sapply(dfs, nrow)))[-length(dfs)]  # All start rows, except the last group
    end_row <- cumsum(sapply(dfs, nrow))  # Cumulative end row for each group

    # Create the frequency table by adding rows from each data frame
    freq_table <- bind_rows(dfs)

    # Create the group labels corresponding to each group of rows
    group_labels <- c("Year", "Open Access Status", "Language", "First Author Country",
                      "First Author Global South", "Last Author Country", "Last Author Global South")

    table_html <- knitr::kable(freq_table) %>%
      kableExtra::group_rows(group_label = group_labels[1], start_row = start_row[1], end_row = end_row[1]) %>%
      kableExtra::group_rows(group_label = group_labels[2], start_row = start_row[2], end_row = end_row[2]) %>%
      kableExtra::group_rows(group_label = group_labels[3], start_row = start_row[3], end_row = end_row[3]) %>%
      kableExtra::group_rows(group_label = group_labels[4], start_row = start_row[4], end_row = end_row[4]) %>%
      kableExtra::group_rows(group_label = group_labels[5], start_row = start_row[5], end_row = end_row[5]) %>%
      kableExtra::group_rows(group_label = group_labels[6], start_row = start_row[6], end_row = end_row[6]) %>%
      kableExtra::group_rows(group_label = group_labels[7], start_row = end_row[6] + 1, end_row = end_row[7]) %>%
      kableExtra::kable_styling("striped", full_width = FALSE)

    HTML(as.character(table_html))
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