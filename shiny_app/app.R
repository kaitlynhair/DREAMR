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
require(leaflet)
require(gender)
require(genderdata)


# Source functions
source("process_data.R")
source("summary_table.R")
source("load_studies.R")
source("get_first_active_year.R")
source("exports.R")
source("flowchart.R")
source("utils.R")

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

  tags$head(
    tags$style(HTML("
    table.dataTable {
      border-collapse: collapse !important;
      width: 100%;
      font-family: 'Calibri', sans-serif;
      font-size: 13px;
    }
    table.dataTable th, table.dataTable td {
      border: 1px solid #333 !important;  /* darker borders */
      padding: 6px 8px;
    }
    table.dataTable th {
      border-bottom: 2px solid #333 !important;
      font-weight: bold;
      text-align: left;
    }
    table.dataTable.stripe tbody tr:nth-child(odd) {
      background-color: #f9f9f9;
    }
    table.dataTable tbody tr:hover {
      background-color: #efefef;  /* subtle hover effect */
    }
  "))
  ),
  
  
  # Application title
  titlePanel(if (is_dev_build) "DREAMR (dev)" else "DREAMR"),

  # Sidebar with file upload options
  sidebarLayout(
    
    sidebarPanel(
      
      h4("File upload ", icon("upload")),
      
      # Single file input, automatically detect type
      fileInput(
        "uploadfile",
        "Choose file to upload (XML, CSV, XLSX):",
        multiple = FALSE,
        placeholder = "No file selected",
        accept = c(".xml", ".csv", ".xlsx", ".xls")),
        # options(shiny.maxRequestSize = 10 * 1024^2)),
      
      # Show detected file type
      textOutput("detected_file_type"),
      
      tags$hr(),

      # Advanced options
      input_switch("show_advanced", "Advanced options", value = FALSE), 
      conditionalPanel(
        condition = "input.show_advanced",

        h5("ORCID retrieval"),

        checkboxInput(inputId = "always_retrieve_first_author", label = "Always retrieve first author", value = TRUE),
        checkboxInput(inputId = "always_retrieve_last_author", label = "Always retrieve last author", value = TRUE),
        numericInput(
          "max_authors",
          "Maximum number of retrieved authors per article",
          value = 1000,
          min = 1,
          max = 1e9,
          step = 1
        ),

        tags$hr(),

        h5("Update results"),

        fileInput(
          "upload_previous_results",
          "For incremental updates of existing results, you can upload previoulsy retrieved results here (select the .zip folder). 
          This avoids repeated metadata retrieval, significantly speeding up the process. Note that you will still need to upload a new file above, which will be combined with the previously retrieved data.",
          multiple = FALSE,
          placeholder = "No file selected",
          accept = c(".zip")
        ),
      ),
      width = 3
    ),

      
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 
                 uiOutput("about")
        ),
          tabPanel("Data completeness",
                 br(),
                 uiOutput("summary_status"),
                 fluidRow(
                   column(4,
                          h5("References"),
                          grVizOutput("flow_references", height = "300px")
                   ),
                   column(4,
                          h5("Institutions"),
                          grVizOutput("flow_institutions", height = "300px")
                   ),
                   column(4,
                          h5("Authors"),
                          grVizOutput("flow_authors", height = "300px")
                   )
                 )
                 
        ),
        # New dedicated Summary Table page
        tabPanel("Summary Table",
                 br(),
                 h4("Summary Table"),
                 DTOutput("summary_table") %>% withSpinner(color="#00695C", type=7)
        ),
        tabPanel(
          "Data documentation",
          bslib::page_fluid(
            bslib::navset_card_tab(
              full_screen = TRUE,
              
              bslib::nav_panel(
                "General info",
                
                bslib::card(
                  bslib::card_header("About these summary data"),
                  bslib::card_body(
                    p(
                      "These summaries are derived primarily from OpenAlex metadata. ",
                      "OpenAlex exposes work-level fields such as open access status, language, publication year, type, and institution-linked country and institution fields, ",
                      "which makes it suitable for high-level descriptive summaries."
                    ),
                    p(
                      strong("Important: "),
                      "these values are best treated as descriptive metadata rather than audited ground truth. ",
                      "Missingness, upstream source inconsistencies, and imperfect record linkage can all affect the summaries."
                    )
                  )
                ),
                
                
                bslib::layout_column_wrap(
                  width = 1/2,
                  
                  bslib::card(
                    bslib::card_header("General limitations"),
                    bslib::card_body(
                      tags$ul(
                        tags$li(
                          strong("Completeness: "),
                          "some OpenAlex fields are missing for some records. Missing metadata often reflects incomplete indexing or source metadata rather than a true absence."
                        ),
                        tags$li(
                          strong("Classification: "),
                          "fields such as field of research, article type, and institution type rely on OpenAlex classification systems and may not always match manual expert judgement."
                        ),
                        tags$li(
                          strong("Affiliations and linkage: "),
                          "author, institution, and country summaries depend on correct parsing of authorships and affiliations. These links are useful but not perfect."
                        ),
                        tags$li(
                          strong("Aggregation: "),
                          "most outputs are descriptive summaries across the retrieved set and should be interpreted as broad indicators."
                        )
                      )
                    )
                  ),
                  
                  bslib::card(
                    bslib::card_header("Variables that need extra caution"),
                    bslib::card_body(
                      tags$ul(
                        tags$li(
                          strong("Gender variables: "),
                          "these do not come from OpenAlex. They are inferred separately using the ",
                          tags$code("gender"),
                          " package, so they are approximate and should not be treated as definitive measures of gender identity."
                        ),
                        tags$li(
                          strong("Years since first publication: "),
                          "this is a derived proxy based on indexed publication history, not a directly supplied OpenAlex variable. It may be affected by incomplete coverage of older works and author disambiguation errors."
                        ),
                        tags$li(
                          strong("Funding source specified: "),
                          "absence of recorded funder information should not be interpreted as evidence that no funding existed."
                        )
                      )
                    )
                  )
                ),
                
                
                bslib::card(
                  bslib::card_header("How to use these summaries"),
                  bslib::card_body(
                    p(
                      "These outputs are intended for descriptive overview and exploratory interpretation. ",
                      "They are strongest for broad patterns, and weaker where they depend on inferred values, incomplete metadata, or affiliation matching."
                    )
                  )
                )
              ),
              
              bslib::nav_panel(
                "Data dictionary",
                
                bslib::card(
                  bslib::card_header("Data dictionary and interpretation notes"),
                  bslib::card_body(
                    tags$dl(
                      tags$dt(strong("Open access")),
                      tags$dd(
                        "Based on OpenAlex open access metadata. Useful for broad description, but status may depend on available source and location metadata."
                      ),
                      
                      tags$dt(strong("Publication language")),
                      tags$dd(
                        "Taken from the OpenAlex language field and converted from code to language name for display. Language metadata may be incomplete or patchy for some records."
                      ),
                      
                      tags$dt(strong("Publication year")),
                      tags$dd(
                        "Taken from publication metadata. Usually reliable, but online-first and print dates can occasionally create inconsistencies."
                      ),
                      
                      tags$dt(strong("Number of journals")),
                      tags$dd(
                        "Calculated from unique source or journal names in the retrieved set. Counts may be affected by title variants or inconsistent source naming."
                      ),
                      
                      tags$dt(strong("Field of research")),
                      tags$dd(
                        "Reflects OpenAlex subject classification. Useful for broad grouping, but not a substitute for manual content classification."
                      ),
                      
                      tags$dt(strong("Article type")),
                      tags$dd(
                        "Based on the OpenAlex work type field. Work types are informative, but source systems do not always label outputs consistently."
                      ),
                      
                      tags$dt(strong("Funding source specified")),
                      tags$dd(
                        "Indicates whether funder information is recorded in metadata. Missing values often reflect incomplete metadata capture rather than true absence of funding."
                      ),
                      
                      tags$dt(strong("Number of authors per paper")),
                      tags$dd(
                        "Calculated from linked author records per paper. Large collaborations, group authorship, and incomplete author lists may affect counts."
                      ),
                      
                      tags$dt(strong("Proportion female authors (first author)")),
                      tags$dd(
                        "Not from OpenAlex. Derived using the ",
                        tags$code("gender"),
                        " package, so it is an inferred measure based on names and should be interpreted cautiously."
                      ),
                      
                      tags$dt(strong("Proportion female authors (last author)")),
                      tags$dd(
                        "As above, this is based on inferred rather than recorded gender and may be inaccurate for many names and contexts."
                      ),
                      
                      tags$dt(strong("Years since first publication")),
                      tags$dd(
                        "A derived proxy for publication history, taken from the ORCID record of authors. It depends on author disambiguation and the completeness of indexed historical outputs."
                      ),
                      
                      tags$dt(strong("Number of institutions per paper")),
                      tags$dd(
                        "Calculated from distinct affiliations linked to each paper. Missing affiliations or imperfect institution matching may affect this count."
                      ),
                      
                      tags$dt(strong("Number of countries per paper")),
                      tags$dd(
                        "Derived from institution-linked country codes. This reflects affiliation geography, not necessarily study setting or author nationality."
                      ),
                      
                      tags$dt(strong("Number of countries")),
                      tags$dd(
                        "Summarises the distinct countries represented across affiliations in the retrieved set."
                      ),
                      
                      tags$dt(strong("Type of institutions (first author)")),
                      tags$dd(
                        "Based on OpenAlex institution type metadata linked to the first-author affiliation. Depends on accurate affiliation linkage and institution classification. Note that one author may have many institutions, and the total number is likely to be larger than the number of references uploaded"
                      ),
                      
                      tags$dt(strong("Type of institutions (last author)")),
                      tags$dd(
                        "Based on OpenAlex institution type metadata linked to the last-author affiliation. Depends on accurate affiliation linkage and institution classification. Note that one author may have many institutions, and the total number is likely to be larger than the number of references uploaded"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel("Author affiliations",
                 br(),
                 radioGroupButtons(
                   inputId = "map_type",
                   label = "Map View:",
                   choices = c("🌍 Global" = "countries", "🎓 Institutional" = "institutions"),
            
                   status = "secondary", 
                   selected = "countries", 
                   individual = TRUE     
                 ),
                 leafletOutput("institution_map", height = 500) %>% withSpinner(color="#754E9B", type = 7),
                 
                 # fluidRow(
                 #   column(6, plotlyOutput("oa_plot")),   # 6-column width (half of the row)
                 #   column(6, plotlyOutput("funder_plot")) # 6-column width (other half)
                 # )
                 br()
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

  output$about <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "about.html",
      width = "100%",
      height = 800
    )
  })

  # Reactive values for storage
  rv <- reactiveValues(
    refdata = NULL,
    citation_summary = data.frame(),
    authors = NULL,
    institutions = NULL,
    oa_data = NULL,
    loading = FALSE
  )

  # User options
  get_options <- reactive({list(
      always_retrieve_first_author = input$always_retrieve_first_author,
      always_retrieve_last_author = input$always_retrieve_last_author,
      max_authors = input$max_authors
    )
  })

  # Load previous results
  observeEvent(input$upload_previous_results, {
    
    req(input$upload_previous_results)
    
    file_path <- input$upload_previous_results$datapath
    if (tools::file_ext(file_path) != "zip") {
      showNotification("Please upload a ZIP file.", type = "error")
      return(NULL)
    }

    # Unzip into a clean temporary directory
    temp_dir <- tempfile(pattern = "upload_")
    dir.create(temp_dir)
    unzip(file_path, exdir = temp_dir)
    
    expected_files <- c("authors.csv", "institutions.csv", "pub_metadata.csv")
    actual_files <- list.files(
      temp_dir,
      recursive = TRUE,
      full.names = TRUE
    )
    found_files <- basename(actual_files)
    missing_files <- setdiff(expected_files, found_files)
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("Missing expected CSV files:", paste(missing_files, collapse = ", ")),
        type = "error"
      )
      return()
    }
    
    # Extract full paths safely
    authors_path <- actual_files[basename(actual_files) == "authors.csv"]
    institutions_path <- actual_files[basename(actual_files) == "institutions.csv"]
    pub_metadata_path <- actual_files[basename(actual_files) == "pub_metadata.csv"]
    
    authors_df <- read.csv(authors_path, stringsAsFactors = FALSE)
    institutions_df <- read.csv(institutions_path, stringsAsFactors = FALSE)
    pub_metadata_df <- read.csv(pub_metadata_path, stringsAsFactors = FALSE)
    
    previous_results <- list(
      authors = authors_df,
      institutions = institutions_df,
      pub_metadata = pub_metadata_df
    )
    
    rv$previous_results <- previous_results
  })

  # File upload
  observeEvent(input$uploadfile, {
    req(input$uploadfile)
    
    # Detect file type based on extension
    ext <- tools::file_ext(input$uploadfile$name)
    method <- switch(tolower(ext),
                     "xml"  = "endnote",
                     "csv"  = "csv",
                     "xls"  = "xlsx",
                     "xlsx" = "xlsx",
                     stop("Unsupported file type"))
    
    output$detected_file_type <- renderText({
      paste("Detected file type:", toupper(ext))
    })
    
    # Load citations
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


  # Run DREAMR extraction when file uploaded
  observeEvent(rv$refdata, {

    shiny::validate(need(rv$refdata, "No reference data available"))
    
    # Check if previous results exist and are not empty
    previous_results_exist <- !is.null(rv$previous_results) &&
        !is.null(rv$previous_results$pub_metadata) &&
        !is.null(rv$previous_results$institutions) &&
        !is.null(rv$previous_results$authors) &&
        nrow(rv$previous_results$pub_metadata) > 0

    # Subset refdata if previous results exist
    refdata <- rv$refdata

    if (previous_results_exist) {
      existing_dois <- format_doi(rv$previous_results$pub_metadata)$doi
      refdata <- format_doi(rv$refdata) |>
        filter(!doi %in% existing_dois)
      
      if (nrow(refdata) < 1) {
        rv$oa_data$pub_metadata <- rv$previous_results$pub_metadata
        rv$oa_data$institutions <- rv$previous_results$institutions
        rv$oa_data$authors <- rv$previous_results$authors
        rv$institutions <- rv$oa_data$institutions
        rv$authors <- rv$oa_data$authors
        return()
      }
    }

    rv$loading <- TRUE
    # Progress feedback
    withProgress(message = "Retrieving & processing metadata", value = 0, {
      progress_fun <- function(detail, amount) {
        incProgress(amount, detail = detail)
      }
      if (is_dev_build) 
        oa_data <- dreamr_extract_cached(refdata, progress=progress_fun, options=get_options())
      else
        oa_data <- dreamr_extract(refdata, progress=progress_fun, options=get_options())
    })

    # Append new results to previous results
    if (previous_results_exist) {
      oa_data$pub_metadata <- rbind(rv$previous_results$pub_metadata, oa_data$pub_metadata)
      oa_data$institutions <- rbind(rv$previous_results$institutions, oa_data$institutions)
      oa_data$authors <- rbind(rv$previous_results$authors, oa_data$authors)
    }

    rv$authors <- oa_data$authors
    rv$institutions <- oa_data$institutions
    rv$oa_data <- oa_data

    rv$loading <- FALSE
  })

  # Location - render leaflet map -----
  output$institution_map <- renderLeaflet({
    
    validate(
      need(nrow(rv$institutions) > 0, "No overview available. Please input a file.")
    )
    
    if (input$map_type == "countries") {
    
      world <- rnaturalearth::ne_countries(returnclass = "sf") %>%
        select(iso_a2, name_long, geometry)


      map_data <- rv$institutions %>%
        group_by(country) %>%
        mutate(num_auth = n()) %>%
        ungroup()


      world_map <- world %>%
        left_join(map_data, by = c("iso_a2" = "country_code")) %>% 
        mutate(num_auth = ifelse(is.na(num_auth), 0, num_auth))

      pal <- colorNumeric(
        palette = c("lightgrey", viridis::viridis(256, option = "D")),
        domain = world_map$num_auth,
        na.color = "lightgrey"
      )

      
      # Define continent centers and names
      continent_labels <- data.frame(
        continent = c("North America", "South America", "Europe", "Africa", "Asia", "Oceania"),
        lat = c(45, -15, 55, 0, 35, -25),
        lng = c(-100, -60, 10, 20, 100, 135),
        stringsAsFactors = FALSE
      )
      
      leaflet(world_map) %>%
            addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(
          data = world_map,
          fillColor = "grey80",
          fillOpacity = 1,
          color = NA
        ) %>%
        addPolygons(
          fillColor = ~pal(num_auth),
          fillOpacity = 0.8,
          color = "white",
          weight = 0.5,
          popup = ~paste0(
            "<b>", name_long, "</b><br>",
            "No. Authors: ", num_auth, "<br>"
          ),
          highlightOptions = highlightOptions(
            weight = 1,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~num_auth[!is.na(num_auth)],
          position = "bottomleft",
          title = "Number of Authors"
        ) %>%
        addLabelOnlyMarkers(
          data = continent_labels,
          lng = ~lng,
          lat = ~lat,
          label = ~continent,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "#333333",
              "font-family" = "Courier New, monospace",
              "font-size" = "14px"
              # "font-weight" = "bold",
              # "text-shadow" = "1px 1px 2px rgba(255,255,255,0.8)"
            )
          )
        ) %>%
        setView(lat = 0, lng = 0, zoom = 1)
      
      } else if (input$map_type == "institutions") {

    
    map_data <- rv$institutions %>% 
      group_by(display_name) %>% 
      mutate(num_auth = n_distinct(author_id)) %>% 
      ungroup() %>% 
      arrange(desc(num_auth))
    
    color_palette <- colorFactor(
      palette = "Set3",
      domain = map_data$type
    )
    
    scale_size <- function(num) {
      scales::rescale(num, c(4, 25))  # Adjust size range as necessary
    }
    

    leaflet(map_data) %>%
      addProviderTiles(providers$Esri.WorldStreetMap
      ) %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        popup = ~paste0("<b>", display_name, "</b><br>",
                        "Institution Type: ", type, "<br>",
                        "No. of Authors: ", num_auth),
        radius = ~scale_size(num_auth),
        color = "black",
        fillColor = ~color_palette(type),
        fillOpacity = 1,
        label = ~display_name,
        weight = 1,
        layerId = ~display_name
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = color_palette,
        values = ~type,
        title = "Institution Type",
        opacity = 0.8
      ) %>%
      setView(lat = 0, lng = 0, zoom = 1) 
    }
  })

# Output --- summary table
  output$summary_table <- renderDT({
    shiny::validate(shiny::need(!is.null(rv$oa_data), "Summary not ready yet"))
    summary_df <- generate_summary_table(rv$oa_data)
 
    datatable(
      summary_df,
      rownames = FALSE,
      options = list(
        dom = 't',               # just the table, no search/paging
        ordering = FALSE,        # turn off sorting
        paging = FALSE,          # show all rows
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ),
      escape = FALSE,  # keep <br> line breaks
      class = 'display compact cell-border stripe'
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
  
  # --- Helpers for flowchart ---
  
  output$flow_references <- renderGrViz({ render_flow_references(rv) })
  output$flow_institutions <- renderGrViz({ render_flow_institutions(rv) })
  output$flow_authors <- renderGrViz({ render_flow_authors(rv) })
}


# Run the Application ==========================================================

shinyApp(ui = ui, server = server)
