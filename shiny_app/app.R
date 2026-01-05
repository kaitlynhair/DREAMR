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
      
      width = 3
    ),

      
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
    
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
        tabPanel("Overview",
                 br(),
                 radioGroupButtons(
                   inputId = "map_type",
                   label = "Map View:",
                   choices = c("🌍 Global" = "countries", "🎓 Institutional" = "institutions"),
            
                   status = "secondary", 
                   selected = "countries", 
                   individual = TRUE     
                 ),
                 leafletOutput("institution_map", height = 500) %>% withSpinner(color="#754E9B", type = 7)
                 
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

  # Reactive values for storage
  rv <- reactiveValues(
    refdata = NULL,
    citation_summary = data.frame(),
    authors = NULL,
    institutions = NULL,
    oa_data = NULL,
    loading = FALSE
  )

  # File upload
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
      group_by(country) %>% 
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
    
    total_citations <- if (!is.null(rv$refdata)) nrow(rv$refdata) else NA_integer_
    summary_df <- generate_summary_table(rv$oa_data, total_citations = total_citations)
 
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
