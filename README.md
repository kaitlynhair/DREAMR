# Diversity Reporting in Evidence and Meta-Research (DREAMR)

## Background

Meta-research rarely considers equity, diversity, and inclusion (EDI), meaning interventions designed to improve science could unintentionally disadvantage certain groups. Our project aims to address this by developing an automated workflow to evaluate the EDI characteristics of studies included in meta-research, similar to a 'Table 1' in clinical trials

## Aim

Develop a workflow (and shiny app) to analyse the sample characteristics of articles included in meta-research.

## Preprint

[https://osf.io/preprints/metaarxiv/2yhux_v2](https://osf.io/preprints/metaarxiv/2yhux_v2)

## Development

This project was developed as part of and Evidence Synthesis Hackathon Project (2024) and a RRIA Unconference project (2025) :rocket:

## Contributing

Contributions are welcome! 🎉  

1. Clone the repository
2. Create a new branch for your changes  
   (`git checkout -b my-branch-name`)
3. Commit and push your branch
4. Open a pull request

If you’re unsure whether something is in scope, feel free to open an issue to discuss it first.

## Run Locally with Docker

- Build and start the Shiny container:

```powershell
git clone https://github.com/kaitlynhair/DREAMR.git
cd DREAMR
docker compose up --build
```

- Open the app at: [http://localhost:3838/dreamr](http://localhost:3838/dreamr)

- Live reload: edits to `shiny_app/` are mounted into the container and picked up automatically (refresh the browser).

- Stop the container:

```powershell
docker compose down
```

### RStudio in Docker (optional)

- Start RStudio (launched together with compose): [http://localhost:8787](http://localhost:8787)
- Login: username `rstudio`, password `rstudio` (or set `RSTUDIO_PASSWORD` env var)
- Project files are available at `/home/rstudio/project` (the repo root). You can run or debug the app with:

```r
setwd("/home/rstudio/project/shiny_app")
shiny::runApp()
```

## Dependencies

This project requires R (≥ 4.1.0) and the following R packages:

- [ASySD](https://github.com/ESHackathon/ASySD)
- bslib
- dplyr
- DT
- ggplot2
- htmlwidgets
- ISOcodes
- knitr
- networkD3
- plotly
- progressr
- RCurl
- readr
- rsconnect
- shiny
- shinyalert
- shinycssloaders
- shinyhelper
- shinythemes
- shinyWidgets
- stringr
- XML

## Installation

You can install the required dependencies by running:

```r
# Install CRAN packages
install.packages(c(
  "bslib",
  "dplyr",
  "DT",
  "ggplot2",
  "htmlwidgets",
  "ISOcodes",
  "knitr",
  "networkD3",
  "plotly",
  "progressr",
  "RCurl",
  "readr",
  "rsconnect",
  "shiny",
  "shinyalert",
  "shinycssloaders",
  "shinyhelper",
  "shinythemes",
  "shinyWidgets",
  "stringr",
  "XML"
))

# Install ASySD from GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ESHackathon/ASySD")
```
