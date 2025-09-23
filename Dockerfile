# Shiny app container for DREAMR
FROM rocker/shiny:latest

# System libraries for common R packages used by the app
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# Pre-install R packages
RUN R -e "install.packages(c(\
  'bslib','dplyr','DT','ggplot2','htmlwidgets','knitr','networkD3',\
  'plotly','progressr','RCurl','readr','rsconnect','shiny','shinyalert',\
  'shinycssloaders','shinyhelper','shinythemes','shinyWidgets','stringr','XML',\
  'RefManageR','glue','bibliometrix','tidyr','openalexR','countrycode','purrr',\
  'httr','zip'\
), repos='https://cloud.r-project.org')"

# Try to install ASySD and openalexR from CRAN; fall back to GitHub if missing
RUN R -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cloud.r-project.org'); \
          if (!requireNamespace('ASySD', quietly=TRUE)) { \
            try(install.packages('ASySD', repos='https://cloud.r-project.org'), silent=TRUE); \
            if (!requireNamespace('ASySD', quietly=TRUE)) remotes::install_github('r-lib/ASySD'); \
          }"

# Copy app into Shiny Server apps dir (use compose volume for live dev)
COPY shiny_app /srv/shiny-server/dreamr

# Shiny Server listens on 3838
EXPOSE 3838

# Let the base image start shiny-server
CMD ["/usr/bin/shiny-server"]
