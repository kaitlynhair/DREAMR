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

# Install devtools and ASySD from GitHub (ASySD is not on CRAN)
RUN R -e "if (!requireNamespace('devtools', quietly=TRUE)) install.packages('devtools', repos='https://cloud.r-project.org'); \
          if (!requireNamespace('ASySD', quietly=TRUE)) devtools::install_github('camaradesuk/ASySD')"

# Use SHINY_ENV as build-time argument
ARG SHINY_ENV=prod
ENV SHINY_ENV=${SHINY_ENV}
RUN if [ "$SHINY_ENV" = "dev" ]; then \
    R -e "install.packages(c('digest'))"; \
  fi

# TO DO: move these packages to above install layer
# Right now here to avoid rebuilding entire packages layer
RUN R -e "install.packages(c('gender', 'DiagrammeR'))"
RUN R -e "if (!requireNamespace('genderdata', quietly=TRUE)) devtools::install_github('lmullen/genderdata')"

# Copy app into Shiny Server apps dir (use compose volume for live dev)
COPY shiny_app /srv/shiny-server/dreamr

# Shiny Server listens on 3838
EXPOSE 3838

# Let the base image start shiny-server
CMD ["/usr/bin/shiny-server"]
