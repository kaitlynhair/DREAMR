# Use multi-stage build to share layer for separate shiny and rstudio images

############################
# Stage 1: build base image
############################

# for reproducibility, don't use latest rocker, always pin version number
FROM rocker/r-ver:4.5.2 AS dreamr-base

# System libraries needed by R packages are handled by pak
# However, subsequent stages must install libs explicitly...
# Alternative: use one base image as starting point for all stages?
# RUN apt-get update && apt-get install -y --no-install-recommends \
# libudunits2-0 \
# libproj25 \
# libgdal34t64 \
# libgeos-c1t64 \
# && rm -rf /var/lib/apt/lists/*

# Pre-install R packages
RUN R -e "install.packages('pak')" && \
  R -e "pak::pak(c('rnaturalearth', 'leaflet', 'viridis',   'bslib','dplyr','DT','ggplot2','htmlwidgets','knitr','networkD3',\
  'plotly','progressr','RCurl','readr','rsconnect','shiny','shinyalert',\
  'shinycssloaders','shinyhelper','shinythemes','shinyWidgets','stringr','XML',\
  'RefManageR','glue','bibliometrix','tidyr','countrycode','purrr',\
  'httr','zip', 'gender', 'DiagrammeR', 'ISOcodes'))"

# Install remotes and ASySD from GitHub (ASySD is not on CRAN)
RUN R -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cloud.r-project.org'); \
  if (!requireNamespace('ASySD', quietly=TRUE)) remotes::install_github('camaradesuk/ASySD'); \
  if (!requireNamespace('genderdata', quietly=TRUE)) remotes::install_github('lmullen/genderdata'); \
  remotes::install_github('ropensci/openalexR', ref = '5774afe')"

RUN export LD_LIBRARY_PATH="/usr/local/lib/R/lib:${LD_LIBRARY_PATH}"

# Use SHINY_ENV as build-time argument
ARG SHINY_ENV=prod
ENV SHINY_ENV=${SHINY_ENV}
RUN if [ "$SHINY_ENV" = "dev" ]; then \
  R -e "install.packages(c('digest'))"; \
  fi

############################
# Stage 2: Shiny runtime
############################
FROM rocker/shiny:4.5.2 AS shiny

RUN apt-get update && apt-get install -y --no-install-recommends \
  libudunits2-0 \
  libproj25 \
  libgdal34t64 \
  libgeos-c1t64 \
  && rm -rf /var/lib/apt/lists/*

# copy R libraries from build stage (sketchy: what if later other missing system dependencies? must always check and install all manually)
COPY --from=dreamr-base \
  /usr/local/lib/R/site-library \
  /usr/local/lib/R/site-library

# Copy app into Shiny Server apps dir (use compose volume for live dev)
COPY shiny_app /srv/shiny-server/dreamr

RUN export LD_LIBRARY_PATH="/usr/local/lib/R/lib:${LD_LIBRARY_PATH}"

############################
# Stage 3: RStudio runtime
############################
FROM rocker/rstudio:4.5.2 AS rstudio

RUN apt-get update && apt-get install -y --no-install-recommends \
  libudunits2-0 \
  libproj25 \
  libgdal34t64 \
  libgeos-c1t64 \
  && rm -rf /var/lib/apt/lists/*

# copy R libraries from build stage
COPY --from=dreamr-base \
  /usr/local/lib/R/site-library \
  /usr/local/lib/R/site-library

# project files
COPY . /home/rstudio/project

RUN export LD_LIBRARY_PATH="/usr/local/lib/R/lib:${LD_LIBRARY_PATH}"