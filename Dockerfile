# Start from the official Rocker Shiny image (has R, Shiny Server, and lots preinstalled)
FROM rocker/shiny:latest

# Install system dependencies (add more as needed)
RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
&& rm -rf /var/lib/apt/lists/*
  
  # Copy your R app and scripts
  WORKDIR /srv/shiny-server
COPY . .

# Install R packages you need (repeat as needed)
RUN R -e "install.packages(c('dplyr','lubridate','conflicted','shiny','plotly','ggplot2','patchwork','scales','config','shinydisconnect','shinyjs','shinythemes','shinyMobile','DT','shinyWidgets','bslib','qs','bsicons','showtext'), repos='https://cran.rstudio.com')"

# Expose port 3838 for Shiny
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
