# init.R
options(repos = c(CRAN = "https://cran.rstudio.com"))

install_if_needed <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# List of CRAN packages to install

cran_packages <- c(
  "dplyr",
  "lubridate",
  "conflicted",
  "shiny",
  "plotly",
  "ggplot2",
  "patchwork",
  "scales",
  "config",
  "shinydisconnect",
  "shinyjs",
  "shinythemes",
  "shinyMobile",
  "DT",
  "shinyWidgets",
  "bslib", 
#  "shinyBS",
  "qs",
  "bsicons",
  "showtext"
)

# Install CRAN packages if needed
install_if_needed(cran_packages)

# Install legiscanrr, which needs devtools
#if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
#if (!requireNamespace("legiscanrr", quietly = TRUE)) devtools::install_github("fanghuiz/legiscanrr")

# Install dwnominate, which needs remotes
#if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
#if (!requireNamespace("basicspace", quietly = TRUE)) {  install.packages("https://cran.r-project.org/src/contrib/Archive/basicspace/basicspace_0.24.tar.gz", repos = NULL, type = "source") }
#if (!requireNamespace("dwnominate", quietly = TRUE)) remotes::install_github('wmay/dwnominate')

# Set conflicts preference to prioritize all dplyr functions
conflicted::conflict_prefer_all("dplyr", quiet=TRUE)
library(showtext)
font_add_google("Archivo", "Archivo")
showtext_auto()
