#deploy.R
library(rsconnect)
cfg <- config::get()
rsconnect::setAccountInfo(
  name   = "apantazi",
  token  = cfg$rsconnect_token,
  secret = cfg$rsconnect_secret
)

server_files <- list.files("servers/", full.names = TRUE)

# List of specific files to include
app_files <- c("ui.R","init.R", "app.R", #"www/styles.css",
               "all_data.qs")

# Combine the list of specific files and server files
all_files <- c(app_files, server_files)

# Deploy the app
rsconnect::deployApp(appName = "AZLegislativeCompass", appFiles = all_files,  account = "apantazi")
