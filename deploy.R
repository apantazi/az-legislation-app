#deploy.R
library(rsconnect)
rsconnect::setAccountInfo(name='apantazi', token='952FC967BB65520EDEA1B1AB01AB4061', secret='pR9ifsSx5ETRfd/EXnTqBfiW4qpkdb3wFQbOaAF+')

server_files <- list.files("servers/", full.names = TRUE)

# List of specific files to include
app_files <- c("ui.R","init.R", "app.R", #"www/styles.css",
               "all_data.qs")

# Combine the list of specific files and server files
all_files <- c(app_files, server_files)

# Deploy the app
rsconnect::deployApp(appName = "AZLegislativeCompass", appFiles = all_files,  account = "apantazi")
