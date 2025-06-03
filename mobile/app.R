# mobile/app.R

library(shiny)

# Run the application
shinyApp(
  ui = source("mobile/ui.R")$value,
  server = source("mobile/server.R")$value
)
