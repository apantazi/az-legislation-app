# mobile/server.R

library(shiny)
library(plotly)
library(dplyr)

server <- function(input, output, session) {
  
  # Heatmap plot for Voting Patterns
  output$heatmapPlot <- renderPlotly({
    data <- app01_vote_patterns
    p <- ggplot(data, aes(x = roll_call_id, y = legislator_name, fill = party)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_manual(values = c("D" = "#1f78b4", "R" = "#e31a1c")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    if (input$toggle_text) {
      p <- p + geom_text(aes(label = vote_text), size = 3)
    }
    ggplotly(p)
  })
  
  # District context text output
  output$districtContextOutput <- renderText({
    # You can update this to reflect your actual district context data and logic
    "District context information will be displayed here."
  })
  
  # Scatterplot for Partisanship
  output$scatterplot <- renderPlotly({
    data <- app03_district_context
    p <- ggplot(data, aes(x = district_number, y = leg_party_loyalty, color = party)) +
      geom_point() +
      theme_minimal()
    ggplotly(p)
  })
  
  # Legislator lookup
  # output$representatives <- renderTable({
  #   input$submit
  #   isolate({
  #     # Example lookup logic
  #     address <- input$address
  #     # You can replace this with actual lookup logic using app data
  #     data.frame(
  #       Name = c("John Doe", "Jane Smith"),
  #       Position = c("Senator", "Representative"),
  #       Party = c("D", "R")
      # )
    # })
  # })
}
