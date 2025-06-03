# mobile/ui.R
ui <- f7Page(
  options = list(
    theme = "auto",
    dark = FALSE,
    filled = TRUE,
    color = "#007aff",
    touch = list(
      tapHold = TRUE,
      tapHoldDelay = 750,
      tapHoldPreventClicks = TRUE
    ),
    toolbar = list(
      hideOnPageScroll = TRUE
    )
  ),
  
  # Define the main layout with tabs
  f7TabLayout(
    navbar = f7Navbar(
      title = "Arizona Legislature Dashboard",
      hairline = TRUE,
      shadow = TRUE
    ),
    
    # Define the tabs
    f7Tabs(
      #animated = TRUE,
      #swipeable = TRUE,
      
      # Tab 1: Voting Patterns
      f7Tab(
        tabName = "VotingPatterns",
        icon = f7Icon("bar_chart"),
        active = TRUE,
        f7BlockTitle("Voting Patterns"),
        f7Block(
          f7Toggle(inputId = "toggle_text", label = "Show Hover Text"),
          plotlyOutput("heatmapPlot",width = "100%", height = "auto")
        )
      ),
      
      # Tab 2: District Context
      f7Tab(
        tabName = "DistrictContext",
        icon = f7Icon("layers"),
        f7BlockTitle("District Context"),
        f7Block(
          textOutput("districtContextOutput")
        )
      ),
      
      # Tab 3: Partisanship Scatterplot
      f7Tab(
        tabName = "PartisanshipScatterplot",
        icon = f7Icon("chart_line"),
        f7BlockTitle("Partisanship Scatterplot"),
        f7Block(
          plotlyOutput("scatterplot")
        )
      ),
      
      # Tab 4: Legislator Lookup
      f7Tab(
        tabName = "LegislatorLookup",
        icon = f7Icon("search"),
        f7BlockTitle("Legislator Lookup"),
        f7Block(
          f7Text(inputId = "address", label = "Your address (include city and state):"),
          f7Button(inputId = "submit", label = "Find Representatives"),
          tableOutput("representatives")
        )
      )
    )
  )
)
