# Arizona Legislative Compass

Arizona Legislative Compass is a Shiny application that reveals how each member of the Arizona Legislature votes and how those decisions compare with the political lean of their districts. The project aims to make legislative data easily accessible for journalists, advocates, and voters by combining voting records with demographic context in one interactive dashboard.

## Why It Exists

Legislative data is often scattered across multiple sources and difficult to analyze without extensive preprocessing. This app consolidates cleaned datasets so users can:

- Review which bills legislators supported or opposed.
- See how frequently lawmakers vote with their parties.
- Compare a district's demographics and partisanship with its representative's record.

By doing so, the tool helps illustrate whether elected officials reflect the preferences of the communities they serve.

## Key Files

- **app.R** – entry point that loads data, sources other scripts, and launches the Shiny application.
- **init.R** – checks for required packages and sets up fonts for consistent styling.
- **ui.R** – builds the user interface, including a helper message shown when a session disconnects due to inactivity.
- **servers/** – server-side scripts for the voting heatmap, district context comparison, scatterplot, and the voting history module.
- **www/** – static resources like custom CSS for responsive layouts.

## How the App Works

1. **Initialization** – `app.R` sources `init.R` to install packages and configure fonts.
2. **Data Loading** – Preprocessed datasets are read from `all_data.qs` and converted to data tables shared by the modules.
3. **User Interface** – `ui.R` defines tabbed pages for exploring vote patterns, legislator information, and district demographics. It also provides a disconnect notice for idle sessions.
4. **Server Logic** – Each script in `servers/` handles a specific feature. They are sourced inside the main server function so all modules can share reactive data.
5. **Styling** – CSS rules in `www/styles.css` adjust the banner, navigation, and layout for both desktop and mobile displays.
6. **Launching** – After building the UI and server, `shinyApp(ui, server)` starts the interactive application.

## Running Locally

Ensure you have R installed, then from the repository directory execute:

```r
shiny::runApp()
```

Alternatively, run `run.R` to mirror the hosted environment.

## Deployment

Use the `deploy.R` script with the **rsconnect** package to publish the app to ShinyApps.io. A `Dockerfile` is provided for creating a container image with all dependencies.

