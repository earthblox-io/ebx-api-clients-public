# Earth Blox API Client Library for R

Version 1.0.0 - Alpha stage, Earth Blox Client Library for R.

[Earth Blox](https://www.earthblox.io) - measure climate and nature risk for all your economic assets and facilities worldwide.
Earth Blox gives sustainability teams satellite imagery analytics to report on climate and biodiversity impacts, risks, and opportunities for millions of assets in minutes.

## Installation

```r
# Install from local directory
install.packages("path/to/ebx-api-clients/r", repos = NULL, type = "source")

# Or using devtools
devtools::install_local("path/to/ebx-api-clients/r")
```

## Client Registration

To register a new client via the API, a user with the `api_org_admin` role is needed to create new clients.

To register a new client to receive a `client_id` and `client_secret` for API access:

```r
library(ebx)

email <- 'user@earthblox.io'
password <- 'your_password'
name <- 'my R client'

oauth_client <- create_oauth_client(email, password, name)
oauth_client$save()  # persist the client details including the secret to disk for later use
```

By default, the details are saved in a file at `.ebx/.ebx.auth.json` in the current working directory. Locations can be changed (see below).

## Authentication

There are several ways to authenticate with the API to make subsequent calls easier.

### Using Saved Credentials

If you have created a client and saved the credentials to a file, you can use them by calling `auth_using_creds()`:

```r
library(ebx)

auth_using_creds()
list_projects()
```

### Using Client ID and Client Secret Directly

If you have a client ID and secret elsewhere, you can use `auth_using_oauth()` and supply those details directly:

```r
library(ebx)

auth_using_oauth(client_id, client_secret)
list_projects()
```

### Using a Token Directly

If you have an access token, you can use it with `auth_using_env()`. You must supply the token via an environment variable called `EBX_API_TOKEN`:

```r
library(ebx)

Sys.setenv(EBX_API_TOKEN = "your_token_here")
auth_using_env()
list_projects()
```

## Using the API

Once you have an authentication mechanism, you can interact with the API:

### Global Scope

This will use the first authentication/client for making calls:

```r
library(ebx)

auth_using_creds()
projects <- list_projects()
```

### Multiple Clients

You can manage multiple clients with named contexts:

```r
library(ebx)

# Create a named client
client1 <- auth_using_creds(name = "client1")
projects <- list_projects()

# Create another client
auth_using_creds(name = "client2")
client2 <- get_client("client2")
projects2 <- list_projects()
```

## API Endpoints

### Projects

```r
# List all projects
projects <- list_projects()

# Get a specific project
project <- get_project(project_id = "project_123")
```

### Runs

```r
# List runs (default limit: 10)
runs <- list_runs(limit = 20)

# Get a specific run
run <- get_run(run_id = "run_123")

# Get run status
status <- get_run_status(run_id = "run_123")

# Create a new run (Method 1: Using project_spec with variables - recommended)
spec <- Project$new(
  id = "project_123",
  variables = list(
    list(key = "var_1", type = "area", value = geojson_data)
  )
)
run_id <- create_run(project_spec = spec)

# Create a new run (Method 2: Using individual parameters with substitutions - legacy)
run_id <- create_run(
  project_id = "project_123",
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  study_area = geojson_data
)

# Optional parameters for create_run:
# include_geometry: Whether to include geometry in output (default: FALSE)
# generate_thumbnails: Whether to generate thumbnails for every layer (default: FALSE)
run_id <- create_run(project_spec = spec, include_geometry = TRUE, generate_thumbnails = TRUE)

# Follow a run until completion (polling)
final_run <- follow_run(run_id = "run_123", interval = 5, max_attempts = 60)

# Get charts from a run (with optional filter)
charts <- get_charts(run_id = "run_123")
charts_filtered <- get_charts(run_id = "run_123", filter = "temperature")

# Get tables from a run (with optional filter)
tables <- get_tables(run_id = "run_123")
tables_filtered <- get_tables(run_id = "run_123", filter = "summary")

# Get layers from a run (with optional filter)
layers <- get_layers(run_id = "run_123")
layers_filtered <- get_layers(run_id = "run_123", filter = "vegetation")
```

## Environment Variables

* `EBX_API_TOKEN` - Use with `auth_using_env()` to set the access token directly for authentication
* `EBX_CLIENT_ID` - Use with `auth_using_oauth()` by not supplying a `client_id` parameter
* `EBX_CLIENT_SECRET` - Use with `auth_using_oauth()` by not supplying a `client_secret` parameter
* `EBX_API_EMULATOR_HOST` - Sets the base URL for the API. By default this is `https://api.earthblox.io`
* `EBX_API_PREFIX_PATH` - Set the API path prefix. This is set to `/v1beta/` by default
* `EBX_API_AUTH_EMULATOR` - Override the OAuth URL for testing
* `EBX_API_CLIENT_REGISTRATION` - Override the client registration URL for testing

## Credentials File

By default, the credentials file is stored in the `.ebx` folder in the current working directory as `.ebx.auth.json`. You can override the saving and loading of this file with extra parameters:

```r
library(ebx)

config <- ClientConfig$new()
config$get_persistence_driver()$path <- "~/ebx"

# Saving to home directory with same file name (.ebx.auth.json)
oauth_client$save(config = config)

# Loading from home directory with same file name (.ebx.auth.json)
client <- auth_using_creds(config = config)
```

## Using with Shiny

The EBX R client is compatible with Shiny applications. Here's a simple example:

```r
library(shiny)
library(ebx)

# Authenticate before starting the app
auth_using_creds()

ui <- fluidPage(
  titlePanel("Earth Blox Projects"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Projects")
    ),
    
    mainPanel(
      tableOutput("projects_table")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store projects
  projects_data <- reactiveVal(list())
  
  # Load projects on startup
  observe({
    projects <- list_projects()
    projects_data(projects)
  })
  
  # Refresh when button clicked
  observeEvent(input$refresh, {
    projects <- list_projects()
    projects_data(projects)
  })
  
  # Render projects table
  output$projects_table <- renderTable({
    projects <- projects_data()
    
    if (length(projects) == 0) {
      return(data.frame(Message = "No projects found"))
    }
    
    # Convert to data frame
    data.frame(
      ID = sapply(projects, function(p) p$id),
      Name = sapply(projects, function(p) p$name),
      Description = sapply(projects, function(p) p$description %||% "")
    )
  })
}

shinyApp(ui = ui, server = server)
```

### Advanced Shiny Example with Runs

```r
library(shiny)
library(ebx)

auth_using_creds()

ui <- fluidPage(
  titlePanel("Earth Blox Runs"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("limit", "Number of runs:", value = 10, min = 1, max = 100),
      actionButton("load_runs", "Load Runs"),
      hr(),
      uiOutput("run_selector")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Runs", tableOutput("runs_table")),
        tabPanel("Charts", verbatimTextOutput("charts_output")),
        tabPanel("Tables", verbatimTextOutput("tables_output"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  runs_data <- reactiveVal(list())
  selected_run <- reactiveVal(NULL)
  
  observeEvent(input$load_runs, {
    runs <- list_runs(limit = input$limit)
    runs_data(runs)
  })
  
  output$run_selector <- renderUI({
    runs <- runs_data()
    if (length(runs) == 0) return(NULL)
    
    choices <- setNames(
      sapply(runs, function(r) r$id),
      sapply(runs, function(r) paste0(r$name %||% r$id, " (", r$status, ")"))
    )
    
    selectInput("selected_run_id", "Select Run:", choices = choices)
  })
  
  output$runs_table <- renderTable({
    runs <- runs_data()
    
    if (length(runs) == 0) {
      return(data.frame(Message = "Click 'Load Runs' to fetch data"))
    }
    
    data.frame(
      ID = sapply(runs, function(r) r$id),
      Name = sapply(runs, function(r) r$name %||% ""),
      Status = sapply(runs, function(r) r$status),
      Created = sapply(runs, function(r) r$created_at %||% "")
    )
  })
  
  output$charts_output <- renderPrint({
    req(input$selected_run_id)
    charts <- get_charts(run_id = input$selected_run_id)
    charts
  })
  
  output$tables_output <- renderPrint({
    req(input$selected_run_id)
    tables <- get_tables(run_id = input$selected_run_id)
    tables
  })
}

shinyApp(ui = ui, server = server)
```

## License

See LICENSE.txt in the repository root.
