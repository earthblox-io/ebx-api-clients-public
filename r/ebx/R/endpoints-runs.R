# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title List Runs
#' @description List runs with optional limit
#' @param limit The maximum number of runs to return (default: 10)
#' @return A list of Run objects
#' @export
list_runs <- function(limit = 10) {
  client <- get_client()
  
  query_params <- list(limit = limit)
  response <- client$get("/runs/", query_params = query_params)
  
  # API returns {data: [{run1}, {run2}, ...]}
  runs_data <- response$data
  
  if (is.null(runs_data)) {
    return(list())
  }
  
  lapply(runs_data, function(run) {
    do.call(Run$new, run)
  })
}

#' @title Get Run
#' @description Get a specific run by ID
#' @param run_id The run ID
#' @return A Run object
#' @export
get_run <- function(run_id) {
  client <- get_client()
  response <- client$get(paste0("/runs/", run_id))
  
  # API returns {data: {run fields}}
  run_data <- response$data
  
  if (is.null(run_data)) {
    stop("No run data returned from API")
  }
  
  do.call(Run$new, run_data)
}

#' @title Get Run Status
#' @description Get the status of a specific run
#' @param run_id The run ID
#' @return The run status as a string
#' @export
get_run_status <- function(run_id) {
  client <- get_client()
  response <- client$get(paste0("/runs/", run_id, "/status"))
  
  # API returns {data: {status: "...", error_code: ...}}
  if (!is.null(response$data)) {
    return(response$data$status)
  }
  
  # Fallback to direct field access if structure differs
  response$status
}

#' @title Get Charts
#' @description Get charts from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to chart titles
#' @return A list of chart data
#' @export
get_charts <- function(run_id, filter = NULL) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/charts")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [outputs...], meta: {...}}
  response$data
}

#' @title Get Tables
#' @description Get tables from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to table titles
#' @return A list of table data
#' @export
get_tables <- function(run_id, filter = NULL) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/tables")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [outputs...], meta: {...}}
  response$data
}

#' @title Get Layers
#' @description Get layers from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to layer titles
#' @return A list of layer data
#' @export
get_layers <- function(run_id, filter = NULL) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/layers")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [layers...], meta: {...}}
  response$data
}

#' @title Follow Run
#' @description Follow a run's progress (polling)
#' @param run_id The run ID
#' @param interval Polling interval in seconds (default: 5)
#' @param max_attempts Maximum number of polling attempts (default: 60)
#' @return The final Run object
#' @export
follow_run <- function(run_id, interval = 5, max_attempts = 60) {
  for (i in 1:max_attempts) {
    run <- get_run(run_id)
    
    if (run$status %in% c("completed", "failed", "cancelled")) {
      return(run)
    }
    
    Sys.sleep(interval)
  }
  
  stop(sprintf("Run %s did not complete within %d attempts", run_id, max_attempts))
}

#' @title Create Run
#' @description Create a new run using the specified project
#' 
#' This function supports two modes:
#' 1. Pass a complete project_spec (list or Project object) with variables
#' 2. Pass individual parameters (project_id + optional substitutions)
#' 
#' @param project_spec Optional complete project specification (list or Project object).
#'   If provided, this takes precedence over individual parameters.
#' @param project_id The project ID (ignored if project_spec provided)
#' @param start_date Optional start date (datetime or string) - uses deprecated substitutions API
#' @param end_date Optional end date (datetime or string) - uses deprecated substitutions API
#' @param study_area Optional study area - uses deprecated substitutions API
#' @param include_geometry Whether to include geometry in output (default: FALSE)
#' @param generate_thumbnails Whether to generate thumbnails for every layer (default: FALSE)
#' @return The run ID as a string
#' @export
#' 
#' @examples
#' \dontrun{
#' # Method 1: Using project_spec with variables (recommended)
#' spec <- Project$new(
#'   id = "project123",
#'   variables = list(
#'     list(key = "var_1", type = "area", value = geojson_data)
#'   )
#' )
#' run_id <- create_run(project_spec = spec)
#' 
#' # Method 2: Using individual parameters with substitutions (deprecated)
#' run_id <- create_run(
#'   project_id = "project123",
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31",
#'   study_area = geojson_data
#' )
#' }
create_run <- function(project_spec = NULL, project_id = NULL, 
                       start_date = NULL, end_date = NULL, 
                       study_area = NULL, include_geometry = FALSE, 
                       generate_thumbnails = FALSE) {
  client <- get_client()
  
  if (!is.logical(include_geometry)) {
    stop("include_geometry must be a boolean")
  }
  
  if (!is.logical(generate_thumbnails)) {
    stop("generate_thumbnails must be a boolean")
  }
  
  # Mode 1: Complete project_spec provided
  if (!is.null(project_spec)) {
    # Convert Project object to list if needed
    if (inherits(project_spec, "Project")) {
      body <- project_spec$to_list()
    } else if (is.list(project_spec)) {
      body <- project_spec
    } else {
      stop("project_spec must be a Project object or a named list")
    }
    
    # Ensure required fields are present
    if (is.null(body$type)) {
      body$type <- "template"
    }
    if (is.null(body$include_geometry)) {
      body$include_geometry <- include_geometry
    }
    if (is.null(body$generate_thumbnails)) {
      body$generate_thumbnails <- generate_thumbnails
    }
    
    # Use project_id from spec if not provided, or from parameter if spec's id field is NULL
    if (is.null(body$project_id) && !is.null(body$id)) {
      body$project_id <- body$id
    }
    if (is.null(body$project_id) && !is.null(project_id)) {
      body$project_id <- project_id
    }
    
    # Remove 'id' field as API expects 'project_id'
    body$id <- NULL
    
  } else {
    # Mode 2: Individual parameters (legacy substitutions API)
    if (is.null(project_id)) {
      stop("Either project_spec or project_id must be provided")
    }
    
    body <- list(
      type = "template",
      project_id = project_id,
      include_geometry = include_geometry,
      generate_thumbnails = generate_thumbnails
    )
    
    has_params <- !is.null(start_date) || !is.null(end_date) || !is.null(study_area)
    
    if (has_params) {
      substitutions <- list()
      
      if (!is.null(start_date) && !is.null(end_date)) {
        substitutions$date_range <- list(
          start_date = start_date,
          end_date = end_date
        )
      } else if (xor(!is.null(start_date), !is.null(end_date))) {
        stop("Both start_date and end_date must be specified if either is specified")
      }
      
      if (!is.null(study_area)) {
        substitutions$study_area <- study_area
      }
      
      body$substitutions <- substitutions
    }
  }
  
  response <- client$post("/runs/", payload = body)
  
  # API returns {data: {run_id: ...}} according to OpenAPI spec
  if (!is.null(response$data) && !is.null(response$data$run_id)) {
    return(response$data$run_id)
  }
  
  # Fallback for direct response format (if API differs from spec)
  if (!is.null(response$run_id)) {
    return(response$run_id)
  }
  
  stop("Could not extract run_id from response. Response structure: ", 
       paste(names(response), collapse = ", "))
}
