# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title List Runs
#' @description List runs with optional limit and raw JSON payloads
#' @param limit The maximum number of runs to return (default: 10)
#' @param include_json Whether to request each run's raw JSON payload
#' @return A list of Run objects
#' @export
list_runs <- function(limit = 10, include_json = FALSE) {
  client <- get_client()
  
  query_params <- list(limit = limit)
  if (isTRUE(include_json)) {
    query_params$include_json <- TRUE
  }
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
#' @description Get a specific run by ID, optionally including its raw JSON payload
#' @param run_id The run ID
#' @param include_json Whether to request the raw JSON payload
#' @return A Run object
#' @export
get_run <- function(run_id, include_json = FALSE) {
  client <- get_client()
  if (isTRUE(include_json)) {
    response <- client$get(
      paste0("/runs/", run_id),
      query_params = list(include_json = TRUE)
    )
  } else {
    response <- client$get(paste0("/runs/", run_id))
  }
  
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
#' @param details Return the complete status response, including error details
#' @return The status string by default, or the full structured response when
#'   \code{details = TRUE}
#' @export
#' @details The default return value remains a status string for compatibility.
get_run_status <- function(run_id, details = FALSE) {
  client <- get_client()
  response <- client$get(paste0("/runs/", run_id, "/status"))
  
  # API returns {data: {status: "...", error_code: ...}}
  if (!is.null(response$data)) {
    if (isTRUE(details)) {
      return(response$data)
    }
    return(response$data$status)
  }
  
  # Fallback to direct field access if structure differs
  if (isTRUE(details)) {
    return(response)
  }
  response$status
}

#' @title Get Charts
#' @description Get charts from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to chart titles
#' @param include_meta Return the complete raw response envelope including metadata
#' @return Chart data by default; the raw \code{data}/\code{meta} envelope when requested
#' @export
get_charts <- function(run_id, filter = NULL, include_meta = FALSE) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/charts")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [outputs...], meta: {...}}
  if (isTRUE(include_meta)) {
    return(response)
  }
  response$data
}

#' @title Get Tables
#' @description Get tables from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to table titles
#' @param include_meta Return the complete raw response envelope including metadata
#' @return Table data by default; the raw \code{data}/\code{meta} envelope when requested
#' @export
get_tables <- function(run_id, filter = NULL, include_meta = FALSE) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/tables")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [outputs...], meta: {...}}
  if (isTRUE(include_meta)) {
    return(response)
  }
  response$data
}

#' @title Get Layers
#' @description Get layers from a run
#' @param run_id The run ID
#' @param filter Optional filter to apply to layer titles
#' @param include_meta Return the complete raw response envelope including metadata
#' @return Layer data by default; the raw \code{data}/\code{meta} envelope when requested
#' @export
get_layers <- function(run_id, filter = NULL, include_meta = FALSE) {
  client <- get_client()
  
  url <- paste0("/runs/", run_id, "/layers")
  query_params <- NULL
  if (!is.null(filter)) {
    query_params <- list(title = filter)
  }
  
  response <- client$get(url, query_params = query_params)
  
  # API returns {data: [layers...], meta: {...}}
  if (isTRUE(include_meta)) {
    return(response)
  }
  response$data
}

#' @title Get Workflow Layers
#' @description Convenience accessor for the layers exported by a single
#'   workflow run. This wraps \code{\link{get_layers}}, issuing a warning
#'   instead of returning an opaque empty result when the workflow does not
#'   export any layers (not every workflow does).
#' @param run_id The run ID
#' @param filter Optional filter to apply to layer titles
#' @return A list of layer records; an empty list (with a warning) if the
#'   workflow run does not export any layers
#' @export
get_workflow_layers <- function(run_id, filter = NULL) {
  layers <- get_layers(run_id, filter = filter)
  if (is.null(layers) || length(layers) == 0) {
    warning(sprintf("Run '%s' does not export any layers", run_id))
    return(list())
  }
  layers
}

#' @title Follow Run
#' @description Follow a run's progress (polling)
#' @param run_id The run ID
#' @param interval Polling interval in seconds (default: 5)
#' @param max_attempts Maximum number of polling attempts (default: 60)
#' @return The final Run object
#' @export
follow_run <- function(run_id, interval = 5, max_attempts = 60) {
  if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
      is.na(max_attempts) || !is.finite(max_attempts) ||
      max_attempts < 1 || max_attempts != floor(max_attempts)) {
    stop("max_attempts must be a positive finite integer")
  }
  if (!is.numeric(interval) || length(interval) != 1L || is.na(interval) ||
      !is.finite(interval) || interval < 0) {
    stop("interval must be a non-negative finite number")
  }
  for (i in seq_len(max_attempts)) {
    run <- get_run(run_id)
    
    if (run$status %in% c("completed", "error", "cancelled", "failed")) {
      return(run)
    }
    
    if (i < max_attempts) {
      Sys.sleep(interval)
    }
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
#' @param include_geometry Whether to include geometry in output (default: FALSE);
#'   NULL sends an explicit JSON null
#' @param generate_thumbnails Whether to generate thumbnails for every layer
#'   (default: FALSE); NULL sends an explicit JSON null
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
  
  if (!is.null(include_geometry) &&
      (!is.logical(include_geometry) || length(include_geometry) != 1L ||
       is.na(include_geometry))) {
    stop("include_geometry must be a boolean or NULL")
  }
  
  if (!is.null(generate_thumbnails) &&
      (!is.logical(generate_thumbnails) || length(generate_thumbnails) != 1L ||
       is.na(generate_thumbnails))) {
    stop("generate_thumbnails must be a boolean or NULL")
  }
  
  # Mode 1: Complete project_spec provided
  if (!is.null(project_spec)) {
    # Convert Project object to list if needed
    if (inherits(project_spec, "Project")) {
      if (identical(project_spec$projectType, "analytics") ||
          identical(project_spec$type, "analytics")) {
        stop("Analytics projects are not workflow run specifications")
      }
      body <- project_spec$to_list()
    } else if (is.list(project_spec)) {
      body <- project_spec
      if (identical(body$projectType, "analytics") ||
          identical(body$type, "analytics")) {
        stop("Analytics projects are not workflow run specifications")
      }
    } else {
      stop("project_spec must be a Project object or a named list")
    }
    
    # Ensure required fields are present
    if (is.null(body$type)) {
      body$type <- "template"
    }
    if (!"include_geometry" %in% names(body)) {
      body["include_geometry"] <- list(include_geometry)
    }
    if (!"generate_thumbnails" %in% names(body)) {
      body["generate_thumbnails"] <- list(generate_thumbnails)
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
