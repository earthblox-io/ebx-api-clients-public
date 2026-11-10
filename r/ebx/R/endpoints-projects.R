# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title List Projects
#' @description List available projects, optionally filtered by API project attributes
#' @param title Optional project-title filter
#' @param has_metric Optional metric-presence filter
#' @param type Optional type filter; the API currently supports the analytics type
#' @param analytics_enabled Optional analytics-enabled filter
#' @param hasMetric Deprecated alias for \code{has_metric}
#' @param analyticsEnabled Deprecated alias for \code{analytics_enabled}
#' @return A list of Project objects
#' @export
list_projects <- function(title = NULL, has_metric = NULL, type = NULL,
                          analytics_enabled = NULL, hasMetric = NULL,
                          analyticsEnabled = NULL) {
  client <- get_client()
  if (!is.null(hasMetric)) {
    if (!is.null(has_metric) && !identical(has_metric, hasMetric)) {
      stop("Specify only one of has_metric and hasMetric")
    }
    has_metric <- hasMetric
  }
  if (!is.null(analyticsEnabled)) {
    if (!is.null(analytics_enabled) && !identical(analytics_enabled, analyticsEnabled)) {
      stop("Specify only one of analytics_enabled and analyticsEnabled")
    }
    analytics_enabled <- analyticsEnabled
  }
  query_params <- list(
    title = title,
    hasMetric = has_metric,
    type = type,
    analyticsEnabled = analytics_enabled
  )
  query_params <- query_params[!vapply(query_params, is.null, logical(1))]
  response <- client$get("/projects/", query_params = query_params)
  
  # API returns {data: [{project1}, {project2}, ...]}
  projects_data <- response$data
  
  if (is.null(projects_data)) {
    return(list())
  }
  
  lapply(projects_data, function(proj) {
    project <- do.call(Project$new, proj)
    project$api_data <- proj
    project
  })
}

#' @title Get Persistent Layers
#' @description Convenience accessor for a project's persistent layers. This
#'   abstracts away reaching into \code{project$api_data$persistent_layers}
#'   directly, and warns instead of erroring when a project does not export
#'   persistent layers.
#' @param project_id The project ID
#' @return A list of persistent-layer records; an empty list (with a warning)
#'   if the project does not export persistent layers
#' @export
get_persistent_layers <- function(project_id) {
  project <- get_project(project_id)
  layers <- project$api_data$persistent_layers
  if (is.null(layers) || length(layers) == 0) {
    warning(sprintf("Project '%s' does not export any persistent layers", project_id))
    return(list())
  }
  layers
}

#' @title Get Project
#' @description Get a specific project by ID
#' @param project_id The project ID
#' @return A Project object
#' @export
get_project <- function(project_id) {
  client <- get_client()
  response <- client$get(paste0("/projects/", project_id))
  
  # API returns {data: {project fields}}
  project_data <- response$data
  
  if (is.null(project_data)) {
    stop("No project data returned from API")
  }
  
  raw_project_data <- project_data

  # Add the project_id since API doesn't return it
  project_data$id <- project_id
  
  # Create Project object (initialize will map 'title' to 'name')
  project <- do.call(Project$new, project_data)
  project$api_data <- raw_project_data
  project
}
