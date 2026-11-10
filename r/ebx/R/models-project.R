# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title Project
#' @description R6 class representing a project
#' @importFrom R6 R6Class
#' @export
Project <- R6::R6Class(
  "Project",
  public = list(
    #' @field id The project ID
    id = NULL,
    
    #' @field name The project name (mapped from API 'title' field)
    name = NULL,
    
    #' @field description The project description
    description = NULL,
    
    #' @field version The project version
    version = NULL,
    
    #' @field api_version The API version
    api_version = NULL,
    
    #' @field api_access Whether API access is enabled
    api_access = NULL,
    
    #' @field variables List of project variables
    variables = NULL,
    
    #' @field exec_parameters Execution parameters configuration
    exec_parameters = NULL,

    #' @field portfolio_metric Portfolio metric data returned for analytics projects
    portfolio_metric = NULL,

    #' @field analytics Analytics metadata or payload returned by the API
    analytics = NULL,

    #' @field payload Raw analytics-project payload, preserved as returned
    payload = NULL,

    #' @field projectType API project type, including analytics projects
    projectType = NULL,

    #' @field api_data Complete API record, including fields not modeled directly
    api_data = NULL,

    #' @field hasMetric Whether the project has a metric
    hasMetric = NULL,

    #' @field type The project type
    type = NULL,

    #' @field analyticsEnabled Whether analytics are enabled
    analyticsEnabled = NULL,
    
    #' @description Create a new Project object
    #' @param id The project ID
    #' @param name The project name
    #' @param description The project description
    #' @param version The project version
    #' @param api_version The API version
    #' @param api_access Whether API access is enabled
    #' @param variables List of project variables
    #' @param exec_parameters Execution parameters
    #' @param portfolio_metric Portfolio metric data, preserved as returned
    #' @param analytics Analytics metadata or payload, preserved as returned
    #' @param payload Raw analytics-project payload, preserved as returned
    #' @param projectType API project type
    #' @param hasMetric API metric filter value
    #' @param type Project type
    #' @param analyticsEnabled API analytics-enabled value
    #' @param title Alternative name for 'name' field (API compatibility)
    #' @param ... Additional API fields retained in \code{api_data}; these are
    #'   not included when serializing a workflow run specification
    #' @return A new Project object
    initialize = function(id = NULL, name = NULL, description = NULL,
                         version = NULL, api_version = NULL, api_access = NULL,
                         variables = NULL, exec_parameters = NULL,
                         portfolio_metric = NULL, analytics = NULL,
                         payload = NULL, projectType = NULL,
                         hasMetric = NULL, type = NULL, analyticsEnabled = NULL,
                         title = NULL, ...) {
      self$id <- id
      # API returns 'title' but we use 'name' for consistency
      self$name <- if (!is.null(name)) name else title
      self$description <- description
      self$version <- version
      self$api_version <- api_version
      self$api_access <- api_access
      self$variables <- variables
      self$exec_parameters <- exec_parameters
      self$portfolio_metric <- portfolio_metric
      self$analytics <- analytics
      self$payload <- payload
      self$projectType <- projectType
      self$hasMetric <- hasMetric
      self$type <- type
      self$analyticsEnabled <- analyticsEnabled
      api_data <- list(
        id = id, name = name, title = title, description = description,
        version = version, api_version = api_version, api_access = api_access,
        variables = variables, exec_parameters = exec_parameters,
        portfolio_metric = portfolio_metric, analytics = analytics,
        payload = payload, projectType = projectType, hasMetric = hasMetric,
        type = type, analyticsEnabled = analyticsEnabled
      )
      additional_fields <- list(...)
      api_data[names(additional_fields)] <- additional_fields
      self$api_data <- api_data
    },
    
    #' @description Print method for Project
    #' @param ... Additional arguments (ignored)
    print = function(...) {
      cat("Project:\n")
      cat("  ID:", self$id, "\n")
      cat("  Name:", self$name, "\n")
      cat("  Description:", self$description, "\n")
      invisible(self)
    },
    
    #' @description Convert Project to a plain list
    #' @return A list containing all project fields
    to_list = function() {
      list(
        id = self$id,
        name = self$name,
        description = self$description,
        version = self$version,
        api_version = self$api_version,
        api_access = self$api_access,
        variables = self$variables,
        exec_parameters = self$exec_parameters
      )
    },
    
    #' @description Convert Project to JSON string
    #' @param pretty Whether to pretty-print the JSON (default: TRUE)
    #' @param ... Additional arguments passed to jsonlite::toJSON
    #' @return A JSON string representation of the project
    to_json = function(pretty = TRUE, ...) {
      json_str <- jsonlite::toJSON(self$to_list(), auto_unbox = TRUE, ...)
      if (pretty) {
        json_str <- jsonlite::prettify(json_str)
      }
      json_str
    }
  )
)
