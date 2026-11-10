# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title Run
#' @description R6 class representing a run
#' @importFrom R6 R6Class
#' @export
Run <- R6::R6Class(
  "Run",
  public = list(
    #' @field id The run ID
    id = NULL,
    
    #' @field project_id The associated project ID
    project_id = NULL,
    
    #' @field status The run status
    status = NULL,
    
    #' @field started_at The run start timestamp
    started_at = NULL,
    
    #' @field completed_at The run completion timestamp
    completed_at = NULL,
    
    #' @field exec_parameters The execution parameters
    exec_parameters = NULL,
    
    #' @field layers The layers associated with the run
    layers = NULL,
    
    #' @field outputs The outputs (charts/tables) associated with the run
    outputs = NULL,
    
    #' @field name The run name (optional)
    name = NULL,

    #' @field error_code Error code returned by the API
    error_code = NULL,

    #' @field tasks Task references returned by the API
    tasks = NULL,

    #' @field variables Run variables returned by the API
    variables = NULL,

    #' @field monitor_id Associated monitor ID
    monitor_id = NULL,

    #' @field schedule_id Associated schedule ID
    schedule_id = NULL,

    #' @field json Raw JSON payload returned by the API
    json = NULL,
    
    #' @description Create a new Run object
    #' @param id The run ID
    #' @param project_id The project ID
    #' @param status The run status
    #' @param started_at The run start timestamp
    #' @param completed_at The run completion timestamp
    #' @param exec_parameters The execution parameters
    #' @param layers The layers list
    #' @param outputs The outputs list
    #' @param name The run name (optional)
    #' @param error_code Error code
    #' @param tasks Task references
    #' @param variables Run variables
    #' @param monitor_id Associated monitor ID
    #' @param schedule_id Associated schedule ID
    #' @param json Raw JSON payload
    #' @param ... Additional fields (ignored)
    #' @return A new Run object
    initialize = function(id = NULL, project_id = NULL, status = NULL, 
                         started_at = NULL, completed_at = NULL, 
                         exec_parameters = NULL, layers = NULL, outputs = NULL,
                         name = NULL, error_code = NULL, tasks = NULL,
                         variables = NULL, monitor_id = NULL, schedule_id = NULL,
                         json = NULL, ...) {
      self$id <- id
      self$project_id <- project_id
      self$status <- status
      self$started_at <- started_at
      self$completed_at <- completed_at
      self$exec_parameters <- exec_parameters
      self$layers <- layers
      self$outputs <- outputs
      self$name <- name
      self$error_code <- error_code
      self$tasks <- tasks
      self$variables <- variables
      self$monitor_id <- monitor_id
      self$schedule_id <- schedule_id
      self$json <- json
    },
    
    #' @description Print method for Run
    #' @param ... Additional arguments (ignored)
    print = function(...) {
      cat("Run:\n")
      cat("  ID:", self$id, "\n")
      cat("  Project ID:", self$project_id, "\n")
      cat("  Status:", self$status, "\n")
      cat("  Started:", self$started_at, "\n")
      cat("  Completed:", self$completed_at, "\n")
      cat("  Layers:", if (!is.null(self$layers)) length(self$layers) else 0, "\n")
      cat("  Outputs:", if (!is.null(self$outputs)) length(self$outputs) else 0, "\n")
      invisible(self)
    },
    
    #' @description Convert Run to a plain list
    #' @return A list containing all run fields
    to_list = function() {
      list(
        id = self$id,
        project_id = self$project_id,
        status = self$status,
        started_at = self$started_at,
        completed_at = self$completed_at,
        exec_parameters = self$exec_parameters,
        layers = self$layers,
        outputs = self$outputs,
        name = self$name,
        error_code = self$error_code,
        tasks = self$tasks,
        variables = self$variables,
        monitor_id = self$monitor_id,
        schedule_id = self$schedule_id,
        json = self$json
      )
    },
    
    #' @description Convert Run to JSON string
    #' @param pretty Whether to pretty-print the JSON (default: TRUE)
    #' @param ... Additional arguments passed to jsonlite::toJSON
    #' @return A JSON string representation of the run
    to_json = function(pretty = TRUE, ...) {
      json_str <- jsonlite::toJSON(self$to_list(), auto_unbox = TRUE, ...)
      if (pretty) {
        json_str <- jsonlite::prettify(json_str)
      }
      json_str
    }
  )
)
