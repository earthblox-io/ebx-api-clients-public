# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title LocalFilePersistence
#' @description R6 class for persisting credentials to local filesystem
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
LocalFilePersistence <- R6::R6Class(
  "LocalFilePersistence",
  public = list(
    #' @field path The directory path for storing files
    path = NULL,
    
    #' @description Create a new LocalFilePersistence object
    #' @param path The directory path for storing files
    #' @return A new LocalFilePersistence object
    initialize = function(path = API_SECRETS_PATH) {
      self$path <- path
      # Directory is created lazily on first save() to avoid writing to the
      # filesystem at construction time (required by CRAN policy).
    },
    
    #' @description Save data to a file
    #' @param filename The name of the file
    #' @param data The data to save (will be converted to JSON)
    #' @return NULL (invisible)
    save = function(filename, data) {
      # Create directory on first write (lazy creation, CRAN-compliant)
      if (!dir.exists(self$path)) {
        dir.create(self$path, recursive = TRUE)
      }
      filepath <- file.path(self$path, filename)
      json_data <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
      writeLines(json_data, filepath)
      invisible(NULL)
    },
    
    #' @description Load data from a file
    #' @param filename The name of the file
    #' @return The loaded data as a list
    load = function(filename) {
      filepath <- file.path(self$path, filename)
      if (!file.exists(filepath)) {
        stop(sprintf("File %s does not exist", filepath))
      }
      json_data <- paste(readLines(filepath, warn = FALSE), collapse = "\n")
      jsonlite::fromJSON(json_data, simplifyVector = TRUE)
    },
    
    #' @description Check if a file exists
    #' @param filename The name of the file
    #' @return TRUE if the file exists, FALSE otherwise
    exists = function(filename) {
      filepath <- file.path(self$path, filename)
      file.exists(filepath)
    }
  )
)
