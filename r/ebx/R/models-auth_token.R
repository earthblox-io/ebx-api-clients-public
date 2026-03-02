# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title AuthToken
#' @description R6 class for authentication tokens
#' @importFrom R6 R6Class
#' @export
AuthToken <- R6::R6Class(
  "AuthToken",
  public = list(
    #' @field token The access token
    token = NULL,
    
    #' @field expires The expiration datetime
    expires = NULL,
    
    #' @description Create a new AuthToken object
    #' @param token The access token
    #' @param expires The expiration datetime (as POSIXct or string)
    #' @return A new AuthToken object
    initialize = function(token = NULL, expires = NULL) {
      # Ensure token is stored as a single character string
      if (!is.null(token)) {
        # Handle cases where token might be a list or vector
        if (is.list(token)) {
          self$token <- as.character(token[[1]])
        } else if (length(token) > 1) {
          self$token <- as.character(token[1])
        } else {
          self$token <- as.character(token)
        }
      } else {
        self$token <- NULL
      }
      
      if (is.character(expires)) {
        # Parse ISO 8601 datetime string with timezone
        # Handle formats like "2026-02-18T15:37:31.206608+00:00"
        self$expires <- as.POSIXct(expires, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
        # If parsing failed, try without fractional seconds
        if (is.na(self$expires)) {
          self$expires <- as.POSIXct(expires, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        }
      } else {
        self$expires <- expires
      }
    },
    
    #' @description Save the token to disk
    #' @param config The client config
    #' @param filename The filename to save to
    #' @return self (invisible)
    save = function(config, filename) {
      data <- list(
        token = self$token,
        expires = format(self$expires, "%Y-%m-%dT%H:%M:%S")
      )
      
      config$get_persistence_driver()$save(filename, data)
      invisible(self)
    },
    
    #' @description Convert AuthToken to a plain list
    #' @param include_token Whether to include the actual token (default: FALSE for security)
    #' @return A list containing auth token fields
    to_list = function(include_token = FALSE) {
      result <- list(
        expires = format(self$expires, "%Y-%m-%dT%H:%M:%S")
      )
      
      if (include_token) {
        result$token <- self$token
      }
      
      result
    },
    
    #' @description Convert AuthToken to JSON string
    #' @param pretty Whether to pretty-print the JSON (default: TRUE)
    #' @param include_token Whether to include the actual token (default: FALSE for security)
    #' @param ... Additional arguments passed to jsonlite::toJSON
    #' @return A JSON string representation of the auth token
    to_json = function(pretty = TRUE, include_token = FALSE, ...) {
      json_str <- jsonlite::toJSON(self$to_list(include_token), auto_unbox = TRUE, ...)
      if (pretty) {
        json_str <- jsonlite::prettify(json_str)
      }
      json_str
    }
  )
)

#' @title Load Auth Token from disk
#' @description Load an auth token from a saved file
#' @param config The client config
#' @param filename The filename to load from
#' @return An AuthToken object
#' @export
load_auth_token <- function(config, filename) {
  data <- config$get_persistence_driver()$load(filename)
  
  # Ensure token is a character string, not a list
  token_value <- data$token
  if (is.list(token_value)) {
    token_value <- unlist(token_value, use.names = FALSE)[1]
  }
  
  AuthToken$new(
    token = token_value,
    expires = data$expires
  )
}

#' @title Check if saved token exists
#' @description Check if a token file exists
#' @param config The client config
#' @param filename The filename to check
#' @return TRUE if the file exists, FALSE otherwise
#' @export
auth_token_exists <- function(config, filename) {
  config$get_persistence_driver()$exists(filename)
}
