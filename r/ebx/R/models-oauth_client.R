# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title OAuthClient
#' @description R6 class for OAuth client credentials
#' @importFrom R6 R6Class
#' @export
OAuthClient <- R6::R6Class(
  "OAuthClient",
  public = list(
    #' @field name The name of the client
    name = NULL,
    
    #' @field description The description of the client
    description = NULL,
    
    #' @field client_id The client ID
    client_id = NULL,
    
    #' @field client_secret The client secret
    client_secret = NULL,
    
    #' @field enabled Whether the client is enabled
    enabled = NULL,
    
    #' @description Create a new OAuthClient object
    #' @param name The name of the client
    #' @param description The description of the client
    #' @param client_id The client ID
    #' @param client_secret The client secret
    #' @param enabled Whether the client is enabled
    #' @return A new OAuthClient object
    initialize = function(name = NULL, description = NULL, client_id = NULL, 
                         client_secret = NULL, enabled = NULL) {
      self$name <- name
      self$description <- description
      self$client_id <- client_id
      self$client_secret <- client_secret
      self$enabled <- enabled
    },
    
    #' @description Save the OAuth client to disk
    #' @param config The client config (optional)
    #' @param filename The filename to save to (optional)
    #' @return self (invisible)
    save = function(config = NULL, filename = API_SECRETS_FILE) {
      if (is.null(config)) {
        config <- ServiceClientConfig$new()
      }
      
      data <- list(
        name = self$name,
        description = self$description,
        client_id = self$client_id,
        client_secret = self$client_secret,
        enabled = self$enabled
      )
      
      config$get_persistence_driver()$save(filename, data)
      invisible(self)
    },
    
    #' @description Convert OAuthClient to a plain list
    #' @param include_secret Whether to include client_secret (default: FALSE for security)
    #' @return A list containing OAuth client fields
    to_list = function(include_secret = FALSE) {
      result <- list(
        name = self$name,
        description = self$description,
        client_id = self$client_id,
        enabled = self$enabled
      )
      
      if (include_secret) {
        result$client_secret <- self$client_secret
      }
      
      result
    },
    
    #' @description Convert OAuthClient to JSON string
    #' @param pretty Whether to pretty-print the JSON (default: TRUE)
    #' @param include_secret Whether to include client_secret (default: FALSE for security)
    #' @param ... Additional arguments passed to jsonlite::toJSON
    #' @return A JSON string representation of the OAuth client
    to_json = function(pretty = TRUE, include_secret = FALSE, ...) {
      json_str <- jsonlite::toJSON(self$to_list(include_secret), auto_unbox = TRUE, ...)
      if (pretty) {
        json_str <- jsonlite::prettify(json_str)
      }
      json_str
    }
  ),
  active = list()
)

#' @title Load OAuth Client from disk
#' @description Load an OAuth client from a saved file
#' @param config The client config (optional)
#' @param filename The filename to load from (optional)
#' @return An OAuthClient object
#' @export
load_oauth_client <- function(config = NULL, filename = API_SECRETS_FILE) {
  if (is.null(config)) {
    config <- ServiceClientConfig$new()
  }
  
  data <- config$get_persistence_driver()$load(filename)
  
  OAuthClient$new(
    name = data$name,
    description = data$description,
    client_id = data$client_id,
    client_secret = data$client_secret,
    enabled = data$enabled
  )
}

#' @title Check if saved OAuth credentials exist
#' @description Check if a credentials file exists
#' @param config The client config (optional)
#' @param filename The filename to check (optional)
#' @return TRUE if the file exists, FALSE otherwise
#' @export
oauth_credentials_exist <- function(config = NULL, filename = API_SECRETS_FILE) {
  if (is.null(config)) {
    config <- ServiceClientConfig$new()
  }
  
  config$get_persistence_driver()$exists(filename)
}
