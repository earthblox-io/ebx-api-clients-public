# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title ClientConfig
#' @description R6 class for Earth Blox API client configuration
#' @importFrom R6 R6Class
#' @export
ClientConfig <- R6::R6Class(
  "ClientConfig",
  public = list(
    #' @field base_url The base URL for the API
    base_url = NULL,
    
    #' @field api_prefix The version prefix for the API
    api_prefix = NULL,
    
    #' @field oauth_path The path for the OAuth flow
    oauth_path = NULL,
    
    #' @field persistence_driver The persistence driver for storing credentials
    persistence_driver = NULL,
    
    #' @description Create a new ClientConfig object
    #' @return A new ClientConfig object
    initialize = function() {
      self$base_url <- BASE_URL
      self$api_prefix <- API_PREFIX
      self$oauth_path <- OAUTH_PATH
      # Select persistence driver based on whether the secrets directory (or its
      # nearest existing ancestor) is writable.  On hosted environments such as
      # shinyapps.io the XDG data directory is not writable, so we fall back to
      # an in-memory store that lives only for the duration of the R session.
      self$persistence_driver <- local({
        p <- API_SECRETS_PATH
        root <- p
        while (nchar(root) > 1 && !dir.exists(root)) root <- dirname(root)
        writable <- tryCatch({
          tf <- tempfile(tmpdir = root)
          file.create(tf)
          file.remove(tf)
          TRUE
        }, error = function(e) FALSE, warning = function(w) FALSE)
        if (writable) LocalFilePersistence$new(path = p) else MemoryPersistence$new()
      })
      
      # Override with environment variables if set
      emulator_host <- Sys.getenv("EBX_API_EMULATOR_HOST", unset = "")
      if (nchar(emulator_host) > 0) {
        self$base_url <- emulator_host
      }
      
      prefix_path <- Sys.getenv("EBX_API_PREFIX_PATH", unset = "")
      if (nchar(prefix_path) > 0) {
        self$api_prefix <- prefix_path
      }
    },
    
    #' @description Get the full API base URL including version prefix
    #' @return The full API base URL
    get_api_base_url = function() {
      paste0(self$base_url, self$api_prefix)
    },
    
    #' @description Get the OAuth URL
    #' @return The OAuth URL
    get_oauth_url = function() {
      auth_emulator <- Sys.getenv("EBX_API_AUTH_EMULATOR", unset = "")
      if (nchar(auth_emulator) > 0) {
        return(auth_emulator)
      }
      paste0(self$base_url, self$oauth_path)
    },
    
    #' @description Get the persistence driver
    #' @return The persistence driver
    get_persistence_driver = function() {
      self$persistence_driver
    }
  )
)

#' @title ServiceClientConfig
#' @description R6 class for Earth Blox API client configuration for service endpoints
#' @importFrom R6 R6Class
#' @export
ServiceClientConfig <- R6::R6Class(
  "ServiceClientConfig",
  inherit = ClientConfig,
  public = list(
    #' @description Create a new ServiceClientConfig object
    #' @return A new ServiceClientConfig object
    initialize = function() {
      super$initialize()
      self$api_prefix <- ""
      self$base_url <- BASE_URL
      
      # Override with environment variable if set
      client_reg <- Sys.getenv("EBX_API_CLIENT_REGISTRATION", unset = "")
      if (nchar(client_reg) > 0) {
        self$base_url <- client_reg
      }
    }
  )
)
