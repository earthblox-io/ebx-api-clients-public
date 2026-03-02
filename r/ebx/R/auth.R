# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title AbstractAuthentication
#' @description Base R6 class for authentication methods
#' @importFrom R6 R6Class
#' @export
AbstractAuthentication <- R6::R6Class(
  "AbstractAuthentication",
  public = list(
    #' @field auth_token The authentication token
    auth_token = NULL,
    
    #' @field config The client configuration
    config = NULL,
    
    #' @description Create a new AbstractAuthentication object
    #' @param config The client configuration
    #' @return A new AbstractAuthentication object
    initialize = function(config) {
      self$auth_token <- NULL
      self$config <- config
    },
    
    #' @description Check if the token has expired
    #' @return TRUE if expired, FALSE otherwise
    has_expired = function() {
      if (is.null(self$auth_token)) {
        return(TRUE)
      }
      
      if (is.null(self$auth_token$expires)) {
        return(TRUE)
      }
      
      # Handle NA or invalid expires values
      if (is.na(self$auth_token$expires)) {
        return(TRUE)
      }
      
      if (self$auth_token$expires < Sys.time()) {
        return(TRUE)
      }
      
      return(FALSE)
    },
    
    #' @description Refresh the authentication token
    #' @return self
    refresh = function() {
      # Override in subclasses
      self
    },
    
    #' @description Get the headers for HTTP requests
    #' @return A named character vector of headers
    get_headers = function() {
      character(0)
    }
  )
)

#' @title EnvAuthentication
#' @description Authentication using environment variable token
#' @importFrom R6 R6Class
#' @export
EnvAuthentication <- R6::R6Class(
  "EnvAuthentication",
  inherit = AbstractAuthentication,
  public = list(
    #' @description Create a new EnvAuthentication object
    #' @param config The client configuration
    #' @return A new EnvAuthentication object
    initialize = function(config) {
      self$config <- config
      self$auth_token <- Sys.getenv("EBX_API_TOKEN", unset = "")
      if (nchar(self$auth_token) == 0) {
        stop("EBX_API_TOKEN environment variable not set")
      }
    },
    
    #' @description Check if token has expired (always FALSE for env auth)
    #' @return FALSE
    has_expired = function() {
      FALSE
    },
    
    #' @description Refresh the token from environment
    #' @return self
    refresh = function() {
      self$auth_token <- Sys.getenv("EBX_API_TOKEN", unset = "")
      self
    },
    
    #' @description Get the headers for HTTP requests
    #' @return A named character vector of headers
    get_headers = function() {
      c(Authorization = paste("Bearer", self$auth_token))
    }
  )
)

#' @title BasicAuth
#' @description Authentication using username and password
#' @importFrom R6 R6Class
#' @importFrom base64enc base64encode
#' @export
BasicAuth <- R6::R6Class(
  "BasicAuth",
  inherit = AbstractAuthentication,
  public = list(
    #' @field email The email address
    email = NULL,
    
    #' @field password The password
    password = NULL,
    
    #' @description Create a new BasicAuth object
    #' @param config The client configuration
    #' @param email The email address
    #' @param password The password
    #' @return A new BasicAuth object
    initialize = function(config, email, password) {
      self$config <- config
      self$email <- email
      self$password <- password
    },
    
    #' @description Check if token has expired (always FALSE for basic auth)
    #' @return FALSE
    has_expired = function() {
      FALSE
    },
    
    #' @description Refresh (not supported for basic auth)
    #' @return self
    refresh = function() {
      stop("BasicAuth does not support refresh")
    },
    
    #' @description Get the headers for HTTP requests
    #' @return A named character vector of headers
    get_headers = function() {
      credentials <- paste0(self$email, ":", self$password)
      base64_creds <- base64enc::base64encode(charToRaw(credentials))
      c(Authorization = paste("Basic", base64_creds))
    }
  )
)

#' @title OAuthAuthentication
#' @description Authentication using OAuth client credentials
#' @importFrom R6 R6Class
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json resp_status
#' @importFrom jsonlite toJSON
#' @export
OAuthAuthentication <- R6::R6Class(
  "OAuthAuthentication",
  inherit = AbstractAuthentication,
  public = list(
    #' @field client_id The client ID
    client_id = NULL,
    
    #' @field client_secret The client secret
    client_secret = NULL,
    
    #' @description Create a new OAuthAuthentication object
    #' @param config The client configuration
    #' @param client_id The client ID
    #' @param client_secret The client secret
    #' @return A new OAuthAuthentication object
    initialize = function(config, client_id = NULL, client_secret = NULL) {
      self$config <- config
      self$client_id <- client_id
      self$client_secret <- client_secret
      
      # Try to get from environment if not provided
      if (is.null(self$client_id)) {
        self$client_id <- Sys.getenv("EBX_CLIENT_ID", unset = "")
        if (nchar(self$client_id) == 0) {
          self$client_id <- NULL
        }
      }
      
      if (is.null(self$client_secret)) {
        self$client_secret <- Sys.getenv("EBX_CLIENT_SECRET", unset = "")
        if (nchar(self$client_secret) == 0) {
          self$client_secret <- NULL
        }
      }
      
      if (is.null(self$client_id)) {
        stop("No client ID provided")
      }
      
      if (is.null(self$client_secret)) {
        stop("No client secret provided")
      }
      
      self$auth_token <- NULL
      self$load_saved_credentials()
      
      # If no saved credentials, get a new token immediately
      if (is.null(self$auth_token)) {
        self$refresh()
      }
    },
    
    #' @description Get the token filename
    #' @return The filename for storing this client's token
    get_token_filename = function() {
      paste0(self$client_id, "_", API_TOKEN_FILE)
    },
    
    #' @description Load saved credentials from disk
    #' @return self
    load_saved_credentials = function() {
      filename <- self$get_token_filename()
      if (auth_token_exists(self$config, filename)) {
        self$auth_token <- load_auth_token(self$config, filename)
      }
      invisible(self)
    },
    
    #' @description Save credentials to disk
    #' @return self
    save_credentials = function() {
      filename <- self$get_token_filename()
      self$auth_token$save(self$config, filename)
      invisible(self)
    },
    
    #' @description Refresh the OAuth token
    #' @return self
    refresh = function() {
      request_data <- list(
        grant_type = "client_credentials",
        client_id = self$client_id,
        client_secret = self$client_secret
      )
      
      req <- httr2::request(self$config$get_oauth_url())
      req <- httr2::req_body_json(req, request_data)
      
      response <- httr2::req_perform(req)
      
      if (httr2::resp_status(response) == 200) {
        response_data <- httr2::resp_body_json(response)
        
        # Use exact field access with [[]] to avoid partial matching
        # Check if API returns 'token' or 'access_token'
        token_value <- if (!is.null(response_data[["token"]])) {
          response_data[["token"]]
        } else if (!is.null(response_data[["access_token"]])) {
          response_data[["access_token"]]
        } else {
          stop("No token field found in OAuth response")
        }
        
        # Check if API returns 'expires' or 'expires_in'
        expires_value <- if (!is.null(response_data[["expires"]])) {
          # String timestamp - pass as-is to AuthToken
          response_data[["expires"]]
        } else if (!is.null(response_data[["expires_in"]])) {
          # Check if expires_in is numeric (seconds) or string (timestamp)
          expires_in_val <- response_data[["expires_in"]]
          if (is.character(expires_in_val)) {
            # Already an ISO 8601 timestamp string
            expires_in_val
          } else {
            # Numeric seconds, convert to ISO 8601 string
            expires_time <- Sys.time() + as.numeric(expires_in_val)
            format(expires_time, "%Y-%m-%dT%H:%M:%S+00:00", tz = "UTC")
          }
        } else {
          stop("No expires field found in OAuth response")
        }
        
        self$auth_token <- AuthToken$new(
          token = token_value,
          expires = expires_value
        )
        self$save_credentials()
        return(self)
      } else {
        stop(sprintf("OAuth authentication failed with status %d", httr2::resp_status(response)))
      }
    },
    
    #' @description Get the headers for HTTP requests
    #' @return A named character vector of headers
    get_headers = function() {
      if (is.null(self$auth_token) || is.null(self$auth_token$token)) {
        stop("No authentication token available. Authentication may have failed.")
      }
      
      # Ensure token is a single character string (not a list or vector)
      token_value <- self$auth_token$token
      if (is.list(token_value)) {
        token_value <- unlist(token_value, use.names = FALSE)[1]
      }
      token_value <- as.character(token_value)[1]
      
      c(Authorization = paste("Bearer", token_value))
    }
  )
)
