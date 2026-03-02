# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title EbxClient
#' @description R6 class for Earth Blox API client
#' @importFrom R6 R6Class
#' @importFrom httr2 request req_headers req_url_query req_body_json req_perform resp_body_json resp_body_string resp_content_type req_timeout
#' @importFrom jsonlite toJSON
#' @export
EbxClient <- R6::R6Class(
  "EbxClient",
  public = list(
    #' @field name The name of the client
    name = NULL,
    
    #' @field config The client configuration
    config = NULL,
    
    #' @field authenticator The authentication method
    authenticator = NULL,
    
    #' @description Create a new EbxClient object
    #' @param authenticator The authentication method
    #' @param config The client configuration (optional)
    #' @param name The client name (optional)
    #' @return A new EbxClient object
    initialize = function(authenticator = NULL, config = NULL, name = NULL) {
      if (is.null(config)) {
        config <- ClientConfig$new()
      }
      
      self$config <- config
      self$name <- name
      
      if (is.null(authenticator)) {
        authenticator <- EnvAuthentication$new(config)
      }
      
      self$authenticator <- authenticator
    },
    
    #' @description Get headers for HTTP requests
    #' @return A named character vector of headers
    get_headers = function() {
      base_headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
      auth_headers <- self$authenticator$get_headers()
      c(base_headers, auth_headers)
    },
    
    #' @description Parse response body based on Content-Type header
    #' @param response The httr2 response object
    #' @return The parsed response data
    parse_response = function(response) {
      content_type <- httr2::resp_content_type(response)
      if (grepl("application/json", content_type, fixed = TRUE)) {
            # print("Response parsed as JSON")
        return(httr2::resp_body_json(response))
      } else if (grepl("text/html", content_type, fixed = TRUE)) {
        tryCatch({
            # print("Response parsed as HTML")
            return(httr2::resp_body_string(response))
        }, error = function(e) {
            # print("Response is not HTML")
            return(httr2::resp_body_json(response))
        })
      } else if (grepl("text/", content_type, fixed = TRUE)) {
        return(httr2::resp_body_string(response))
      } else {
        # Default to JSON for unknown types (backwards compatibility)
        return(httr2::resp_body_json(response))
      }
    },
    
    #' @description Make a GET request to the API
    #' @param url The endpoint URL
    #' @param query_params Query parameters (optional)
    #' @param headers Additional headers (optional)
    #' @param timeout Request timeout in seconds (optional)
    #' @return The parsed response data
    get = function(url, query_params = NULL, headers = NULL, timeout = NULL) {
      if (self$authenticator$has_expired()) {
        self$authenticator <- self$authenticator$refresh()
      }
      
      request_headers <- self$get_headers()
      if (!is.null(headers)) {
        request_headers <- c(request_headers, headers)
      }
      
      base_url <- self$config$get_api_base_url()
      full_url <- paste0(base_url, url)
      
      req <- httr2::request(full_url)
      
      # Add headers one by one (required for proper httr2 behavior)
      for (header_name in names(request_headers)) {
        header_value <- request_headers[[header_name]]
        req <- httr2::req_headers(req, !!header_name := header_value)
      }
      
      if (!is.null(query_params)) {
        req <- httr2::req_url_query(req, !!!query_params)
      }
      
      if (!is.null(timeout)) {
        req <- httr2::req_timeout(req, timeout)
      }
      
      response <- httr2::req_perform(req)
      self$parse_response(response)
    },
    
    #' @description Make a POST request to the API
    #' @param url The endpoint URL
    #' @param payload The request payload (optional)
    #' @param headers Additional headers (optional)
    #' @param timeout Request timeout in seconds (optional)
    #' @return The parsed response data
    post = function(url, payload = NULL, headers = NULL, timeout = NULL) {
      if (self$authenticator$has_expired()) {
        self$authenticator <- self$authenticator$refresh()
      }
      
      request_headers <- self$get_headers()
      if (!is.null(headers)) {
        request_headers <- c(request_headers, headers)
      }
      
      base_url <- self$config$get_api_base_url()
      full_url <- paste0(base_url, url)
      
      if (is.null(payload)) {
        payload <- list()
      }
      
      req <- httr2::request(full_url)
      
      # Add headers one by one (required for proper httr2 behavior)
      for (header_name in names(request_headers)) {
        header_value <- request_headers[[header_name]]
        req <- httr2::req_headers(req, !!header_name := header_value)
      }
      
      req <- httr2::req_body_json(req, payload)
      
      if (!is.null(timeout)) {
        req <- httr2::req_timeout(req, timeout)
      }
      
      response <- httr2::req_perform(req)
      self$parse_response(response)
    }
  )
)

# Global client store for managing multiple clients
.ebx_clients <- new.env(parent = emptyenv())

#' @title Get or create a client
#' @description Get an existing client or the current default client
#' @param name The client name (optional)
#' @return An EbxClient object
#' @export
get_client <- function(name = NULL) {
  if (!is.null(name)) {
    client <- .ebx_clients[[name]]
    if (is.null(client)) {
      stop(sprintf("No client with name '%s' has been created", name))
    }
    return(client)
  }
  
  # Check for current client stored in the environment
  current <- .ebx_clients[[".current"]]
  if (!is.null(current)) {
    return(current)
  }
  
  # Get the first client
  client_names <- setdiff(ls(.ebx_clients), ".current")
  if (length(client_names) == 0) {
    stop("No clients have been created")
  }
  
  .ebx_clients[[client_names[1]]]
}

#' @title Set the current client
#' @description Set a client as the current default
#' @param client The EbxClient object
#' @param name The client name (optional)
#' @return The client (invisible)
#' @keywords internal
set_client <- function(client, name = NULL) {
  if (is.null(name)) {
    # Count clients excluding the .current marker
    client_count <- length(setdiff(ls(.ebx_clients), ".current"))
    name <- sprintf("client_%d", client_count + 1)
  }
  
  .ebx_clients[[name]] <- client
  # Store current client reference in the environment (avoids locked binding issue)
  .ebx_clients[[".current"]] <- client
  
  invisible(client)
}

#' @title Authenticate using an authenticator
#' @description Low-level authentication function
#' @param authenticator The authentication object
#' @param config The client configuration (optional)
#' @param name The client name (optional)
#' @return An EbxClient object
#' @export
auth_using <- function(authenticator, config = NULL, name = NULL) {
  if (is.null(config)) {
    config <- ClientConfig$new()
  }
  
  client <- EbxClient$new(authenticator = authenticator, config = config, name = name)
  set_client(client, name)
  
  client
}

#' @title Authenticate using environment variable
#' @description Authenticate using the EBX_API_TOKEN environment variable
#' @param name The client name (optional)
#' @param config The client configuration (optional)
#' @return An EbxClient object
#' @export
auth_using_env <- function(name = NULL, config = NULL) {
  if (is.null(config)) {
    config <- ClientConfig$new()
  }
  
  authenticator <- EnvAuthentication$new(config)
  auth_using(authenticator, config, name)
}

#' @title Authenticate using OAuth
#' @description Authenticate using OAuth client credentials
#' @param client_id The client ID (optional, can be from env)
#' @param client_secret The client secret (optional, can be from env)
#' @param name The client name (optional)
#' @param config The client configuration (optional)
#' @return An EbxClient object
#' @export
auth_using_oauth <- function(client_id = NULL, client_secret = NULL, name = NULL, config = NULL) {
  if (is.null(config)) {
    config <- ClientConfig$new()
  }
  
  authenticator <- OAuthAuthentication$new(config, client_id, client_secret)
  auth_using(authenticator, config, name)
}

#' @title Authenticate using saved credentials
#' @description Authenticate using saved OAuth credentials from disk
#' @param filename The credentials filename (optional)
#' @param name The client name (optional)
#' @param config The client configuration (optional)
#' @return An EbxClient object
#' @export
auth_using_creds <- function(filename = API_SECRETS_FILE, name = NULL, config = NULL) {
  if (is.null(config)) {
    config <- ClientConfig$new()
  }
  
  oauth_client <- load_oauth_client(config, filename)
  authenticator <- OAuthAuthentication$new(config, oauth_client$client_id, oauth_client$client_secret)
  auth_using(authenticator, config, name)
}
