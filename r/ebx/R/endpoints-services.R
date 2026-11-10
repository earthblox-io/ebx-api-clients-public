# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title Create Token
#' @description Obtain a bearer access token directly from the OAuth token
#'   endpoint using HTTP Basic authentication with client credentials. This
#'   wraps the raw request previously written by hand (requesting
#'   \code{/services/oauth/token} with \code{req_auth_basic()} and a
#'   \code{client_credentials} grant), returning just the token string so it
#'   can be passed to \code{\link{auth_using_token}}.
#' @param client_id The client ID
#' @param secret The client secret
#' @param config The client configuration (optional)
#' @return The bearer access token as a character string
#' @importFrom httr2 req_auth_basic
#' @export
create_token <- function(client_id, secret, config = NULL) {
  if (is.null(config)) {
    config <- ClientConfig$new()
  }

  req <- httr2::request(paste0(config$base_url, "/services/oauth/token"))
  req <- httr2::req_auth_basic(req, client_id, secret)
  req <- httr2::req_body_json(req, list(grant_type = "client_credentials"))

  response <- httr2::req_perform(req)
  response_data <- httr2::resp_body_json(response)

  token_value <- if (!is.null(response_data[["access_token"]])) {
    response_data[["access_token"]]
  } else if (!is.null(response_data[["token"]])) {
    response_data[["token"]]
  } else {
    stop("No access_token field found in token response")
  }

  as.character(token_value)
}

#' @title Create OAuth Client
#' @description Register a new OAuth client with the API
#' @param email The email address of the user
#' @param password The password of the user
#' @param name The name for the new client
#' @param description The description for the new client (optional)
#' @param scopes The scopes for the new client (optional)
#' @return An OAuthClient object
#' @export
create_oauth_client <- function(email, password, name, description = "", scopes = c()) {
  config <- ServiceClientConfig$new()
  authenticator <- BasicAuth$new(config, email, password)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  payload <- list(
    name = name,
    description = description,
    scopes = scopes
  )
  
  response <- client$post(CLIENT_REGISTRATION_PATH, payload = payload)
  
  OAuthClient$new(
    name = response$data$name,
    description = response$data$description,
    client_id = response$data$client_id,
    client_secret = response$data$client_secret,
    enabled = response$data$enabled
  )
}
