# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

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
