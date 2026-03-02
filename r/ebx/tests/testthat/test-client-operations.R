# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("get_client returns named client", {
  # Clear the global client store
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  .ebx_clients[["test_client"]] <- client
  
  retrieved <- get_client("test_client")
  expect_identical(retrieved, client)
})

test_that("get_client throws error for non-existent named client", {
  withr::defer(rm(list = ls(envir = ebx:::.ebx_clients), envir = ebx:::.ebx_clients))
  
  expect_error(
    get_client("nonexistent"),
    "No client with name 'nonexistent'"
  )
})

test_that("get_client returns current client when no name given", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  .ebx_clients[[".current"]] <- client
  
  retrieved <- get_client()
  expect_identical(retrieved, client)
})

test_that("get_client returns first client when no current set", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  .ebx_clients[["client_1"]] <- client
  
  retrieved <- get_client()
  expect_equal(retrieved$name, client$name)
  expect_s3_class(retrieved, "EbxClient")
})

test_that("get_client throws error when no clients exist", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  # Clear ALL entries including .current
  rm(list = ls(.ebx_clients, all.names = TRUE), envir = .ebx_clients)
  withr::defer(rm(list = ls(envir = .ebx_clients, all.names = TRUE), envir = .ebx_clients))
  
  expect_error(
    get_client(),
    "No clients have been created"
  )
})

test_that("set_client stores client with given name", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  set_client(client, "my_client")
  
  expect_identical(.ebx_clients[["my_client"]], client)
  expect_identical(.ebx_clients[[".current"]], client)
})

test_that("set_client auto-generates name when not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)
  
  set_client(client)
  
  expect_identical(.ebx_clients[["client_1"]], client)
})

test_that("set_client increments auto-generated names", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  client1 <- EbxClient$new(authenticator = authenticator, config = config)
  client2 <- EbxClient$new(authenticator = authenticator, config = config)
  
  set_client(client1)
  set_client(client2)
  
  expect_identical(.ebx_clients[["client_1"]], client1)
  expect_identical(.ebx_clients[["client_2"]], client2)
})

test_that("auth_using creates and stores client", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  
  client <- auth_using(authenticator, config, "test_auth")
  
  expect_s3_class(client, "EbxClient")
  expect_identical(.ebx_clients[["test_auth"]], client)
  expect_identical(.ebx_clients[[".current"]], client)
})

test_that("auth_using creates default config when not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  config <- ClientConfig$new()
  authenticator <- EnvAuthentication$new(config)
  
  client <- auth_using(authenticator)
  
  expect_s3_class(client$config, "ClientConfig")
})

test_that("auth_using_env creates client with EnvAuthentication", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token_123")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  client <- auth_using_env("env_client")
  
  expect_s3_class(client, "EbxClient")
  expect_s3_class(client$authenticator, "EnvAuthentication")
  expect_equal(client$authenticator$auth_token, "test_token_123")
})

test_that("auth_using_env creates default config when not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_API_TOKEN = "test_token")
  withr::defer(Sys.unsetenv("EBX_API_TOKEN"))
  
  client <- auth_using_env()
  
  expect_s3_class(client$config, "ClientConfig")
  expect_equal(client$config$base_url, "https://api.earthblox.io")
})

test_that("auth_using_oauth creates client with OAuthAuthentication", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_client_test", ".ebx")
  withr::defer(unlink(file.path(temp_dir, "ebx_oauth_client_test"), recursive = TRUE))
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Pre-save a token to avoid HTTP call
  dummy_token <- AuthToken$new(
    token = "dummy_oauth_token",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config, "oauth_id_123_.ebx.token.json")
  
  client <- auth_using_oauth(
    client_id = "oauth_id_123",
    client_secret = "oauth_secret_456",
    name = "oauth_client",
    config = config
  )
  
  expect_s3_class(client, "EbxClient")
  expect_s3_class(client$authenticator, "OAuthAuthentication")
  expect_equal(client$authenticator$client_id, "oauth_id_123")
})

test_that("auth_using_oauth uses environment variables when credentials not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  Sys.setenv(EBX_CLIENT_ID = "env_oauth_id")
  Sys.setenv(EBX_CLIENT_SECRET = "env_oauth_secret")
  withr::defer({
    Sys.unsetenv("EBX_CLIENT_ID")
    Sys.unsetenv("EBX_CLIENT_SECRET")
  })
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_env_test", ".ebx")
  withr::defer(unlink(file.path(temp_dir, "ebx_oauth_env_test"), recursive = TRUE))
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Pre-save a token to avoid HTTP call
  dummy_token <- AuthToken$new(
    token = "dummy_env_token",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config, "env_oauth_id_.ebx.token.json")
  
  client <- auth_using_oauth(config = config)
  
  expect_equal(client$authenticator$client_id, "env_oauth_id")
  expect_equal(client$authenticator$client_secret, "env_oauth_secret")
})

test_that("auth_using_creds loads credentials from file", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_creds_test", ".ebx")
  withr::defer(unlink(file.path(temp_dir, "ebx_creds_test"), recursive = TRUE))
  
  config <- ServiceClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Save OAuth client credentials
  oauth_client <- OAuthClient$new(
    name = "Stored Client",
    client_id = "stored_id_789",
    client_secret = "stored_secret_abc",
    enabled = TRUE
  )
  oauth_client$save(config = config, filename = "test_creds.json")
  
  # Pre-save a token to avoid HTTP call
  config2 <- ClientConfig$new()
  config2$persistence_driver <- LocalFilePersistence$new(path = test_path)
  dummy_token <- AuthToken$new(
    token = "stored_token",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config2, "stored_id_789_.ebx.token.json")
  
  client <- auth_using_creds(
    filename = "test_creds.json",
    name = "creds_client",
    config = config2
  )
  
  expect_s3_class(client, "EbxClient")
  expect_equal(client$authenticator$client_id, "stored_id_789")
  expect_equal(client$authenticator$client_secret, "stored_secret_abc")
})

test_that("auth_using_creds uses default filename when not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_creds_default_test", ".ebx")
  withr::defer(unlink(file.path(temp_dir, "ebx_creds_default_test"), recursive = TRUE))
  
  config <- ServiceClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Save OAuth client with default filename
  oauth_client <- OAuthClient$new(
    name = "Default Client",
    client_id = "default_id",
    client_secret = "default_secret",
    enabled = TRUE
  )
  oauth_client$save(config = config)  # Uses default filename
  
  # Pre-save a token
  config2 <- ClientConfig$new()
  config2$persistence_driver <- LocalFilePersistence$new(path = test_path)
  dummy_token <- AuthToken$new(
    token = "default_token",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config2, "default_id_.ebx.token.json")
  
  client <- auth_using_creds(config = config2)
  
  expect_s3_class(client, "EbxClient")
  expect_equal(client$authenticator$client_id, "default_id")
})

test_that("auth_using_creds creates default config when not provided", {
  .ebx_clients <- get(".ebx_clients", envir = asNamespace("ebx"))
  withr::defer(rm(list = ls(envir = .ebx_clients), envir = .ebx_clients))
  
  # This will fail because the default path won't have credentials,
  # but we're testing that it tries to create ClientConfig
  expect_error(
    auth_using_creds(),
    # Will fail trying to load the client, but that's expected
    "does not exist"
  )
})
