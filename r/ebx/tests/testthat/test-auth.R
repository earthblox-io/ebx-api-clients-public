# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("EnvAuthentication requires EBX_API_TOKEN", {
  # Ensure env var is not set
  Sys.unsetenv("EBX_API_TOKEN")
  
  config <- ClientConfig$new()
  
  expect_error(
    EnvAuthentication$new(config),
    "EBX_API_TOKEN"
  )
})

test_that("EnvAuthentication uses token from environment", {
  Sys.setenv(EBX_API_TOKEN = "test_token_from_env")
  
  config <- ClientConfig$new()
  auth <- EnvAuthentication$new(config)
  
  expect_equal(auth$auth_token, "test_token_from_env")
  expect_false(auth$has_expired())
  
  headers <- auth$get_headers()
  expect_equal(headers[["Authorization"]], "Bearer test_token_from_env")
  
  Sys.unsetenv("EBX_API_TOKEN")
})

test_that("BasicAuth generates correct headers", {
  config <- ClientConfig$new()
  auth <- BasicAuth$new(config, "test@example.com", "password123")
  
  headers <- auth$get_headers()
  
  expect_true("Authorization" %in% names(headers))
  expect_true(grepl("^Basic ", headers[["Authorization"]]))
  expect_false(auth$has_expired())
})

test_that("BasicAuth does not support refresh", {
  config <- ClientConfig$new()
  auth <- BasicAuth$new(config, "test@example.com", "password123")
  
  expect_error(auth$refresh(), "does not support refresh")
})

test_that("OAuthAuthentication requires client_id and client_secret", {
  config <- ClientConfig$new()
  
  # Ensure env vars are not set
  Sys.unsetenv("EBX_CLIENT_ID")
  Sys.unsetenv("EBX_CLIENT_SECRET")
  
  expect_error(
    OAuthAuthentication$new(config),
    "No client"
  )
  
  expect_error(
    OAuthAuthentication$new(config, client_id = "test_id"),
    "No client secret"
  )
})

test_that("OAuthAuthentication validates credentials from environment", {
  Sys.setenv(EBX_CLIENT_ID = "env_client_id")
  Sys.setenv(EBX_CLIENT_SECRET = "env_client_secret")
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_auth_test", ".ebx")
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Save a dummy token to avoid HTTP call in initialize
  dummy_token <- AuthToken$new(
    token = "dummy_token_123",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config, "env_client_id_.ebx.token.json")
  
  # This should work now without making HTTP calls
  auth <- OAuthAuthentication$new(config)
  
  expect_equal(auth$client_id, "env_client_id")
  expect_equal(auth$client_secret, "env_client_secret")
  expect_equal(auth$auth_token$token, "dummy_token_123")
  
  Sys.unsetenv("EBX_CLIENT_ID")
  Sys.unsetenv("EBX_CLIENT_SECRET")
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_oauth_auth_test"), recursive = TRUE)
})

test_that("OAuthAuthentication get_token_filename includes client_id", {
  Sys.setenv(EBX_CLIENT_ID = "test_id_123")
  Sys.setenv(EBX_CLIENT_SECRET = "test_secret")
  
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_auth_test2", ".ebx")
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  # Save a dummy token to avoid HTTP call in initialize
  dummy_token <- AuthToken$new(
    token = "dummy_token_456",
    expires = "2099-12-31T23:59:59"
  )
  dummy_token$save(config, "test_id_123_.ebx.token.json")
  
  auth <- OAuthAuthentication$new(config)
  
  filename <- auth$get_token_filename()
  expect_true(grepl("test_id_123", filename))
  expect_true(grepl(".ebx.token.json", filename))
  
  Sys.unsetenv("EBX_CLIENT_ID")
  Sys.unsetenv("EBX_CLIENT_SECRET")
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_oauth_auth_test2"), recursive = TRUE)
})
