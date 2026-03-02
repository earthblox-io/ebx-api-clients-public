# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("OAuthClient can be created", {
  client <- OAuthClient$new(
    name = "test_client",
    description = "A test client",
    client_id = "test_id_123",
    client_secret = "test_secret_456",
    enabled = TRUE
  )
  
  expect_equal(client$name, "test_client")
  expect_equal(client$client_id, "test_id_123")
  expect_equal(client$client_secret, "test_secret_456")
  expect_true(client$enabled)
})

test_that("OAuthClient can be saved and loaded", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_test", ".ebx")
  
  config <- ServiceClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  client <- OAuthClient$new(
    name = "test_client",
    description = "A test client",
    client_id = "test_id_123",
    client_secret = "test_secret_456",
    enabled = TRUE
  )
  
  client$save(config = config, filename = "test_oauth.json")
  
  loaded_client <- load_oauth_client(config = config, filename = "test_oauth.json")
  
  expect_equal(loaded_client$name, "test_client")
  expect_equal(loaded_client$client_id, "test_id_123")
  expect_equal(loaded_client$client_secret, "test_secret_456")
  expect_true(loaded_client$enabled)
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_oauth_test"), recursive = TRUE)
})

test_that("oauth_credentials_exist returns correct status", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_oauth_test2", ".ebx")
  
  config <- ServiceClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  expect_false(oauth_credentials_exist(config = config, filename = "test.json"))
  
  client <- OAuthClient$new(
    name = "test",
    client_id = "id",
    client_secret = "secret"
  )
  client$save(config = config, filename = "test.json")
  
  expect_true(oauth_credentials_exist(config = config, filename = "test.json"))
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_oauth_test2"), recursive = TRUE)
})
