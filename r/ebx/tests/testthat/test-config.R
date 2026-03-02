# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("ClientConfig initializes with default values", {
  config <- ClientConfig$new()
  
  expect_equal(config$base_url, "https://api.earthblox.io")
  expect_equal(config$api_prefix, "/v1beta")
  expect_equal(config$oauth_path, "/services/oauth/token/")
  expect_s3_class(config$persistence_driver, "LocalFilePersistence")
})

test_that("ClientConfig respects environment variables", {
  # Set environment variables
  Sys.setenv(EBX_API_EMULATOR_HOST = "http://localhost:8080")
  Sys.setenv(EBX_API_PREFIX_PATH = "/api/v2")
  
  config <- ClientConfig$new()
  
  expect_equal(config$base_url, "http://localhost:8080")
  expect_equal(config$api_prefix, "/api/v2")
  
  # Cleanup
  Sys.unsetenv("EBX_API_EMULATOR_HOST")
  Sys.unsetenv("EBX_API_PREFIX_PATH")
})

test_that("ClientConfig generates correct API base URL", {
  config <- ClientConfig$new()
  
  api_url <- config$get_api_base_url()
  expect_equal(api_url, "https://api.earthblox.io/v1beta")
})

test_that("ClientConfig generates correct OAuth URL", {
  config <- ClientConfig$new()
  
  oauth_url <- config$get_oauth_url()
  expect_equal(oauth_url, "https://api.earthblox.io/services/oauth/token/")
})

test_that("ServiceClientConfig has empty API prefix", {
  config <- ServiceClientConfig$new()
  
  expect_equal(config$api_prefix, "")
  expect_equal(config$base_url, "https://api.earthblox.io")
})
