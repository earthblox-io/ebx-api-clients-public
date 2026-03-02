# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("AuthToken can be created with string expires", {
  token <- AuthToken$new(
    token = "test_token_123",
    expires = "2026-12-31T23:59:59"
  )
  
  expect_equal(token$token, "test_token_123")
  expect_s3_class(token$expires, "POSIXct")
})

test_that("AuthToken can be created with POSIXct expires", {
  expires_time <- as.POSIXct("2026-12-31 23:59:59", tz = "UTC")
  
  token <- AuthToken$new(
    token = "test_token_123",
    expires = expires_time
  )
  
  expect_equal(token$token, "test_token_123")
  expect_equal(token$expires, expires_time)
})

test_that("AuthToken can be saved and loaded", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_token_test", ".ebx")
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  token <- AuthToken$new(
    token = "test_token_123",
    expires = "2026-12-31T23:59:59"
  )
  
  token$save(config, "test_token.json")
  
  loaded_token <- load_auth_token(config, "test_token.json")
  
  expect_equal(loaded_token$token, "test_token_123")
  expect_s3_class(loaded_token$expires, "POSIXct")
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_token_test"), recursive = TRUE)
})

test_that("auth_token_exists returns correct status", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_token_test2", ".ebx")
  
  config <- ClientConfig$new()
  config$persistence_driver <- LocalFilePersistence$new(path = test_path)
  
  expect_false(auth_token_exists(config, "test.json"))
  
  token <- AuthToken$new(token = "test", expires = "2026-12-31T23:59:59")
  token$save(config, "test.json")
  
  expect_true(auth_token_exists(config, "test.json"))
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_token_test2"), recursive = TRUE)
})
