# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("LocalFilePersistence creates directory lazily on first save", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_test", ".ebx")

  persistence <- LocalFilePersistence$new(path = test_path)

  # Directory should NOT be created at construction time (CRAN policy: no
  # filesystem writes outside tempdir() without user confirmation).
  expect_false(dir.exists(test_path))

  # Directory IS created on the first save().
  persistence$save("test.json", list(key = "value"))
  expect_true(dir.exists(test_path))

  # Cleanup
  unlink(file.path(temp_dir, "ebx_test"), recursive = TRUE)
})

test_that("LocalFilePersistence can save and load data", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_test2", ".ebx")
  
  persistence <- LocalFilePersistence$new(path = test_path)
  
  test_data <- list(
    name = "test",
    value = 123,
    enabled = TRUE
  )
  
  persistence$save("test.json", test_data)
  expect_true(persistence$exists("test.json"))
  
  loaded_data <- persistence$load("test.json")
  expect_equal(loaded_data$name, "test")
  expect_equal(loaded_data$value, 123)
  expect_equal(loaded_data$enabled, TRUE)
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_test2"), recursive = TRUE)
})

test_that("LocalFilePersistence handles missing files", {
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "ebx_test3", ".ebx")
  
  persistence <- LocalFilePersistence$new(path = test_path)
  
  expect_false(persistence$exists("nonexistent.json"))
  expect_error(persistence$load("nonexistent.json"), "does not exist")
  
  # Cleanup
  unlink(file.path(temp_dir, "ebx_test3"), recursive = TRUE)
})
