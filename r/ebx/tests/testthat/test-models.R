# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("Project can be created", {
  project <- Project$new(
    id = "proj_123",
    name = "Test Project",
    description = "A test project"
  )
  
  expect_equal(project$id, "proj_123")
  expect_equal(project$name, "Test Project")
  expect_equal(project$description, "A test project")
})

test_that("Project can be created with title field", {
  # API returns 'title' but we map it to 'name'
  project <- Project$new(
    id = "proj_456",
    title = "Title Project",
    description = "Project created with title"
  )
  
  expect_equal(project$id, "proj_456")
  expect_equal(project$name, "Title Project")
  expect_equal(project$description, "Project created with title")
})

test_that("Project to_list works correctly", {
  project <- Project$new(
    id = "proj_123",
    name = "Test Project",
    description = "A test project",
    version = "1.0",
    api_access = TRUE
  )
  
  project_list <- project$to_list()
  
  expect_type(project_list, "list")
  expect_equal(project_list$id, "proj_123")
  expect_equal(project_list$name, "Test Project")
  expect_equal(project_list$description, "A test project")
  expect_equal(project_list$version, "1.0")
  expect_true(project_list$api_access)
})

test_that("Project to_json works correctly", {
  project <- Project$new(
    id = "proj_123",
    name = "Test Project",
    description = "A test project"
  )
  
  json_string <- project$to_json()
  
  expect_type(json_string, "character")
  expect_true(grepl("proj_123", json_string))
  expect_true(grepl("Test Project", json_string))
})

test_that("Run can be created", {
  run <- Run$new(
    id = "run_123",
    project_id = "proj_123",
    status = "completed",
    name = "Test Run"
  )
  
  expect_equal(run$id, "run_123")
  expect_equal(run$project_id, "proj_123")
  expect_equal(run$status, "completed")
  expect_equal(run$name, "Test Run")
})

test_that("Run to_list works correctly", {
  run <- Run$new(
    id = "run_123",
    project_id = "proj_123",
    status = "completed",
    name = "Test Run",
    started_at = "2024-01-01T10:00:00",
    completed_at = "2024-01-01T11:00:00"
  )
  
  run_list <- run$to_list()
  
  expect_type(run_list, "list")
  expect_equal(run_list$id, "run_123")
  expect_equal(run_list$project_id, "proj_123")
  expect_equal(run_list$status, "completed")
  expect_equal(run_list$name, "Test Run")
  expect_equal(run_list$started_at, "2024-01-01T10:00:00")
  expect_equal(run_list$completed_at, "2024-01-01T11:00:00")
})

test_that("Run to_json works correctly", {
  run <- Run$new(
    id = "run_123",
    project_id = "proj_123",
    status = "completed",
    name = "Test Run"
  )
  
  json_string <- run$to_json()
  
  expect_type(json_string, "character")
  expect_true(grepl("run_123", json_string))
  expect_true(grepl("proj_123", json_string))
  expect_true(grepl("completed", json_string))
})
