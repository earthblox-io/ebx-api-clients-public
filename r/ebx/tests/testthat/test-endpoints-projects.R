# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("list_projects returns list of Project objects", {
  # Mock the get_client and HTTP response
  mock_response <- list(
    data = list(
      list(id = "proj_1", title = "Project 1", description = "First project"),
      list(id = "proj_2", title = "Project 2", description = "Second project")
    )
  )
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(url, "/projects/")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  projects <- list_projects()
  
  expect_type(projects, "list")
  expect_length(projects, 2)
  expect_s3_class(projects[[1]], "Project")
  expect_equal(projects[[1]]$id, "proj_1")
  expect_equal(projects[[1]]$name, "Project 1")
  expect_equal(projects[[2]]$id, "proj_2")
  expect_equal(projects[[2]]$name, "Project 2")
})

test_that("list_projects returns empty list when no projects", {
  mock_response <- list(data = list())
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  projects <- list_projects()
  
  expect_type(projects, "list")
  expect_length(projects, 0)
})

test_that("list_projects returns empty list when data is NULL", {
  mock_response <- list(data = NULL)
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  projects <- list_projects()
  
  expect_type(projects, "list")
  expect_length(projects, 0)
})

test_that("get_project returns Project object", {
  mock_response <- list(
    data = list(
      title = "Test Project",
      description = "A test project",
      version = "1.0.0",
      api_access = TRUE
    )
  )
  
  mock_client <- list(
    get = function(url) {
      expect_equal(url, "/projects/proj_123")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  project <- get_project("proj_123")
  
  expect_s3_class(project, "Project")
  expect_equal(project$id, "proj_123")
  expect_equal(project$name, "Test Project")
  expect_equal(project$description, "A test project")
  expect_equal(project$version, "1.0.0")
  expect_true(project$api_access)
})

test_that("get_project handles missing data", {
  mock_response <- list(data = NULL)
  
  mock_client <- list(
    get = function(url) {
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(get_project("proj_123"), "No project data returned")
})

test_that("get_project maps title to name", {
  mock_response <- list(
    data = list(
      title = "API Title",
      description = "Testing title mapping"
    )
  )
  
  mock_client <- list(
    get = function(url) {
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  project <- get_project("proj_456")
  
  expect_equal(project$id, "proj_456")
  expect_equal(project$name, "API Title")
})
