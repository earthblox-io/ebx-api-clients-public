# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

# list_runs tests
test_that("list_runs returns list of Run objects with default limit", {
  mock_response <- list(
    data = list(
      list(id = "run_1", project_id = "proj_1", status = "completed", name = "Run 1"),
      list(id = "run_2", project_id = "proj_1", status = "running", name = "Run 2")
    )
  )
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(url, "/runs/")
      expect_equal(query_params$limit, 10)
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  runs <- list_runs()
  
  expect_type(runs, "list")
  expect_length(runs, 2)
  expect_s3_class(runs[[1]], "Run")
  expect_equal(runs[[1]]$id, "run_1")
  expect_equal(runs[[1]]$status, "completed")
})

test_that("list_runs respects custom limit", {
  mock_response <- list(data = list())
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(query_params$limit, 25)
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  runs <- list_runs(limit = 25)
  expect_type(runs, "list")
})

test_that("list_runs returns empty list when no runs", {
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
  
  runs <- list_runs()
  expect_length(runs, 0)
})

# get_run tests
test_that("get_run returns Run object", {
  mock_response <- list(
    data = list(
      id = "run_123",
      project_id = "proj_456",
      status = "completed",
      name = "Test Run",
      started_at = "2024-01-01T10:00:00",
      completed_at = "2024-01-01T11:00:00"
    )
  )
  
  mock_client <- list(
    get = function(url) {
      expect_equal(url, "/runs/run_123")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run <- get_run("run_123")
  
  expect_s3_class(run, "Run")
  expect_equal(run$id, "run_123")
  expect_equal(run$project_id, "proj_456")
  expect_equal(run$status, "completed")
})

test_that("get_run handles missing data", {
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
  
  expect_error(get_run("run_123"), "No run data returned")
})

# get_run_status tests
test_that("get_run_status returns status string", {
  mock_response <- list(
    data = list(
      status = "running",
      error_code = NULL
    )
  )
  
  mock_client <- list(
    get = function(url) {
      expect_equal(url, "/runs/run_123/status")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  status <- get_run_status("run_123")
  expect_equal(status, "running")
})

test_that("get_run_status handles alternative response format", {
  mock_response <- list(
    status = "completed"
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
  
  status <- get_run_status("run_123")
  expect_equal(status, "completed")
})

# get_charts tests
test_that("get_charts returns chart data", {
  mock_response <- list(
    data = list(
      list(title = "Chart 1", type = "line"),
      list(title = "Chart 2", type = "bar")
    )
  )
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(url, "/runs/run_123/charts")
      expect_null(query_params)
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  charts <- get_charts("run_123")
  expect_length(charts, 2)
})

test_that("get_charts applies filter", {
  mock_response <- list(data = list())
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(query_params$title, "temperature")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  charts <- get_charts("run_123", filter = "temperature")
  expect_type(charts, "list")
})

# get_tables tests
test_that("get_tables returns table data", {
  mock_response <- list(
    data = list(
      list(title = "Table 1"),
      list(title = "Table 2")
    )
  )
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(url, "/runs/run_123/tables")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  tables <- get_tables("run_123")
  expect_length(tables, 2)
})

test_that("get_tables applies filter", {
  mock_response <- list(data = list())
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(query_params$title, "summary")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  tables <- get_tables("run_123", filter = "summary")
  expect_type(tables, "list")
})

# get_layers tests
test_that("get_layers returns layer data", {
  mock_response <- list(
    data = list(
      list(title = "Layer 1"),
      list(title = "Layer 2")
    )
  )
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(url, "/runs/run_123/layers")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  layers <- get_layers("run_123")
  expect_length(layers, 2)
})

test_that("get_layers applies filter", {
  mock_response <- list(data = list())
  
  mock_client <- list(
    get = function(url, query_params = NULL) {
      expect_equal(query_params$title, "vegetation")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  layers <- get_layers("run_123", filter = "vegetation")
  expect_type(layers, "list")
})

# follow_run tests
test_that("follow_run polls until completion", {
  call_count <- 0
  
  mock_client <- list(
    get = function(url) {
      call_count <<- call_count + 1
      if (call_count < 3) {
        return(list(data = list(id = "run_123", status = "running")))
      } else {
        return(list(data = list(id = "run_123", status = "completed")))
      }
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run <- follow_run("run_123", interval = 0.1, max_attempts = 10)
  
  expect_s3_class(run, "Run")
  expect_equal(run$status, "completed")
  expect_gte(call_count, 3)
})

test_that("follow_run stops on failed status", {
  mock_client <- list(
    get = function(url) {
      return(list(data = list(id = "run_123", status = "failed")))
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run <- follow_run("run_123", interval = 0.1, max_attempts = 10)
  expect_equal(run$status, "failed")
})

test_that("follow_run stops on cancelled status", {
  mock_client <- list(
    get = function(url) {
      return(list(data = list(id = "run_123", status = "cancelled")))
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run <- follow_run("run_123", interval = 0.1, max_attempts = 10)
  expect_equal(run$status, "cancelled")
})

test_that("follow_run throws error on timeout", {
  mock_client <- list(
    get = function(url) {
      return(list(data = list(id = "run_123", status = "running")))
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(
    follow_run("run_123", interval = 0.1, max_attempts = 2),
    "did not complete within 2 attempts"
  )
})

# create_run tests
test_that("create_run with project_spec sends correct payload", {
  mock_response <- list(data = list(run_id = "new_run_123"))
  
  mock_client <- list(
    post = function(url, payload) {
      expect_equal(url, "/runs/")
      expect_equal(payload$type, "template")
      expect_equal(payload$project_id, "proj_123")
      expect_false(payload$include_geometry)
      expect_false(payload$generate_thumbnails)
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  spec <- Project$new(id = "proj_123", name = "Test Project")
  run_id <- create_run(project_spec = spec)
  
  expect_equal(run_id, "new_run_123")
})

test_that("create_run with project_spec and variables", {
  mock_response <- list(data = list(run_id = "new_run_456"))
  
  mock_client <- list(
    post = function(url, payload) {
      expect_equal(payload$project_id, "proj_123")
      expect_length(payload$variables, 1)
      expect_equal(payload$variables[[1]]$key, "var_1")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  spec <- Project$new(
    id = "proj_123",
    variables = list(list(key = "var_1", type = "area", value = "geojson"))
  )
  run_id <- create_run(project_spec = spec)
  
  expect_equal(run_id, "new_run_456")
})

test_that("create_run with project_id and substitutions", {
  mock_response <- list(data = list(run_id = "new_run_789"))
  
  mock_client <- list(
    post = function(url, payload) {
      expect_equal(payload$project_id, "proj_123")
      expect_equal(payload$substitutions$date_range$start_date, "2024-01-01")
      expect_equal(payload$substitutions$date_range$end_date, "2024-12-31")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run_id <- create_run(
    project_id = "proj_123",
    start_date = "2024-01-01",
    end_date = "2024-12-31"
  )
  
  expect_equal(run_id, "new_run_789")
})

test_that("create_run with study_area substitution", {
  mock_response <- list(data = list(run_id = "new_run_abc"))
  
  mock_client <- list(
    post = function(url, payload) {
      expect_equal(payload$substitutions$study_area, "geojson_data")
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  run_id <- create_run(
    project_id = "proj_123",
    study_area = "geojson_data"
  )
  
  expect_equal(run_id, "new_run_abc")
})

test_that("create_run with include_geometry and generate_thumbnails", {
  mock_response <- list(data = list(run_id = "new_run_def"))
  
  mock_client <- list(
    post = function(url, payload) {
      expect_true(payload$include_geometry)
      expect_true(payload$generate_thumbnails)
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  spec <- Project$new(id = "proj_123")
  run_id <- create_run(
    project_spec = spec,
    include_geometry = TRUE,
    generate_thumbnails = TRUE
  )
  
  expect_equal(run_id, "new_run_def")
})

test_that("create_run requires both start_date and end_date", {
  mock_client <- list(post = function(url, payload) {})
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(
    create_run(project_id = "proj_123", start_date = "2024-01-01"),
    "Both start_date and end_date"
  )
  
  expect_error(
    create_run(project_id = "proj_123", end_date = "2024-12-31"),
    "Both start_date and end_date"
  )
})

test_that("create_run validates include_geometry is boolean", {
  mock_client <- list(post = function(url, payload) {})
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(
    create_run(project_id = "proj_123", include_geometry = "yes"),
    "include_geometry must be a boolean"
  )
})

test_that("create_run validates generate_thumbnails is boolean", {
  mock_client <- list(post = function(url, payload) {})
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(
    create_run(project_id = "proj_123", generate_thumbnails = "yes"),
    "generate_thumbnails must be a boolean"
  )
})

test_that("create_run requires project_spec or project_id", {
  mock_client <- list(post = function(url, payload) {})
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  expect_error(
    create_run(),
    "Either project_spec or project_id must be provided"
  )
})

test_that("create_run handles alternative response format", {
  mock_response <- list(run_id = "direct_run_id")
  
  mock_client <- list(
    post = function(url, payload) {
      return(mock_response)
    }
  )
  
  local_mocked_bindings(
    get_client = function() mock_client,
    .package = "ebx"
  )
  
  spec <- Project$new(id = "proj_123")
  run_id <- create_run(project_spec = spec)
  
  expect_equal(run_id, "direct_run_id")
})
