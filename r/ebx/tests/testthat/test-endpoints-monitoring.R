# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("monitoring list and detail endpoints preserve structured data", {
  nested <- list(
    id = "monitor_1",
    location = list(coordinates = c(1, 2)),
    observations = list(list(value = 7), NULL, list(value = 9))
  )
  mock_client <- list(
    get = function(url, query_params = NULL) {
      if (identical(url, "/monitors")) {
        return(list(data = list(nested)))
      }
      if (identical(url, "/monitors/monitor_1")) {
        return(list(data = nested))
      }
      if (identical(url, "/monitors/monitor_1/cycles")) {
        expect_equal(query_params$limit, 4)
        return(list(data = list(list(id = "cycle_1"))))
      }
      if (identical(url, "/monitors/monitor_1/results")) {
        expect_equal(query_params$cycle_limit, 3)
        return(list(data = nested))
      }
      if (identical(url, "/monitor-cycles/cycle_1")) {
        return(list(data = nested))
      }
      if (identical(url, "/schedules")) {
        return(list(data = list(list(id = "schedule_1"))))
      }
      if (identical(url, "/schedules/schedule_1")) {
        return(list(data = nested))
      }
      if (identical(url, "/alerts")) {
        expect_equal(query_params$monitor_id, "monitor_1")
        return(list(data = list(nested)))
      }
      if (identical(url, "/alerts/alert_1")) {
        return(list(data = nested))
      }
      stop("Unexpected URL: ", url)
    }
  )
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  expect_equal(list_monitors(), list(nested))
  expect_equal(get_monitor("monitor_1"), nested)
  expect_equal(list_monitor_cycles("monitor_1", limit = 4), list(list(id = "cycle_1")))
  expect_equal(get_monitor_results("monitor_1", cycle_limit = 3), nested)
  expect_equal(get_monitor_cycle("cycle_1"), nested)
  expect_equal(list_schedules(), list(list(id = "schedule_1")))
  expect_equal(get_schedule("schedule_1"), nested)
  expect_equal(list_alerts("monitor_1"), list(nested))
  expect_equal(get_alert("alert_1"), nested)
})

test_that("as_json returns a pretty-printed JSON string instead of an R list", {
  nested <- list(id = "monitor_1", location = list(coordinates = c(1, 2)))
  mock_client <- list(
    get = function(url, query_params = NULL) list(data = nested)
  )
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  json_result <- get_monitor("monitor_1", as_json = TRUE)
  expect_type(json_result, "character")
  expect_identical(json_result, to_json(nested))
  expect_match(json_result, '"id": \\[?"monitor_1"')

  list_result <- get_monitor("monitor_1", as_json = FALSE)
  expect_identical(list_result, nested)
})

test_that("to_json and print_json render R structures as JSON", {
  x <- list(a = 1, b = list(c = "two"))

  compact <- to_json(x, pretty = FALSE)
  pretty <- to_json(x, pretty = TRUE)
  expect_type(compact, "character")
  expect_true(nchar(pretty) >= nchar(compact))

  expect_output(result <- print_json(x), "\"a\"")
  expect_identical(result, pretty)
})

test_that("alert updates validate and send only specified fields", {
  mock_client <- list(
    patch = function(url, payload) {
      expect_equal(url, "/alerts/alert_1")
      expect_identical(payload, list(is_read = FALSE, status = "acknowledged"))
      list(data = list(id = "alert_1", is_read = FALSE, status = "acknowledged"))
    }
  )
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  updated <- update_alert("alert_1", is_read = FALSE, status = "acknowledged")
  expect_identical(updated$status, "acknowledged")
  expect_error(update_alert("alert_1"), "at least one")
  expect_error(update_alert("alert_1", is_read = 1), "is_read must")
  expect_error(update_alert("alert_1", status = "failed"), "status must")
})

test_that("mark-all-read uses query filter and extracts updated count", {
  mock_client <- list(
    post = function(url, query_params = NULL) {
      expect_equal(url, "/alerts/mark-all-read")
      expect_equal(query_params$monitor_id, "monitor_1")
      list(data = list(updated_count = 5L))
    }
  )
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  expect_identical(mark_all_alerts_read("monitor_1"), 5L)
})

test_that("supplied token authentication uses bearer header without refresh", {
  client <- auth_using_token("short_lived_token", name = "user-token-test")
  expect_s3_class(client$authenticator, "UserTokenAuthentication")
  expect_identical(
    client$authenticator$get_headers(),
    c(Authorization = "Bearer short_lived_token")
  )
  expect_false(client$authenticator$has_expired())
  expect_error(client$authenticator$refresh(), "cannot be refreshed")
  expect_error(auth_using_token(""), "non-empty")
  rm(list = ls(envir = ebx:::.ebx_clients), envir = ebx:::.ebx_clients)
})

test_that("HTTP transport supports methods, repeated query values, and JSON payloads", {
  captured <- list()
  local_mocked_bindings(
    req_perform = function(req) {
      captured[[length(captured) + 1L]] <<- req
      httr2::response_json(
        url = req$url,
        method = req$method,
        headers = list(`content-type` = "application/json"),
        body = list(data = list(ok = TRUE))
      )
    },
    .package = "httr2"
  )

  client <- EbxClient$new(
    authenticator = UserTokenAuthentication$new(ClientConfig$new(), "unit-token"),
    config = ClientConfig$new()
  )
  expect_equal(
    client$patch(
      "/alerts/alert_1",
      payload = list(is_read = FALSE, nullable = NULL),
      query_params = list(tag = c("one", "two"), omitted = NULL),
      headers = c(`X-Test` = "example"),
      timeout = 8
    ),
    list(data = list(ok = TRUE))
  )
  expect_equal(captured[[1]]$method, "PATCH")
  expect_match(captured[[1]]$url, "tag=one")
  expect_match(captured[[1]]$url, "tag=two")
  expect_false(grepl("omitted", captured[[1]]$url, fixed = TRUE))
  expect_true("Authorization" %in% names(captured[[1]]$headers))
  expect_true("X-Test" %in% names(captured[[1]]$headers))
  expect_equal(captured[[1]]$options$timeout_ms, 8000)
  expect_identical(captured[[1]]$body$data$is_read, FALSE)
  expect_true("nullable" %in% names(captured[[1]]$body$data))

  client$put("/resource", payload = list(values = c(1, 2)))
  client$delete("/resource", query_params = list(skip = NULL))
  client$post("/resource", payload = list(value = "x"), query_params = list(tag = c("a", "b")))
  expect_identical(vapply(captured, `[[`, character(1), "method"),
                   c("PATCH", "PUT", "DELETE", "POST"))
  expect_null(captured[[3]]$body)
  expect_match(captured[[4]]$url, "tag=a")
  expect_match(captured[[4]]$url, "tag=b")
})

test_that("project filters and run detail additions preserve response structure", {
  project_record <- list(
    id = "analytics_1",
    title = "Analytics project",
    projectType = "analytics",
    hasMetric = TRUE,
    portfolio_metric = list(score = 0.8),
    payload = list(series = list(1, 2)),
    orgId = "org-example"
  )
  mock_client <- list(
    get = function(url, query_params = NULL) {
      if (identical(url, "/projects/")) {
        expect_identical(
          query_params,
          list(title = "Analytics", hasMetric = TRUE, type = "analytics", analyticsEnabled = TRUE)
        )
        return(list(data = list(project_record)))
      }
      if (identical(url, "/runs/run_1")) {
        expect_identical(query_params, list(include_json = TRUE))
        return(list(data = list(
          id = "run_1", status = "error", error_code = "failed",
          tasks = list(list(id = "task_1")), variables = list(list(key = "x")),
          monitor_id = "monitor_1", schedule_id = "schedule_1",
          json = list(payload = TRUE), outputs = list(list(id = "output_1"))
        )))
      }
      if (identical(url, "/runs/run_1/status")) {
        return(list(data = list(status = "error", error_code = "failed")))
      }
      if (identical(url, "/runs/run_1/charts")) {
        return(list(data = list(list(title = "chart")), meta = list(`next` = NULL)))
      }
      stop("Unexpected URL: ", url)
    }
  )
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  project <- list_projects(
    title = "Analytics", has_metric = TRUE, type = "analytics",
    analytics_enabled = TRUE
  )[[1]]
  expect_equal(project$portfolio_metric, list(score = 0.8))
  expect_equal(project$api_data$payload, list(series = list(1, 2)))
  expect_equal(project$payload, list(series = list(1, 2)))
  expect_equal(project$api_data$orgId, "org-example")
  expect_equal(project$projectType, "analytics")
  expect_error(create_run(project_spec = project), "not workflow run specifications")

  run <- get_run("run_1", include_json = TRUE)
  expect_equal(run$error_code, "failed")
  expect_equal(run$tasks[[1]]$id, "task_1")
  expect_equal(run$variables[[1]]$key, "x")
  expect_equal(run$monitor_id, "monitor_1")
  expect_equal(run$schedule_id, "schedule_1")
  expect_equal(run$json$payload, TRUE)
  expect_equal(get_run_status("run_1"), "error")
  expect_equal(get_run_status("run_1", details = TRUE)$error_code, "failed")
  expect_equal(get_charts("run_1", include_meta = TRUE)$meta, list(`next` = NULL))
})

test_that("workflow variables retain scalar types, legacy keys, and null fields", {
  variables <- list(
    list(key = "threshold", type = "number", value = 0.5),
    list(key = "label", type = "string", value = ""),
    list(key = "window_legacy", type = "date range", value = list(
      start_date = "2026-01-01", end_date = "2026-01-31"
    )),
    list(key = "window", type = "date_range", value = list(
      start_date = "2026-02-01", end_date = "2026-02-28"
    )),
    list(key = "area", type = "area", value = NULL, location_name_column = "site")
  )
  mock_client <- list(post = function(url, payload) {
    expect_equal(url, "/runs/")
    expect_identical(payload$variables, variables)
    list(data = list(run_id = "run_variables"))
  })
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  spec <- Project$new(id = "workflow-example", variables = variables)
  expect_identical(create_run(project_spec = spec), "run_variables")
})

test_that("monitor list endpoints preserve null and empty data responses", {
  mock_client <- list(get = function(url) {
    if (identical(url, "/monitors")) {
      return(list(data = NULL))
    }
    list(data = list())
  })
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  expect_null(list_monitors())
  expect_identical(list_schedules(), list())
})
