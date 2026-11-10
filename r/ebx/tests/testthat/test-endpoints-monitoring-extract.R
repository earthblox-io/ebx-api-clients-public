# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

monitor_results_fixture <- function() {
  list(
    fields = list(
      list(key = "CellCode", source = "location"),
      list(key = "metric_value", source = "metric")
    ),
    locations = list(
      list(id = "loc_1", values = list(CellCode = "A1")),
      list(id = "loc_2", values = list(CellCode = "A2"))
    ),
    observations = list(
      list(
        location_id = "loc_1", cycle_id = "cyc_1",
        monitoring_end_date = "2026-01-31T00:00:00Z",
        values = list(metric_value = 5)
      ),
      list(
        location_id = "loc_2", cycle_id = "cyc_1",
        monitoring_end_date = "2026-02-28T00:00:00Z",
        values = list(metric_value = 7)
      )
    )
  )
}

test_that("get_monitor_locations extracts a locations data frame", {
  mock_client <- list(get = function(url, query_params = NULL) list(data = monitor_results_fixture()))
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  locations_df <- get_monitor_locations("monitor_1")

  expect_s3_class(locations_df, "data.frame")
  expect_identical(locations_df$id, c("loc_1", "loc_2"))
  expect_identical(locations_df$CellCode, c("A1", "A2"))
})

test_that("get_monitor_observations extracts an observations data frame", {
  mock_client <- list(get = function(url, query_params = NULL) list(data = monitor_results_fixture()))
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  observations_df <- get_monitor_observations("monitor_1")

  expect_s3_class(observations_df, "data.frame")
  expect_identical(observations_df$location_id, c("loc_1", "loc_2"))
  expect_identical(observations_df$metric_value, c(5, 7))
})

test_that("get_monitor_table merges locations and observations, most recent first", {
  mock_client <- list(get = function(url, query_params = NULL) list(data = monitor_results_fixture()))
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  monitor_table <- get_monitor_table("monitor_1")

  expect_s3_class(monitor_table, "data.frame")
  expect_identical(names(monitor_table), c("end_date", "CellCode", "metric_value", "cycle_id"))
  # Most recent monitoring_end_date (loc_2, Feb) should sort first
  expect_identical(monitor_table$CellCode, c("A2", "A1"))
  expect_identical(monitor_table$metric_value, c(7, 5))
})

test_that("get_monitor_table honors an explicit location_cols selection", {
  fixture <- monitor_results_fixture()
  fixture$locations <- list(
    list(id = "loc_1", values = list(CellCode = "A1", Country = "X")),
    list(id = "loc_2", values = list(CellCode = "A2", Country = "Y"))
  )
  mock_client <- list(get = function(url, query_params = NULL) list(data = fixture))
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  monitor_table <- get_monitor_table("monitor_1", location_cols = "Country")

  expect_identical(names(monitor_table), c("end_date", "Country", "metric_value", "cycle_id"))
  expect_false("CellCode" %in% names(monitor_table))
})

test_that("monitor data frame helpers handle empty results", {
  mock_client <- list(get = function(url, query_params = NULL) {
    list(data = list(fields = list(), locations = list(), observations = list()))
  })
  local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

  expect_equal(nrow(get_monitor_locations("monitor_1")), 0)
  expect_equal(nrow(get_monitor_observations("monitor_1")), 0)
})
