# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("get_task returns the task record", {
    task <- list(task_id = "task_1", status = "RUNNING", type = "upload", error = NULL)
    mock_client <- list(get = function(url, query_params = NULL) {
        expect_identical(url, "/tasks/task_1")
        list(data = task)
    })
    local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

    expect_identical(get_task("task_1"), task)
})

test_that("follow_task polls until a terminal status", {
    statuses <- c("PENDING", "RUNNING", "SUCCEEDED")
    calls <- 0
    local_mocked_bindings(
        get_task = function(task_id) {
            calls <<- calls + 1
            list(task_id = task_id, status = statuses[[calls]], dataset_id = "dataset_1")
        },
        .package = "ebx"
    )

    task <- follow_task("task_1", interval = 0)
    expect_identical(task$status, "SUCCEEDED")
    expect_identical(task$dataset_id, "dataset_1")
    expect_equal(calls, 3)
})

test_that("follow_task returns failed tasks and enforces limits", {
    local_mocked_bindings(
        get_task = function(task_id) list(task_id = task_id, status = "FAILED", error = "Bad CSV"),
        .package = "ebx"
    )
    task <- follow_task("task_1", interval = 0)
    expect_identical(task$error, "Bad CSV")

    local_mocked_bindings(
        get_task = function(task_id) list(task_id = task_id, status = "RUNNING"),
        .package = "ebx"
    )
    expect_error(follow_task("task_1", interval = 0, max_attempts = 2), "did not finish within 2 attempts")
    expect_error(follow_task("task_1", max_attempts = 0), "max_attempts")
    expect_error(follow_task("task_1", interval = -1), "interval")
})
