# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("dataset list and detail endpoints return API records", {
    record <- list(id = "dataset_1", title = "Sites", status = "COMPLETED", bands = NULL)
    captured_query <- "unset"
    mock_client <- list(
        get = function(url, query_params = NULL) {
            if (identical(url, "/datasets/")) {
                captured_query <<- query_params
                return(list(data = list(record)))
            }
            if (identical(url, "/datasets/dataset_1")) {
                return(list(data = record))
            }
            stop("Unexpected URL: ", url)
        }
    )
    local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

    expect_equal(list_datasets(), list(record))
    expect_null(captured_query)
    list_datasets(type = "FeatureCollection")
    expect_identical(captured_query, list(type = "FeatureCollection"))
    expect_equal(get_dataset("dataset_1"), record)
})

test_that("update_dataset sends only supplied fields", {
    captured <- NULL
    mock_client <- list(patch = function(url, payload) {
        expect_identical(url, "/datasets/dataset_1")
        captured <<- payload
        list(data = list(id = "dataset_1", location = "organisation", permission = "viewable"))
    })
    local_mocked_bindings(get_client = function() mock_client, .package = "ebx")

    result <- update_dataset("dataset_1", location = "organisation", permission = "viewable")
    expect_identical(captured, list(location = "organisation", permission = "viewable"))
    expect_identical(result$location, "organisation")

    update_dataset("dataset_1", title = "Renamed", cadence_interval = 2L)
    expect_identical(captured, list(title = "Renamed", cadence_interval = 2L))

    update_dataset("dataset_1", location = "user")
    expect_identical(captured, list(location = "user"))
})

test_that("update_dataset validates location and permission", {
    local_mocked_bindings(
        get_client = function() list(patch = function(...) stop("should not call API")),
        .package = "ebx"
    )
    expect_error(update_dataset("dataset_1"), "at least one field")
    expect_error(update_dataset("dataset_1", location = "organisation"), "permission .* is required")
    expect_error(update_dataset("dataset_1", location = "user", permission = "viewable"), "cannot be set")
    expect_error(update_dataset("dataset_1", location = "team"), "should be one of")
    expect_error(update_dataset("dataset_1", location = "organisation", permission = "admin"), "should be one of")
})

test_that("create_dataset sends a location CSV request and uploads the file", {
    csv <- withr::local_tempfile(fileext = ".csv")
    writeLines(c("name,lat,lon,value", "A,55.9,-3.2,1.5"), csv)

    captured <- NULL
    uploaded <- NULL
    mock_client <- list(post = function(url, payload) {
        expect_identical(url, "/datasets/")
        captured <<- payload
        list(
            task_id = "user_task",
            upload_method = "PUT",
            expires_at = "2026-01-01T00:00:00Z",
            uploads = list(list(filename = basename(csv), upload_url = "https://upload.example/signed"))
        )
    })
    local_mocked_bindings(
        get_client = function() mock_client,
        upload_dataset_file = function(upload_url, file, method = "PUT") {
            uploaded <<- list(url = upload_url, file = file, method = method)
            invisible(TRUE)
        },
        .package = "ebx"
    )

    result <- create_dataset(
        csv, name = "Sites", type = "location", destination = "bigquery",
        lat_column = "lat", lon_column = "lon",
        columns = c(name = "String", lat = "Decimal", lon = "Decimal", value = "Decimal")
    )

    expect_identical(captured$name, "Sites")
    expect_identical(captured$type, "location")
    expect_identical(captured$destination, "bigquery")
    expect_identical(captured$files, list(basename(csv)))
    expect_identical(captured$lat_column, "lat")
    expect_identical(captured$lon_column, "lon")
    expect_identical(captured$columns[[1]], list(key = "name", value = "String"))
    expect_length(captured$columns, 4)
    expect_false(any(c("description", "projection") %in% names(captured)))
    expect_identical(uploaded, list(url = "https://upload.example/signed", file = csv, method = "PUT"))
    expect_identical(result$task_id, "user_task")
    expect_true(result$uploaded)
})

test_that("create_dataset can skip upload and validates inputs", {
    csv <- withr::local_tempfile(fileext = ".csv")
    writeLines("name", csv)
    mock_client <- list(post = function(url, payload) {
        list(task_id = "t", upload_method = "PUT", expires_at = "x", uploads = list())
    })
    local_mocked_bindings(
        get_client = function() mock_client,
        upload_dataset_file = function(...) stop("should not upload"),
        .package = "ebx"
    )

    result <- create_dataset(csv, name = "Table", type = "table", upload = FALSE)
    expect_false(result$uploaded)
    expect_error(create_dataset("missing.csv", name = "x"), "existing file")
    expect_error(create_dataset(csv, name = "x", type = "image"), "should be one of")
    expect_error(create_dataset(csv, name = "x", columns = c("String")), "must be named")
    expect_error(create_dataset(csv, name = "x", type = "table"), "Expected one upload URL")
})

test_that("upload_dataset_file sends the file without API credentials", {
    csv <- withr::local_tempfile(fileext = ".csv")
    writeLines(c("name,lat,lon", "A,1,2"), csv)
    seen <- NULL
    httr2::local_mocked_responses(function(req) {
        seen <<- req
        httr2::response(status_code = 200)
    })

    expect_true(upload_dataset_file("https://upload.example/signed", csv, method = "PUT"))
    expect_identical(seen$method, "PUT")
    expect_identical(seen$url, "https://upload.example/signed")
    expect_identical(seen$body$content_type, "text/csv")
    expect_null(seen$headers$Authorization)
})
