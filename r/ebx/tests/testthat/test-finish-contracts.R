# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

test_that("create_run preserves explicit nulls, omitted fields, and arrays", {
    captured <- NULL
    local_mocked_bindings(
        get_client = function() list(post = function(url, payload) {
            captured <<- payload
            list(data = list(run_id = "run-example"))
        }),
        .package = "ebx"
    )
    spec <- list(
        project_id = "workflow-example", include_geometry = NULL,
        generate_thumbnails = NULL, batch_mode = TRUE,
        values = list(), nullable = NULL,
        variables = list(list(key = "number", type = "number", value = 2))
    )
    create_run(project_spec = spec)
    expect_identical(captured[names(spec)], spec)
    expect_false("not_supplied" %in% names(captured))
    create_run(project_id = "workflow-example",
               include_geometry = NULL, generate_thumbnails = NULL)
    expect_true(all(c("include_geometry", "generate_thumbnails") %in% names(captured)))
    expect_null(captured$include_geometry)
    expect_null(captured$generate_thumbnails)
    expect_error(create_run(project_id = "x", include_geometry = NA), "boolean")
})

test_that("false project filters remain supplied", {
    local_mocked_bindings(
        get_client = function() list(get = function(url, query_params) {
            expect_identical(query_params, list(hasMetric = FALSE, analyticsEnabled = FALSE))
            list(data = list())
        }),
        .package = "ebx"
    )
    expect_identical(list_projects(has_metric = FALSE, analytics_enabled = FALSE), list())
})

test_that("named token replacement and printing never expose user tokens", {
    store <- ebx:::.ebx_clients
    previous <- as.list(store, all.names = TRUE)
    withr::defer({
        rm(list = ls(store, all.names = TRUE), envir = store)
        list2env(previous, envir = store)
    })
    old <- auth_using_token("old-sensitive-token", name = "replacement")
    replacement <- auth_using_token("new-sensitive-token", name = "replacement")
    expect_identical(get_client("replacement"), replacement)
    expect_identical(get_client(), replacement)
    expect_identical(replacement$authenticator$get_headers(),
                     c(Authorization = "Bearer new-sensitive-token"))
    for (object in list(old, replacement, old$authenticator, replacement$authenticator)) {
        printed <- paste(capture.output(print(object)), collapse = "\n")
        expect_false(grepl("sensitive-token", printed, fixed = TRUE))
    }
    expect_null(replacement$authenticator$auth_token)
})

test_that("polling is bounded for unknown statuses and never sleeps after terminal", {
    sleeps <- 0L
    calls <- 0L
    current_status <- "unknown"
    local_mocked_bindings(
        get_run = function(run_id) {
            calls <<- calls + 1L
            Run$new(id = run_id, status = current_status)
        },
        .package = "ebx"
    )
    local_mocked_bindings(
        Sys.sleep = function(interval) sleeps <<- sleeps + 1L,
        .package = "base"
    )
    expect_error(follow_run("run-example", interval = 0, max_attempts = 3), "3 attempts")
    expect_equal(calls, 3L)
    expect_equal(sleeps, 2L)
    for (status in c("completed", "error", "cancelled", "failed")) {
        current_status <- status
        sleeps <- 0L
        expect_equal(follow_run("run-example", interval = 0)$status, status)
        expect_equal(sleeps, 0L)
    }
    expect_error(follow_run("x", max_attempts = Inf), "finite integer")
    expect_error(follow_run("x", max_attempts = 0), "finite integer")
})

test_that("a rejected user token fails clearly without automatic refresh", {
    httr2::local_mocked_responses(list(httr2::response(
        status_code = 401,
        headers = list(`content-type` = "application/json"),
        body = charToRaw('{"detail":"Token expired"}')
    )))
    client <- EbxClient$new(
        authenticator = UserTokenAuthentication$new(ClientConfig$new(), "expired-example"),
        config = ClientConfig$new()
    )
    expect_error(client$get("/monitors"), "401")
})
