# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

# A minimal authenticator whose has_expired() always reports FALSE (as a
# locally cached, not-yet-expired token would), but whose token value
# changes each time refresh() is called, so we can detect a retry.
FakeStaleAuthentication <- R6::R6Class(
  "FakeStaleAuthentication",
  inherit = AbstractAuthentication,
  public = list(
    refresh_count = 0,
    initialize = function(config) {
      self$config <- config
    },
    has_expired = function() FALSE,
    refresh = function() {
      self$refresh_count <- self$refresh_count + 1
      self
    },
    get_headers = function() {
      c(Authorization = paste("Bearer token", self$refresh_count))
    }
  )
)

test_that("request retries once after a 401 and succeeds with a refreshed token", {
  config <- ClientConfig$new()
  authenticator <- FakeStaleAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)

  attempt <- 0
  local_mocked_bindings(
    req_perform = function(req) {
      attempt <<- attempt + 1
      if (attempt == 1) {
        resp <- httr2::response(status_code = 401, url = req$url)
        cnd <- rlang::catch_cnd(
          rlang::abort(
            "HTTP 401 Unauthorized.",
            status = 401,
            resp = resp,
            request = req,
            class = c("httr2_http_401", "httr2_http", "httr2_error", "rlang_error")
          )
        )
        stop(cnd)
      }
      httr2::response_json(
        url = req$url,
        headers = list(`content-type` = "application/json"),
        body = list(ok = TRUE)
      )
    },
    .package = "httr2"
  )

  result <- client$get("/runs/abc123")

  expect_identical(result, list(ok = TRUE))
  expect_equal(attempt, 2)
  expect_equal(authenticator$refresh_count, 1)
})

test_that("request does not retry more than once if the refreshed token is still rejected", {
  config <- ClientConfig$new()
  authenticator <- FakeStaleAuthentication$new(config)
  client <- EbxClient$new(authenticator = authenticator, config = config)

  local_mocked_bindings(
    req_perform = function(req) {
      resp <- httr2::response(status_code = 401, url = req$url)
      cnd <- rlang::catch_cnd(
        rlang::abort(
          "HTTP 401 Unauthorized.",
          status = 401,
          resp = resp,
          request = req,
          class = c("httr2_http_401", "httr2_http", "httr2_error", "rlang_error")
        )
      )
      stop(cnd)
    },
    .package = "httr2"
  )

  expect_error(client$get("/runs/abc123"), class = "httr2_http_401")
  expect_equal(authenticator$refresh_count, 1)
})
