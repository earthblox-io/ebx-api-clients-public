# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

library(testthat)
library(ebx)

test_that("create_oauth_client function exists with correct signature", {
  # Verify the function exists
  expect_true(exists("create_oauth_client", where = "package:ebx"))
  
  # Get the function for examination
  fn <- get("create_oauth_client", envir = asNamespace("ebx"))
  fn_args <- names(formals(fn))
  
  # Check all required parameters exist
  expect_true("email" %in% fn_args)
  expect_true("password" %in% fn_args)
  expect_true("name" %in% fn_args)
  expect_true("description" %in% fn_args)
  expect_true("scopes" %in% fn_args)
  
  # Check default values
  expect_equal(formals(fn)$description, "")
  # scopes default is c() which is a call object
  expect_true(!is.null(formals(fn)$scopes))
})

test_that("create_oauth_client requires all mandatory parameters", {
  # Test that function requires necessary parameters
  expect_error(
    create_oauth_client(),
    class = "error"
  )
  
  expect_error(
    create_oauth_client(email = "test@example.com"),
    class = "error"
  )
  
  expect_error(
    create_oauth_client(email = "test@example.com", password = "pass"),
    class = "error"
  )
})

# Note: Full integration tests for create_oauth_client would require either:
# 1. A mock HTTP server (e.g., using httptest2 package)
# 2. Live API credentials (not suitable for unit tests)
# 3. More complex dependency injection patterns
# 
# The function is tested indirectly through:
# - Manual testing with real credentials
# - Integration tests in a separate test suite
# - The OAuthClient model tests verify the return type structure
