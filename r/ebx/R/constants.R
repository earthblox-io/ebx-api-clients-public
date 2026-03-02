# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

# Internal constants — not exported, no Rd pages generated.

#' @keywords internal
BASE_URL <- "https://api.earthblox.io"

#' @keywords internal
VERSION <- "v1beta"

#' @keywords internal
OAUTH_PATH <- "/services/oauth/token/"

#' @keywords internal
CLIENT_REGISTRATION_PATH <- "/services/auth/client/"

#' @keywords internal
API_PREFIX <- paste0("/", VERSION)

#' @keywords internal
API_SECRETS_FILE <- ".ebx.auth.json"

#' @keywords internal
#' Uses tools::R_user_dir() to comply with CRAN policy (requires R >= 4.0.0).
#' Falls back to tempdir() on hosted environments (e.g. shinyapps.io) where
#' the XDG base directory ancestor may not be writable.
API_SECRETS_PATH <- local({
  p <- tools::R_user_dir("ebx", which = "data")
  # Walk up to the nearest existing ancestor, then test writability by
  # actually creating and immediately deleting a temp file. file.access() is
  # documented to be unreliable (e.g. with ACLs), so a write test is safer.
  root <- p
  while (nchar(root) > 1 && !dir.exists(root)) root <- dirname(root)
  writable <- tryCatch({
    tf <- tempfile(tmpdir = root)
    file.create(tf)
    file.remove(tf)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  path <- if (writable) p else file.path(tempdir(), "ebx")
  message("ebx: using secrets path: ", path)
  path
})

#' @keywords internal
API_TOKEN_FILE <- ".ebx.token.json"
