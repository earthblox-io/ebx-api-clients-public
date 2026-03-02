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
  root <- p
  while (!dir.exists(root)) root <- dirname(root)
  path <- if (file.access(root, mode = 2) != 0) file.path(tempdir(), "ebx") else p
  message("ebx: using secrets path: ", path)
  path
})

#' @keywords internal
API_TOKEN_FILE <- ".ebx.token.json"
