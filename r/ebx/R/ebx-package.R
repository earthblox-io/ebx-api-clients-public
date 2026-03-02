# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title ebx: Earth Blox API Client for R
#'
#' @description
#' R client library for the Earth Blox API. Provides authentication
#' and endpoints for interacting with Earth Blox geospatial analytics services.
#' Compatible with Shiny for R applications.
#'
#' @section Authentication:
#' The package supports three authentication methods:
#' \itemize{
#'   \item \code{auth_using_env()}: Use EBX_API_TOKEN environment variable
#'   \item \code{auth_using_oauth()}: Use OAuth client credentials directly
#'   \item \code{auth_using_creds()}: Use saved OAuth credentials from disk
#' }
#'
#' @section Client Registration:
#' To register a new OAuth client:
#' \code{
#'   oauth_client <- create_oauth_client(email, password, name)
#'   oauth_client$save()
#' }
#'
#' @section API Endpoints:
#' Available endpoints include:
#' \itemize{
#'   \item \code{list_projects()}, \code{get_project()}
#'   \item \code{list_runs()}, \code{get_run()}, \code{create_run()}
#'   \item \code{get_charts()}, \code{get_tables()}, \code{get_layers()}
#' }
#'
#' @keywords internal
"_PACKAGE"
