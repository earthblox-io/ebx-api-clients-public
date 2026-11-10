# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title Print JSON
#' @description Pretty-print an R object (such as the data structure returned
#'   by an API endpoint) as formatted JSON. This is a convenience wrapper
#'   around \code{jsonlite::toJSON()} and \code{jsonlite::prettify()} for
#'   objects that are otherwise difficult to read via \code{print()} or
#'   \code{str()}, such as the nested lists returned by monitoring endpoints.
#' @param x The object to convert, typically a list returned by an API endpoint
#' @param pretty Whether to pretty-print the JSON (default: TRUE)
#' @param ... Additional arguments passed to \code{jsonlite::toJSON}
#' @return The JSON string, invisibly
#' @examples
#' \dontrun{
#' print_json(ebx::get_monitor_results(monitor_id))
#' }
#' @export
print_json <- function(x, pretty = TRUE, ...) {
  json_str <- to_json(x, pretty = pretty, ...)
  cat(json_str, "\n")
  invisible(json_str)
}

#' @title Convert to JSON
#' @description Convert an R object (such as the data structure returned by an
#'   API endpoint) to a JSON string, without printing it. Use this when you
#'   want the JSON string itself (e.g. to write to a file or pass elsewhere)
#'   rather than printing it to the console; see \code{\link{print_json}} for
#'   the printing variant.
#' @param x The object to convert, typically a list returned by an API endpoint
#' @param pretty Whether to pretty-print the JSON (default: TRUE)
#' @param ... Additional arguments passed to \code{jsonlite::toJSON}
#' @return A JSON string
#' @importFrom jsonlite toJSON prettify
#' @export
to_json <- function(x, pretty = TRUE, ...) {
  json_str <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", ...)
  if (pretty) {
    json_str <- jsonlite::prettify(json_str)
  }
  as.character(json_str)
}
