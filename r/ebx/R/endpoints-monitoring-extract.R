# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

# Convenience accessors that abstract away extracting and shaping the
# locations/observations records nested inside get_monitor_results(), so
# callers do not need to hand-write the record-to-data.frame wrangling.

#' @title Flatten monitor value records to a data frame
#' @description Internal helper that converts a list of records, each with a
#'   \code{values} list, into a data frame with one row per record and one
#'   column per value key. Non-scalar (list) or missing values become
#'   \code{NA}.
#' @param records A list of records (e.g. \code{results$locations} or
#'   \code{results$observations})
#' @param id_cols The record field names to carry through as identifier columns
#' @return A data frame with the identifier columns followed by one column
#'   per value key; an empty data frame if \code{records} is empty
#' @keywords internal
.monitor_values_df <- function(records, id_cols) {
  if (is.null(records) || length(records) == 0) {
    return(data.frame())
  }

  rows <- lapply(records, function(r) {
    vals <- lapply(r$values, function(v) if (is.null(v) || is.list(v)) NA else v)
    data.frame(r[id_cols], vals, check.names = FALSE, stringsAsFactors = FALSE)
  })

  cols <- unique(unlist(lapply(rows, names)))
  do.call(rbind, lapply(rows, function(d) {
    d[setdiff(cols, names(d))] <- NA
    d[cols]
  }))
}

#' @title Get Monitor Locations
#' @description Fetch monitor results and extract the locations as a data
#'   frame, abstracting away the parsing of the nested \code{values} records
#'   returned by \code{\link{get_monitor_results}}.
#' @param monitor_id The monitor ID
#' @param cycle_limit Maximum number of cycles included in the underlying results
#' @return A data frame with one row per location, an \code{id} column, and
#'   one column per location value field
#' @export
get_monitor_locations <- function(monitor_id, cycle_limit = 10) {
  results <- get_monitor_results(monitor_id, cycle_limit = cycle_limit)
  .monitor_values_df(results$locations, "id")
}

#' @title Get Monitor Observations
#' @description Fetch monitor results and extract the observations as a data
#'   frame, abstracting away the parsing of the nested \code{values} records
#'   returned by \code{\link{get_monitor_results}}.
#' @param monitor_id The monitor ID
#' @param cycle_limit Maximum number of cycles included in the underlying results
#' @return A data frame with one row per observation, \code{location_id},
#'   \code{cycle_id}, and \code{monitoring_end_date} columns, and one column
#'   per observed metric
#' @export
get_monitor_observations <- function(monitor_id, cycle_limit = 10) {
  results <- get_monitor_results(monitor_id, cycle_limit = cycle_limit)
  .monitor_values_df(
    results$observations, c("location_id", "cycle_id", "monitoring_end_date")
  )
}

#' @title Get Monitor Table
#' @description Fetch monitor results and build a single, analysis-ready
#'   table combining location attributes with their observed metric values.
#'   This abstracts away extracting \code{locations} and \code{observations}
#'   from \code{\link{get_monitor_results}}, flattening their \code{values}
#'   records, merging them, and sorting by monitoring end date.
#' @param monitor_id The monitor ID
#' @param cycle_limit Maximum number of cycles included in the underlying results
#' @param location_cols Optional character vector of location value columns to
#'   include (default: all location value columns present in the results)
#' @return A data frame with an \code{end_date} column, the requested
#'   \code{location_cols}, one column per metric field, and a \code{cycle_id}
#'   column, sorted by \code{end_date} (and the first metric column) with the
#'   most recent observations first
#' @export
get_monitor_table <- function(monitor_id, cycle_limit = 10, location_cols = NULL) {
  results <- get_monitor_results(monitor_id, cycle_limit = cycle_limit)

  locations_df <- .monitor_values_df(results$locations, "id")
  observations_df <- .monitor_values_df(
    results$observations, c("location_id", "cycle_id", "monitoring_end_date")
  )

  if (is.null(location_cols)) {
    location_cols <- setdiff(names(locations_df), "id")
  }

  metric_fields <- Filter(function(f) identical(f$source, "metric"), results$fields)
  metric_keys <- vapply(metric_fields, `[[`, character(1), "key")

  monitor_table <- merge(
    locations_df[, c("id", location_cols), drop = FALSE],
    observations_df[, c("location_id", "cycle_id", "monitoring_end_date", metric_keys), drop = FALSE],
    by.x = "id", by.y = "location_id"
  )

  monitor_table$end_date <- as.Date(substr(monitor_table$monitoring_end_date, 1, 10))

  sort_keys <- list(monitor_table$end_date)
  if (length(metric_keys) > 0) {
    sort_keys <- c(sort_keys, list(monitor_table[[metric_keys[1]]]))
  }
  monitor_table <- monitor_table[
    do.call(order, c(sort_keys, list(decreasing = TRUE, method = "radix"))),
  ]

  monitor_table[, c("end_date", location_cols, metric_keys, "cycle_id")]
}
