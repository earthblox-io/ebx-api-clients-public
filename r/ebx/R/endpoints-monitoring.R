# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

# These endpoints return the API's records without model conversion so nested
# locations, observations, nulls, and evolving fields remain intact. Each
# endpoint accepts an `as_json` flag; when TRUE the result is returned as a
# pretty-printed JSON string (via `to_json()`) instead of the raw R list,
# which is often easier to read for deeply nested monitoring payloads.

#' @title List Monitors
#' @description List monitors available to the authenticated user
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return A list of monitor records, or a JSON string when \code{as_json = TRUE}
#' @details Monitoring endpoints require a privileged user API token or server
#'   permissions for one of the roles \code{admin}, \code{superadmin},
#'   \code{customer_support}, \code{creator}, \code{explorer}, or \code{monitor}.
#'   OAuth client-credentials tokens generally do not have these permissions.
#' @export
list_monitors <- function(as_json = FALSE) {
  response <- get_client()$get("/monitors")
  .as_json_if_requested(response$data, as_json)
}

#' @title Get Monitor
#' @description Get a monitor record by ID
#' @param monitor_id The monitor ID
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return The monitor record, or a JSON string when \code{as_json = TRUE}
#' @export
get_monitor <- function(monitor_id, as_json = FALSE) {
  response <- get_client()$get(paste0("/monitors/", monitor_id))
  .as_json_if_requested(response$data, as_json)
}

#' @title List Monitor Cycles
#' @description List cycles belonging to a monitor
#' @param monitor_id The monitor ID
#' @param limit Maximum number of cycles to return
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return A list of monitor-cycle records, or a JSON string when
#'   \code{as_json = TRUE}
#' @export
list_monitor_cycles <- function(monitor_id, limit = 10, as_json = FALSE) {
  response <- get_client()$get(
    paste0("/monitors/", monitor_id, "/cycles"),
    query_params = list(limit = limit)
  )
  .as_json_if_requested(response$data, as_json)
}

#' @title Get Monitor Results
#' @description Get results for a monitor, optionally limiting the cycles returned
#' @param monitor_id The monitor ID
#' @param cycle_limit Maximum number of cycles included in the results
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return The structured monitor-results payload, or a JSON string when
#'   \code{as_json = TRUE}. See \code{\link{get_monitor_locations}},
#'   \code{\link{get_monitor_observations}}, and \code{\link{get_monitor_table}}
#'   for convenience accessors that extract specific parts of this payload as
#'   data frames.
#' @export
get_monitor_results <- function(monitor_id, cycle_limit = 10, as_json = FALSE) {
  response <- get_client()$get(
    paste0("/monitors/", monitor_id, "/results"),
    query_params = list(cycle_limit = cycle_limit)
  )
  .as_json_if_requested(response$data, as_json)
}

#' @title List Schedules
#' @description List schedules available to the authenticated user
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return A list of schedule records, or a JSON string when \code{as_json = TRUE}
#' @export
list_schedules <- function(as_json = FALSE) {
  response <- get_client()$get("/schedules")
  .as_json_if_requested(response$data, as_json)
}

#' @title Get Schedule
#' @description Get a schedule record by ID
#' @param schedule_id The schedule ID
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return The schedule record, or a JSON string when \code{as_json = TRUE}
#' @export
get_schedule <- function(schedule_id, as_json = FALSE) {
  response <- get_client()$get(paste0("/schedules/", schedule_id))
  .as_json_if_requested(response$data, as_json)
}

#' @title List Alerts
#' @description List alerts, optionally filtered by monitor
#' @param monitor_id Optional monitor ID filter
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return A list of alert records, or a JSON string when \code{as_json = TRUE}
#' @export
list_alerts <- function(monitor_id = NULL, as_json = FALSE) {
  query_params <- if (is.null(monitor_id)) NULL else list(monitor_id = monitor_id)
  response <- get_client()$get("/alerts", query_params = query_params)
  .as_json_if_requested(response$data, as_json)
}

#' @title Get Alert
#' @description Get an alert record by ID
#' @param alert_id The alert ID
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return The alert record, or a JSON string when \code{as_json = TRUE}
#' @export
get_alert <- function(alert_id, as_json = FALSE) {
  response <- get_client()$get(paste0("/alerts/", alert_id))
  .as_json_if_requested(response$data, as_json)
}

#' @title Get Monitor Cycle
#' @description Get a monitor-cycle record by ID
#' @param cycle_id The monitor-cycle ID
#' @param as_json Return the result as a pretty-printed JSON string instead
#'   of an R list (default: FALSE)
#' @return The monitor-cycle record, or a JSON string when \code{as_json = TRUE}
#' @export
get_monitor_cycle <- function(cycle_id, as_json = FALSE) {
  response <- get_client()$get(paste0("/monitor-cycles/", cycle_id))
  .as_json_if_requested(response$data, as_json)
}

#' @title Update Alert
#' @description Update an alert's read state, status, or both
#' @param alert_id The alert ID
#' @param is_read Optional read-state boolean
#' @param status Optional status: \code{open}, \code{acknowledged}, or \code{closed}
#' @return The updated alert record
#' @export
update_alert <- function(alert_id, is_read = NULL, status = NULL) {
  if (is.null(is_read) && is.null(status)) {
    stop("Supply at least one of is_read or status")
  }
  if (!is.null(is_read) &&
      (!is.logical(is_read) || length(is_read) != 1L || is.na(is_read))) {
    stop("is_read must be a single non-missing logical value")
  }
  if (!is.null(status) &&
      (!is.character(status) || length(status) != 1L || is.na(status) ||
       !status %in% c("open", "acknowledged", "closed"))) {
    stop("status must be one of 'open', 'acknowledged', or 'closed'")
  }

  payload <- list()
  if (!is.null(is_read)) payload$is_read <- is_read
  if (!is.null(status)) payload$status <- status
  response <- get_client()$patch(
    paste0("/alerts/", alert_id),
    payload = payload
  )
  response$data
}

#' @title Mark All Alerts Read
#' @description Mark all alerts as read, optionally for one monitor
#' @param monitor_id Optional monitor ID filter
#' @return The number of updated alerts
#' @export
mark_all_alerts_read <- function(monitor_id = NULL) {
  query_params <- if (is.null(monitor_id)) NULL else list(monitor_id = monitor_id)
  response <- get_client()$post(
    "/alerts/mark-all-read",
    query_params = query_params
  )
  response$data$updated_count
}

#' @title Convert response data to JSON if requested
#' @description Internal helper shared by monitoring endpoints to optionally
#'   return their result as a pretty-printed JSON string.
#' @param data The R data structure to optionally convert
#' @param as_json Whether to convert \code{data} to a JSON string
#' @return \code{data} unchanged, or its JSON string representation when
#'   \code{as_json} is \code{TRUE}
#' @keywords internal
.as_json_if_requested <- function(data, as_json) {
  if (isTRUE(as_json)) {
    return(to_json(data))
  }
  data
}
