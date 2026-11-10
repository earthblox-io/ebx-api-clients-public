# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title Get Task
#' @description Get the status of an asynchronous task, such as dataset upload
#'   ingestion or an export
#' @param task_id The task ID, e.g. \code{create_dataset()$task_id}
#' @return The task record, including \code{status} (\code{PENDING},
#'   \code{RUNNING}, \code{CANCELLING}, \code{SUCCEEDED}, \code{FAILED} or
#'   \code{CANCELLED}), \code{error} when failed, and \code{dataset_id} when an
#'   upload has succeeded
#' @export
get_task <- function(task_id) {
    response <- get_client()$get(paste0("/tasks/", task_id))
    response$data
}

#' @title Follow Task
#' @description Poll a task until it reaches \code{SUCCEEDED}, \code{FAILED}
#'   or \code{CANCELLED}
#' @param task_id The task ID
#' @param interval Polling interval in seconds (default: 10)
#' @param max_attempts Maximum number of polling attempts (default: 60)
#' @return The final task record. A failed or cancelled task is returned, not
#'   raised; check \code{status} and \code{error}.
#' @export
follow_task <- function(task_id, interval = 10, max_attempts = 60) {
    if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
        is.na(max_attempts) || !is.finite(max_attempts) ||
        max_attempts < 1 || max_attempts != floor(max_attempts)) {
        stop("max_attempts must be a positive finite integer")
    }
    if (!is.numeric(interval) || length(interval) != 1L || is.na(interval) ||
        !is.finite(interval) || interval < 0) {
        stop("interval must be a non-negative finite number")
    }
    for (i in seq_len(max_attempts)) {
        task <- get_task(task_id)
        if (task$status %in% c("SUCCEEDED", "FAILED", "CANCELLED")) {
            return(task)
        }
        if (i < max_attempts) {
            Sys.sleep(interval)
        }
    }
    stop(sprintf("Task %s did not finish within %d attempts", task_id, max_attempts))
}
