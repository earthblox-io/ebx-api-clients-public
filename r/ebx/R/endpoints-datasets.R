# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title List Datasets
#' @description List the user and organisation datasets available to the
#'   authenticated user
#' @param type Optional dataset type filter, e.g. \code{"FeatureCollection"}
#' @return A list of dataset records
#' @export
list_datasets <- function(type = NULL) {
    query_params <- if (is.null(type)) NULL else list(type = type)
    response <- get_client()$get("/datasets/", query_params = query_params)
    response$data
}

#' @title Get Dataset
#' @description Get a dataset record by ID
#' @param dataset_id The dataset ID
#' @return The dataset record
#' @export
get_dataset <- function(dataset_id) {
    response <- get_client()$get(paste0("/datasets/", dataset_id))
    response$data
}

#' @title Update Dataset
#' @description Update a dataset's details, or move it between the user's own
#'   datasets and the organisation's shared datasets. Only supplied fields are
#'   changed.
#' @param dataset_id The dataset ID
#' @param title,description,short_description,license,provider,provider_url
#'   Optional replacement values for the dataset's descriptive fields
#' @param location Optional target location: \code{"user"} (private to the
#'   uploading user) or \code{"organisation"} (shared with the organisation)
#' @param permission Organisation permission: \code{"viewable"} or
#'   \code{"editable"}. Required when moving to \code{"organisation"}; not
#'   allowed for \code{"user"} datasets, which are always editable.
#' @param cadence,cadence_interval Optional temporal cadence unit and interval
#' @return The updated dataset record
#' @examples
#' \dontrun{
#' # Share an uploaded dataset with the organisation
#' update_dataset("dataset_123", location = "organisation", permission = "viewable")
#' }
#' @export
update_dataset <- function(dataset_id, title = NULL, description = NULL,
                           short_description = NULL, license = NULL,
                           provider = NULL, provider_url = NULL,
                           location = NULL, permission = NULL,
                           cadence = NULL, cadence_interval = NULL) {
    if (!is.null(location)) {
        location <- match.arg(location, c("user", "organisation"))
    }
    if (!is.null(permission)) {
        permission <- match.arg(permission, c("viewable", "editable"))
    }
    if (identical(location, "organisation") && is.null(permission)) {
        stop("permission ('viewable' or 'editable') is required when location is 'organisation'")
    }
    if (identical(location, "user") && !is.null(permission)) {
        stop("permission cannot be set when location is 'user'; user datasets are always editable")
    }

    payload <- list(
        title = title, description = description,
        short_description = short_description, license = license,
        provider = provider, provider_url = provider_url,
        location = location, permission = permission,
        cadence = cadence, cadence_interval = cadence_interval
    )
    payload <- payload[!vapply(payload, is.null, logical(1))]
    if (!length(payload)) {
        stop("Supply at least one field to update")
    }

    response <- get_client()$patch(paste0("/datasets/", dataset_id), payload = payload)
    response$data
}

#' @title Create Dataset
#' @description Create a dataset from a CSV, XLSX or GeoJSON file. The API
#'   returns a short-lived signed upload URL; by default the file is then
#'   uploaded to it, which starts ingestion.
#' @param file Path to the local file to upload
#' @param name Dataset name
#' @param type \code{"location"} for geospatial data or \code{"table"} for
#'   non-geospatial data
#' @param destination For \code{type = "location"}: \code{"bigquery"} or
#'   \code{"earth_engine"}. Must be \code{NULL} for \code{type = "table"}.
#' @param lat_column,lon_column Latitude and longitude column names. Required
#'   for CSV/XLSX location uploads.
#' @param columns Column types as a named character vector or list, e.g.
#'   \code{c(name = "String", lat = "Decimal", lon = "Decimal")}. Allowed types
#'   are \code{String}, \code{Integer}, \code{Decimal} and \code{Date}.
#'   Required for CSV/XLSX location uploads.
#' @param description Optional dataset description
#' @param projection Optional coordinate reference system, e.g.
#'   \code{"EPSG:4326"}
#' @param upload Whether to upload the file to the returned signed URL
#'   (default: TRUE)
#' @return A list with \code{task_id}, \code{upload_method},
#'   \code{expires_at}, \code{uploads} and \code{uploaded}
#' @details Ingestion runs asynchronously after upload. Use
#'   \code{follow_task(result$task_id)} to wait for it; on success the task's
#'   \code{dataset_id} identifies the new dataset. Image uploads are not
#'   supported by this function.
#' @export
create_dataset <- function(file, name, type = c("location", "table"),
                           destination = NULL, lat_column = NULL,
                           lon_column = NULL, columns = NULL,
                           description = NULL, projection = NULL,
                           upload = TRUE) {
    type <- match.arg(type)
    if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
        stop("file must be the path to an existing file")
    }

    payload <- list(name = name, type = type, files = list(basename(file)))
    if (!is.null(description)) payload$description <- description
    if (!is.null(destination)) payload$destination <- destination
    if (!is.null(lat_column)) payload$lat_column <- lat_column
    if (!is.null(lon_column)) payload$lon_column <- lon_column
    if (!is.null(projection)) payload$projection <- projection
    if (!is.null(columns)) payload$columns <- dataset_columns(columns)

    response <- get_client()$post("/datasets/", payload = payload)
    response$uploaded <- FALSE

    if (isTRUE(upload)) {
        if (length(response$uploads) != 1L) {
            stop("Expected one upload URL, received ", length(response$uploads))
        }
        upload_dataset_file(response$uploads[[1]]$upload_url, file,
                            method = response$upload_method)
        response$uploaded <- TRUE
    }
    response
}

# Convert a named vector/list of column types to the API's [{key, value}] form.
dataset_columns <- function(columns) {
    keys <- names(columns)
    if (is.null(keys) || any(!nzchar(keys))) {
        stop("columns must be named, e.g. c(name = \"String\", lat = \"Decimal\")")
    }
    unname(Map(function(key, value) list(key = key, value = value),
               keys, as.character(unlist(columns))))
}

# Upload a file to a signed storage URL. The URL carries its own authorisation,
# so the API client's bearer token must not be sent.
upload_dataset_file <- function(upload_url, file, method = "PUT") {
    extension <- tolower(sub(".*\\.", "", basename(file)))
    content_type <- switch(
        extension,
        csv = "text/csv",
        geojson = "application/geo+json",
        json = "application/json",
        xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        "application/octet-stream"
    )
    req <- httr2::request(upload_url)
    req <- httr2::req_method(req, method)
    req <- httr2::req_body_file(req, file, type = content_type)
    httr2::req_perform(req)
    invisible(TRUE)
}
