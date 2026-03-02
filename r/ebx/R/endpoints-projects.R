# Copyright (c) 2026 Quosient Ltd.
# SPDX-License-Identifier: MIT

#' @title List Projects
#' @description List all available projects
#' @return A list of Project objects
#' @export
list_projects <- function() {
  client <- get_client()
  response <- client$get("/projects/")
  
  # API returns {data: [{project1}, {project2}, ...]}
  projects_data <- response$data
  
  if (is.null(projects_data)) {
    return(list())
  }
  
  lapply(projects_data, function(proj) {
    do.call(Project$new, proj)
  })
}

#' @title Get Project
#' @description Get a specific project by ID
#' @param project_id The project ID
#' @return A Project object
#' @export
get_project <- function(project_id) {
  client <- get_client()
  response <- client$get(paste0("/projects/", project_id))
  
  # API returns {data: {project fields}}
  project_data <- response$data
  
  if (is.null(project_data)) {
    stop("No project data returned from API")
  }
  
  # Add the project_id since API doesn't return it
  project_data$id <- project_id
  
  # Create Project object (initialize will map 'title' to 'name')
  do.call(Project$new, project_data)
}
