#' Get a data frame of csv files within a project component or folder
#'
#' @description This function uses a project component id or WaterButler api link to return a dataframe of all csv files within the data folder of the project.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the project node
#'
#' @param api_link WaterButler link to use in place of id if no id exists
#'
#' @return A dataframe object
#'
#' @export get_data

get_data <- function(id = NULL, api_link = NULL) {
  if (!is.null(id)) {
    node <- get_nodes(id, files = TRUE, private = TRUE)
    api_link <- node$data[[1]]$relationships$files$links$related$href
  }

  call <- httr::GET(api_link, config = get_config(TRUE))
  res <- rjson::fromJSON(httr::content(call, 'text', encoding = "UTF-8"))

  dat <- data.frame()
  tmp <- tempdir()

  for (i in seq_along(res$data)) {
    dir <- paste0(tmp, "/", res$data[[i]]$attributes$name)
    link <- res$data[[i]]$links$download
    httr::GET(link, config = get_config(TRUE), httr::write_disk(dir, overwrite = TRUE))
    new_dat <- readr::read_csv(dir)
    dat <- rbind(dat, new_dat)
  }

  return(dat)
}
