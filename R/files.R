#' Upload files to OSF via a specified OSF ID.
#'
#' @param id An OSF id.
#'
#' @param path The path to the file to be uploaded.
#'
#' @param name The name the file should have on OSF. Defaults to the name of the file in the filepath.
#' @export upload_files

upload_files <- function(id, path, name = NULL) {
  if (is.null(name)) {
    name <- basename(path)
  }
  url_file <- sprintf("https://files.osf.io/v1/resources/%s/providers/osfstorage/?kind=file&name=%s", id, name)
  req <- httr::PUT(url_file,
                   config = get_config(),
                   body = httr::upload_file(path))
  if (req$status_code == "201") {
    cat("Upload successful")
  } else {
    cat(sprintf("Upload unsuccessful. Returned code: %s.", req$status_code))
  }
}


#' Upload revised files to OSF via a specified OSF ID.
#'
#' @export upload_revisions

upload_revisions <- function() {

}


#' Download files from OSF via a specified file ID.
#'
#' @param id An OSF id.
#'
#' @param path The path to which the file should be downloaded.
#'
#' @export download_files

download_files <- function(id, path) {
  url_file <- sprintf("https://osf.io/download/%s/", id)
  
  req <- httr::GET(url_file,
                   config = get_config(),
                   httr::write_disk(path, overwrite = TRUE))

  if (req$status_code == "200") {
    cat("Download successful")
  } else {
    cat(sprintf("Download unsuccessful. Returned code: %s.", req$status_code))
  }
}


#' Move files from one location to another in OSF using a parent node id.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)
#'
#' @param year The current year component or year component to which file moving is desired
#'
#' @param from The file number from which the user wishes to begin. Defaults to the first file in the data folder.
#'
#' @param to The file number that we want to end the file moving process on. Defaults to the last file in the data folder. Useful to change if warnings indicate files that failed to upload.
#' @export move_to_projects

# NEED TO:
# 1. Add a "name" variable to allow user to upload a file by name/project/whatever
move_to_projects <- function(id = "judwb", year, from = 1, to = 0) {
  cat("Setting up. Please wait (This could take a couple minutes).\n")
  # Get current year's component, then get the project components (children) dictionary of that component
  all_child_components <- suppressMessages(get_dictionary(id = id, type = "children"))
  working_child_id <- all_child_components[[year]]
  project_components <- suppressMessages(get_dictionary(id = working_child_id, type = "children"))
  # NEED A WAY TO GET DELETE LINKS TO PASS TO MOVE FILES
  # NEED TO get_nodes ON TO COMPONENT ID
  # Get dictionary of files for current year
  data_component_id <- all_child_components[["Data"]]
  raw_data_component <- suppressMessages(get_dictionary(id = data_component_id, type = "children"))
  raw_data_component_id <- raw_data_component[["Raw_Data"]]
  files_dict <- suppressMessages(get_dictionary(id = raw_data_component_id, type = "files"))
  current_files <- vector(mode = "list")
  for (i in names(files_dict)) {
    if (stringr::str_detect(i, year)) {
      current_files[[i]] <- files_dict[[i]]
    }
  }
  len <- length(current_files)
  cur <- from

  if (to > 0) {
    len_cur <- c(from:to)
  } else {
    len_cur <- c(from:len)
  }

  # Move files
  for (j in names(project_components)) {
    for (k in len_cur) {
      l <- names(current_files[k])
      if (stringr::str_detect(tolower(l),
                              tolower(stringr::str_replace_all(j,
                                                               "[^[:alnum:]]",
                                                               "")))) {
        cat(l, "in project", j, ":", "\n")
        # HERE
        # Might need to issue GET requests to grab info on the files in the project components.
        proj_current <- suppressMessages(get_dictionary(project_components[[j]], type = "files"))
        for (m in names(proj_current)) {
          if (m %in% c("Data", "Journal", "Pictures")) {
            req <- httr::GET(proj_current[[m]], config = get_config())
            res <- rjson::fromJSON(httr::content(req, 'text', encoding = "UTF-8"))
            for (o in seq_along(res$data)) {
              if (!is.null(res$data[[o]]$attributes$name)) {
                if (l == res$data[[o]]$attributes$name) {
                  httr::DELETE(res$data[[o]]$links$delete, config = get_config())
                  cat("FILE: ", l, " DELETED\n")
                }
              }
            }
          }
        }

        folders_dict <- get_dictionary(id = project_components[[j]], type = "folders")
        if (stringr::str_detect(l, ".csv")) {
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Data",
               file_num = k)
        } else if (stringr::str_detect(l, ".txt")) {
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Journal",
               file_num = k)
        } else {
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Pictures",
               file_num = k)
        }
        cat(cur, "/", len, "\n")
        cur <- cur + 1
      }
    }
  }
}


#' Download all data files from each project (in "Data" folder) in main OSF project.
#'
#' @param download_local Logical. If TRUE, files will be downloaded to specified local directory.
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @param path Path to download files. Defaults to working directory.
#'
#' @return Nothing. If download_local == FALSE, then saves list of dataframes to global environment.
#'
#' @export download_all

download_all <- function(download_local = FALSE, id = "judwb", path = getwd()) {
  links <- get_all_file_links(id)

  if (download_local == TRUE) {
    j <- 1

    message("Downloading files:")
    for (i in 1:length(links$link)) {
      f_path <- paste0(path, "/", links$name[[i]])
      httr::GET(links$link[[i]], config = get_config(),
                httr::write_disk(f_path, overwrite = TRUE))

      cat(j, "/", length(links$link), "\n")
      j <- j + 1
    }
  } else {
    j <- 1

    tmp <- tempdir()
    df_list <- vector(mode = "list")

    message("Reading data:")
    for (i in 1:length(links$link)) {
      f_path <- paste0(tmp, "/", links$name[[i]])
      httr::GET(links$link[[i]], config = get_config(),
                httr::write_disk(f_path, overwrite = TRUE))

      tryCatch({
        if (stringr::str_detect(links$name[[i]], ".csv")) {
          # Read the file to unique name
          f_name <- paste0(
            gsub(".csv", "", links$name[[i]]),
            "_YEAR=",
            links$year[[i]],
            "_PROJECT=",
            links$project[[i]]
          )
          df_list[[f_name]] <- suppressMessages(readr::read_csv(f_path))
        } else if (stringr::str_detect(links$name[[i]], ".xlsx")) {
          # Read the file to unique name
          f_name <- paste0(
            gsub(".xlsx", "", links$name[[i]]),
            "_YEAR=",
            links$year[[i]],
            "_PROJECT=",
            links$project[[i]]
          )
          df_list[[f_name]] <- readxl::read_xlsx(f_path)
        } else if (stringr::str_detect(links$name[[i]], ".xls")) {
          f_name <- paste0(
            gsub(".xls", "", links$name[[i]]),
            "_YEAR=",
            links$year[[i]],
            "_PROJECT=",
            links$project[[i]]
          )
          df_list[[f_name]] <- readxl::read_xls(f_path)
        } else {
          message(paste("File",
                        links$name[[i]],
                        "not read in due to unused file extension."))
        }
      }, error = function(e){})
      cat(j, "/", length(links$link), "\n")
      j <- j + 1
    }

    osfr_list_data <<- df_list
  }

}


#' Move a file that is untouched (has no id) from one component or folder to another component or folder
#'
#' @param from A file path beginning with the deepest level component id to the name of the file itself ("XXXX/foldername/filename")
#'
#' @param from A file path beginning with the deepest level component id to the name of the destination folder itself ("XXXX/foldername/subfoldername") or simply a component id
#'
#' @export move_waterbutler
move_waterbutler <- function(from, to) {
  from_list <- as.vector(strsplit(from, "/")[[1]], mode = "list")
  from_url <- get_node_url(from_list)
  file_name <- from_list[[length(from_list)]]

  to_list <- as.vector(strsplit(to, "/")[[1]], mode = "list")
  to_url <- get_node_url(to_list)

  path <- paste0(tempdir(), "/", file_name)
  to_folder_url <- paste0(to_url, "?kind=file&name=", file_name)

  httr::GET(from_url, config = get_config(), httr::write_disk(path, overwrite = TRUE))
  httr::PUT(to_folder_url, config = get_config(), body = httr::upload_file(path))
}
