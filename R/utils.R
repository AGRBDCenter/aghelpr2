#' Get a dictionary (vector) of files, folders, or children using the parent node id
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node
#'
#' @param type Type of OSF child ("children", "files", "folders") for which you are obtaining a dictionary
#'
#' @return A dictionary (vector) with values = ids or WaterButler Links if no id available and keys = names of children
#' @export get_dictionary

get_dictionary <- function(id, type) {
  
  node_ids <- vector(mode = "list")
  node_names <- vector(mode = "list")
  
  if (type == "children") {
    node_info <- get_nodes(id = id, children = TRUE)
    
    for (i in seq_along(node_info$data)) {
      node_ids[[i]] <- node_info$data[[i]]$id
      node_names[[i]] <- node_info$data[[i]]$attributes$title
    }
    
  } else if (type == "files" || type == "folders") {
    # THIS MAY NEED TO BE CHANGED TO get_file_info
    node_info <- get_nodes(id = id, files = TRUE)
    
    for (i in seq_along(node_info$data)) {
      node_ids[[i]] <- node_info$data[[i]]$links$move
      node_names[[i]] <- node_info$data[[i]]$attributes$name
    }
    
  } else {
    stop('Supported types are "children", "files", and "folders".')
  }
  
  # Generate dictionary with 'values = component ids' and 'keys = component names'
  names(node_ids) <- node_names
  
  return(node_ids)
}


#' Move a file from one location to another using WaterButler links.
#'
#' @param files A dictionary of key = file names and value = Waterbutler link
#'
#' @param folders A dictionary of key = folder names and value = Waterbutler link
#'
#' @param file_name Name of the file to be moved
#'
#' @param folder_name Name of the folder that gets the moved file
#'
#' @param file_num The number of the file for use when a warning displays a file that has not correctly been pushed to OSF.
#' @export move

move <- function(files, folders, file_name, folder_name, file_num) {
  path <- paste0(tempdir(), "/", file_name)
  to_folder_url <- paste0(folders[[folder_name]], "?kind=file&name=", file_name)
  
  
  
  get_req <- httr::GET(files[[file_name]], config = get_config(), httr::write_disk(path, overwrite = TRUE))
  cat("GET Code:", get_req$status_code, "\n")
  put_req <- httr::PUT(to_folder_url, config = get_config(), body = httr::upload_file(path))
  cat("PUT Code:", put_req$status_code, "\n")
  if (put_req$status_code != "201") {
    warning(paste0("PUT code for file ",
                   file_name,
                   " returned ",
                   put_req$status_code,
                   crayon::green(".\n FILE NUMBER: "),
                   crayon::green$bold(file_num)))
  }
}


#' Get all file links from project
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @export get_all_file_links

get_all_file_links <- function(id = "judwb") {
  file_links <- data.frame(link = rep(NA, 20000),
                           name = NA,
                           year = NA,
                           project = NA)
  
  message("Building dictionary")
  if (exists("get_file_dict", envir = .GlobalEnv)) {
    get_file_dict <- get("get_file_dict", envir = .GlobalEnv)
  } else {
    get_file_dict <- suppressMessages(aghelpR::concoct_directory(id))
    get_file_dict <<- get_file_dict
  }
  
  o <- 1
  message("Issuing requests:")
  for (i in seq_along(get_file_dict$data)) {
    for (j in get_file_dict$data[[i]]$attributes$title) {
      for (k in seq_along(get_file_dict$components[[j]]$data)) {
        for (l in get_file_dict$components[[j]]$data[[k]]$attributes$title) {
          # Files stored differently in different years ISSUE WITH 2016
          if (j %in% c("2014", "2015", "2017")) {
            for (m in seq_along(get_file_dict$components[[j]]$files[[l]]$data)) {
              if (!is.null(get_file_dict$components[[j]]$files[[l]]$data[[m]]$links$download)) {
                file_links$link[o] <- get_file_dict$components[[j]]$files[[l]]$data[[m]]$links$download
                file_links$name[o] <- gsub(" ", "_",
                                           get_file_dict$components[[j]]$files[[l]]$data[[m]]$attributes$name)
                file_links$year[o] <- j
                file_links$project[o] <- gsub(" ", "_", l)
                message(o)
                o <- o + 1
              }
            }
          } else {
            for (name in get_file_dict$components[[j]]$files[[l]]$data) {
              if (stringr::str_detect(name$attributes$name, "Data")) {
                file_url <- get_file_dict$components[[j]]$files[[l]]$data[[1]]$relationships$files$links$related$href
                
                if (!is.null(file_url)) {
                  req <- httr::GET(file_url, config = get_config())
                  res <- rjson::fromJSON(httr::content(req, 'text', encoding = "UTF-8"))
                  for (m in seq_along(res$data)) {
                    file_links$link[o] <- res$data[[m]]$links$download
                    file_links$name[o] <- gsub(" ", "_", res$data[[m]]$attributes$name)
                    file_links$year[o] <- j
                    file_links$project[o] <- gsub(" ", "_", l)
                    message(o)
                    o <- o + 1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  # 1-174 works 175, 176 bad: fixed with empty error catcher in download_all
  file_links <- subset(file_links, !is.na(file_links$link))
  file_links <- file_links
  return(file_links)
}


#' Get a directory of all nodes, files, and folders within a parent node
#'
#' @description This function is recursive and can take a long time. Please plan accordingly.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node
#'
#' @return A vector with parent node information and options to look at component or file information using extensions e.g. origin$files$ComponentName will return an rjson style vector of all data on files within ComponentName whereas origin$components$ComponentName will return information on all components within ComponentName
#'
#' @export concoct_directory

concoct_directory <- function(id) {
  
  children <- suppressMessages(get_nodes(id = id,
                                         children = TRUE))
  
  for (i in seq_along(children$data)) {
    if (length(children$data) != 0) {
      index <- children$data[[i]]$attributes$title
      children[["components"]][[index]] <- suppressMessages(
        get_nodes(children$data[[i]]$id,
                  children = TRUE)
      )
      
      # This works but not quite how we want
      children[["files"]][[index]] <- suppressMessages(
        get_nodes(children$data[[i]]$id,
                  files = TRUE)
      )
      
      children[["components"]][[index]] <- concoct_directory(children$data[[i]]$id)
      
      cat(".")
    }
  }
  
  return(children)
}


#' Get url at the end of a file path. Helper function for move_waterbutler()
#'
#' @param list A vector of a file path
#'
#' @export get_node_url

get_node_url <- function(list) {
  for (i in (length(list) - 1)) {
    if (i == 1) {
      comp_dict <- get_dictionary(list[[1]], type = "files")
    } else {
      call <- httr::GET(url = comp_dict[[list[[i]]]], config = get_config())
      res <- rjson::fromJSON(httr::content(call, 'text', encoding = "UTF-8"))
      node_ids <- vector(mode = "list")
      node_names <- vector(mode = "list")
      for (j in seq_along(res$data)) {
        node_ids[[j]] <- node_info$data[[j]]$links$move
        node_names[[j]] <- node_info$data[[j]]$attributes$name
      }
      names(node_ids) <- node_names
      comp_dict <- node_ids
    }
  }
  
  for (k in names(comp_dict)) {
    if (k == list[[length(list)]]) {
      node_url <- comp_dict[[k]]
    }
  }
  
  return(node_url) # WE SHOULD RETURN COMP_DICT AND MAKE THIS FUNCTION "GET_SUBFOLDER_DICT" eventually...
  
}


#' Get nodes information from OSF.
#' 
#' @param id OSF id.
#' 
#' @param contributors Looking for contributors?
#' 
#' @param files Looking for files?
#' 
#' @param children Looking for children...wait...that could be taken the wrong way...Looking for child nodes?
#' @export get_nodes

get_nodes <- function(id = NULL, contributors = FALSE, files = FALSE,
                      children = FALSE) {
  
  config <- get_config()
  
  call <- httr::GET(paste("https://api.osf.io/v2", "nodes", id, sep = "/"), config)
  
  res <- rjson::fromJSON(httr::content(call, "text", encoding = "UTF-8"))
  
  if (names(res)[1] == "errors" && !is.null(id))
    stop("Node not found.")
  
  if (sum(c(contributors, files, children)) > 1)
    stop("Specify contributors OR files OR children.")
  
  if (contributors) {
    call <- httr::GET(res$data$relationships$contributors$links$related$href, config)
    res <- rjson::fromJSON(httr::content(call, "text", encoding = "UTF-8"))
  }
  
  if (files) {
    # Change to access actual files id under guid tag within osfstorage
    call <- httr::GET(paste0(res$data$relationships$files$links$related$href, "/osfstorage/"), config)
    res <- rjson::fromJSON(httr::content(call, "text", encoding = "UTF-8"))
  }
  
  if (children) {
    call <- httr::GET(res$data$relationships$children$links$related$href, config)
    res <- rjson::fromJSON(httr::content(call, "text", encoding = "UTF-8"))
    
    if (!is.list(res$data))
      stop(sprintf("No children available for node %s", id))
  }
  
  # This part compiles all the pages (hence "next")
  while (!is.null(res$links$`next`)) {
    whilst <- rjson::fromJSON(
      httr::content(
        httr::GET(res$links$`next`, config), "text", encoding = "UTF-8"))
    res$data <- c(res$data, whilst$data)
    res$links$`next` <- whilst$links$`next`
    message(paste0(res$links$`next`))
  }
  
  return(res)
}
