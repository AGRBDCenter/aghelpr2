#' @title Retreive Master Admin Project Sheet
#' @description This function retrieves the excel sheet that is updated to contain all current project information. The Admin Sheet is the fulcrum by which the applications operate. If any odd errors arise ensure that you check the admin sheet and that this function is reading it in properly
#' @param osf_file_id A string constant containing the code at the end of an OSF url. The default value is the current address to the admin sheet, it might change in the future.
#' For example, https://osf.io/z8kpa/.
#' The default string value is directly to the master sheet.
#'
#' @return A tibble in long format
#'
#' @example
#' get_osf_pat()
#' admin_dat <- get_admin_sheet()
#'
#' @export get_admin_sheet

get_admin_sheet <- function(osf_file_id = "z8kpa") {

  admin_dat <- download_files(id = osf_file_id,
                              path = tempdir()) %>%
    readxl::read_xlsx()

  # Transform
  test <- admin_dat %>%
    mutate(treatments = str_split(treatments, ",")) %>%
    unnest() %>%
    mutate(variables = str_split(variables, ",")) %>%
    unnest() %>%
    mutate(replications = map(replications, ~as.character(as.roman(1:.x)))) %>%
    unnest() %>%
    mutate(units_of_measurement = str_split(units_of_measurement, ",")) %>%
    unnest() %>%
    mutate_all(trimws) %>%
    select(type, project_name, treatments, variables, units_of_measurement, replications)

  return(test)
}
