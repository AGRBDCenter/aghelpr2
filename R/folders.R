#' Construct folder structure for a year based on admin sheet contained in Administrator component.
#'
#' @param year The year as a character. Defaults to current year.
#' 
#' @export

build_struct <- function(year = format(Sys.Date(), "%Y")) {
  parent_id <- "judwb"
  admin_sheet <- get_admin_sheet()
  projects <- unique(admin_sheet$project_name)
  folders <- c("Analysis", "Data", "Journal", "Pictures", "Protocols_Treatments_PlotMaps", "Report")
  
  # Create structure
  year_id <- create_component(id = parent_id, title = year)
  cat('Year component created\n')
  for (i in projects) {
    project_id <- create_component(id = year_id, title = i)
    cat('  ', i, 'component created\n')
    for (j in folders) {
      create_folder(project_id, j)
      cat('     ', j, 'folder created\n')
    }
  }
}

