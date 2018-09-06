#' @title Retreive Proper OSF PAT from System Environment
#' @description This function is a replacement for osfr::login() and will properly assign OSF_PAT to the system environment. This is required to make connection with the account on OSF.
#' @param pat Personal Access Token (PAT) - Create a PAT at https://osf.io/settings/tokens/ if no token is given an error is thrown.
#'
#'
#'
#'
#'
#'
get_osf_pat <- function(pat = NULL){

  if(!is.null(pat)) {
    Sys.setenv(OSF_PAT = pat)
  } else if(Sys.getenv("OSF_PAT") == '') {
    stop("Error: System Environment must contain OSF_PAT. No PAT detected - Create a PAT at https://osf.io/settings/tokens")
  }

  invisible(Sys.getenv('OSF_PAT'))

}

#' Create authorization config (function from package osfr)
#'
#' @param login_required Boolean
#'
#' @return configuration for use in httr request
#' @export get_config
get_config <- function() {

  config <- httr::add_headers(Authorization = sprintf('Bearer %s', get_osf_pat()))

  return(config)
}
