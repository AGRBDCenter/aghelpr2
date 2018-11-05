# This script includes functions vital for statistical analysis.

#' Run ANOVA tests on all data.
#'
#' @param data A dataframe or tibble object containing data in a standardized form. Must include at the minimum: project_name, treatments, replications, variables, value.
#' @export anova_models

anova_models <- function(data) {
  return(models)
}


#' Run Tukey HSD tests on all data.
#'
#' @param models A list of ANOVA models that is returned by anova_models.
#' @export tukey_models

tukey_models <- function(data) {
  return(models)
}


#' Parse Tukey HSD tests to csv and png. (Possibly not png anymore as may not be useful). Will include information on how to read the analysis and what the columns mean, etc.
#'
#' @param models A list of Tukey HSD tibbles that is returned by tukey_models.
#' @export write_analysis

write_analysis <- function(models) {
  
}