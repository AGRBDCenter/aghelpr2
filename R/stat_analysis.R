# This script includes functions vital for statistical analysis.

###################
# ANOVA supporters
###################

# Split the data into projects
split_aov_proj <- function(data, proj) {
  data_filt <- data %>% 
    filter(project_name == proj)
  
  return(data_filt)
}

# Split the data into variables
split_aov_var <- function(data, var) {
  data_filt <- data %>%
    filter(variables == var) %>% 
    mutate(treatments = as.factor(.$treatments),
           replications = as.factor(.$replications))
  
  return(data_filt)
}

# Split the data into dates
# UNIMPLEMENTED. MAY NEED TO BE IMPLEMENTED.
split_aov_date <- function(data, date) {
  
}

# Check the block to see if it is significant
check_block <- function(anova_model, alpha) {
  mod_df <- broom::tidy(mod)
  if (dplyr::filter(mod_df, term == "replications")$p.value <= alpha) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check to see if the model should be a basic blocked design
check_basic_block <- function(data) {
  if (length(unique(data$replications)) > 1 &&
      length(unique(data$treatments)) > 1 &&
      length(unique(data$value)) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check to see if the model should be a basic blocked design with variety as an interaction term
check_variety_block <- function(data) {
  if (length(unique(data$replications)) > 1 &&
      length(unique(data$treatments)) > 1 &&
      length(unique(data$value)) > 1 &&
      length(unique(data$variety)) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check to see if the model should be a basic ANOVA design with no block
check_basic <- function(data) {
  if (length(unique(data$date_collected)) > 1 &&
      length(unique(data$treatments)) > 1 &&
      length(unique(data$value)) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check to see if the model should be a basic design with variety as an interaction term
check_variety <- function(data) {
  if (length(unique(data$date_collected)) > 1 &&
      length(unique(data$treatments)) > 1 &&
      length(unique(data$value)) > 1 &&
      length(unique(data$variety)) > 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }  
}

#' A function to get all ANOVA models. Returns a list of models.
#' 
#' @param data A dataframe or tibble object containing data in a standardized form. Must include at the minimum: project_name, treatments, replications, variables, value.
#' @param alpha The level of significance
#' @param date Whether or not we should split the analysis by date
#' @export anova_models

anova_models <- function(data, alpha = 0.05, date = FALSE) {
  models <- vector(mode = "list")
  
  # Iterate through projects and variables
  for (proj in unique(data$project_name)) {
    data_filt <- split_aov_proj(data, proj)
    
    for (var in unique(data_filt$variables)) {
      data2 <- split_aov_var(data_filt, var)
      
      cat("Attempting ", proj, " ", var, "\n")
      
      # Variety with Block
      if (check_variety_block(data2)) {
        mod <- aov(value ~ treatments*variety + replications, data = data2)
        mod_name <- paste0(str_replace_all(proj, "[^[:alnum:]]", ""), "_",
                           str_replace_all(var, "[^[:alnum:]]", ""))
        
        if (check_block(mod, alpha)) {
          models[[mod_name]] <- mod
        } else {
          mod <- aov(value ~ treatments*variety, data = data2)
          models[[mod_name]] <- mod
        }
        cat(crayon::cyan(proj), crayon::magenta(var), crayon::green("ANOVA Complete"), "\n")
        
        # Variety
      } else if (check_variety(data2)) {
        mod <- aov(value ~ treatments*variety, data = data2)
        mod_name <- paste0(str_replace_all(proj, "[^[:alnum:]]", ""), "_",
                           str_replace_all(var, "[^[:alnum:]]", ""))
        cat(crayon::cyan(proj), crayon::magenta(var), crayon::green("ANOVA Complete"), "\n")
        
        # Basic with Block
      } else if (check_basic_block(data2)) {
        mod <- aov(value ~ treatments + replications, data = data2)
        mod_name <- paste0(str_replace_all(proj, "[^[:alnum:]]", ""), "_",
                           str_replace_all(var, "[^[:alnum:]]", ""))
        
        if (check_block(mod, alpha)) {
          models[[mod_name]] <- mod
        } else {
          mod <- aov(value ~ treatments, data = data2)
          models[[mod_name]] <- mod
        }
        cat(crayon::cyan(proj), crayon::magenta(var), crayon::green("ANOVA Complete"), "\n")
        
        # Basic
      } else if (check_basic(data2)) {
        mod <- aov(value ~ treatments, data = data2)
        mod_name <- paste0(str_replace_all(proj, "[^[:alnum:]]", ""), "_",
                           str_replace_all(var, "[^[:alnum:]]", ""))
        cat(crayon::cyan(proj), crayon::magenta(var), crayon::green("ANOVA Complete"), "\n")
      } else {
        cat("ANOVA failed for ", crayon::cyan(proj), " ", crayon::magenta(var), "\n")
      }
    }
  }
  return(models)
}


#' Run Tukey HSD tests on all data.
#'
#' @param models A list of ANOVA models that is returned by anova_models.
#' @export tukey_models

tukey_models <- function(models) {
  tukies <- vector(mode = "list")
  for (model_name in names(models)) {
    cat(crayon::cyan(model_name), crayon::green(" Running Tukey HSD"), "\n")
    model <- models[[model_name]]
    tukey <- HSD.test(model, trt = 'treatments', group = TRUE, alpha = 0.1)
    treatment <- rownames(tukey$groups)
    rownames(tukey$groups) <- NULL
    groups <- as.tibble(cbind(treatment, tukey$groups[1], tukey$groups[2])) %>% 
      mutate(treatment = parse_character(treatment)) %>% 
      arrange(treatment)
    
    tukies[[model_name]] <- groups
  }
  return(tukies)
}


#' Returns an initial report to be reviewed for significance in xlsx format with tabs for each project. Tukey HSD models are written to this report.
#' Models should be evaluated to determine if orthogonal contrasts should be used, other variables included in the model, or other processes on a case by case basis.
#'
#' @param models A list of Tukey HSD tibbles that is returned by tukey_models.
#' @param path The path where you want the output saved.
#' @export write_analysis

write_analysis <- function(models, path) {
  wb <- openxlsx::createWorkbook("RBDC Data Science Team")
  names_list <- list()
  
  for (i in seq_along(names(models))) {
    proj <- str_split(names(models)[[i]], "_")[[1]][1]
    names_list[[i]] <- proj
  }
  
  for (j in unique(names_list)) {
    openxlsx::addWorksheet(wb, j)
    proj_data <- data.frame()
    for (k in names(models)) {
      if (str_detect(k, j)) {
        proj_data %<>%
          plyr::rbind.fill(data.frame(treatment = "")) %>%
          plyr::rbind.fill(data.frame(treatment = k, value = "", groups = "")) %>%
          plyr::rbind.fill(data.frame(treatment = "treatment", value = "value", groups = "groups")) %>%
          plyr::rbind.fill(models[[k]])
        
        openxlsx::writeData(wb, sheet = j, proj_data, colNames = FALSE)
        openxlsx::setColWidths(wb, sheet = j, cols = 1, widths = 30)
      }
    }
  }
  
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}