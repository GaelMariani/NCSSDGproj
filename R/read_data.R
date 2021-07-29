#' Read All Sheets Of The Supplementary Material
#'
#' @return a list of dataframe for each ecosystem
#' @export
#'
#' @examples
read_all_sheets <- function(){
  
  sheets <- openxlsx::getSheetNames(here::here("rawdata", "supplementary_material_V5.xlsx"))[-1] # remove scoring system and Polar sheets
  sheets_list <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = here::here("rawdata", "supplementary_material_V5.xlsx"))
  
  names(sheets_list) <- sheets
  
  return(sheets_list)
}


#' Load SDG icons path 
#'
#' @return a character with the list of paths to read and transform SDG icons 
#' @export
#'
#' @examples
load_SDG_icon <- function() {
  
  # Extract path of the 17 SDG
  paste0("rawdata/SDG_icon/", list.files(path=here::here("rawdata", "SDG_icon"), pattern = ".png"))
  
}


#' Load NCS icons path 
#'
#' @return a character with the list of paths to read and transform NCS icons
#' @export 
#'
#' @examples
load_NCS_icon <- function() {
  
  # Extract path of the 17 SDG
  path <- paste0("rawdata/Ecosystem_icon/", list.files(path=here::here("rawdata", "Ecosystem_icon"), pattern = ".PNG"))
  path <- c(path, paste0("rawdata/Ecosystem_icon/", list.files(path=here::here("rawdata", "Ecosystem_icon"), pattern = ".png")))
  
  return(path)
}


#' Load Legend Of NCS
#'
#' @return A RData to plot Figure 1B
#' @export
#'
#' @examples
load_legend <- function(){
  
  load(here::here("results", "legend.RData"))
  return(legend)
  
}


#' Load Vertical Legend Of NCS
#'
#' @return A RData to plot circular plot
#' @export
#'
#' @examples
load_vert_legend <- function(){
  
  load(here::here("results", "vert_legend.RData"))
  return(vert_legend)
  
}


#' Load Observed Metrics
#'
#' @return a list of 2 elements, with observed indices values for positive and negative scores
#' @export 
#'
#' @examples
load_metric_obs <- function(){
  
  ### POSITIVE data
  load(here::here("results", "Nest_Modu_res_pos.RData"))
  nest_mod_obs <- res
  
  load(here::here("results", "TUI_TOI_res_pos.RData"))
  insurance_obs <- res
  
    ## Bind data
    metric_obs_pos <- rbind(nest_mod_obs, insurance_obs)
  
  ### NEGATIVE DATA
  load(here::here("results", "Nest_Modu_res_neg.RData"))
  nest_mod_obs <- res
  
  load(here::here("results", "TUI_TOI_res_neg.RData"))
  insurance_obs <- res
  
    ## Bind data
    metric_obs_neg <- rbind(nest_mod_obs, insurance_obs)
  
  metric_obs <- list("score_pos" = metric_obs_pos, "score_neg" = metric_obs_neg)
  return(metric_obs)
}

