
#' Read the binary matrix
#'
#' @return a dataframe with targets of the SDGs in columns and NCSs in rows
#' @export
#'
#' @examples
read_matrix <- function(){
  
  read.csv(here::here("rawdata", "matrix01_NCS_SDG.csv"), sep = ";", check.names = FALSE)
  
}


#' Read All Sheets Of The Supplementary Material
#'
#' @return a list of dataframe for each ecosystem
#' @export
#'
#' @examples
read_all_sheets <- function(){
  
  sheets <- openxlsx::getSheetNames(here::here("rawdata", "supplementary_material.xlsx"))[-c(1,13)] # remove scoring system and Polar sheets
  sheets_list <- lapply(sheets, openxlsx::read.xlsx, xlsxFile = here::here("rawdata", "supplementary_material.xlsx"))
  
  names(sheets_list) <- sheets
  
  return(sheets_list)
}


#' Load SDG icons path 
#'
#' @return list of path to read and transform SDG icon 
#' @export
#'
#' @examples
load_SDG_icon <- function() {
  
  # Extract path of the 17 SDG
  paste0("rawdata/SDG_icon/", list.files(path=here::here("rawdata", "SDG_icon"), pattern = ".png"))
  
}



#' Load NCS icons path 
#'
#' @return list of path to read and transform NCS icon 
#' @export 
#'
#' @examples
load_NCS_icon <- function() {
  
  # Extract path of the 17 SDG
  path <- paste0("rawdata/Ecosystem_icon/", list.files(path=here::here("rawdata", "Ecosystem_icon"), pattern = ".PNG"))
  path <- c(path, paste0("rawdata/Ecosystem_icon/", list.files(path=here::here("rawdata", "Ecosystem_icon"), pattern = ".png")))
  
  return(path)
}


#' Load First Figure Panel A
#'
#' @return A RData to plot Figure 1A
#' @export
#'
#' @examples
load_Fig1A <- function(){
  
  load(here::here("results", "network_SDG_NCS.RData"))
  return(netw)
  
}


#' Load First Figure Panel B
#'
#' @return A RData to plot Figure 1B
#' @export
#'
#' @examples
load_Fig1B <- function(){
  
  load(here::here("results", "barplot_pourc.RData"))
  return(barplot_pourc)
  
}


#' Load First Figure Panel B test
#'
#' @return A RData to plot Figure 1B
#' @export
#'
#' @examples
load_Fig1B_test <- function(){
  
  load(here::here("results", "test_barplot.RData"))
  return(test_barplot_pourc)
  
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


#' Load Correspondance Analysis Legend
#'
#' @return A RData to plot circular plot
#' @export
#'
#' @examples
load_CA_legend <- function(){
  
  load(here::here("results", "CA_legend.RData"))
  return(CA_legend)
  
}


#' Read the binary matrix
#'
#' @return a dataframe with target's contribution and type to the second axis variance
#' @export
#'
#' @examples
SDG_contrib_tbl <- function(){
  
  readxl::read_excel(here::here("rawdata", "colnames12_v2.xlsx"))
  
}

