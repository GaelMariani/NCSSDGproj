
#' Read the binary matrix
#'
#' @return a dataframe with targets of the SDGs in columns and NCSs in rows
#' @export
#'
#' @examples
read_matrix <- function(){
  
  read.csv(here::here("rawdata", "matrix01_NCS_SDG.csv"), sep = ";", check.names = FALSE)
  
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
  return(barplot)
  
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


