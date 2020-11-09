
#' Read the binary matrix
#'
#' @return a dataframe with targets of the SDGs in columns and NCSs in rows
#' @export
#'
#' @examples
read_matrix <- function(){
  
  readr::read_csv2(here::here("rawdata", "matrix01_NCS_SDG.csv"))
  
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






