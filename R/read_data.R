
#' Read the binary matrix
#'
#' @return a dataframe with targets of the SDGs in columns and NCSs in rows
#' @export
#'
#' @examples
read_matrix <- function(){
  
  readr::read_csv2(here::here("rawdata", "matrix01_NCS_SDG.csv"))
  
}

