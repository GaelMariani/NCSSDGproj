
#' Matrix Long Format
#'
#' @param matrix01 the raw matrix with targets in columns and NCS in rows
#'
#' @return a dataframe to the long format with value 0 or 1 for each SDG's target and each ecosystem
#' @export
#'
#' @examples
matrix_to_longDF <- function(data_long) {
  
  data_long <- matrix01 %>%
    tidyr::gather(., goal.target, value, -1) %>% # long format
    magrittr::set_names(c("ecosystem", "goal.target", "value")) %>%
    tidyr::separate(goal.target, c("goal", "target"), sep = "[.]", remove = FALSE) %>%
    dplyr::mutate(goal = paste("SDG", goal)) %>%
    dplyr::filter(goal != 17) %>%
    dplyr::select(-"target") %>%
    dplyr::arrange(match(x = ecosystem, c("Peatland ", "Urban forests", "Forest", "Grassland ",
                                          "Saltmarshes", "Mangroves", "Seagrasses", "Macroalgae",
                                          "Pelagic areas", "Polar marine ecosystem", "Mesopelagic areas"))) %>%
    dplyr::mutate(ecosystem = forcats::as_factor(ecosystem),
                  goal.target = factor(goal.target))
  
  return(data_long)

}



#' Weighted Contingency Matrix of SDG
#'
#' @param data_long A dataframe with 0 and 1 value for each NCS and SDG's targets
#'
#' @return a weighted matrix with SDG in columns and NCS in rows
#' @export
#'
#' @examples
matrix_SDG <- function(data_long) {
  
  mat_SDG <- data_long %>%
    reshape2::acast(., factor(ecosystem, levels = unique(ecosystem))~goal, sum) %>%
    magrittr::set_rownames(c("Peatland", "Urban forest", "Forest", "Grassland", "Tidal marsh", "Mangrove",
                             "Seagrasse", "Kelp forest", "Pelagic", "Polar area", "Mesopelagic")) 
  
  SDG_matrix <- mat_SDG[, c("SDG 7", "SDG 6", "SDG 15", "SDG 11", "SDG 5", "SDG 3", "SDG 13", "SDG 9",
                            "SDG 1", "SDG 4", "SDG 8", "SDG 16", "SDG 12", "SDG 10", "SDG 2", "SDG 14")]
  
  return(SDG_matrix)
  
}



#' From Matrix to Network object
#'
#' @param matrix a weighted or binary matrix with SDG in column and NCS in row
#' @param mode1 
#' @param mode2 
#'
#' @return a network object 
#' @export
#' 
#' @importFrom network `%v%<-` `%v%`
#'
#' @examples
matrix_to_network <- function (matrix, mode1="P", mode2="A") {

  if(!is.matrix(matrix)) matrix <- as.matrix(matrix)
  
  p <- dim(matrix)[1]    
  a <- dim(matrix)[2]    
  net <- network::network(matrix,
                          matrix.type = "bipartite",
                          ignore.eval = FALSE,
                          names.eval = "weights")
  net
  network::set.vertex.attribute(net, "mode", c(rep(mode1, p), rep(mode2, a)))
  
  # Rename vertex (or nodes) names
  network::network.vertex.names(net) <- c(rep("Ecosystem", 11), rep("SDG", 16))
  
  # Create "phono" to assign a shape 
  net %v% "phono" = ifelse(network::network.vertex.names(net) == "Ecosystem", "Ecosystem", "SDG")
  net %v% "shape" = ifelse(net %v% "phono" == "Ecosystem", 19, 15)
  
  return(net)
  
}


#' Icon In Raster Format
#'
#' @param path path to load icons
#' @param icon_SDG if TRUE, format SDG icons, else, format NCS icons
#'
#' @return A list of raster object with icons ready to plot with ggplot2
#' @export
#'
#' @examples
format_icons <- function(path, icon_SDG = TRUE) {
  
  # Read .png objects 
  icon_png <- lapply(here::here(path), png::readPNG)
  
  # Transform icon_png into a list of raster object
  icon_rast <- lapply(icon_png, grid::rasterGrob, interpolate = TRUE)
  
  if(icon_SDG == TRUE) {
    
    SDG_order <- c("SDG 7", "SDG 6", "SDG 15", "SDG 11", "SDG 5", "SDG 3", "SDG 13", "SDG 9", "SDG 1", "SDG 4", "SDG 8", "SDG 16", "SDG 12", "SDG 10", "SDG 2", "SDG 14")
    names(icon_rast) <- c(paste(rep("SDG", 17), seq(1,17,1)))
    icon <- icon_rast[SDG_order]
    
  } else { icon <- icon_rast[c(1,2,3,4,7,8,9,10,11,5,6)] }
  
  return(icon)
  
}


#' Percentage Of Target Achieved
#'
#' @param data_long a dataframe to the long format with value 0 or 1 for each SDG's target and each ecosystem
#'
#' @return a data frame with percentage of target achieve totally + by group of NCS, values to be plotted 
#' @export
#'
#' @examples
perc_SDG <- function(data_long) {
  
  # % of SDG' targets achieved by group of NCS, terrestrial vs coastal vs marine
  perc_group <- data_long %>%
    dplyr::mutate(group = dplyr::case_when((ecosystem == "Peatland " | ecosystem == "Urban forests" | ecosystem == "Forest" | ecosystem == "Grassland ") ~ "Terrestrial",
                                           (ecosystem == "Saltmarshes" | ecosystem == "Mangroves" | ecosystem == "Seagrasses" | ecosystem == "Macroalgae") ~ "Coastal",
                                           TRUE ~ "Marine")) %>%
    dplyr::group_by(goal.target, group, goal) %>%
    dplyr::summarise(value_grp = dplyr::if_else(sum(value) >= 1, 1, 0),
                     n_target = length(unique(goal.target))) %>%
    dplyr::group_by(goal, group) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::mutate(perc_group = round((value_grp*100)/n_target, digits = 0)) %>%
    dplyr::select(-n_target)
  
  # % of SDG' target achieved + merge with perc_group
  perc_plot <- data_long %>%
    dplyr::group_by(goal.target, goal) %>%
    dplyr::summarise(value = dplyr::if_else(sum(value) >= 1, 1, 0)) %>%
    dplyr::group_by(goal) %>%
    dplyr::summarise(value_goal = sum(value),
                     n_target = length(unique(goal.target))) %>%
    dplyr::mutate(perc_goal = round((value_goal*100)/n_target, digits = 0),
                  text = paste0(value_goal, "/", n_target)) %>%
    dplyr::left_join(., perc_group, by = "goal")
    
    
  return(perc_plot)
  
}
