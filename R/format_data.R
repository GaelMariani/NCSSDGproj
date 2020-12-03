
#' Matrix Long Format
#'
#' @param matrix01 the raw matrix with targets in columns and NCS in rows
#'
#' @return a dataframe to the long format with value 0 or 1 for each SDG's target and each ecosystem
#' @export
#'
#' @examples
matrix_to_longDF <- function(matrix01) {
  
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
  
  # sum of targets achieved in each SDG 
  sums <- perc_group %>% 
    dplyr::group_by(goal) %>% 
    dplyr::summarise(value_tot = sum(value_grp))
  
  
  # % of SDG' target achieved + merge with perc_group
  perc_plot <- data_long %>%
    dplyr::group_by(goal.target, goal) %>%
    dplyr::summarise(value = dplyr::if_else(sum(value) >= 1, 1, 0)) %>%
    dplyr::group_by(goal) %>%
    dplyr::summarise(value_goal = sum(value),
                     n_target   = length(unique(goal.target))) %>%
    dplyr::mutate(perc_goal = round((value_goal*100)/n_target, digits = 0),
                  text      = paste0(value_goal, "/", n_target)) %>%
    dplyr::left_join(., perc_group, by = "goal") %>%
    dplyr::left_join(., sums, by = "goal") %>%
    dplyr::mutate(perc_global = (value_grp/value_tot)*100,
                  relative_pourcent = (perc_global*perc_goal)/100) %>%
    dplyr::mutate(SDG_number = stringr::str_sub(goal, 5))
      
    
  return(perc_plot)
  
}


#' Contingency Matrix Of SDG's Targets For Network Indices And Unipartit Plot
#'
#' @param raw_dat a dataframe with targets of the SDGs in columns and NCSs in rows
#'
#' @return a matrix of 0 and 1 
#' @export
#'
#' @examples
contingency_mat_targets <- function(raw_dat) {
  
  raw_dat %>%
    replace(., . < 0, 0) %>%
    magrittr::set_rownames(.[,1]) %>%
    dplyr::select(-1) %>%
    bipartite::empty() %>% # delete rows and columns full of 0
    as.matrix
  
}


#' Format Data For TI Estimate
#'
#' @param matrix a matrix with targets in rows and NCS in columns
#'
#' @return a dataframe with the number of time each target is achieved
#' @export
#'
#' @examples
data_TI <- function(matrix) {
  
  as.data.frame(matrix) %>% 
    dplyr::bind_cols(rownames(.), .) %>%
    tidyr::gather(., target, value, -1) %>%
    stats::setNames(c("ecosystem", "target", "value")) %>%
    replace(., . < 0, 0) %>%
    dplyr::group_by(target) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::filter(value != 0)
  
}



#' Insurance Data To Plot
#'
#' @param matrix01 a matrix with targets in columns and NCS in rows
#' @param Ntarget the total number of targets
#'
#' @return A data frame with number of times a target is achieved with a column identifying observed data vs. null data
#'
#' @export
#'
#' @examples
Insurance_data2plot <- function(matrix01, Ntarget) {
  
  ## Observed data
  data_obs <- NCSSDGproj::data_TI(matrix01) %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(group = "Observed distribution",
                  xval = rownames(.))
  

  target_insurance <- sum(data_obs$value)/Ntarget
  
  ## Null data
  null_matrix <- asplit(stats::simulate(vegan::nullmodel(matrix01, "r00"), nsim = 1), 3)
  data_null <- NCSSDGproj::data_TI(null_matrix[[1]]) %>%
    dplyr::arrange(-value) %>%
    dplyr::mutate(group = "Expected distribution",
                  xval = rownames(.))
  
  
  ## Bind data
  data_plot <- rbind(data_obs, data_null)
  
  return(data_plot)
  
  
}



#' Network Format For Unipartite Plot
#'
#' @param contin_mat_target a matrix with targets in rows and NCS in columns
#' @param method see layout in ggnetwork
#' @param colNCS_ter color for terrestrial nodes
#' @param colNCS_coast color for coastal nodes 
#' @param colNCS_mar color for marine nodes
#' @param colTARG color for targets nodes
#'
#' @return A data.frame object with coordinates and color columns for each node
#' 
#' @export
#'
#' @examples
network_uniP <- function(contin_mat_target, method, colNCS_ter, colNCS_coast, colNCS_mar, colTARG) {
  
  # NB: transpose the matrix so that I can format edge' colors according to their starting point (NCS) and not 
  # according to their ending point (SDG's target)
  netw <- ggnetwork::ggnetwork(t(contin_mat_target), layout = method, cell.jitter = 0.5)
  
  # Format the network objec to plot with ggplot2
  terrestrial <- c("Peatland ", "Urban forests", "Forest", "Grassland ")
  coastal <- c("Mangroves", "Saltmarshes", "Seagrasses", "Macroalgae")
  marine <- c("Mesopelagic areas", "Polar marine ecosystem", "Pelagic areas")
  
  # Gives a color to each type of nodes
  netw <- netw %>%
    dplyr::mutate(color = dplyr::case_when(vertex.names %in% terrestrial ~ colNCS_ter,
                                           vertex.names %in% coastal ~ colNCS_coast,
                                           vertex.names %in% marine ~ colNCS_mar,
                                           vertex.names %in% colnames(contin_mat_target) ~ colTARG))
              
  return(netw)
  
}


#' Sustainable Development Goals Infos
#'
#' @param matrix01 a matrix with targets in columns and NCS in rows
#'
#' @return a dataframe with with the category, the color and the name of each SDG and target
#' @export
#'
#' @examples
SDG_infos <- function(matrix01){
  
  ## Build a data frame with the name, category and color of each SDG and targets
  data <- data.frame(name = colnames(matrix01)) %>%
    tidyr::separate(name, c("SDG", "target"), sep = "[.]", remove = FALSE) %>%
    
    # Add a colomn category (environment vs Economy vs Governance vs Society)
    dplyr::mutate(category = dplyr::case_when((SDG == "6" | SDG == "12" | SDG == "14" | SDG == "15" | SDG == "13") ~ "Environment",
                                              (SDG == "7" | SDG == "8" | SDG == "9" | SDG == "11") ~ "Economy",
                                              (SDG == "16") ~ "Governance",
                                              TRUE ~ as.character("Society")),
                  
                  # Add a column with SDG names
                  SDG_name = dplyr::case_when((SDG == 1) ~ "No Poverty",
                                              (SDG == 2) ~ "Zero Hunger",
                                              (SDG == 3) ~ "Good Health",
                                              (SDG == 4) ~ "Quality Education",
                                              (SDG == 5) ~ "Gender Equality",
                                              (SDG == 6) ~ "Clean Water",
                                              (SDG == 7) ~ "Affordable + Clean Energy", 
                                              (SDG == 8) ~ "Decent Work", 
                                              (SDG == 9) ~ "Industry + Innovation",
                                              (SDG == 10) ~ "Reduced Inequalities",
                                              (SDG == 11) ~ "Sustainable Cities",
                                              (SDG == 12) ~ "Responsible Consumption",
                                              (SDG == 13) ~"Climate Action",
                                              (SDG == 14) ~ "Life Bellow Water",
                                              (SDG == 15) ~ "Life on Land",
                                              (SDG == 16) ~ "Peace, Justice"), 
                                
                  # Add a column with the official color of each SDG
                  color = dplyr::case_when((SDG == 1) ~ "#F41528",
                                           (SDG == 2) ~ "#D3A029",
                                           (SDG == 3) ~ "#279B48",
                                           (SDG == 4) ~ "#C5192D",
                                           (SDG == 5) ~ "#FF3A21",
                                           (SDG == 6) ~ "#00AED9",
                                           (SDG == 7) ~ "#FDB713", 
                                           (SDG == 8) ~ "#8F1838", 
                                           (SDG == 9) ~ "#F36D25",
                                           (SDG == 10) ~ "#E11484",
                                           (SDG == 11) ~ "#F99D26",
                                           (SDG == 12) ~ "#CF8D2A",
                                           (SDG == 13) ~ "#3F7E44",
                                           (SDG == 14) ~ "#007DBC",
                                           (SDG == 15) ~ "#3EB049",
                                           (SDG == 16) ~ "#02558B")) %>%
    dplyr::arrange(SDG_name)
  
  return(data)
  
}


#' Natural Climate Solutions Infos
#'
#' @param matrix01 a matrix with targets in columns and NCS in rows
#'
#' @return a dataframe with type of NCS info i.e. terrestrial vs coastal vs marine
#' @export
#'
#' @examples
NCS_info <- function(matrix01){

  data.frame(Ecosystem = rownames(matrix01)) %>%
    dplyr::mutate(group = dplyr::case_when((Ecosystem == "Peatland " | Ecosystem == "Urban forests" | Ecosystem == "Forest" | Ecosystem == "Grassland ") ~ "Terrestrial",
                                           (Ecosystem == "Saltmarshes" | Ecosystem == "Mangroves" | Ecosystem == "Seagrasses" | Ecosystem == "Macroalgae") ~ "Coastal",
                                            TRUE ~ "Marine"))

}



#' Format Data For Circular Plot
#'
#' @param data_Insurance 
#' @param SDG_info 
#'
#' @return
#' @export
#'
#' @examples
circular_data_Insurance <- function(data_Insurance, SDG_info){
  
  ### Bind data
  tmp <- data.frame(SDG = c(1:16), 
                    SDG_order = as.factor(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")))
  
  data <- data_Insurance[(1:(nrow(data_Insurance)/2)), -4] %>%
    dplyr::left_join(., SDG_info[, -3], by = c("target" = "name")) %>%
    dplyr::mutate(SDG = as.numeric(SDG)) %>%
    dplyr::left_join(., tmp, by = "SDG")
    
  ### Set a number of empty bars
  empty_bar <- 2
  
  to_add <- data.frame(matrix(NA, empty_bar*nlevels(data$SDG_order), ncol(data)))
  colnames(to_add) <- colnames(data)
  to_add$SDG_order <- rep(levels(data$SDG_order), each = empty_bar)
  data <- rbind(data, to_add)
  
  data <- data %>% 
    dplyr::arrange(SDG_order)
    
  data$id <- seq(1, nrow(data))
    
  ### Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar     
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle + 180, angle)
  
  ### Prepare a data frame for base lines
  base_data <- data %>% 
    dplyr::group_by(SDG_order) %>% 
    dplyr::summarize(start = min(id), end = max(id) - empty_bar) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(title = mean(c(start, end)))
  
  base_data$SDG <- 1:16
  
  base_data[5, 3:4] <- base_data[5, 3:4] + 0.5
  
  ### Prepare a data frale for grid
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  data_CircuPlot <- list("data" = data,
                         "label data" = label_data,
                         "base_data" = base_data,
                         "grid data" = grid_data)
  
  
  return(data_CircuPlot)
  
}

