#' Read the binary matrix - for the 1st scoring system
#'
#' @return a dataframe with targets of the SDGs in columns and NCSs in rows
#' @export
#'
#' @examples
read_matrix <- function(){
  
  read.csv(here::here("rawdata", "matrix01_NCS_SDG.csv"), sep = ";", check.names = FALSE)
  
}


#' Weighted Contingency Matrix of SDG - for the 1st scoring system
#'
#' @param data_long A dataframe with 0 and 1 value for each NCS and SDG's targets
#'
#' @return a weighted matrix with SDG in columns and NCS in rows
#' @export
#'
#' @examples
matrix_SDG_not_use <- function(data_long) {
  
  mat_SDG <- data_long %>%
    reshape2::acast(., factor(ecosystem, levels = unique(ecosystem))~goal, sum) %>%
    magrittr::set_rownames(c("Urban forest", "Forest", "Tidalmarsh", "Seagrass", "Macroalgae",
                             "Pelagic", "Mesopelagic", "Peatland", "Grassland", "Mangrove","Antarctic"))
  
  SDG_matrix <- mat_SDG[c("Peatland", "Urban forest", "Forest", "Grassland", "Tidalmarsh", "Mangrove",
                          "Seagrass", "Macroalgae", "Pelagic", "Antarctic", "Mesopelagic"), 
                        c("SDG 7", "SDG 6", "SDG 15", "SDG 11", "SDG 5", "SDG 3", "SDG 13", "SDG 9",
                          "SDG 1", "SDG 4", "SDG 8", "SDG 16", "SDG 12", "SDG 10", "SDG 2", "SDG 14")]
  
  return(SDG_matrix)
  
}


#' Plot Network - for the 1st scoring system
#'
#' @param network_obj 
#' @param matrix 
#' @param icon_SDG 
#' @param icon_NCS 
#' @save if TRUE the plot is saved in the results folder
#' 
#'
#' @return
#' @export
#' 
#'
#' @examples
plot_network_unused <- function(network_obj, matrix, icon_SDG, icon_NCS, nodes_col, save = FALSE) {
  
  ## Plot the network
  netw <- GGally::ggnet2(net = network_obj, 
                         mode = NCSSDGproj::coords(mymat = matrix, maxX = 6, maxY = 15),
                         label = FALSE,
                         shape = "shape",
                         size = c(rowSums(matrix), rep(15, 16)),
                         max_size = 18, 
                         label.size = 2,
                         edge.size = NCSSDGproj::edge_size(matrix, 5)/1.3, 
                         edge.alpha= 0.4,
                         color = rep("white", 27),
                         edge.color = NCSSDGproj::edge_col(matrix),
                         layout.exp = 0.5) +
    
    # Add silhouette of SDG (xmax = 1.1 to plot with barplot)
    ggplot2::annotation_custom(icon_SDG[[1]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = 1.05) + 
    ggplot2::annotation_custom(icon_SDG[[2]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .917) +
    ggplot2::annotation_custom(icon_SDG[[3]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .784) +
    ggplot2::annotation_custom(icon_SDG[[4]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .651) +
    ggplot2::annotation_custom(icon_SDG[[5]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .518) +
    ggplot2::annotation_custom(icon_SDG[[6]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .385) +
    ggplot2::annotation_custom(icon_SDG[[7]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .252) +
    ggplot2::annotation_custom(icon_SDG[[8]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .119) +
    ggplot2::annotation_custom(icon_SDG[[9]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.0178) +   
    ggplot2::annotation_custom(icon_SDG[[10]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.15) + 
    ggplot2::annotation_custom(icon_SDG[[11]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.283) +
    ggplot2::annotation_custom(icon_SDG[[12]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.416) + 
    ggplot2::annotation_custom(icon_SDG[[13]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.549) +
    ggplot2::annotation_custom(icon_SDG[[14]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.682) +
    ggplot2::annotation_custom(icon_SDG[[15]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.815) +
    ggplot2::annotation_custom(icon_SDG[[16]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.948) +
    
    # Add silhouette for NCS (xmin=-0.75 (-0.1 for peatland) to plot without barplot_percent) +0.1
    ggplot2::annotation_custom(icon_NCS[[1]],  xmin = -0.085, xmax = 0.085, ymin = -Inf, ymax = 1.05) +
    ggplot2::annotation_custom(icon_NCS[[2]],  xmin = -0.075, xmax = 0.075, ymin = -Inf, ymax = 0.85) +
    ggplot2::annotation_custom(icon_NCS[[3]],  xmin = -0.120, xmax = 0.120, ymin = -Inf, ymax = 0.65) +
    ggplot2::annotation_custom(icon_NCS[[4]],  xmin = -0.083, xmax = 0.083, ymin = -Inf, ymax = 0.45) +
    ggplot2::annotation_custom(icon_NCS[[5]],  xmin = -0.089, xmax = 0.089, ymin = -Inf, ymax = 0.25) +
    ggplot2::annotation_custom(icon_NCS[[6]],  xmin = -0.098, xmax = 0.098, ymin = -Inf, ymax = 0.05) +
    ggplot2::annotation_custom(icon_NCS[[7]],  xmin = -0.087, xmax = 0.087, ymin = -Inf, ymax = -0.15) +
    ggplot2::annotation_custom(icon_NCS[[8]],  xmin = -0.085, xmax = 0.085, ymin = -Inf, ymax = -0.35) +
    ggplot2::annotation_custom(icon_NCS[[9]],  xmin = -0.099, xmax = 0.099, ymin = -Inf, ymax = -.55) +
    ggplot2::annotation_custom(icon_NCS[[10]], xmin = -0.073, xmax = 0.073, ymin = -Inf, ymax = -.75) +
    ggplot2::annotation_custom(icon_NCS[[11]], xmin = -0.069, xmax = 0.069, ymin = -Inf, ymax = -.95) + 
    
    # Reverse y axis to have terrestrial ecosystems at the top of the diagramm
    ggplot2::scale_y_reverse() + 
    
    # add text to ecosystem
    ggplot2::annotate(geom = "text", 
                      x = c(rep(-0.2,11)), 
                      y = seq(0,1,0.1), 
                      label = rownames(matrix),
                      color = nodes_col, 
                      alpha = 0.8,
                      size = 3.3, 
                      fontface = "bold") +
    
    
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(), 
                   legend.position = "none") 
  
  ## Save plot
  if(save == TRUE) {
    
    save(netw, file = here::here("results", "network_SDG_NCS.RData"))
    ggplot2::ggsave(here::here("figures", "network_SDG_NCS.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(netw)}
  
}


#' Build Figure Two
#'
#' @return
#' @export
#' 
#'
#' @examples
Figure2_unused <- function(save = FALSE) {
  
  # Load panels
  fig1a <- NCSSDGproj::load_Fig1A()
  fig1b <- NCSSDGproj::load_Fig1B()
  legend <- NCSSDGproj::load_legend()
  
  # Assemble panels
  fig1 <- cowplot::ggdraw() +
    
    cowplot::draw_plot(fig1a, x = 0, y = 0.02, width = 0.61, height = 0.98) +
    cowplot::draw_plot(fig1b, x = 0.5, y = 0.04, width = 0.5, height = 1) +
    cowplot::draw_plot(legend, x = 0.3, y = 0, width = 0.5, height = 0.05) +
    cowplot::draw_plot_label(label = c("a", "b"),
                             size = 15,
                             x = c(0, 0.55),
                             y = c(1, 1)) 
  
  # save
  if(save == TRUE) {
    
    ggplot2::ggsave(here::here("figures", "Figure2.png"), width=10, height=9, device="png")   
    
  } else {return(fig1)}
}


#' Unipartite Plot Of Targets 
#'
#' @param netw a dataframe network object from the network_uniP function
#' @param colNCS_ter color for terrestrial nodes
#' @param colNCS_coast color for coastal nodes
#' @param colNCS_mar color for marine nodes
#' @param save if TRUE the plot is saved in the results folder
#' 
#'
#' @return
#' @export
#'
#' @examples
unipart_plot <- function(netw, colNCS_ter, colNCS_coast, colNCS_mar, save){
  
  plot <- ggplot2::ggplot() +
    
    # Plot edges
    ggnetwork::geom_edges(data = netw,
                          mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = color),
                          curvature = 0, 
                          size = 1, 
                          alpha = 0.25) +
    
    # Format edges
    ggplot2::scale_color_manual(values = c(colNCS_ter, colNCS_mar, colNCS_coast),
                                labels = c("Terrestrial", "Marine", "Coastal"),
                                name = NULL) +
    
    # Plot nodes 
    ggnetwork::geom_nodes(data = netw, 
                          mapping = ggplot2::aes(x=x, y=y), 
                          size = 5) +
    
    # Format nodes
    ggraph::geom_node_point(data = netw,
                            size = 6,
                            color = netw$color,
                            fill = netw$color) +
    
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = c(0.1, 0.7),
                   legend.text = ggplot2::element_text(size = 15)) 
  
  
  ## Save plot
  if(save == TRUE) {
    
    save(plot, file = here::here("results", "unipartite_plot.RData"))
    ggplot2::ggsave(here::here("figures", "unipartite_plot.png"), width = 11, height = 6, device = "png")
    
  } else {return(plot)}
  
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



#' Correspondance Analysis Plot
#'
#' @param ca matrix or data frame of axis scores for each observation obtained with ade4::dudi.coa
#' @param ca_subset a susbet of variables
#' @param NCS_info dataframe obtain with NCSSDGproj::NCS_info
#' @param colors 3 colors vector for terrestrial, coastal and marine NCS
#' @param ellipse show ellipse default = TRUE
#' @param hull show convexhull default = FALSE
#' @param save save plot default = FALSE
#' 
#'
#' @return a CA plot
#' @export
#'
#' @examples
plot_CorresAna <- function(ca, ca_subset, NCS_info, colors, ellipse = TRUE, hull = FALSE, save = FALSE){
  
  ## Arrow format
  arrow = arrow(angle=13, type = "closed", length = unit(0.75, "cm"), ends = "last")
  
  
  ## Plot
  CA_plot <- ggord::ggord(ord_in = ca, 
                          grp_in = factor(NCS_info$group, levels = unique(NCS_info$group)),
                          ellipse = ellipse,
                          hull = hull,
                          cols = colors,
                          var_sub = ca_subset,
                          txt = 5.5,
                          repel = TRUE,
                          obslab = FALSE, 
                          grp_title = NULL) +
    
    ggplot2::geom_segment(mapping = ggplot2::aes(x = -2, y = 1.75, xend = 2, yend = 1.75),
                          arrow = arrow, 
                          color = "#B2182B", 
                          linejoin = "mitre",
                          lwd = 1.0, 
                          show.legend = NA) +
    
    ggplot2::annotate(geom ="text", 
                      x = 0, 
                      y = 1.85, 
                      label = "Land-oceans continuum", 
                      color = "#B2182B", 
                      size = 5) +
    
    ggplot2::theme(legend.position = "top", 
                   legend.text = ggplot2::element_text(size = 18),
                   axis.text = ggplot2::element_text(size = 15),
                   axis.title = ggplot2::element_text(size = 16))  # legend position)
  
  
  ## Save plot
  if(save == TRUE) {
    
    save(CA_plot, file = here::here("results", "CorrespAnalysis_plot.RData"))
    ggplot2::ggsave(here::here("figures", "CorrespAnalysis_plot.png"), width = 8.5, height = 8.5, device = "png")
    
  } else {return(CA_plot)}
  
}

