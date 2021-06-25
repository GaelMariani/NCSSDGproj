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



#' Supplementary Fig 1
#'
#' @param data obtained with NCSSDGproj::CA_contri_vars
#' @param arrow TRUE if arrow must be plotted
#' @param data_arrow if TRUE, a df specifying the position of each arrow
#' @param colNCS_ter color for terrestrial ecosystems
#' @param colNCS_coast color for coastal ecosystems
#' @param colNCS_mar color for marine ecosystems
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved
#'
#'
#' @return the Supplementary Fig 1 in the paper
#' @export
#'
#' @examples
supp_fig1 <- function(data, arrow, data_arrow, colNCS_ter, colNCS_coast, colNCS_mar, save = FALSE, name){
  
  ### Legend
  legend <- NCSSDGproj::load_legend()
  
  # CA_legend <- NCSSDGproj::legend_CA(data = data)
  # CA_legend <- NCSSDGproj::load_CA_legend()
  
  ### Plot NCS from CA analysis
  
  
  ## Plot CA for NCS points
  ca_NCS_12 <- factoextra::fviz_ca_row(X         = data,
                                       axes      = c(1,2),
                                       title     = "",
                                       pointsize = 3,
                                       habillage = data[["grp"]]$group,
                                       palette   = c(colNCS_coast, colNCS_mar, colNCS_ter),
                                       repel     = TRUE,
                                       invisible = "quali") +
    
    
    ggplot2::ggtitle(NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") 
  
  if(arrow == TRUE){
    
    arrow <- ggplot2::arrow(angle  = 20, 
                            type   = "closed", 
                            length = ggplot2::unit(0.3, "cm"), 
                            ends   = "last")
    
    ca_NCS_12 <- ca_NCS_12 +  
      
      # Arrows
      ggplot2::geom_segment(data        = data_arrow,
                            mapping     = ggplot2::aes(x    = x,
                                                       xend = xmax,
                                                       y    = y,
                                                       yend = ymax),
                            arrow       = arrow, 
                            color       = data_arrow$color, 
                            linejoin    = "mitre",
                            lwd         = 1.0, 
                            show.legend = NA) +
      
      # Text above arrows
      ggplot2::annotate(geom  ="text", 
                        x     = c(median(data_arrow$x[1:2]), median(data_arrow$x[3:4]), median(data_arrow$x[5:6])), 
                        y     = c(rep(1.1, 2), 1.05), 
                        label = data_arrow$text[c(1,3,5)], 
                        color = data_arrow$color[c(1,3,5)], 
                        size  = 4.5) 
  } 
  
  ## Barplot of contribution for axis 1 
  NCS_axis1 <- NCSSDGproj::CA_barplot(data     = data, 
                                      axis     = 1, 
                                      variable = "row",
                                      ymin     = -50,
                                      # ymax     = 58,
                                      ytitle   = -50)
  
  ## Barplot of contribution for axis 2
  NCS_axis2 <- NCSSDGproj::CA_barplot(data     = data, 
                                      axis     = 2, 
                                      variable = "row",
                                      ymin     = -50,
                                      # ymax     = 59,
                                      ytitle   = -50)
  
  
  ### Plot the most important targets
  # all_targ <- as.data.frame(data[["col"]][["coord"]]) %>%
  #   dplyr::mutate(target = rownames(.)) %>%
  #   dplyr::select(c(1:2, 6)) %>%
  #   stats::setNames(c("Coord1", "Coord2", "target"))
  # 
  # contrib_target <- NCSSDGproj::SDG_contrib_tbl() %>%
  #   dplyr::right_join(., all_targ, by = "target")
  # 
  # contrib_target$Color_CA[is.na(contrib_target$Color_CA)] <- "grey90"
  # contrib_target$Type_CA[is.na(contrib_target$Type_CA)] <- "below expected"
  # 
  # data[["grp_targ"]] <- contrib_target
  # 
  #   ## CA plot
  #   ca_SDG_12 <- ggplot2::ggplot(data    = contrib_target,
  #                                mapping = ggplot2::aes(x     = Coord1,
  #                                                       y     = Coord2,
  #                                                       group = Type_CA)) +
  # 
  #     ggplot2::geom_point(shape = 17,
  #                         color = contrib_target$Color_CA) +
  # 
  #     ggrepel::geom_text_repel(mapping = ggplot2::aes(label = ifelse(Type_CA != "below expected", target, ""),
  #                                                     group = Type_CA),
  #                              color   = contrib_target$Color_CA) +
  # 
  #     ggplot2::geom_hline(yintercept = 0,
  #                         linetype   = "dashed") +
  # 
  #     ggplot2::geom_vline(xintercept = 0,
  #                         linetype   = "dashed") +
  # 
  #     ggplot2::labs(x = "Dim 1 (28.8%)",
  #                   y = "") +
  # 
  #     ggplot2::theme_bw()
  # 
  # 
  #   ## Circular plot axis 1
  #   SDG_axis1 <- NCSSDGproj::CA_barplot(data = data,
  #                                       axis = 1,
  #                                       variable = "col",
  #                                       ymin = -5,
  #                                       # ymax = 6.5,
  #                                       ytitle = -5)
  # 
  #   ## Circular plot axis 2
  #   SDG_axis2 <- NCSSDGproj::CA_barplot(data = data,
  #                                       axis = 2,
  #                                       variable = "col",
  #                                       ymin = -5,
  #                                       # ymax = 6.5,
  #                                       ytitle = -5)
  
  ### Arrange plots together
  supp_fig <- cowplot::ggdraw() +
    cowplot::draw_plot(ca_NCS_12, x = 0.1, y = 0.5, width = 0.8, height = 0.5) +
    # cowplot::draw_plot(ca_SDG_12, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
    cowplot::draw_plot(NCS_axis1, x = 0.20, y = 0.06, width = 0.30, height = 0.47) +
    cowplot::draw_plot(NCS_axis2, x = 0.5, y = 0.06, width = 0.32, height = 0.47) +
    # cowplot::draw_plot(SDG_axis1, x = 0.5, y = 0.029, width = 0.22, height = 0.53) +
    # cowplot::draw_plot(SDG_axis2, x = 0.75, y = 0.029, width = 0.22, height = 0.53) +
    cowplot::draw_plot(legend, x = 0.25, y = 0, width = 0.5, height = 0.1) +
    cowplot::draw_plot_label(label = c("a", "b", "c"),
                             size = 15,
                             x = c(0.08, 0.18, 0.5),
                             y = c(1, 0.45, 0.45))
  
  
  supp_fig
  
  ### Save plot
  if(save == TRUE) {
    
    save(supp_fig, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 12, height = 8.5, device = "png")
    
  } else {return(supp_fig)}
  
}



#' Correspondance Analysis Of Variable Contribution
#' 
#' @param matrix_cont a matrix with NCS in rows and SDG targets in columns - use contingency_mat_targets
#' @param colNCS_ter color for terrestrial ecosystems
#' @param colNCS_coast color for coastal ecosystems
#' @param colNCS_mar color for marine ecosystems
#'
#' @return a list of three files, with CA analysis results, contribution of row to the variance and the data to add arrow on the plot
#' @export
#'
#' @examples
CA_contri_vars_unused <- function(matrix_cont, colNCS_ter, colNCS_coast, colNCS_mar){
  
  # ### Correspondance Analysis on the matrix
  res.ca <- FactoMineR::CA(matrix_cont, graph = FALSE)
  res.ca[["grp"]] <- NCSSDGproj::NCS_info(matrix_cont)
  # 
  # ### Contribution of columns (targets) to the variance of the different axis
  # col_contrib <- as.data.frame(factoextra::get_ca_col(res.ca)[["contrib"]]) %>%
  #   dplyr::mutate(target = as.factor(rownames(.)))
  # 
  #   ## select rownames of the most contributing targets (those with a contribution significantly higher than expected)
  #   col_expect_contrib <- 100/ncol(matrix_cont)
  # 
  #     # 1st axis
  #     name1 <- as.character(col_contrib$target[col_contrib[,1] >= col_expect_contrib])
  #     # 2nd axis
  #     name2 <- as.character(col_contrib$target[col_contrib[,2] >= col_expect_contrib])
  #     # 3rd axis
  #     name3 <- as.character(col_contrib$target[col_contrib[,3] >= col_expect_contrib])
  #     # 4th axis
  #     name4 <- as.character(col_contrib$target[col_contrib[,4] >= col_expect_contrib])
  #     
  #     # 1st and 2nd axis together
  #     col_names12 <- data.frame(target = unique(c(name1, name2))) %>%
  #       dplyr::left_join(., col_contrib[, c(1:2, 6)], by = "target")
  #     
  #     save(col_names12, file = here::here("rawdata", "col_names12.RData"))
  #       
  #     col_names34 <- unique(c(name3, name4))
  #     
  #   ## TOP 20 most contributing targets on axis 1 and 2
  #     
  #     # Axis 1
  #     TOP20_axis1 <- col_contrib %>%
  #       dplyr::arrange(dplyr::desc(col_contrib$`Dim 1`)) %>%
  #       dplyr::top_n(20, wt = `Dim 1`) %>%
  #       dplyr::select(c("Dim 1")) %>%
  #       stats::setNames("Dim") %>%
  #       dplyr::mutate(target = rownames(.)) %>%
  #       dplyr::right_join(., axis2_targ[1:20,], by = "target")
  #     
  #     # Axis 2
  #     TOP20_axis2 <- col_contrib %>%
  #       dplyr::arrange(dplyr::desc(col_contrib$`Dim 2`)) %>%
  #       dplyr::top_n(20, wt = `Dim 2`) %>%
  #       dplyr::select(c("Dim 2")) %>%
  #       stats::setNames("Dim") %>%
  #       dplyr::mutate(target = rownames(.)) %>%
  #       dplyr::right_join(., axis2_targ[21:40,], by = "target")
  
  ### Contribution of rows (NCS) to the variance of each axis
  row_contrib <- as.data.frame(factoextra::get_ca_row(res.ca)[["contrib"]])
  
  ## select rownames of the most contributing targets (those with a contribution higher than expected)
  row_expect_contrib <- 100/nrow(matrix_cont)
  
  # 1st axis 
  name1_r <- rownames(row_contrib[row_contrib[,1] >= row_expect_contrib,])
  # 2nd axis
  name2_r <- rownames(row_contrib[row_contrib[,2] >= row_expect_contrib,])
  
  row_names12 <- unique(c(name1_r, name2_r))
  
  ### Add a row because seagrass do not have negative interactions.    
  if(nrow(res.ca[["row"]][["coord"]]) == 10){
    
    tmp <- res.ca[["row"]][["coord"]]
    
    new_mat <- matrix(NA, nrow = 11, ncol = ncol(tmp), dimnames = list(c("Urban forest", "Forest", "Peatland", "Grassland", "Seagrass", "Mangrove", "Tidalmarsh", "Macroalgae", "Pelagic area", "Antarctic", "Mesopelagic area"),
                                                                       dimnames(tmp)[[2]]))
    new_mat[-5,] <- tmp
    
    res.ca[["row"]][["coord"]] <- new_mat
  }
  
  ### Format data to draw arrows on plot
  data_arrow <- data.frame(y     = c(rep(1.02, 4), 0.95, 0.95),
                           ymax  = c(rep(1.02, 4), 0.95, 0.95),
                           x     = c(min(res.ca[["row"]][["coord"]][9:11, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][9:11, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     max(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     min(res.ca[["row"]][["coord"]][1:4, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][1:4, "Dim 1"])),
                           
                           xmax  = c(max(res.ca[["row"]][["coord"]][9:11, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][9:11, "Dim 1"]),
                                     max(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     min(res.ca[["row"]][["coord"]][5:8, "Dim 1"], na.rm = TRUE),
                                     max(res.ca[["row"]][["coord"]][1:4, "Dim 1"]),
                                     min(res.ca[["row"]][["coord"]][1:4, "Dim 1"])),
                           color = c(rep(colNCS_mar, 2), rep(colNCS_coast, 2), rep(colNCS_ter, 2)),
                           text  = c(rep("Marine NCS", 2), rep("Coastal NCS", 2), rep("Terrestrial NCS", 2)))
  
  ### Remove the sup row added above 
  if(sum(is.na(res.ca[["row"]][["coord"]])) > 0){
    res.ca[["row"]][["coord"]] <-  res.ca[["row"]][["coord"]][-5,]
  }
  
  ### Save data
  CA_contrib <- list("CorresAna"        = res.ca,
                     # "TOP20_axis1_targ" = TOP20_axis1,
                     # "TOP20_axis2_targ" = TOP20_axis2,
                     # "col_contrib"      = list("tot"   = col_contrib, 
                     #                           "axe12" = col_names12, 
                     #                           "axe34" = col_names34),
                     "row_contrib"      = list("tot"   = row_contrib,
                                               "axe12" = row_names12),
                     "data_arrow"       = data_arrow)
  
  return(CA_contrib)
  
}      


