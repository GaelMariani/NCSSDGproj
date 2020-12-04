
#' Railway Coordinates Spread Nodes Equally
#'
#' @param mymat a weighted or binary matrix with SDG in column and NCS in row
#' @param maxX 
#' @param maxY 
#'
#' @return a 2 columns matrix with coordinates of the nodes
#' @export
#'
#' @examples
coords <- function (mymat, maxX, maxY) {
  
  ## Coords for mode "P"
  coordP <- cbind(rep(-1*maxX, dim(mymat)[1]), seq(from=0, to=maxY, by= maxY/(dim(mymat)[1]-1)))
  ## Coords for mode "A"
  
  coordA <- cbind(rep(maxX, dim(mymat)[2]), seq(from=0, to=maxY, by = maxY/(dim(mymat)[2]-1)))
  mylayout <- as.matrix(rbind(coordP, coordA))
  
  return(mylayout) 
}


#' Scale Edges Size
#'
#' @param matrix a weighted or binary matrix 
#' @param x a number 
#'
#' @return  a numeric vector of each edge size scaled as a linear function of the number of link
#' @export
#'
#' @examples
edge_size <- function(matrix, x) {
  
  if(!is.matrix(matrix)) as.matrix(matrix)
  # Transpose.
  M <- t(matrix)
  # Edge list and weights.
  M <- cbind(expand.grid(dimnames(M))[2:1], as.vector(M))
  # Discard null weights.
  M <- subset(M, M[, 3] != 0)
  # Scaled weights.
  M.scaled <- x*(M[, 3] + 1) / max((M[, 3] + 1))
  # Vector of edge weights.
  return(M.scaled) # A numeric vector with scaled edge lengths.
}


#' Set Edges Color
#'
#' @param matrix a weighted or binary matrix with SDG in column and NCS in row
#'
#' @return edge color for each type of NCS
#' @export
#'
#' @examples
edge_col <- function(matrix) {
  
  nodes_col <- c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 3))
    #c(rep("#66c2a5", 4), rep("#31859C", 4), rep("#1134A6", 3))
  
  edge.cols <- vector(mode="character", length = 0)
  
  for(i in 1:dim(matrix)[1]) {
    edge.cols <- c(edge.cols, rep(nodes_col[i], sum(matrix[i,]>0)))
  }
  
  return(edge.cols)
  
}



#' Plot Network
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
plot_network <- function(network_obj, matrix, icon_SDG, icon_NCS, nodes_col, save = FALSE) {
  
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


#' Plot Percentage Of Target Achieved
#'
#' @param data_plot a data frame with percentage of target achieve totally + by group of NCS
#' @param save if TRUE the plot is saved in the results folder
#' @param color color for each type of NCS
#' @param legend save the legend of the plot, default = FALSE
#'
#' @return a ggplot object, barplot, of the % of SDG' target achieved
#' @export
#' 
#'
#' @examples
barplot_percSDG <- function(data_plot, color, save = FALSE, legend = FALSE) {
  
  color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                  "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                  "#CF8D2A", "#E11484", "#D3A029", "#007DBC")
  
  order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
  order_group <- rev(c("Terrestrial", "Coastal", "Marine"))
  
  text_plot <-  data_plot[seq(1,46,3),]
  
  
  barplot_pourc <- ggplot2::ggplot() +
    
    ## Plot bars
    ggplot2::geom_col(data = data_plot, 
                      mapping = ggplot2::aes(x = factor(SDG_number, levels = rev(unique(order))), 
                                             y = relative_pourcent,
                                             fill = factor(group, levels = unique(order_group))), 
                      width = 0.65, 
                      alpha = 0.8,
                      show.legend = FALSE) +
   
    ## Add text (number of targets achieved in each SDG)
    ggplot2::geom_text(mapping = ggplot2::aes(x = SDG_number, 
                                              y = perc_goal + 5, 
                                              label = text),
                       nudge_y = 2, 
                       data = text_plot, 
                       size = 5) +
    
    ## scale modif
    ggplot2::scale_fill_manual(values = color , 
                               name = NULL) +
    
    ggplot2::scale_y_continuous(position = "right", 
                                breaks = seq(0, max(data_plot$perc_global), 10), 
                                expand = c(0.03,0,0.1,0)) +
    
    ggplot2::scale_x_discrete(labels = paste(rep("SDG", 11), rev(c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14))), 
                              expand = c(0.03,0.03))  +
    
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(color = rev(color_text), face = "bold"),
                   axis.title = ggplot2::element_text(size = 18),
                   
                   # Legend modifications
                   legend.position = c(0.90, 0.90),
                   legend.text = ggplot2::element_text(size = 16),
                   legend.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
                   
                   # Remove grid on the background
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)) +
    
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  
  ## Save plot
  if(save == TRUE) {
    
    save(barplot_pourc, file = here::here("results", "barplot_pourc.RData"))
    ggplot2::ggsave(here::here("figures", "barplot_pourc.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(barplot)}
  
  
  if(legend == TRUE){
    
    plot_leg <- ggplot2::ggplot() +
      ggplot2::geom_col(data = data_plot, 
                        mapping = ggplot2::aes(x = factor(SDG_number, levels = rev(unique(order))),
                                               y = relative_pourcent,
                                               fill = factor(group, levels = unique(order_group))), 
                        width = 0.65) +
      
      ggplot2::scale_fill_manual(values = color , 
                                 name = NULL) +
      
      ggplot2::theme(legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = 16),
                     legend.background = ggplot2::element_rect(fill = "transparent", 
                                                               color = "transparent")) +
      
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
    
    legend <- ggpubr::get_legend(plot_leg)
    save(legend, file = here::here("results", "legend.RData"))
      
    
  }

}


#' Build Figure Two
#'
#' @return
#' @export
#' 
#'
#' @examples
Figure2 <- function(save = FALSE) {
  
  # Load panels
  fig1a <- NCSSDGproj::load_Fig1A()
  fig1b <- NCSSDGproj::load_Fig1B()
  legend <- NCSSDGproj::load_legend()
  
  # Assemble panels
  fig1 <- cowplot::ggdraw() +
    
    cowplot::draw_plot(fig1a, x=0, y=0.02, width=0.61, height=0.98) +
    cowplot::draw_plot(fig1b, x=0.50, y=0.04, width= 0.5, height=1) +
    cowplot::draw_plot(legend, x=0.3, y=0, width = 0.5, height = 0.05)
  
  # save
  if(save == TRUE) {
    
    ggplot2::ggsave(here::here("figures", "Figure2.png"), width=10, height=9, device="png")   
    
  } else {return(fig1)}
}


#' Plot Modularity
#'
#' @param matrix01 formatted matrix to calculate network indices, nestedness and modularity. Use data_netw_indice to format
#'
#' @return
#' @export
#'
#' @examples
modularity_plot <- function(matrix01) {
  
  
  
  
  
  
}





#' Insurance Plot
#'
#' @param data A data frame with number of times a target is achieved with a column identifying observed data vs. null data
#' @param TI Target Insurance
#' @param TUI_obs Target Under Insurance observed
#' @param TUI_null Target Under Insurance from null matrix
#' @param obs_col color for observed data
#' @param null_col color for null data
#' @param save if TRUE the plot is saved in the results folder
#' 
#' 
#'
#' @return
#' @export
#'
#' @examples
Insurance_plot <- function(data, TI, TUI_obs, TUI_null, obs_col, null_col, save) {
  
  arrow = ggplot2::arrow(angle=15, type = "closed", length = ggplot2::unit(0.1, "inches"))
  
  Insurance_plot <- ggplot2::ggplot(data = data, 
                                    mapping = ggplot2::aes(x = as.numeric(xval), 
                                                           y = value, 
                                                           color = group)) +
    
    ggplot2::geom_ribbon(data = data[1:(nrow(data)/2), ], 
                         mapping = ggplot2::aes(ymin = 0, 
                                                ymax = data$value[1:(nrow(data)/2)]), 
                         color = "transparent", 
                         fill = "#ACACF7") +
    
    ggplot2::geom_hline(yintercept = TI, 
                        color = "grey20", 
                        linetype = "dashed") +
    
    ggplot2::scale_color_manual(values = c(null_col, obs_col), 
                                name = NULL) +
    
    ggplot2::scale_x_continuous(breaks = seq(0, 83, 5), 
                                expand = c(0, 1, 0.1, 0)) +
    
    ggplot2::scale_y_continuous(breaks = seq(0, 11, 1), 
                                expand = c(0, 0, 0.1, 0)) +
    
    
    ggplot2::geom_line() +
    
    ggplot2::geom_segment(mapping = ggplot2::aes(x = as.numeric(min(data_Insurance$xval[data_Insurance$value[1:84] == 1])), 
                                                 y = 6, 
                                                 xend = as.numeric(max(data_Insurance$xval[data_Insurance$value[1:84] == 1])), 
                                                 yend = 6),
                          arrow = arrow, 
                          color = obs_col, 
                          show.legend = NA) +
            
    ggplot2::geom_segment(mapping = ggplot2::aes(x = as.numeric(max(data_Insurance$xval[data_Insurance$value[1:84] == 1])), 
                                                 y = 6, 
                                                 xend = as.numeric(min(data_Insurance$xval[data_Insurance$value[1:84] == 1])), 
                                                 yend = 6),
                          arrow = arrow, 
                          color = obs_col, 
                          show.legend = NA) +
    
    ggplot2::geom_segment(mapping = ggplot2::aes(x = as.numeric(min(data_Insurance$xval[data_Insurance$value[1:84] == 1])), 
                                                 y = 0, 
                                                 xend = as.numeric(min(data_Insurance$xval[data_Insurance$value[1:84] == 1])),
                                                 yend = 6), 
                          color =  obs_col, 
                          linetype = "dashed") +
    
    ggplot2::geom_segment(mapping = ggplot2::aes(x = as.numeric(max(data_Insurance$xval[data_Insurance$value[1:84] == 1])),
                                                 y = 0,
                                                 xend = as.numeric(max(data_Insurance$xval[data_Insurance$value[1:84] == 1])),
                                                 yend = 6), 
                          color =  obs_col, 
                          linetype = "dashed") +
    
    ggplot2::annotate(geom = "text", 
                      x = 78.5, 
                      y = 7.5, 
                      label = "Targets underinsured", 
                      color = "black",
                      fontface = "bold", 
                      size = 5) +
    
    ggplot2::annotate(geom = "text", 
                      x = 75, 
                      y = 6.7, 
                      label = as.character(TUI_obs), 
                      color = "#0000EB", 
                      size = 5) +
            
    ggplot2::annotate(geom = "text", 
                      x = 82, 
                      y = 6.7, 
                      label = as.character(TUI_null), 
                      color = "black", 
                      size = 5) +
    
    ggplot2::labs(x = "Rank of SDGs targets", y = "Number of NCS linked") +
    
    ggplot2::theme_classic() +
    
    ggplot2::theme(legend.position = c(0.75, 0.85),
                   legend.text = ggplot2::element_text(size = 16, face="bold"),
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 16)) 
  
  ## Save plot
  if(save == TRUE) {
    
    save(Insurance_plot, file = here::here("results", "Insurance_plot.RData"))
    ggplot2::ggsave(here::here("figures", "Insurance_plot.png"), width = 10.5, height = 5.5, device = "png")
    
  } else {return(Insurance_plot)}
  
  
  
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


#' Barplot Of NCS Contribution 
#'
#' @param data obrained with NCSSDGproj::CA_contri_vars
#' @param axis 1 or 2 whether you want to plot 1st or 2nd axis
#'
#' @return
#' @export
#'
#' @examples
CA_barplot_NCS <- function(data, axis, colNCS_ter, colNCS_coast, colNCS_mar){
  
  ## Format data
  contrib_NCS <- as.data.frame(data[["row"]][["contrib"]][, c('Dim 1', 'Dim 2')]) %>%
    dplyr::mutate(group = data[["grp"]][, 'group'],
                  NCS = rownames(data[["row"]][["contrib"]])) %>%
    stats::setNames(., c("Dim1", "Dim2", "group", "NCS"))
  
  y <- contrib_NCS[, axis]
  
  ## Plot
  ggplot2::ggplot(data = contrib_NCS) +
    
    ggplot2::geom_col(mapping = ggplot2::aes(x =  reorder(NCS, y), 
                                             y =  y,
                                             fill = group,
                                             color = group),
                      width = 0.75,
                      show.legend = FALSE) +
    
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 100/11), 
                        color = "red",
                        linetype = "dashed") +
    
    ggplot2::scale_fill_manual(values = scales::alpha(c(colNCS_coast, colNCS_mar, colNCS_ter), 0.7)) +
    ggplot2::scale_color_manual(values = c(colNCS_coast, colNCS_mar, colNCS_ter)) +
    
    
    ggplot2::labs(title = "CA axis 1", x = NULL, y = "% contribution") +
    
    #ggplot2::coord_flip() +
    ggplot2::coord_polar() +
    ggplot2::theme_minimal()
}


#' Separate Correspondance Analysis
#'
#' @param data obtained with NCSSDGproj::CA_contri_vars
#' @param colNCS_ter 
#' @param colNCS_coast 
#' @param colNCS_mar 
#' @param save 
#' @param targ_contrib12 
#' @param NCScontrib12 
#' @param data_arrow 
#'
#' @return
#' @export
#'
#' @examples
CA_contrib_plot <- function(data, targ_contrib12, NCScontrib12, data_arrow, colNCS_ter, colNCS_coast, colNCS_mar, save = FALSE){
  
  ### Plot NCS from CA analysis
  
  arrow = ggplot2::arrow(angle=13, type = "closed", length = ggplot2::unit(0.75, "cm"), ends = "last")
  
    ## Plot CA for NCS points
    ca_NCS_12 <- factoextra::fviz_ca_row(X = data,
                                         axes = c(1,2),
                                         title = "",
                                         pointsize = 3,
                                         habillage = data[["grp"]]$group,
                                         palette = c(colNCS_coast, colNCS_mar, colNCS_ter),
                                         repel = TRUE,
                                         invisible = "quali") +
      
      # Arrows
      ggplot2::geom_segment(data = data_arrow,
                            mapping = ggplot2::aes(x = x,
                                                   xend = xmax,
                                                   y = y,
                                                   yend = ymax),
                            arrow = arrow, 
                            color = data_arrow$color, 
                            linejoin = "mitre",
                            lwd = 1.0, 
                            show.legend = NA) +
      
      # Text above arrows
      ggplot2::annotate(geom ="text", 
                        x = c(median(data_arrow$x[1:2]), median(data_arrow$x[3:4]), median(data_arrow$x[5:6])), 
                        y = rep(0.85, 3), 
                        label = data_arrow$text[c(1,3,5)], 
                        color = data_arrow$color[c(1,3,5)], 
                        size = 5) +
      
      ggplot2::ggtitle(NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") 
    
    ## Barplot of contribution for axes 1 
    NCS_axis1 <- NCSSDGproj::CA_barplot_NCS(data = data, 
                                            axis = 1, 
                                            colNCS_ter, 
                                            colNCS_coast,
                                            colNCS_mar)
    
    ## Barplot of contribution for axes 2
    NCS_axis2 <- NCSSDGproj::CA_barplot_NCS(data = data, 
                                            axis = 2, 
                                            colNCS_ter, 
                                            colNCS_coast,
                                            colNCS_mar)
  
  
  ### Plot the most important targets
  ca_SDG_12 <- factoextra::fviz_ca_col(X = data, 
                                       axes = c(1,2),
                                       col.col = "contrib",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                       select.col = list(name = targ_contrib12),
                                       repel = TRUE) +
    
    ggplot2::ggtitle(NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  
  ca_SDG_12
  
}



#' Insurance Circular Plot
#'
#' @param data 
#' @param label_data 
#' @param base_data 
#' @param grid_data 
#'
#' @return
#' @export
#'
#' @examples
circular_plot_Insurance <- function(data, label_data, base_data, grid_data, SDG_info, colNCS_ter, colNCS_coast, colNCS_mar, save = FALSE){
  
  col <- SDG_info %>%
    dplyr::mutate(SDG = as.numeric(SDG)) %>%
    dplyr::group_by(SDG) %>%
    dplyr::summarise(color = unique(color)) 
 

  plot <- ggplot2::ggplot(data = data, 
                       mapping = ggplot2::aes(x = as.factor(id), 
                                              y = as.numeric(value_group), 
                                              fill = factor(group)),
                       show.legend = FALSE) +
    
    ggplot2::geom_bar(mapping = ggplot2::aes(x = as.factor(id), 
                                             y = as.numeric(value_group), 
                                             group = factor(group)),
                      stat = "identity",
                      alpha = 0.5,
                      show.legend = FALSE) +
    
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end, 
                                                 y = 10, 
                                                 xend = start, 
                                                 yend = 10), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.3 , 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end,
                                                 y = 8,
                                                 xend = start,
                                                 yend = 8),
                          colour = "grey",
                          alpha = 1,
                          size = 0.3,
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end, 
                                                 y = 6,
                                                 xend = start,
                                                 yend = 6), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.3, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end, 
                                                 y = 4, 
                                                 xend = start, 
                                                 yend = 4), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.3, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end, 
                                                 y = 2, 
                                                 xend = start, 
                                                 yend = 2), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.3, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data = grid_data, 
                          mapping = ggplot2::aes(x = end, 
                                                 y = 0, 
                                                 xend = start, 
                                                 yend = 0), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.3, 
                          inherit.aes = FALSE) +
    
    # Add text showing the value of each 100/75/50/25 lines
    ggplot2::annotate(geom = "text", 
                      x = rep(max(data$id), 6), 
                      y = c(10, 8, 6, 4, 2, 0), 
                      label = c("10", "8", "6", "4", "2", "0"), 
                      color = "grey", 
                      size = 3,
                      angle = 0, 
                      fontface = "bold", 
                      hjust = 1) +
      
    ggplot2::geom_bar(mapping = ggplot2::aes(x = as.factor(id), 
                                             y = value_group, 
                                             fill = as.factor(group)), 
                      stat = "identity", 
                      alpha=0.5) +
    
    ggplot2::ylim(-20, 12) +
    
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(-1,4), "cm")) +
    
    ggplot2::coord_polar() +
    
    ggplot2::geom_text(data = label_data,
                       mapping = ggplot2::aes(x = id, 
                                              y = tot + 0.5, 
                                              label = goal.target, 
                                              hjust = hjust), 
                       color = "black", 
                       fontface = "bold",
                       alpha = 0.6, 
                       size = 3.5, 
                       angle = label_data$angle, 
                       inherit.aes = FALSE ) +
    
    # Add base line information
    ggplot2::geom_segment(data = base_data, 
                          mapping = ggplot2::aes(x = start, 
                                                 y = -1, 
                                                 xend = end, 
                                                 yend = -1), 
                          colour = col$color, 
                          alpha = 0.8, 
                          size = 1, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_text(data = base_data, 
                       mapping = ggplot2::aes(x = title, 
                                              y = -3.5, 
                                              label = SDG), 
                       colour = col$color, 
                       alpha = 0.8, 
                       size = 4, 
                       fontface = "bold", 
                       inherit.aes = FALSE) +
    
    ggplot2::scale_fill_manual(values = c(colNCS_coast, colNCS_mar, colNCS_ter), 
                               name = NULL)
  
  
  ## Add SDG icons
  plot <- cowplot::ggdraw(plot)
  
  circular_plot <- plot +
    cowplot::draw_grob(icon_SDG[[9]], x = 0.504, y = 0.7, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[15]], x = 0.543, y = 0.666, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[6]], x = 0.578, y = 0.595, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[10]], x = 0.594, y = 0.522, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[5]], x = 0.593, y = 0.46, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[2]], x = 0.588, y = 0.40, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[1]], x = 0.569, y = 0.345, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[11]], x = 0.535, y = 0.288, width = 0.04, height = 0.04) + 
    cowplot::draw_grob(icon_SDG[[8]], x = 0.493, y = 0.263, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[14]], x = 0.461, y = 0.265, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[4]], x = 0.422, y = 0.293, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[13]], x = 0.385, y = 0.368, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[7]], x = 0.37, y = 0.43, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[16]], x = 0.367, y = 0.528, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[3]], x = 0.407, y = 0.649, width = 0.04, height = 0.04) +
    cowplot::draw_grob(icon_SDG[[12]], x = 0.455, y = 0.696, width = 0.04, height = 0.04) 
  
  
  ## Save plot
  if(save == TRUE) {
    
    save(circular_plot, file = here::here("results", "circular_plot.RData"))
    ggplot2::ggsave(here::here("figures", "circular_plot.png"), width = 8.5, height = 8.5, device = "png")
    
  } else {return(circular_plot)}

}


  