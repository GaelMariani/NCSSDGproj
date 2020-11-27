
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
  
  nodes_col <- c(rep("#66c2a5", 4), rep("#e5c494", 4), rep("#8da0cb", 3))
  
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
                         edge.alpha= 0.40,
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
                      size = 3.3, 
                      fontface = "bold") +
    
  
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(), 
                   legend.position = "none") 
  
  ## Save plot
  if(save == TRUE) {
    
    save(netw, file = here::here("results", "network_SDG_NCS.RData"))
    ggplot2::ggsave(here::here("results", "network_SDG_NCS.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(netw)}
  
}


#' Plot Percentage Of Target Achieved
#'
#' @param data_plot a data frame with percentage of target achieve totally + by group of NCS
#' @param save if TRUE the plot is saved in the results folder
#' 
#' @import ggplot2
#'
#' @return a ggplot object, barplot, of the % of SDG' target achieved
#' @export
#' 
#'
#' @examples
barplot_percSDG <- function(data_plot, save = FALSE, legend = FALSE) {
  
  color <- c("#8da0cb", "#e5c494","#66c2a5")
  color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                  "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                  "#CF8D2A", "#E11484", "#D3A029", "#007DBC")
  
  order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
  order_group <- rev(c("Terrestrial", "Coastal", "Marine"))
  
  text_plot <-  data_pour[seq(1,46,3),]
  
  
  barplot <- ggplot2::ggplot() +
    
    ## Plot bars
    ggplot2::geom_col(data = data_pour, 
                      mapping = aes(x = factor(SDG_number, levels = rev(unique(order))), 
                                    y = relative_pourcent,
                                    fill = factor(group, levels = unique(order_group))), 
                      width = 0.65, 
                      show.legend = FALSE) +
   
    ## Add text (number of targets achieved in each SDG)
    ggplot2::geom_text(mapping = aes(x = SDG_number, 
                                     y = perc_goal + 5, 
                                     label = text),
                       nudge_y = 2, 
                       data = text_plot, 
                       size = 5) +
    
    ## scale modif
    ggplot2::scale_fill_manual(values = color , 
                               name = NULL) +
    
    ggplot2::scale_y_continuous(position = "right", 
                                breaks = seq(0, max(data_pour$perc_global), 10), 
                                expand = c(0.03,0,0.1,0)) +
    
    ggplot2::scale_x_discrete(labels = paste(rep("SDG", 11), rev(c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14))), 
                              expand = c(0.03,0.03))  +
    
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text = element_text(size = 12),
                   axis.text.y = element_text(color = rev(color_text), face = "bold"),
                   axis.title = element_text(size = 18),
                   
                   # Legend modifications
                   legend.position = c(0.90, 0.90),
                   legend.text = element_text(size = 16),
                   legend.background = element_rect(fill = "transparent", color = "transparent"),
                   
                   # Remove grid on the background
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_rect(fill = "transparent", colour = NA)) +
    
    ggplot2::guides(fill = guide_legend(reverse = TRUE))
  
  ## Save plot
  if(save == TRUE) {
    
    save(barplot, file = here::here("results", "barplot_pourc.RData"))
    ggplot2::ggsave(here::here("figures", "barplot_pourc.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(barplot)}
  
  
  if(legend == TRUE){
    
    plot_leg <- ggplot2::ggplot() +
      geom_col(data = data_pour, 
               mapping = aes(x = factor(SDG_number, levels = rev(unique(order))),
                             y = relative_pourcent,
                             fill = factor(group, levels = unique(order_group))), 
               width = 0.65) +
      
      ggplot2::scale_fill_manual(values = color , 
                                 name = NULL) +
      
      ggplot2::theme(legend.position = "bottom",
                     legend.text = element_text(size = 16),
                     legend.background = element_rect(fill = "transparent", 
                                                      color = "transparent")) +
      
      ggplot2::guides(fill = guide_legend(reverse = TRUE))
    
    legend <- ggpubr::get_legend(plot_leg)
    save(legend, file = here::here("results", "legend.RData"))
      
    
  }

}


#' Build Figure One
#'
#' @return
#' @export
#'
#' @examples
Figure1 <- function(save = FALSE) {
  
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
    
    ggplot2::ggsave(here::here("figures", "Fig1.png"), width=10, height=9, device="png")   
    
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
#' @return
#' @export
#'
#' @examples
Insurance_plot <- function(data, TI, TUI_obs, TUI_null, obs_col, null_col, save) {
  
  ggplot(data = data, 
         mapping = aes(x = as.numeric(xval), 
                       y = value, 
                       color = group)) + 
    
    geom_ribbon(data = data[1:(nrow(data)/2), ], 
                mapping = aes(ymin = 0, 
                              ymax = data_obs$value), 
                color = "transparent", 
                fill = "#ACACF7") +
    
    geom_hline(yintercept = TI, 
               color = "grey20", 
               linetype = "dashed") +
    
    scale_color_manual(values = c(null_col, obs_col), 
                       name = NULL)+
    
    scale_x_continuous(breaks = seq(0, 83, 5), 
                       expand = c(0, 1, 0.1, 0))+
    
    scale_y_continuous(breaks = seq(0, 11, 1), 
                       expand = c(0, 0, 0.1, 0)) +
    
    geom_line() +
    
    geom_segment(mapping = aes(x = 74, y = 6, xend = 83, yend = 6),
                 arrow = arrow, 
                 color = obs_col, 
                 show.legend = NA) +
    
    geom_segment(mapping = aes(x = 83, y = 6, xend = 74, yend = 6),
                 arrow = arrow, 
                 color = obs_col, 
                 show.legend = NA) +
    
    geom_segment(mapping = aes(x = 74, y = 0, xend = 74, yend = 6), 
                 color =  obs_col, 
                 linetype = "dashed") +
    
    geom_segment(mapping = aes(x = 83, y = 0, xend = 83, yend = 6), 
                 color =  obs_col, 
                 linetype = "dashed") +
    
    annotate(geom = "text", 
             x = 78.5, 
             y = 7.5, 
             label = "Targets underinsured", 
             color = "black",
             fontface = "bold", 
             size = 5) +
    
    annotate(geom = "text", 
             x = 75, 
             y = 6.7, 
             label = as.character(TUI_obs), 
             color = "#0000EB", 
             size = 5) +
    
    annotate(geom = "text", 
             x = 82, 
             y = 6.7, 
             label = as.character(TUI_null), 
             color = "black", 
             size = 5) +
    
    labs(x = "Rank of SDGs targets", y = "Number of NCS linked") +
    
    theme_classic() +
    
    theme(legend.position = c(0.75, 0.85),
          legend.text = element_text(size = 16, face="bold"),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) 
  
  ## Save plot
  if(save == TRUE) {
    
    save(plot, file = here::here("results", "barplot_pourc.RData"))
    ggplot2::ggsave(here::here("figures", "barplot_pourc.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(plot)}
  
  
  
}


#' Unipartite Plot Of Targets 
#'
#' @param netw a dataframe network object from the network_uniP function
#' @param colNCS_ter color for terrestrial nodes
#' @param colNCS_coast color for coastal nodes
#' @param colNCS_mar color for marine nodes
#' @param save if TRUE the plot is saved in the results folder
#'
#' @return
#' @export
#'
#' @examples
unipart_plot <- function(netw, colNCS_ter, colNCS_coast, colNCS_mar, save){
  
  plot <- ggplot() +
    
    # Plot edges
    geom_edges(data = netw,
               mapping = aes(x = x, y = y, xend = xend, yend = yend, color = color),
               curvature = 0, 
               size = 1, 
               alpha = 0.25) +
    
    # Format edges
    scale_color_manual(values = c(colNCS_ter, colNCS_mar, colNCS_coast),
                       labels = c("Terrestrial", "Marine", "Coastal"),
                       name = NULL) +
    
    # Plot nodes 
    geom_nodes(data = netw, 
               mapping = aes(x=x, y=y), 
               size = 5) +
    
    # Format nodes
    geom_node_point(data = netw,
                    size = 6,
                    color = netw_target$color,
                    fill = netw_target$color) +
    
    theme_void() +
    theme(legend.position=c(0.1, 0.7),
          legend.text = element_text(size = 15)) 
  
  
  ## Save plot
  if(save == TRUE) {
    
    save(plot, file = here::here("results", "barplot_pourc.RData"))
    ggplot2::ggsave(here::here("results", "barplot_pourc.png"), width = 5, height = 6.8, device = "png")
    
  } else {return(plot)}
  
}


  
  