#' Railway Coordinates Spread Nodes Equally
#'
#' @param mymat a weighted contingency matrix with SDG in columns and NCS in rows - use matrix_SDG
#' @param maxX a number
#' @param maxY a number
#'
#' @return a 2 columns matrix with coordinates of each nodes
#' @export
#'
#' @examples
coords <- function(mymat, maxX, maxY) {
  
  ## Coords for mode "P"
  coordP <- cbind(rep(-1*maxX, dim(mymat)[1]), seq(from=0, to=maxY, by= maxY/(dim(mymat)[1]-1)))
  ## Coords for mode "A"
  
  coordA <- cbind(rep(maxX, dim(mymat)[2]), seq(from=0, to=maxY, by = maxY/(dim(mymat)[2]-1)))
  mylayout <- as.matrix(rbind(coordP, coordA))
  
  return(mylayout) 
}


#' Scale Edges Size
#'
#' @param matrix a weighted contingency matrix with SDG in columns and NCS in rows - use matrix_SDG
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
#' @param matrix a weighted contingency matrix with SDG in columns and NCS in rows - use matrix_SDG
#'
#' @return edge color for each type of NCS
#' @export
#'
#' @examples
edge_col <- function(matrix, neg = TRUE) {
  
  nodes_col <- c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 4)) # terr, coast, marine
    #c(rep("#66c2a5", 4), rep("#31859C", 4), rep("#1134A6", 3))
  
  if(neg == TRUE){
    
  edge.cols <- vector(mode = "character", length = 0)
  
    for(i in 1:dim(matrix)[1]) {
      edge.cols <- c(edge.cols, rep(nodes_col[i], sum(matrix[i,]>0)))
    }
  
  } else {
    
    matrix_col <- matrix(NA, nrow = nrow(matrix), ncol = ncol(matrix))
    
    for(i in 1:nrow(matrix)){
      
      for(j in 1:ncol(matrix)){
        
        matrix_col[i,j] <- ifelse(matrix[i, j] > 0, nodes_col[j], NA)
        
      } # end FOR j
      
    } # end FOR i 
  
  edge.cols <- as.vector(t(matrix_col))
  edge.cols <- edge.cols[!is.na(edge.cols)]
    
  } # end ELSE

  return(edge.cols)
  
}


#' Plot Network As Flux Diagramm For Positive Links
#'
#' @param network_obj a network object - use matrix_to_network
#' @param matrix a weighted contingency matrix with SDG in columns and NCS in rows - use matrix_SDG
#' @param icon_SDG a list of 16 rastergrob objects for each SDG - use format_icons
#' @param icon_NCS a list of 11 rastergrob objects for each NCS - use format_icons
#' @param nodes_col a character vector specifying the color of nodes
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved
#' 
#'
#' @return a flux diagramm of the positive links between SDG and NCS
#' @export
#' 
#'
#' @examples
plot_network_pos <- function(network_obj, matrix, icon_SDG, icon_NCS, nodes_col, save = FALSE, name) {
  
  ## Plot the network
  netw <- GGally::ggnet2(net        = network_obj, 
                         mode       = NCSSDGproj::coords(mymat = matrix, maxX = 6, maxY = 15),
                         label      = FALSE,
                         shape      = "shape",
                         size       = 0.1,
                         max_size   = 18, 
                         label.size = 2,
                         edge.size  = NCSSDGproj::edge_size(matrix, 5)/1.3, 
                         edge.alpha = 0.4,
                         color      = rep("white", 28),
                         edge.color = NCSSDGproj::edge_col(matrix, neg = FALSE),
                         layout.exp = 0.25) +
    
    ## Add silhouette of SDG (xmax = 1.1 to plot with barplot)
    ggplot2::annotation_custom(icon_SDG[[1]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = 1.05) +
    ggplot2::annotation_custom(icon_SDG[[2]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .917) +
    ggplot2::annotation_custom(icon_SDG[[3]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .784) +
    ggplot2::annotation_custom(icon_SDG[[4]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .651) +
    ggplot2::annotation_custom(icon_SDG[[5]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .518) +
    ggplot2::annotation_custom(icon_SDG[[6]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .385) +
    ggplot2::annotation_custom(icon_SDG[[7]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .252) +
    ggplot2::annotation_custom(icon_SDG[[8]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = .119) +
    ggplot2::annotation_custom(icon_SDG[[9]],  xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.0178) +
    ggplot2::annotation_custom(icon_SDG[[10]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.15) +
    ggplot2::annotation_custom(icon_SDG[[11]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.283) +
    ggplot2::annotation_custom(icon_SDG[[12]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.416) +
    ggplot2::annotation_custom(icon_SDG[[13]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.549) +
    ggplot2::annotation_custom(icon_SDG[[14]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.682) +
    ggplot2::annotation_custom(icon_SDG[[15]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.815) +
    ggplot2::annotation_custom(icon_SDG[[16]], xmin = -0.12, xmax = 0.05, ymin = -Inf, ymax = -.948) +
    
    ## Add silhouette for NCS (xmin=-0.75 (-0.1 for peatland) to plot without barplot_percent) +0.1
    ggplot2::annotation_custom(icon_NCS[[1]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = 1.05) +
    ggplot2::annotation_custom(icon_NCS[[2]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = 0.86) +
    ggplot2::annotation_custom(icon_NCS[[3]],  xmin = 0.92, xmax = 1.16, ymin = -Inf, ymax = 0.675) +
    ggplot2::annotation_custom(icon_NCS[[4]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = 0.49) +
    ggplot2::annotation_custom(icon_NCS[[5]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = 0.31) +
    ggplot2::annotation_custom(icon_NCS[[6]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = 0.125) +
    ggplot2::annotation_custom(icon_NCS[[7]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.05) +
    ggplot2::annotation_custom(icon_NCS[[8]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.23) +
    ggplot2::annotation_custom(icon_NCS[[9]],  xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.41) +
    ggplot2::annotation_custom(icon_NCS[[10]], xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.59) +
    ggplot2::annotation_custom(icon_NCS[[11]], xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.76) +
    ggplot2::annotation_custom(icon_NCS[[12]], xmin = 0.93, xmax = 1.15, ymin = -Inf, ymax = -0.95) +
    

    
    # Reverse y axis to have terrestrial ecosystems at the top of the diagramm
    ggplot2::scale_y_reverse() + 
    
    # add text to ecosystem
    # ggplot2::annotate(geom = "text", 
    #                   x = c(rep(-0.203,11)), 
    #                   y = seq(0,1,0.1), 
    #                   label = rownames(matrix),
    #                   color = nodes_col, 
    #                   # alpha = 0.8,
    #                   size = 3.75, 
    #                   fontface = "bold") +
    
    
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(), 
                   plot.background = ggplot2::element_blank(),
                   legend.position = "none") 
  
  ## Save plot
  if(save == TRUE) {
    
    save(netw, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 5, height = 6.8, device = "png")
    
  } else {return(netw)}
  
}


#' Plot Network As Flux Diagramm For Negative Links
#'
#' @param network_obj a network object - use matrix_to_network
#' @param matrix a weighted contingency matrix with SDG in columns and NCS in rows - use matrix_SDG
#' @param icon_SDG a list of 16 rastergrob objects for each SDG - use format_icons
#' @param icon_NCS a list of 11 rastergrob objects for each NCS - use format_icons
#' @param nodes_col a character vector specifying the color of nodes
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved 
#'
#'
#' @return a flux diagramm of the negative links between SDG and NCS
#' @export
#' 
#'
#' @examples
plot_network_neg <- function(network_obj, matrix, icon_SDG, icon_NCS, nodes_col, save = FALSE, name) {
  
  ## Plot the network
  netw <- GGally::ggnet2(net        = network_obj, 
                         mode       = NCSSDGproj::coords(mymat = matrix, maxX = 6, maxY = 15),
                         label      = FALSE,
                         shape      = "shape",
                         size       = 0.1,
                         max_size   = 9, 
                         label.size = 2,
                         edge.size  = NCSSDGproj::edge_size(matrix, 5)/1.3, 
                         edge.alpha = 0.4,
                         color      = rep("white", 28),
                         edge.color = NCSSDGproj::edge_col(matrix, neg = TRUE),
                         layout.exp = 0.25) +
    
    # Add silhouette of SDG (xmax = 1.1 to plot with barplot)
    ggplot2::annotation_custom(icon_SDG[[1]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = 1.05) + 
    ggplot2::annotation_custom(icon_SDG[[2]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .917) +
    ggplot2::annotation_custom(icon_SDG[[3]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .784) +
    ggplot2::annotation_custom(icon_SDG[[4]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .651) +
    ggplot2::annotation_custom(icon_SDG[[5]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .518) +
    ggplot2::annotation_custom(icon_SDG[[6]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .385) +
    ggplot2::annotation_custom(icon_SDG[[7]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .252) +
    ggplot2::annotation_custom(icon_SDG[[8]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = .119) +
    ggplot2::annotation_custom(icon_SDG[[9]],  xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.0178) +   
    ggplot2::annotation_custom(icon_SDG[[10]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.15) + 
    ggplot2::annotation_custom(icon_SDG[[11]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.283) +
    ggplot2::annotation_custom(icon_SDG[[12]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.416) + 
    ggplot2::annotation_custom(icon_SDG[[13]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.549) +
    ggplot2::annotation_custom(icon_SDG[[14]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.682) +
    ggplot2::annotation_custom(icon_SDG[[15]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.815) +
    ggplot2::annotation_custom(icon_SDG[[16]], xmin = 0.95, xmax = 1.12, ymin = -Inf, ymax = -.948) +
    
    # Add silhouette for NCS (xmin=-0.75 (-0.1 for peatland) to plot without barplot_percent) +0.1
    ggplot2::annotation_custom(icon_NCS[[1]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = 1.05) +
    ggplot2::annotation_custom(icon_NCS[[2]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = 0.86) +
    ggplot2::annotation_custom(icon_NCS[[3]],  xmin = -0.16, xmax = 0.08, ymin = -Inf, ymax = 0.675) +
    ggplot2::annotation_custom(icon_NCS[[4]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = 0.49) +
    ggplot2::annotation_custom(icon_NCS[[5]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = 0.31) +
    ggplot2::annotation_custom(icon_NCS[[6]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = 0.125) +
    ggplot2::annotation_custom(icon_NCS[[7]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.05) +
    ggplot2::annotation_custom(icon_NCS[[8]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.23) +
    ggplot2::annotation_custom(icon_NCS[[9]],  xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.41) +
    ggplot2::annotation_custom(icon_NCS[[10]], xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.59) +
    ggplot2::annotation_custom(icon_NCS[[11]], xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.76) +
    ggplot2::annotation_custom(icon_NCS[[12]], xmin = -0.15, xmax = 0.07, ymin = -Inf, ymax = -0.95) + 
    

     
    # Reverse y axis to have terrestrial ecosystems at the top of the diagramm
    ggplot2::scale_y_reverse() +

    ggplot2::theme(axis.text.y = ggplot2::element_blank(), 
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(), 
                   plot.background = ggplot2::element_blank(),
                   legend.position = "none") 
  
  ## Save plot
  if(save == TRUE) {
    
    save(netw, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 5, height = 6.8, device = "png")
    
  } else {return(netw)}
  
}


#' Barplot Percentage Of Target Achieved 
#'
#' @param SDG_network Matrices with positive and negative relationship - use outputs in SDG_network object
#' @param color a character vector specifying the color of each type of NCS
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved
#'
#' @return a barplot with positive and negative values percent of targets linked
#' @export
#'
#' @examples
barplot_perc_achieve <- function(SDG_network, color, save = FALSE, name){
  
  ### Colors of SDGs
  color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                  "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                  "#CF8D2A", "#E11484", "#D3A029", "#007DBC")
  
  ### Order SDG to match with order of SDG in panel A
  order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
  order_group <- rev(c("Terrestrial","Terrestrial_neg","Coastal", "Coastal_neg", "Marine", "Marine_neg"))
  
  
  ### Format data
  SDG_network[["score_pos"]][["data_pourc"]]$pos_neg <- "+"
  SDG_network[["score_neg"]][["data_pourc"]]$pos_neg <- "-"
  
  data_plot <- rbind(SDG_network[["score_pos"]][["data_pourc"]], SDG_network[["score_neg"]][["data_pourc"]]) %>%
    
    ## column with negative percentage
    dplyr::mutate(rel_pourc_neg = ifelse(test = pos_neg == "-",
                                         yes  = -1*relative_pourcent, 
                                         no   = relative_pourcent),
                  
                  ## New group to differenciate positive and negative impacts of ecosystems
                  group_neg     = ifelse(test = pos_neg == "-",
                                         yes  = paste0(group, "_neg"), 
                                         no   = group),
                  
                  ## Position of labels for text
                  text_labs_pos = ifelse(test = pos_neg == "-",
                                         yes  = (-1*perc_goal) - 9, 
                                         no   = perc_goal + 9),
                  
                  ## Group order
                  group_order   = forcats::fct_relevel(group_neg, "Marine", "Coastal", "Terrestrial", "Marine_neg", "Coastal_neg", "Terrestrial_neg"))
  
  
  ## Extract text 
  text_plot <-  data_plot[seq(1,96,3),]
  
  ### Plot
  barplot_perc_achieve <- ggplot2::ggplot() +
    
    ## Color area for values below  0 in red
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin = Inf , 
                                              xmax = -Inf, 
                                              ymin = 0, 
                                              ymax = -Inf),
                       fill    = "red",
                       alpha   = 0.10) +
    
    ## Color area for values below  0 in green
    ggplot2::geom_rect(mapping = ggplot2::aes(xmin = Inf , 
                                              xmax = -Inf, 
                                              ymin = 0, 
                                              ymax = Inf),
                       fill    = "#E9B200",
                       alpha   = 0.10) +
    
    ## Plot bars
    ggplot2::geom_col(data        = data_plot, 
                      mapping     = ggplot2::aes(x     = as.numeric(factor(SDG_number, levels = rev(unique(order)))), 
                                                 y     = rel_pourc_neg,
                                                 fill  = group_order),
                      position    = "stack",
                      stat        = "identity",
                      width       = 0.65, 
                      alpha       = 0.8,
                      show.legend = FALSE) +
    
    ## Add a vertical bar at 0
    ggplot2::geom_hline(yintercept = 0) +
    
    ## Add + and - sign 
    ggplot2::annotate(geom  = "text", 
                      y     = c(-100, 100), 
                      x     = c(rep(16.1, 2)), 
                      label = c("-", "+"), 
                      color = c("red", "#E9B200"), 
                      face  = "bold",
                      size  = 9) +
    
    ## Add text (number of targets achieved in each SDG)
    # ggplot2::geom_text(mapping     = ggplot2::aes(x     = as.numeric(factor(SDG_number, levels = rev(unique(order)))), 
    #                                               y     = -text_labs_pos*1.08, 
    #                                               group = pos_neg,
    #                                               color = pos_neg,
    #                                               label = text),
    #                    stat        = "identity",
    #                    data        = text_plot, 
    #                    size        = 4,
    #                    show.legend = FALSE) +
    
    
    ## scale modif
    ggplot2::scale_fill_manual(values = color,
                               name   = NULL) +
    
    ggplot2::scale_color_manual(values = c("red", "#9c7c32"),
                                name   = NULL) +
    
    ggplot2::scale_y_continuous(position = "right", 
                                breaks   = seq(plyr::round_any(min(data_plot$text_labs_pos), 10), 
                                               max(data_plot$text_labs_pos), 20),
                                labels   = abs(seq(plyr::round_any(min(data_plot$text_labs_pos), 10), 
                                               max(data_plot$text_labs_pos), 20)),
                                expand   = c(0.08,0)) +
    
    ggplot2::scale_x_continuous(breaks   = 1:16,
                                # labels   = paste(rep("SDG", 11), rev(c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14))),
                                labels   = rev(c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)),
                                expand   = c(0.01, 0.01),
                                sec.axis = ggplot2::dup_axis()) +
    
    ggplot2::coord_flip() +
    
    ggplot2::labs(x = "", y = "% linked") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x  = ggplot2::element_text(size  = 12, 
                                                        face  = "bold",
                                                        color = c(rep("red", 5), "#4D4D4D", rep("#E9B200", 5))),
                   # axis.text.y    = ggplot2::element_text(size   = 13,
                   #                                        color  = rev(color_text),
                   #                                        face   = "bold"),
                   axis.text.y  = ggplot2::element_blank(),
                   # axis.ticks.y = ggplot2::element_blank(),
                   # axis.title   = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size   = 14, face = "bold"),
                                                        # vjust  = 5,
                                                        # hjust  = 1.25),
                                                        # margin = ggplot2::margin(0,15,0,0)),
                   
                   # Legend modifications
                   legend.position   = c(0.90, 0.90),
                   legend.text       = ggplot2::element_text(size = 16),
                   legend.background = ggplot2::element_rect(fill  = "transparent", 
                                                             color = "transparent"),
                   
                   # Widen the left margin
                   # plot.margin = grid::unit(c(1, 4, 1, -4), "lines"),
                   
                   # Remove grid on the background
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_rect(fill   = "transparent", 
                                                            colour = NA)) +
    
    
    ggplot2::guides(fill  = ggplot2::guide_legend(reverse = TRUE)) 
  
  ## Save plot
  if(save == TRUE) {
    
    save(barplot_perc_achieve, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 4.5, height = 6.8, device = "png")
    
  } else {return(barplot_perc_achieve)}
  
}


#' Plot Legend Of Figure Two
#'
#' @param data_plot get it from SDG_network and use positive matrix - SDG_network--"score_pos"-- --"data_pourc"--
#' @param color color for each type of NCS
#'
#' @return a the legend of the barplot
#' @export
#' 
#'
#' @examples
barplot_legend <- function(data_plot, color) {
  
  color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                  "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                  "#CF8D2A", "#E11484", "#D3A029", "#007DBC")
  
  order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
  order_group <- rev(c("Terrestrial", "Coastal", "Marine"))
  
  text_plot <-  data_plot[seq(1,48,3),]
  
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


#' Produce Vertical Legend
#'
#' @param data_plot 
#' @param color 
#'
#' @return
#' @export
#'
#' @examples
legend_verti <- function(data_plot, color){
  
  color_text <- c("#FDB713", "#00AED9", "#3EB049", "#F99D26", "#EF402B", "#279B48",
                  "#48773E", "#F36D25", "#EB1C2D", "#C31F33", "#8F1838", "#02558B",
                  "#CF8D2A", "#E11484", "#D3A029", "#007DBC")
  
  order <- c(7,6,15,11,5,3,13,9,1,4,8,16,12,10,2,14)
  order_group <- rev(c("Terrestrial", "Coastal", "Marine"))
  
  text_plot <-  data_plot[seq(1,46,3),]
  
  plot_leg <- ggplot2::ggplot() +
    ggplot2::geom_col(data = data_plot, 
                      mapping = ggplot2::aes(x = factor(SDG_number, levels = rev(unique(order))),
                                             y = relative_pourcent,
                                             fill = factor(group, levels = unique(order_group))), 
                      width = 0.65) +
    
    ggplot2::scale_fill_manual(values = color, 
                               name = NULL) +
    
    ggplot2::theme(legend.position = "left",
                   legend.text = ggplot2::element_text(size = 16),
                   legend.background = ggplot2::element_rect(fill = "transparent", 
                                                             color = "transparent")) +
    
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  
  vert_legend <- ggpubr::get_legend(plot_leg)
  save(vert_legend, file = here::here("results", "vert_legend.RData"))
  
}


#' Build Figure Two 
#'
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved
#'
#' @return Figure 2 in the paper
#' @export
#' 
#'
#' @examples
Figure2 <- function(save = FALSE, name) {
  
  # Plot panels
  fig1a <- NCSSDGproj::plot_network_neg(network_obj = SDG_network[["score_neg"]][["network"]],
                                        matrix      = SDG_network[["score_neg"]][["matrix"]],
                                        icon_SDG    = icon_SDG,
                                        icon_NCS    = icon_NCS,
                                        nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 4)),
                                        save        = FALSE)


  fig1b <- NCSSDGproj::barplot_perc_achieve(SDG_network = SDG_network,
                                            color       = c("#1134A6", "#5EA9A2",  "#228B22", "#1134A6", "#5EA9A2",  "#228B22"), # Mar, Coast, Ter, Mar_neg, Coast_neg, Ter_neg
                                            save        = FALSE)

  fig1c <- NCSSDGproj::plot_network_pos(network_obj = SDG_network[["score_pos"]][["network"]],
                                        matrix      = SDG_network[["score_pos"]][["matrix"]],
                                        icon_SDG    = icon_SDG,
                                        icon_NCS    = icon_NCS,
                                        nodes_col   = c(rep("#228B22", 4), rep("#5EA9A2", 4), rep("#1134A6", 4)),
                                        save        = FALSE)
  
  NCSSDGproj::barplot_legend(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                             color     = c("#1134A6", "#5EA9A2", "#228B22")) # produce legend in horizontal format
  
  NCSSDGproj::legend_verti(data_plot = SDG_network[["score_pos"]][["data_pourc"]], 
                           color     = c("#1134A6", "#5EA9A2", "#228B22")) # produce legend in vertical format
  
  
  legend <- NCSSDGproj::load_legend()
  
  # Assemble panels
  fig1 <- cowplot::ggdraw() +
    
    cowplot::draw_plot(fig1a, x = -0.02, y = 0.005, width = 0.38, height = 0.97) +
    cowplot::draw_plot(fig1b, x = 0.325, y = 0.026, width = 0.35, height = 0.98) +
    cowplot::draw_plot(fig1c, x = 0.63, y = 0.005, width = 0.38, height = 0.97) +
    cowplot::draw_plot(legend, x = 0.25, y = 0, width = 0.5, height = 0.02) +
    cowplot::draw_plot_label(label = c("a", "b", "c"),
                             size = 15,
                             x = c(0, 0.33, 0.65),
                             y = c(0.98, 0.98, 0.98))
  
  # save
  if(save == TRUE) {
    
    save(fig1, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width=10, height=9, device="png")   
    
  } else {return(fig1)}
}


#' Insurance Circular Plot - Figure 3
#'
#' @param data a df with the number of time a target is achieved by each time of ecosystem - use circular_data_Insurance
#' @param label_data a df with label text and position - use circular_data_Insurance
#' @param base_data a df with the position of each semi-circle - use circular_data_Insurance
#' @param grid_data a df with the position of grid - use circular_data_Insurance
#' @param SDG_info a dataframe with the category, the color and the name of each SDG and target - use SDG_infos
#' @param colNCS_ter color for terrestrial ecosystems
#' @param colNCS_coast color for coastal ecosystems
#' @param colNCS_mar color for marine ecosystems
#' @param iconSDG a list of 16 rastergrob objects for each SDG - use format_icons 
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved 
#'
#' @return figure 3 in the paper
#' @export
#'
#' @examples
circular_plot_Insurance <- function(data, label_data, base_data, grid_data, SDG_info, colNCS_ter, colNCS_coast, colNCS_mar, icon_SDG, save = FALSE, name){
  
  # Color scale
  col <- SDG_info %>%
    dplyr::mutate(SDG = as.numeric(SDG)) %>%
    dplyr::group_by(SDG) %>%
    dplyr::summarise(color = unique(color)) 
  
  # Join null data and observed data 
  data$null_vals <- NA
  data$null_vals[is.na(data$goal.target) == FALSE] <- 6
  
  
  # vertical legend
  vert_legend <- NCSSDGproj::load_vert_legend()
  
  # Plot
  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(x = as.factor(id),
                                                 y = as.numeric(value_group),
                                                 fill = factor(group)),
                          show.legend = FALSE) +
    
    ggplot2::geom_bar(mapping = ggplot2::aes(x = as.factor(id),
                                             y = as.numeric(value_group),
                                             group = factor(group)),
                      stat = "identity",
                      alpha = 0.7,
                      show.legend = FALSE) +
    
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 12, 
                                                     xend = start, 
                                                     yend = 12), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 10, 
                                                     xend = start, 
                                                     yend = 10), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end,
                                                     y    = 8,
                                                     xend = start,
                                                     yend = 8),
                          colour      = "black",
                          alpha       = 0.7,
                          size        = 0.4,
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 6,
                                                     xend = start,
                                                     yend = 6), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 4, 
                                                     xend = start, 
                                                     yend = 4), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 2, 
                                                     xend = start, 
                                                     yend = 2), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 0, 
                                                     xend = start, 
                                                     yend = 0), 
                          colour      = "black", 
                          alpha       = 0.7,
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    # Add text showing the value of each 0/2/4/6/8/10 lines
    ggplot2::annotate(geom     = "text", 
                      x        = rep(max(data$id), 7), 
                      y        = c(12, 10, 8, 6, 4, 2, 0), 
                      label    = c("12", "10", "8", "6", "4", "2", "0"), 
                      color    = "black", 
                      alpha    = 0.7, 
                      size     = 3,
                      angle    = 0, 
                      fontface = "bold", 
                      hjust    = 1) +
    
    ggplot2::geom_bar(mapping     = ggplot2::aes(x    = as.factor(id), 
                                                 y    = value_group, 
                                                 fill = as.factor(group)), 
                      stat        = "identity", 
                      alpha       = 0.5,
                      show.legend = FALSE) +
    
    ## Add points for null values
    ggplot2::geom_point(data    = data,
                        mapping = ggplot2::aes(x     = as.factor(id),
                                               y     = null_vals,
                                               group = factor(group)),
                        color   = "firebrick1",
                        fill    = "firebrick1",
                        shape   = 21,
                        size    = 2) +
    
    ggplot2::ylim(-20, 13) +
    
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(-1,4), "cm")) +
    
    ggplot2::coord_polar() +
    
    # Add target's number above each bars
    ggplot2::geom_text(data = label_data,
                       mapping = ggplot2::aes(x = id,
                                              y = max(tot, na.rm = TRUE) + 0.5, 
                                              label = goal.target, 
                                              hjust = hjust), 
                       color = "black", 
                       fontface = "bold",
                       alpha = 0.7, 
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
    
    
  ggplot2::scale_fill_manual(values = c(colNCS_coast, colNCS_mar, colNCS_ter), 
                             name = NULL) 
  
  
  
  ## Add SDG icons
  plot <- cowplot::ggdraw(plot)
  
  ## Remove unwanted icons 
  data$SDG2 <- paste("SDG", data$SDG, sep = " ")
  
  for(i in 1:length(icon_SDG)){
    
    if(! names(icon_SDG)[i] %in% data$SDG2){
      icon_SDG[i] <- list(NULL)
    }
    
  }
  
  circular_plot <- plot +
    cowplot::draw_plot(vert_legend,          x = 0.135,  y = 0.135,  width = 0.75,  height = 0.75) +
    cowplot::draw_grob(icon_SDG[["SDG 1"]],  x = 0.514,  y = 0.690,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 2"]],  x = 0.583,  y = 0.664,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 3"]],  x = 0.640,  y = 0.615,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 4"]],  x = 0.679,  y = 0.548,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 5"]],  x = 0.692,  y = 0.474,  width = 0.045, height = 0.045) + 
    cowplot::draw_grob(icon_SDG[["SDG 6"]],  x = 0.675,  y = 0.400,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 7"]],  x = 0.638,  y = 0.340,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 8"]],  x = 0.584,  y = 0.295,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 9"]],  x = 0.504,  y = 0.268,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 10"]], x = 0.425,  y = 0.272,  width = 0.045, height = 0.045) +  
    cowplot::draw_grob(icon_SDG[["SDG 11"]], x = 0.360,  y = 0.308,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 12"]], x = 0.296,  y = 0.375,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 13"]], x = 0.270,  y = 0.437,  width = 0.045, height = 0.045) + 
    cowplot::draw_grob(icon_SDG[["SDG 14"]], x = 0.272,  y = 0.521,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 15"]], x = 0.330,  y = 0.627,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 16"]], x = 0.423,  y = 0.683,  width = 0.045, height = 0.045)
  
  ## Save plot
  if(save == TRUE) {
    
    save(circular_plot, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 10.5, height = 10.5, device = "png")
    
  } else {return(circular_plot)}
  
}


#' Insurance Circular Plot For Negative Data
#'
#' @param data a df with the number of time a target is achieved by each time of ecosystem - use circular_data_Insurance
#' @param label_data a df with label text and position - use circular_data_Insurance
#' @param base_data a df with the position of each semi-circle - use circular_data_Insurance
#' @param grid_data a df with the position of grid - use circular_data_Insurance
#' @param SDG_info a dataframe with the category, the color and the name of each SDG and target - use SDG_infos
#' @param colNCS_ter color for terrestrial ecosystems
#' @param colNCS_coast color for coastal ecosystems
#' @param colNCS_mar color for marine ecosystems
#' @param iconSDG a list of 16 rastergrob objects for each SDG - use format_icons 
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved 
#'
#' @return figure 4 in the paper
#' @export
#'
#' @examples
circular_plot_Insurance_neg <- function(data, label_data, base_data, grid_data, SDG_info, colNCS_ter, colNCS_coast, colNCS_mar, icon_SDG, save = FALSE, name){
  
  # Color scale
  col <- SDG_info %>%
    dplyr::mutate(SDG = as.numeric(SDG)) %>%
    dplyr::group_by(SDG) %>%
    dplyr::summarise(color = unique(color)) 
  
  # Join null data and observed data 
  data$null_vals <- NA
  data$null_vals[is.na(data$goal.target) == FALSE] <- 5.5
  
  
  # vertical legend
  vert_legend <- NCSSDGproj::load_vert_legend()
  
  # modify base_data if end == start (if only one target in a SDG, no bars)
  for(i in 1:nrow(base_data)){
    
    if(base_data$start[i] == base_data$end[i]){
      base_data$start[i] <- base_data$start[i] - 0.25
      base_data$end[i] <- base_data$end[i] + 0.25
    }
    
  }
  
  # Plot
  plot <- ggplot2::ggplot(data        = data,
                          mapping     = ggplot2::aes(x    = as.factor(id),
                                                     y    = as.numeric(value_group),
                                                     fill = factor(group)),
                          show.legend = FALSE) +
    
    ggplot2::geom_bar(mapping     =  ggplot2::aes(x     = as.factor(id),
                                                  y     = as.numeric(value_group),
                                                  group = factor(group)),
                      width       = 0.8,
                      stat        = "identity",
                      alpha       = 0.7,
                      show.legend = FALSE) +
    
    
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end,
                                                     y    = 12,
                                                     xend = start,
                                                     yend = 12),
                          colour      = "black",
                          alpha       = 0.7,
                          size        = 0.4,
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end,
                                                     y    = 10,
                                                     xend = start,
                                                     yend = 10),
                          colour      = "black",
                          alpha       = 0.7,
                          size        = 0.4,
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end,
                                                     y    = 8,
                                                     xend = start,
                                                     yend = 8),
                          colour      = "black",
                          alpha       = 0.7,
                          size        = 0.4,
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 6,
                                                     xend = start,
                                                     yend = 6), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 4, 
                                                     xend = start, 
                                                     yend = 4), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 2, 
                                                     xend = start, 
                                                     yend = 2), 
                          colour      = "black", 
                          alpha       = 0.7, 
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_segment(data        = grid_data, 
                          mapping     = ggplot2::aes(x    = end, 
                                                     y    = 0, 
                                                     xend = start, 
                                                     yend = 0), 
                          colour      = "black", 
                          alpha       = 0.7,
                          size        = 0.4, 
                          inherit.aes = FALSE) +
    
    # Add text showing the value of each 0/2/4/6/8 lines
    ggplot2::annotate(geom     = "text", 
                      x        = rep(max(data$id), 7), 
                      y        = c(12, 10, 8, 6, 4, 2, 0), 
                      label    = c("12", "10", "8", "6", "4", "2", "0"), 
                      color    = "black", 
                      alpha    = 0.7, 
                      size     = 3,
                      angle    = 0, 
                      fontface = "bold", 
                      hjust    = 1) +
    
    ggplot2::geom_bar(mapping     = ggplot2::aes(x    = as.factor(id), 
                                                 y    = value_group, 
                                                 fill = as.factor(group)), 
                      width       = 0.8, 
                      stat        = "identity", 
                      alpha       = 0.5,
                      show.legend = FALSE) +
    
    ## Add points for null values
    ggplot2::geom_point(data    = data,
                        mapping = ggplot2::aes(x     = as.factor(id),
                                               y     = null_vals,
                                               group = factor(group)),
                        color   = "firebrick1",
                        fill    = "firebrick1",
                        shape   = 21,
                        size    = 2) +
    
    ggplot2::ylim(-20, 13) +
    
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(-1,4), "cm")) +
    
    ggplot2::coord_polar() +
    
    # Add target's number above each bars
    ggplot2::geom_text(data = label_data,
                       mapping = ggplot2::aes(x = id, 
                                              y = max(tot, na.rm = TRUE) + 0.5, 
                                              label = goal.target, 
                                              hjust = hjust), 
                       color = "black", 
                       fontface = "bold",
                       alpha = 0.7, 
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
    
    
    ggplot2::scale_fill_manual(values = c(colNCS_coast, colNCS_mar, colNCS_ter), 
                               name = NULL) 
  
  
  
  ## Add SDG icons
  plot <- cowplot::ggdraw(plot)
  
  ## Remove unwanted icons 
  data$SDG2 <- paste("SDG", data$SDG, sep = " ")
  
  for(i in 1:length(icon_SDG)){
    
    if(! names(icon_SDG)[i] %in% data$SDG2){
      icon_SDG[i] <- list(NULL)
    }
    
  }
  
  circular_plot <- plot +
    cowplot::draw_plot(vert_legend,          x = 0.135,  y = 0.135,  width = 0.75,  height = 0.75) +
    cowplot::draw_grob(icon_SDG[["SDG 1"]],  x = 0.538,  y = 0.685,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 2"]],  x = 0.637,  y = 0.619,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 3"]],  x = 0.689,  y = 0.515,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 4"]],  x = 0.684,  y = 0.414,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 5"]],  x = 0.655,  y = 0.364,  width = 0.045, height = 0.045) + 
    cowplot::draw_grob(icon_SDG[["SDG 6"]],  x = 0.616,  y = 0.315,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 7"]],  x = 0.563,  y = 0.279,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 8"]],  x = 0.478,  y = 0.263,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 9"]],  x = 0.401,  y = 0.276,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 10"]], x = 0.342,  y = 0.320,  width = 0.045, height = 0.045) + 
    cowplot::draw_grob(icon_SDG[["SDG 11"]], x = 0.294,  y = 0.363,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 12"]], x = 0.275,  y = 0.415,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 13"]], x = 0.266,  y = 0.418,  width = 0.045, height = 0.045) + 
    cowplot::draw_grob(icon_SDG[["SDG 14"]], x = 0.269,  y = 0.510,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 15"]], x = 0.325,  y = 0.621,  width = 0.045, height = 0.045) +
    cowplot::draw_grob(icon_SDG[["SDG 16"]], x = 0.411,  y = 0.680,  width = 0.045, height = 0.045)
  
  ## Save plot
  if(save == TRUE) {
    
    save(circular_plot, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 10.5, height = 10.5, device = "png")
    
  } else {return(circular_plot)}
  
}


#' Circular Barplot Of Contribution For Supplementary Fig 1 And 2
#'
#' @param data obtained with NCSSDGproj::CA_contri_vars 1st element of the list
#' @param axis un number corresponding to the axis of the correspondance analysis
#' @param variable a character specifying if working on "row" vs "column" 
#' @param ymin a number for y min value
#' @param ymax a number for y max value
#' @param ytitle a number for title position
#'
#' @return a circular barplot in Supp Fig 1 and 2
#' @export
#'
#' @examples
CA_barplot <- function(data, axis, variable, ymin, ytitle){
  
  if(variable == "row"){
    data_cont <- NCSSDGproj::circular_data_CA(data_contrib = data, axis = axis, variable = variable)
    
  } else {    
    
    data_cont <- NCSSDGproj::circular_data_CA(data_contrib = data,  axis = axis, variable = variable)
    
  }
  
  ### Set ymax value
  ymax_axis1 <- NCSSDGproj::circular_data_CA(data_contrib = data, axis = 1, variable = variable)
  ymax_axis2 <- NCSSDGproj::circular_data_CA(data_contrib = data, axis = 2, variable = variable)
  # ymax <- max(c(ymax_axis1[["data"]]$Dim, ymax_axis2[["data"]]$Dim))
  
  if(abs(max(ymax_axis1[["data"]]$Dim) - max(ymax_axis2[["data"]]$Dim)) > 10 & axis == 1){
    ymin <- -50
    ytitle <- -50
  } else {
    ymin <- -70
    ytitle <- -70
  }

  segment_data <- data_cont[["segment data"]]
  base_data <- data_cont[["base_data"]]
  grid_data <- data_cont[["grid data"]]
  data_contrib <- data_cont[["data"]] 
  label_data <- data_cont[["label data"]] %>%
    dplyr::mutate(name_var = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", name_var2, perl=T)) # select text inside parenthesis
  
  ### Plot
  ggplot2::ggplot(data = data_contrib,
                  mapping = ggplot2::aes(x = as.factor(id), 
                                         y =  Dim,
                                         fill = group,
                                         color = group)) +
    
    ggplot2::geom_bar(mapping = ggplot2::aes(x = as.factor(id), 
                                             y =  Dim,
                                             fill = group,
                                             color = group),
                      color = data_contrib$color,
                      fill = scales::alpha(data_contrib$color, 0.7),
                      stat = "identity",
                      width = 0.75) +
    
    ggplot2::geom_segment(data = segment_data, 
                          mapping = ggplot2::aes(x = xstart, 
                                                 y = ystart, 
                                                 xend = xend, 
                                                 yend = yend), 
                          colour = "grey", 
                          alpha = 1, 
                          size = 0.09, 
                          inherit.aes = FALSE) +
    
    ggplot2::geom_bar(mapping = ggplot2::aes(x = as.factor(id),
                                             y = Dim,
                                             fill = group,
                                             color = group), 
                      color = data_contrib$color,
                      fill = scales::alpha(data_contrib$color, 0.7),
                      stat = "identity",
                      show.legend = FALSE, 
                      width = 0.75) +
    
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 100/nrow(data[[variable]][["contrib"]])), 
                        color = "red",
                        linetype = "dashed") +
  
    ggplot2::annotate(geom = "text", 
                      x = rep(0.2, 6), 
                      y = seq(round(min(data_contrib$Dim), -1), 
                              plyr::round_any(max(data_contrib$Dim), 10, f = ceiling) - 5, 
                              (plyr::round_any(max(data_contrib$Dim), 10, f = ceiling) - 5)/5), 
                      
                      label = c(as.character(seq(round(min(data_contrib$Dim), -1), 
                                                 plyr::round_any(max(data_contrib$Dim), 10, f = ceiling) -5, 
                                                 (plyr::round_any(max(data_contrib$Dim), 10, f = ceiling) -5)/5))), 
                      color = "black", 
                      size = 3, 
                      angle = 0, 
                      fontface = "bold", 
                      hjust = 1) +


    ggplot2::ylim(ymin, max(data_contrib$Dim)*1.3) +
    
    ggplot2::theme_minimal() +

    ggplot2::theme(legend.position = "none",
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(rep(-1,4), "cm")) +

    ggplot2::coord_polar() +
    
    ggplot2::geom_text(data = label_data, 
                       mapping = ggplot2::aes(x = id, 
                                              y = Dim + 1, 
                                              label = name_var, 
                                              hjust = hjust), 
                       color = "black",
                       fontface="bold",
                       alpha = 1, 
                       size = 3.5, 
                       angle = label_data$angle, 
                       inherit.aes = FALSE) +

    
    ggplot2::theme(plot.title = ggplot2::element_text(vjust = -40))+
    
    ggplot2::theme(legend.text = ggplot2::element_text(colour = "grey", 
                                                       size = 10,
                                                       face = "bold"),
                   legend.position = "none") +
    
    ggplot2::annotate(geom = "text", 
                      x = 0, 
                      y = ytitle, 
                      label = paste("CA\naxis", axis), 
                      color = "gray47", 
                      size = 4, 
                      angle = 0, 
                      fontface = "bold") 
  
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
supp_fig1 <- function(data, arrow, data_arrow, colNCS_ter, colNCS_coast, colNCS_mar, neg, save = FALSE, name){
  
  ### Legend
  legend <- NCSSDGproj::load_legend()
  
  ### Plot NCS from CA analysis

  
    ## Plot CA for NCS points
    ca_NCS_12 <- factoextra::fviz_ca_row(X         = data,
                                         axes      = c(1,2),
                                         title     = "",
                                         pointsize = 3,
                                         habillage = data[["grp"]]$group,
                                         palette   = c(colNCS_coast, colNCS_mar, colNCS_ter),
                                         repel     = TRUE,
                                         invisible = "quali",
                                         label     = data[["grp"]]$Ecosystem) +
      
      
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
                                        variable = "row")
                                        # ymin     = -15,
                                        # # ymax     = 58,
                                        # ytitle   = -15)
    
    ## Barplot of contribution for axis 2
    NCS_axis2 <- NCSSDGproj::CA_barplot(data     = data, 
                                        axis     = 2, 
                                        variable = "row")
                                        # ymin     = -50,
                                        # # ymax     = 59,
                                        # ytitle   = -50)
  

  ### Arrange plots together
  supp_fig <- cowplot::ggdraw() +
    cowplot::draw_plot(ca_NCS_12, x = 0.1, y = 0.5, width = 0.8, height = 0.5) +
    cowplot::draw_plot(NCS_axis1, x = 0.20, y = 0.06, width = 0.30, height = 0.47) +
    cowplot::draw_plot(NCS_axis2, x = 0.5, y = 0.06, width = 0.32, height = 0.47) +
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


#' Biplot Of Negative Versus Positive Links
#'
#' @param data data to be plotted - use sheets_to_df
#' @param save if TRUE the plot is saved in the results folder
#' @param name the name of the plot to be saved
#'
#' @return Supp figure 3 and 4 in the paper
#' @export
#'
#' @examples
supp_fig3_4 <- function(data_pos, data_neg, save = TRUE, name1, biplot = TRUE, name2){
  
  ### Calculate number of links for each ecosystem
  
  ## Positive data
  pos <- data.frame(ecosystem   = data_pos$ecosystem,
                    n_links_pos = rowSums(data_pos[, -1])) %>%
    
    dplyr::mutate(group = dplyr::case_when((ecosystem == "Peatland" | ecosystem == "Urban forest" | ecosystem == "Forest" | ecosystem == "Grassland") ~ "#228B22",
                                           (ecosystem == "Tidalmarsh" | ecosystem == "Mangrove" | ecosystem == "Seagrass" | ecosystem == "Macroalgae") ~ "#5EA9A2",
                                           TRUE ~ "#1134A6"),
                  link = "positive") 
  
  ## Negative data
  neg <- data.frame(ecosystem   = data_neg$ecosystem,
                    n_links_neg = rowSums(data_neg[, -1])) %>%
    
    dplyr::mutate(group = dplyr::case_when((ecosystem == "Peatland" | ecosystem == "Urban forest" | ecosystem == "Forest" | ecosystem == "Grassland") ~ "#228B22",
                                           (ecosystem == "Tidalmarsh" | ecosystem == "Mangrove" | ecosystem == "Seagrass" | ecosystem == "Macroalgae") ~ "#5EA9A2",
                                           TRUE ~ "#1134A6"),
                  link = "negative")
  
  ## Bind data
  data_bars <-  neg %>%
    dplyr::mutate(n_links_neg = -n_links_neg) %>%
    magrittr::set_colnames(colnames(pos)) %>%
    rbind(pos) %>%
    dplyr::arrange(plyr::desc(n_links_pos)) %>%
    dplyr::mutate(order = c(seq(1, length(unique(ecosystem)), 1), rep(0, length(unique(ecosystem)))))
  
  
  
  ### Plot barplot
  plot_bars <- ggplot2::ggplot() +
    
    ## Plot bars
    ggplot2::geom_col(data        = data_bars, 
                      mapping     = ggplot2::aes(x     = reorder(ecosystem, -order), 
                                                 y     = n_links_pos,
                                                 fill  = link),
                      show.legend = FALSE) +
    
    ## Add a vertical bar at 0
    ggplot2::geom_hline(yintercept = 0) +
    
    ## scale color modif
    ggplot2::scale_fill_manual(values = ggplot2::alpha(c("red", "darkgreen"), 0.75),
                               name    = NULL) +
    
    ggplot2::scale_y_continuous(breaks = seq(min(data_bars$n_links_pos), max(data_bars$n_links_pos), 5)) +
    
    ggplot2::coord_flip() +
    
    ggplot2::labs(x = "", y = "Number of links") +
    
    ggplot2::theme_bw() +
    
    ggplot2::theme(axis.text        = ggplot2::element_text(size  = 16),
                   # axis.text.y = ggplot2::element_text(color = reorder(data_bars$group, abs(data_bars$n_links_pos))),
                   axis.title       = ggplot2::element_text(size  = 18),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank())
  
  
  ### Save plot
  if(save == TRUE) {
    
    save(plot_bars, file = here::here("results", paste0(name1, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name1, ".png")), width = 15, height = 8.5, device = "png")
    
  } else {return(plot_bars)}
  
  
  ### Plot biplot   
  if(biplot == TRUE){
    
    data_biplot <- dplyr::left_join(x  = pos[, c("ecosystem", "n_links_pos", "group")],
                                    y  = neg[, c("ecosystem", "n_links_neg")],
                                    by = "ecosystem")
    
    ### Plot
    biplot <- ggplot2::ggplot() +
      
      ggplot2::geom_point(data        = data_biplot, 
                          mapping     = ggplot2::aes(x     = n_links_pos, 
                                                     y     = n_links_neg,
                                                     color = group),
                          color       = scales::alpha(data_biplot$group, 0.8),
                          show.legend = TRUE) +
      
      ggplot2::labs(x = "Positive links",
                    y = "Negative links") +
      
      ggplot2::geom_line(mapping = ggplot2::aes(x = c(0, max(data_biplot$n_links_pos)), 
                                                y = c(0, max(data_biplot$n_links_pos)))) +
      
      ggplot2::scale_x_continuous(breaks = seq(0, 70, 10))  +
      ggplot2::scale_y_continuous(breaks = seq(0, 70, 10))  +
      
      ggplot2::expand_limits(x = c(0, 70)) +
      
      ggrepel::geom_text_repel(data    = data_biplot,
                               mapping = ggplot2::aes(x     = n_links_pos, 
                                                      y     = n_links_neg,
                                                      label = ecosystem), 
                               color = data_biplot$group,
                               size   = 4) +
      
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title       = ggplot2::element_text(size = 17),
                     axis.text        = ggplot2::element_text(size = 14),
                     legend.title     = ggplot2::element_text(size = 17),
                     legend.text      = ggplot2::element_text(size = 14),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank())
    
    ### Save plot
    if(save == TRUE) {
      
      save(biplot, file = here::here("results", paste0(name2, ".RData")))
      ggplot2::ggsave(here::here("figures", paste0(name2, ".png")), width = 11, height = 6.8, device = "png")
      
    } else {return(biplot)} 
    
  }
  
}




  