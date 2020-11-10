
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
  
  if(!is.matrix(M)) as.matrix(M)
  # Transpose.
  M <- t(M)
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
#' 
#'
#' @return
#' @export
#' 
#'
#' @examples
plot_network <- function(network_obj, matrix, icon_SDG, icon_NCS) {
  
  ## Plot the network
  plot <- ggnet::ggnet2(network_obj, 
                        mode = NCSSDGproj::coords(mymat = matrix, maxX = 6, maxY = 15),
                        label = FALSE,
                        shape = "shape",
                        size = c(rowSums(matrix), rep(15, 16)),
                        max_size = 18, 
                        label.size = 2,
                        edge.size = NCSSDGproj::edge_size(matrix, 5)/1.3, 
                        edge.alpha= 0.40,
                        color = c(rep("white", 11), rep("white",16)),
                        edge.color = NCSSDGproj::edge_col(matrix),
                        layout.exp = 0.5) +
    
    # Add silhouette of SDG (xmax = 1.1 to plot with barplot)
    annotation_custom(icon_SDG[[1]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = 1.05) +
    annotation_custom(icon_SDG[[2]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .917) +
    annotation_custom(icon_SDG[[3]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .784) +
    annotation_custom(icon_SDG[[4]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .651) +
    annotation_custom(icon_SDG[[5]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .518) +
    annotation_custom(icon_SDG[[6]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .385) +
    annotation_custom(icon_SDG[[7]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .252) +
    annotation_custom(icon_SDG[[8]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = .119) +
    annotation_custom(icon_SDG[[9]],  xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.0178) +   
    annotation_custom(icon_SDG[[10]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.15) + 
    annotation_custom(icon_SDG[[11]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.283) +
    annotation_custom(icon_SDG[[12]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.416) + 
    annotation_custom(icon_SDG[[13]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.549) +
    annotation_custom(icon_SDG[[14]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.682) +
    annotation_custom(icon_SDG[[15]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.815) +
    annotation_custom(icon_SDG[[16]], xmin = 0.96, xmax = 1.1, ymin = -Inf, ymax = -.948) +
    
    # Add silhouette for NCS (xmin=-0.75 (-0.1 for peatland) to plot without barplot_percent) +0.1
    annotation_custom(icon_NCS[[1]],  xmin = -0.085, xmax = 0.085, ymin = -Inf, ymax = 1.05) +
    annotation_custom(icon_NCS[[2]],  xmin = -0.075, xmax = 0.075, ymin = -Inf, ymax = 0.85) +
    annotation_custom(icon_NCS[[3]],  xmin = -0.120, xmax = 0.120, ymin = -Inf, ymax = 0.65) +
    annotation_custom(icon_NCS[[4]],  xmin = -0.083, xmax = 0.083, ymin = -Inf, ymax = 0.45) +
    annotation_custom(icon_NCS[[5]],  xmin = -0.089, xmax = 0.089, ymin = -Inf, ymax = 0.25) +
    annotation_custom(icon_NCS[[6]],  xmin = -0.098, xmax = 0.098, ymin = -Inf, ymax = 0.05) +
    annotation_custom(icon_NCS[[7]],  xmin = -0.087, xmax = 0.087, ymin = -Inf, ymax = -0.15) +
    annotation_custom(icon_NCS[[8]],  xmin = -0.085, xmax = 0.085, ymin = -Inf, ymax = -0.35) +
    annotation_custom(icon_NCS[[9]],  xmin = -0.099, xmax = 0.099, ymin = -Inf, ymax = -.55) +
    annotation_custom(icon_NCS[[10]], xmin = -0.073, xmax = 0.073, ymin = -Inf, ymax = -.75) +
    annotation_custom(icon_NCS[[11]], xmin = -0.069, xmax = 0.069, ymin = -Inf, ymax = -.95) + 
    
    # Reverse y axis to have terrestrial ecosystems at the top of the diagramm
    scale_y_reverse() + 
    
    # add text to ecosystem
    annotate(geom = "text", x = c(rep(-0.2,11)), y = seq(0,1,0.1), label = rownames(matrix),
             color = nodes_col, size = 3.3, fontface = "bold") +
    
  
    theme(axis.text.y=element_blank(), 
          axis.text.x=element_blank(),
          axis.ticks=element_blank(), 
          legend.position="none") 
  
  ## Save plot
  ggplot2::ggsave(here::here("results", "network_SDG_NCS.png"), width = 5, height = 6.8, device = "png")
    
}



