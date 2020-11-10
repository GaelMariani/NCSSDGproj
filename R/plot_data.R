
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
bip_edgewt<- function(matrix, x) {
  
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