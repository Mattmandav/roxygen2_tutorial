#' HullPlotMD
#'
#' Uses Jarvis March to find and plot the convex hull of a list of 2D vectors.
#'
#' @param LofV List of 2D vectors.
#' @return A plot of the data and the convex hull.

HullPlotMD <- function(LofV){
  A <- LofV
  B <- ConvexHullMD(A)
  n <- length(B)
  B <- c(B,B[1])
  B <- unlist(B)
  A <- unlist(A)
  xhull <- B[c(TRUE, FALSE)]
  yhull <- B[c(FALSE, TRUE)]
  x <- A[c(TRUE, FALSE)]
  y <- A[c(FALSE, TRUE)]
  plot(xhull,yhull,type ='l',col='red')
  points(x,y,pch=19)
}