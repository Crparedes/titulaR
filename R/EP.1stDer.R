#' Potentiometric equivalence point using first derivate method
#'
#' @export

EP.1stDer <- function(curve, length = 10000, sub = FALSE, subregion = c(0.985, 1.015), plot = FALSE, ...) {
  if (sub) {
    l1 <- which.min(abs(curve$aproxTitFrac - subregion[1]))
    l2 <- which.min(abs(curve$aproxTitFrac - subregion[2]))
    curve <- curve[l1:l2, ]
  }
  smoothed <- curveDeriv(curve, length = length)
  mn <- smoothed$Titrant[which.min(smoothed$firstDeriv)]
  if (plot) plotCurve(curve = curve, length = length, ...)
  #  print(mn)
  return(mn)
}
