#' Plots titration curves
#'
#' @export

plotCurve <- function(curve, xlab = 'Titulante (mL)', ylab = 'Potencial (mV)', smoothLi = TRUE,
                      smoothDer1 = TRUE, smoothDer2 = FALSE, length = 10000, main = NULL,
                      spar = 0.2, plot = TRUE) {
  p <- ggplot(data = curve, mapping = aes(x = Titrant, y = Signal)) +
    geom_point(alpha = 0.4, shape = 1) + theme_bw() + labs(x = xlab, y = ylab, title = main)

  if (smoothLi) {
    smoothed <- curveDeriv(curve, length = length)
    smoothed$firstDeriv <- scales::rescale(smoothed$firstDeriv,
                                           to = c(min(curve$Signal), max(curve$Signal)))
    p <- p + geom_line(data = smoothed)
  }
  if (smoothDer1) p <- p + geom_line(data = smoothed, mapping = aes(x = Titrant, y = firstDeriv), color = 2)
  if (smoothDer2) p <- p + geom_line(data = smoothed, mapping = aes(x = Titrant, y = derScaling * secondDeriv), color = 4)
  if (plot) print(p)
  return(p)
}
