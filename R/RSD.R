#' Relative standard deviation
#'
#' @export


RSD <- function(x, perc = TRUE, signif = 2) {
  if(perc) print(paste0(signif(sd(x) / mean(x), signif), '%'))
  return(sd(x) / mean(x))
}
