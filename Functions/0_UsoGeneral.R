spcs <- function(n) {return(HTML(paste0(rep('&nbsp;', n), collapse = '')))}
Nlns <- function(n = 2) {return(HTML(paste0(rep('<br>', n), collapse = '')))}

ReqField <- function(x, Nspcs = 3) {return(HTML(paste0(x, '<font color=\"#FF0000\">*</font>', spcs(Nspcs))))}
NonReqField <- function(x, Nspcs = 3) {return(HTML(paste0(x, spcs(Nspcs))))}

is.null.empty <- function(x) {if (is.null(x)) {return(TRUE)} else {if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {return(TRUE)} else {return(FALSE)}}}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}
