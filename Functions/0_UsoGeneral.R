spcs <- function(n) {return(HTML(paste0(rep('&nbsp;', n), collapse = '')))}
Nlns <- function(n = 2) {return(HTML(paste0(rep('<br>', n), collapse = '')))}

ReqField <- function(x, Nspcs = 3, PrevSp = 0) {return(HTML(paste0(spcs(PrevSp), x, '<font color=\"#FF0000\">*</font>', spcs(Nspcs))))}
NonReqField <- function(x, Nspcs = 3, PrevSp = 0) {return(HTML(paste0(spcs(PrevSp), x, spcs(Nspcs))))}

is.null.empty <- function(x) {if (is.null(x)) {return(TRUE)} else {if (length(x) == 0 || x == '' || any(is.na(unlist(x)))) {return(TRUE)} else {return(FALSE)}}}
are.null.empty <- function(x) {
  for (i in 1:length(x)) {
    if (is.null.empty(x[i])) {return(TRUE)}}
  return(FALSE)
}

iso8601 <- function(fecha = Sys.Date(), niceHTML = FALSE) {
  tm <- as.POSIXct(sub(Sys.Date(), fecha, Sys.time()))
  tm_iso8601 <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', strftime(tm, "%Y-%m-%dT%H:%M:%S%z"), fixed = FALSE)
  if (niceHTML) {
    niceDIV <- tags$div(style = 'font-size:12px;', spcs(5),
                        tags$a(href = 'https://www.iso.org/iso-8601-date-and-time-format.html', tags$b('dateTime: '), target = '_blank'),
                        tags$div(style = 'font-size:14px;display:inline;', tm_iso8601))
    return(niceDIV)
  } else {
    return(tm_iso8601)
  }
}

decimals <- function(x) {nchar(sub("^-?\\d*\\.?", "", format(x, scientific = F)))}

pred <- function(p, conf.lev = 0.95) {
  if (p < 1 - conf.lev) return(tags$span(style = 'color:red;', p)) else return(p)
}
