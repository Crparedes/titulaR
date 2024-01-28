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

iso8601 <- function(fecha = Sys.Date(), niceHTML = FALSE) {
  tm <- as.POSIXct(sub(Sys.Date(), fecha, Sys.time()))
  tm_iso8601 <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', strftime(tm, "%Y-%m-%dT%H:%M:%S%z"), fixed = FALSE)
  if (niceHTML) {
    niceDIV <- tags$div(style = 'font-size:12px;', spcs(5),
                        tags$a(href = 'https://www.iso.org/iso-8601-date-and-time-format.html', tags$b('ISO 8601: '), target = '_blank'),
                        tm_iso8601)
    return(niceDIV)
  } else {
    return(tm_iso8601)
  }
}

GetValueEstandUncert <- function(MrcXml, property = NULL, node = NULL) {
  if (!missing(node)) MrcXml <- xml_child(MrcXml, search = node)
  if (!missing(property)) {
    QUDTnodes <- xml_find_all(MrcXml, '//si:quantityTypeQUDT')
    PropNode <- gsub(pattern = '/si:real/si:quantityTypeQUDT', replacement = '', 
                     xml_path(QUDTnodes[which(sapply(QUDTnodes, function(x) {as_list(x)[[1]]}) == property)]))
    GetValueEstandUncert(xml_find_all(xml_find_all(MrcXml, xpath = PropNode), 'si:real'))
  } else {
    value <- xml_double(xml_find_all(MrcXml, xpath = 'si:value'))
    unitV <- xml_text(xml_find_all(MrcXml, xpath = 'si:unit'))
    kFact <- xml_double(xml_find_all(xml_child(MrcXml, search = 'si:expandedUnc'), xpath = 'si:coverageFactor'))
    stUnc <- xml_double(xml_find_all(xml_child(MrcXml, search = 'si:expandedUnc'), xpath = 'si:uncertainty')) / kFact
    return(list(ValUnc = c(value, stUnc), Units = unitV))
  }
}


