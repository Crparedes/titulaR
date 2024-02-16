SiRealXML <- function(quantityTypeQUDT = NULL, value = NULL, units = NULL,
                      uncert = NULL, covFac = NULL, covProp = NULL, distribution = 'normal', SI.list = NULL) {
  SiRealXML <- read_xml('<si:real xmlns:si="https://ptb.de/si" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>')
  if(!missing(SI.list)) {
    addDataToMRXML(SiRealXML, SI.list)
  } else {
    if(missing(covProp)) covProp <- round(pnorm(covFac) - pnorm(-covFac), 3)
    addDataToMRXML(SiRealXML, list(
      'si:quantityTypeQUDT' = quantityTypeQUDT, 'si:value' = value, 'si:unit' = units,
      'si:expandedUnc' = list(
        'si:uncertainty' = uncert, 'si:coverageFactor' = covFac,
        'si:coverageProbability' = covProp, 'si:distribution' = distribution)))
  }
  return(SiRealXML)
}

CopySiRealFromXML <- function(xmlObject, property, node = NULL, as.char = TRUE) {
  if (!missing(node)) xmlObject <- xml_child(xmlObject, search = node)
  QUDTnodes <- xml_find_all(xmlObject, '//si:quantityTypeQUDT')
  return(read_xml(as.character(xml_parent(QUDTnodes[which(xml_text(QUDTnodes) == property)]))))
}

GetValueEstandUncert <- function(xmlObject, property = NULL, node = NULL, InChiKey = NULL) {
  if (!missing(node)) xmlObject <- xml_child(xmlObject, search = node)
  if (!missing(property)) {
    QUDTnodes <- xml_find_all(xmlObject, '//si:quantityTypeQUDT')
    
    if (!missing(InChiKey)) {
      InChiKeyNodes <- xml_find_all(xmlObject, '//mr:InChiKey')
      index <- xml_parent(xml_parent(QUDTnodes[which(xml_text(QUDTnodes) == property)])) %in%
        xml_parent(xml_parent(InChiKeyNodes[which(xml_text(InChiKeyNodes) == InChiKey)]))
      GetValueEstandUncert(xml_parent(xml_parent(QUDTnodes[which(xml_text(QUDTnodes) == property)]))[index], node = 'si:real')
    } else {
      GetValueEstandUncert(xml_parent(QUDTnodes[which(xml_text(QUDTnodes) == property)]))
    }
  } else {
    value <- xml_double(xml_find_all(xmlObject, xpath = 'si:value'))
    unitV <- xml_text(xml_find_all(xmlObject, xpath = 'si:unit'))
    kFact <- xml_double(xml_find_all(xml_child(xmlObject, search = 'si:expandedUnc'), xpath = 'si:coverageFactor'))
    stUnc <- xml_double(xml_find_all(xml_child(xmlObject, search = 'si:expandedUnc'), xpath = 'si:uncertainty')) / kFact
    return(list(ValUnc = c(value, stUnc), Units = unitV))
  }
}

SiRealListXML <- function(quantityTypeQUDT = NULL, values = NULL, listUnit = NULL, SI.list = NULL) {
  SiRealListXML <- read_xml('<si:realList xmlns:si="https://ptb.de/si" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>')
  if(!missing(SI.list)) {
    addDataToMRXML(SiRealListXML, SI.list)
  } else {
    addDataToMRXML(SiRealListXML, list('si:quantityTypeQUDT' = quantityTypeQUDT, 'si:listUnit' = listUnit))
    
    lapply(values, function (x) {xml_add_child(.x = SiRealListXML, .value = 'si:real') %>% xml_add_child(., 'si:value', x)})
  }
  return(SiRealListXML)
}
# SiRealListXML(quantityTypeQUDT = 'ElectricPotentialDifference', values = 5:10, listUnit = '\\milli\\volt')
