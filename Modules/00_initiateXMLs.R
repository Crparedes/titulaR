# This file create the XML files of CRMs and RMs
library(dplyr)
library(stringr)
library(xml2)

genericHeading <- function(x, inclVerEnc = FALSE) {
  VerEnc <- '<?xml version="1.0" encoding="UTF-8"?>'
  heading <- 'YYY
  <XXX
    xmlns:mr="https://inm.gov.co/mr"
    xmlns:si="https://ptb.de/si"
    xmlns:qudt="http://qudt.org/vocab/"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="https://ptb.de/si https://www.ptb.de/si/v2.2.0/SI_Format.xsd"/>'
  heading <- gsub('YYY', ifelse(inclVerEnc, VerEnc, ''), gsub('XXX', x, heading))
  return(heading)
}

initiateMRXML <- function(name) {
  heading <- str_replace(genericHeading('mr:MRXML', TRUE), 'MRXML', name)
  xmlObject <- read_xml(heading) 
  xmlObject %>% {xml_add_child(., 'mr:administrativeData'); xml_add_child(., 'mr:certifiedValues'); xml_add_child(., 'mr:additionalValues')}
  return(xmlObject)}

addDataToMRXML <- function(xmlObject, fields, node = NULL) {
  if(missing(node)) {x <- xmlObject} else {x <- xml_child(xmlObject, node)}
  lapply(seq_along(fields), function (y) {
    if (!is.list(fields[[y]])) {
      if (length(fields[[y]]) == 1) {
        xml_add_child(.x = x, .value = names(fields)[y], fields[[y]])
      } else {
        AdditAtrib <- paste(paste0(names(fields[[y]][-1]), " = '", fields[[y]][-1], "'"), collapse = ', ')
        eval(parse(text = paste(
          "xml_add_child(.x = x, .value = names(fields)[y], fields[[y]][1],", AdditAtrib, ")")))
      }
    } else {
      xml_add_child(.x = x, .value = names(fields)[y])
      addDataToMRXML(xmlObject = x, fields = fields[[y]], node = names(fields)[y])
    }
  })
  return(xmlObject)
}

addPropToMRXML <- function(xmlObject, fields, node) {
  x <- xml_child(xmlObject, node)
  lapply(seq_along(fields), function (y) {
    x <- xml_add_child(.x = x, .value = 'mr:property', id = names(fields)[y])
    addDataToMRXML(xmlObject = x, fields = fields[[y]])})
  return(xmlObject)
}


############ Personal

initiatePersonXML <- function(name) {
  # headingPerson <- '<?xml version="1.0" encoding="UTF-8"?>
  # <respPerson/>'
  # xmlObject <-  
  # xmlObject %>% {xml_add_child(., 'data'); xml_add_child(., 'inst')}
  return(read_xml('<respPerson/>'))
}

############## Disoluciones
initiateSolutionXML <- function() {
  xmlObject <- read_xml(genericHeading('mr:standardSolution', TRUE))
  xmlObject %>% {xml_add_child(., 'mr:coreData'); xml_add_child(., 'mr:property')}
  return(xmlObject)
}

##### Resultados titulaciones
initiateTitrationXML <- function(name) {
  xmlObject <- read_xml(genericHeading('mr:singleTitration', TRUE)) 
  xmlObject %>% {xml_add_child(., 'mr:coreData'); xml_add_child(., 'mr:titrationResult'); xml_add_child(., 'mr:additionalInfo')}
  return(xmlObject)
}

##### Resultados generales
initiateResultsXML <- function(name) {
  xmlObject <- read_xml(genericHeading('mr:combinedResults', TRUE))
  xmlObject %>% {xml_add_child(., 'mr:coreData'); xml_add_child(., 'mr:measurementResult'); xml_add_child(., 'mr:additionalInfo')}
  return(xmlObject)
}
