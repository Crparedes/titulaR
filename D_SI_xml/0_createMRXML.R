# This file create the XML files of CRMs and RMs
library(dplyr)
library(stringr)
library(xml2)

headingMR <- '<?xml version="1.0" encoding="UTF-8"?>
<mr:MRXML
  xmlns:mr="https://inm.gov.co/mr"
  xmlns:inst="https://inm.gov.co/inst"
  xmlns:si="https://ptb.de/si"
  xmlns:qudt="http://qudt.org/vocab/"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="https://ptb.de/si https://www.ptb.de/si/v2.2.0/SI_Format.xsd"/>'

initiateMRXML <- function(name) {
  heading <- str_replace(headingMR, 'MRXML', name)
  xmlObject <- read_xml(heading) 
  xmlObject %>% {
    xml_add_child(., 'mr:administrativeData')
    xml_add_child(., 'mr:certifiedValues')
    xml_add_child(., 'mr:additionalValues')
  }
  return(xmlObject)
}

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
    addDataToMRXML(xmlObject = x, fields = fields[[y]])
  })
  return(xmlObject)
}


############ Personal
headingPerson <- '<?xml version="1.0" encoding="UTF-8"?>
<person:PersonXML
  xmlns:person="https://inm.gov.co/personal"
  xmlns:inst="https://inm.gov.co/inst"/>'

read_xml(headingPerson) 

initiatePersonXML <- function(name) {
  heading <- str_replace(headingPerson, 'PersonXML', name)
  xmlObject <- read_xml(heading) 
  xmlObject %>% {
    xml_add_child(., 'person:data')
    xml_add_child(., 'person:inst')
  }
  return(xmlObject)
}

############## Ambient conditions
headingAmbiente <- '
<mr:ambiente ID = "idAmbiente"
  xmlns:mr="https://inm.gov.co/mr"
  xmlns:si="https://ptb.de/si"
  xmlns:qudt="http://qudt.org/vocab/"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="https://ptb.de/si https://www.ptb.de/si/v2.2.0/SI_Format.xsd"/>'

initiateAmbienteXML <- function(name) {
  heading <- str_replace(headingAmbiente, 'idAmbiente', name)
  xmlObject <- read_xml(headingAmbiente) 
  xmlObject %>% {
    xml_add_child(., 'mr:ambientConditions')
    xml_add_child(., 'mr:airDensity')
  }
  return(xmlObject)
}

############## Disoluciones
headingSolution <- '<?xml version="1.0" encoding="UTF-8"?>
<solution:DISOLUCION
  xmlns:solution="https://inm.gov.co/disoluciones"
  xmlns:mr="https://inm.gov.co/mr"
  xmlns:inst="https://inm.gov.co/inst"
  xmlns:si="https://ptb.de/si"
  xmlns:qudt="http://qudt.org/vocab/"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="https://ptb.de/si https://www.ptb.de/si/v2.2.0/SI_Format.xsd"/>'

initiateSolutionXML <- function(name) {
  heading <- str_replace(headingSolution, 'DISOLUCION', name)
  xmlObject <- read_xml(heading) 
  xmlObject %>% {
    xml_add_child(., 'solution:administrativeData')
    xml_add_child(., 'solution:propertyValues')
  }
  return(xmlObject)
}

