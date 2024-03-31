data_Analista <- list(
  'name' = 'Analista',
  'email' = 'contacto@inm.gov.co',
  'orcid' = 'https://orcid.org/')

# inst_Analista2 <- list(
#   'name' = c('Instituto Nacional de Metrología de Colombia', lang = 'ES'),
#   'shortName' = 'INM',
#   'country' = 'CO',
#   'alternativeName' = c('National Metrology Institute of Colombia', lang = 'EN'),
#   'ror' = 'https://ror.org/028s91538',
#   'url' = 'https://inm.gov.co')

Analista <- read_xml('<respPerson/>')
addDataToMRXML(Analista, fields = data_Analista)#, node = 'data')
# addDataToMRXML(xmlObject = Analista2, fields = inst_Analista2, node = 'inst')
message(Analista)

write_xml(Analista, 'www/Personal/Analista.xml')



