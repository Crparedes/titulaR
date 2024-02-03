data_Analista2 <- list(
  'name' = 'Analista2',
  'email' = 'contacto@inm.gov.co',
  'orcid' = 'https://orcid.org/')

# inst_Analista2 <- list(
#   'name' = c('Instituto Nacional de Metrología de Colombia', lang = 'ES'),
#   'shortName' = 'INM',
#   'country' = 'CO',
#   'alternativeName' = c('National Metrology Institute of Colombia', lang = 'EN'),
#   'ror' = 'https://ror.org/028s91538',
#   'url' = 'https://inm.gov.co')

Analista2 <- read_xml('<respPerson/>')
addDataToMRXML(Analista2, fields = data_Analista2)#, node = 'data')
# addDataToMRXML(xmlObject = Analista2, fields = inst_Analista2, node = 'inst')
message(Analista2)

write_xml(Analista2, 'www/Personal/Analista 2.xml')



