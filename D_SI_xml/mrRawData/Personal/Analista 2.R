data_Analista2 <- list(
  'person:name' = 'Analista2',
  'person:email' = 'contacto@inm.gov.co',
  'person:orcid' = 'https://orcid.org/')

inst_Analista2 <- list(
  'inst:name' = c('Instituto Nacional de Metrología de Colombia', lang = 'ES'),
  'inst:shortName' = 'INM',
  'inst:country' = 'CO',
  'inst:alternativeName' = c('National Metrology Institute of Colombia', lang = 'EN'),
  'inst:ror' = 'https://ror.org/028s91538',
  'inst:url' = 'https://inm.gov.co')

Analista2 <- initiatePersonXML('Analista2')
addDataToMRXML(Analista2, fields = data_Analista2, node = 'person:data')
addDataToMRXML(xmlObject = Analista2, fields = inst_Analista2, node = 'person:inst')
message(Analista2)

write_xml(Analista2, 'www/Personal/Analista 2.xml')



