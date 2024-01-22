data_CristhianParedes <- list(
  'name' = 'Cristhian Paredes',
  'email' = 'caparedes@inm.gov.co',
  'orcid' = 'https://orcid.org/0000-0001-7049-9597')

inst_CristhianParedes <- list(
  'name' = c('Instituto Nacional de Metrología de Colombia', lang = 'ES'),
  'shortName' = 'INM',
  'country' = 'CO',
  'alternativeName' = c('National Metrology Institute of Colombia', lang = 'EN'),
  'ror' = 'https://ror.org/028s91538',
  'url' = 'https://inm.gov.co')

CristhianParedes <- initiatePersonXML()
addDataToMRXML(CristhianParedes, fields = data_CristhianParedes, node = 'data')
addDataToMRXML(xmlObject = CristhianParedes, fields = inst_CristhianParedes, node = 'inst')
message(CristhianParedes)

write_xml(CristhianParedes, 'www/Personal/Cristhian Paredes.xml')



