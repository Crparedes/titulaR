data_CristhianParedes <- list(
  'person:name' = 'Cristhian Paredes',
  'person:email' = 'caparedes@inm.gov.co',
  'person:orcid' = 'https://orcid.org/0000-0001-7049-9597')

inst_CristhianParedes <- list(
  'inst:name' = c('Instituto Nacional de Metrología de Colombia', lang = 'ES'),
  'inst:shortName' = 'INM',
  'inst:country' = 'CO',
  'inst:alternativeName' = c('National Metrology Institute of Colombia', lang = 'EN'),
  'inst:ror' = 'https://ror.org/028s91538',
  'inst:url' = 'https://inm.gov.co')

CristhianParedes <- initiatePersonXML('CristhianParedes')
addDataToMRXML(CristhianParedes, fields = data_CristhianParedes, node = 'person:data')
addDataToMRXML(xmlObject = CristhianParedes, fields = inst_CristhianParedes, node = 'person:inst')
message(CristhianParedes)

write_xml(CristhianParedes, 'www/Personal/Cristhian Paredes.xml')



