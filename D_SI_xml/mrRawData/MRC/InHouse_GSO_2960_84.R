source('Modules/00_DigitalReferences.R')
adminData_InHouse_GSO_2960_84 <- list(
  'mr:name' = 'InHouse GSO 2960-84',
  'mr:batch' = 12,
  'mr:type' = 'inHouse',
  'mr:producer' = RORs$UNIIM)

additData_InHouse_GSO_2960_84 <- list(
  refProp1 = list(
    'mr:substance' = Substances$Na2EDTA.2H2O,
    'mr:matrix' = 'High purity reagent',
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 0.99874,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0007,
        'si:coverageFactor' = 1.983,
        'si:coverageProbability' = 0.95)),
    'mr:source' = list(
      'mr:doi' = c('https://doi.org/10.1007/s12647-022-00602-0', note = 'Measurement report'),
      'mr:orcid' = 'https://orcid.org/0000-0001-7049-9597', note = 'Measurements responsible person')),
  molarMass = list(
    'mr:substance' = Substances$Na2EDTA.2H2O,
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MolarMass',
      'si:value' = 3.722368e+02,
      'si:unit' = '\\gram\\mole\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 6.332914e-03 * sqrt(3),
        'si:coverageFactor' = sqrt(3),
        'si:coverageProbability' = 0.95))),
  saltDensity = list(
    'si:real' = list(
      'si:quantityTypeQUDT' = 'Density',
      'si:value' = 0.860,
      'si:unit' = '\\gram\\centi\\meter\\tothe{-3}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.05 * sqrt(3),
        'si:coverageFactor' = sqrt(3),
        'si:coverageProbability' = 0.95))))

InHouse_GSO_2960_84 <- initiateMRXML('InHouse_GSO_2960_84')
addDataToMRXML(InHouse_GSO_2960_84, fields = adminData_InHouse_GSO_2960_84, node = 'mr:administrativeData')
addPropToMRXML(xmlObject = InHouse_GSO_2960_84, fields = additData_InHouse_GSO_2960_84, node = 'mr:additionalValues')
message(xmlObject <- InHouse_GSO_2960_84)

write_xml(InHouse_GSO_2960_84, 'www/MR_MRC/Para calibrantes/InHouse_GSO_2960_84.xml')



