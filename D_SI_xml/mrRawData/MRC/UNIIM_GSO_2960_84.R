source('Modules/00_DigitalReferences.R')
adminData_UNIIM_GSO_2960_84 <- list(
  'mr:name' = 'UNIIM GSO 2960-84',
  'mr:batch' = 12,
  'mr:type' = 'certified',
  'mr:producer' = RORs$UNIIM,
  'mr:validUntil' = '2020/08/30')

certiData_UNIIM_GSO_2960_84 <- list(
  certProp1 = list(
    'mr:substance' = Substances$Na2EDTA.2H2O,
    'mr:matrix' = 'High purity reagent',
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 0.9986,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0003,
        'si:coverageFactor' = 2,
        'si:coverageProbability' = 0.95))))

additData_UNIIM_GSO_2960_84 <- list(
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

UNIIM_GSO_2960_84 <- initiateMRXML('UNIIM_GSO_2960_84')
addDataToMRXML(UNIIM_GSO_2960_84, fields = adminData_UNIIM_GSO_2960_84, node = 'mr:administrativeData')
addPropToMRXML(xmlObject = UNIIM_GSO_2960_84, fields = certiData_UNIIM_GSO_2960_84, node = 'mr:certifiedValues')
addPropToMRXML(xmlObject = UNIIM_GSO_2960_84, fields = additData_UNIIM_GSO_2960_84, node = 'mr:additionalValues')
message(xmlObject <- UNIIM_GSO_2960_84)

write_xml(UNIIM_GSO_2960_84, 'www/MR_MRC/Para calibrantes/UNIIM_GSO_2960_84.xml')



