source('Modules/00_DigitalReferences.R')
adminData_NIST_SRM_928 <- list(
  'mr:name' = 'NIST SRM 928',
  'mr:type' = 'certified',
  'mr:producer' = RORs$NIST,
  'mr:validUntil' = '2025/12/02')

certiData_NIST_SRM_928 <- list(
  certProp1 = list(
    'mr:substance' = Substances$PbNO3,
    'mr:matrix' = 'High purity reagent',
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 1.0000,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0003,
        'si:coverageFactor' = 2,
        'si:coverageProbability' = 0.95))))

additData_NIST_SRM_928 <- list(
  ionMassFraction = list(
    'mr:substance' = Substances$PbII,
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 207.209 / 331.219,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0003,
        'si:coverageFactor' = 2,
        'si:coverageProbability' = 0.95))),
    ionMolarMass = list(
      'mr:substance' = Substances$PbII,
      'si:real' = list(
        'si:quantityTypeQUDT' = 'MolarMass',
        'si:value' = 207.209,
        'si:unit' = '\\gram\\mole\\tothe{-1}',
        'si:expandedUnc' = list(
          'si:uncertainty' = 0.005 * sqrt(3),
          'si:coverageFactor' = sqrt(3),
          'si:coverageProbability' = 0.95))),
  saltDensity = list(
    'si:real' = list(
      'si:quantityTypeQUDT' = 'Density',
      'si:value' = 4.53,
      'si:unit' = '\\gram\\centi\\meter\\tothe{-3}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.05 * sqrt(3),
        'si:coverageFactor' = sqrt(3),
        'si:coverageProbability' = 0.95))))

NIST_SRM_928 <- initiateMRXML('NIST_SRM_928')
addDataToMRXML(NIST_SRM_928, fields = adminData_NIST_SRM_928, node = 'mr:administrativeData')
addPropToMRXML(xmlObject = NIST_SRM_928, fields = certiData_NIST_SRM_928, node = 'mr:certifiedValues')
addPropToMRXML(xmlObject = NIST_SRM_928, fields = additData_NIST_SRM_928, node = 'mr:additionalValues')
message(xmlObject <- NIST_SRM_928)

write_xml(NIST_SRM_928, 'www/MR_MRC/Para EDTA/NIST_SRM_928.xml')

# str(listNIST_SRM_928 <- as_list(read_xml('www/CertMRC/XML/NIST_SRM_928.xml'))[[1]])
# listNIST_SRM_928$administrativeData$producer$ror[[1]]
