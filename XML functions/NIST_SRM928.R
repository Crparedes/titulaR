source('./XML functions/0_createMRXML.R')

adminData_NIST_SRM928 <- list(
  'mr:name' = 'NIST SRM 928',
  'mr:type' = 'certified',
  'mr:producer' = list(
    'inst:name' = c('National Institute of Standards and Technology', lang = 'EN', meta = '2'),
    'inst:shortName' = 'NIST',
    'inst:country' = 'US',
    'inst:ror' = 'https://ror.org/05xpvk416',
    'inst:url' = 'https://www.nist.gov/'),
  'mr:validUntil' = '2023/XX/XX')

certiData_NIST_SRM928 <- list(
  certProp1 = list(
    'mr:substance' = list(
      'mr:name' = 'Lead nitrate',
      'mr:InChI' = c('1S/2NO3.Pb/c2*2-1(3)4;/q2*-1;+2', version = '1.0.6'),
      'mr:InChiKey' = c('RLJMLMKIBZAXJO-UHFFFAOYSA-N', version = '1.0.6')),
    'mr:matrix' = 'High purity reagent',
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 1.0000,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0003,
        'si:coverageFactor' = 2,
        'si:coverageProbability' = 0.95))))

additData_NIST_SRM928 <- list(
  ionMassFraction = list(
    'mr:substance' = list(
      'mr:name' = 'lead(2+)',
      'mr:InChI' = c('1S/Pb/q+2', version = '1.0.6'),
      'mr:InChiKey' = c('RVPVRDXYQKGNMQ-UHFFFAOYSA-N', version = '1.0.6')),
    'si:real' = list(
      'si:quantityTypeQUDT' = 'MassFraction',
      'si:value' = 207.209 / 331.219,
      'si:unit' = '\\gram\\gram\\tothe{-1}',
      'si:expandedUnc' = list(
        'si:uncertainty' = 0.0003,
        'si:coverageFactor' = 2,
        'si:coverageProbability' = 0.95))),
    ionMolarMass = list(
      'substance' = list(
        'mr:name' = 'lead(2+)',
        'mr:InChI' = c('1S/Pb/q+2', version = '1.0.6'),
        'mr:InChiKey' = c('RVPVRDXYQKGNMQ-UHFFFAOYSA-N', version = '1.0.6')),
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

NIST_SRM928 <- initiateMRXML('NIST_SRM928')
addDataToMRXML(NIST_SRM928, fields = adminData_NIST_SRM928, node = 'mr:administrativeData')
addPropToMRXML(xmlObject = NIST_SRM928, fields = certiData_NIST_SRM928, node = 'mr:certifiedValues')
addPropToMRXML(xmlObject = NIST_SRM928, fields = additData_NIST_SRM928, node = 'mr:additionalValues')
message(xmlObject <- NIST_SRM928)

write_xml(NIST_SRM928, 'www/CertMRC/Pb/NIST_SRM928.xml')

str(listNIST_SRM928 <- as_list(read_xml('www/CertMRC/Pb/NIST_SRM928.xml'))[[1]])
listNIST_SRM928$administrativeData$producer$ror[[1]]
