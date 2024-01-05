adminData_UNIIM_GSO_2960_84 <- list(
  'mr:name' = 'UNIIM GSO 2960-84',
  'mr:batch' = 12,
  'mr:type' = 'certified',
  'mr:producer' = list(
    'inst:name' = c('Ural Scientific Research Institute for Metrology', lang = 'EN'),
    'inst:shortName' = 'UNIIM',
    'inst:country' = 'RU',
    'inst:alternativeName' = c('Уральский научно-исследовательский институт метрологии', lang = 'RU'),
    'inst:ror' = '',
    'inst:url' = 'https://uniim.ru/'),
  'mr:validUntil' = '2020/08/30')

certiData_UNIIM_GSO_2960_84 <- list(
  certProp1 = list(
    'mr:substance' = list(
      'mr:name' = 'EDTA disodium salt dihydrate',
      'mr:InChI' = c('1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6'),
      'mr:InChiKey' = c('FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')),
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
    'mr:substance' = list(
      'mr:name' = 'EDTA disodium salt dihydrate',
      'mr:InChI' = c('1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6'),
      'mr:InChiKey' = c('FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')),
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

write_xml(UNIIM_GSO_2960_84, 'www/CertMRC/XML/UNIIM_GSO_2960_84.xml')



