adminData_InHouse_GSO_2960_84 <- list(
  'mr:name' = 'InHouse GSO 2960-84',
  'mr:batch' = 12,
  'mr:type' = 'inHouse',
  'mr:producer' = list(
    'inst:name' = c('Ural Scientific Research Institute for Metrology', lang = 'EN'),
    'inst:shortName' = 'UNIIM',
    'inst:country' = 'RU',
    'inst:alternativeName' = c('Уральский научно-исследовательский институт метрологии', lang = 'RU'),
    'inst:ror' = '',
    'inst:url' = 'https://uniim.ru/'))

additData_InHouse_GSO_2960_84 <- list(
  refProp1 = list(
    'mr:substance' = list(
      'mr:name' = 'EDTA disodium salt dihydrate',
      'mr:InChI' = c('1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6'),
      'mr:InChiKey' = c('FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')),
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

InHouse_GSO_2960_84 <- initiateMRXML('InHouse_GSO_2960_84')
addDataToMRXML(InHouse_GSO_2960_84, fields = adminData_InHouse_GSO_2960_84, node = 'mr:administrativeData')
addPropToMRXML(xmlObject = InHouse_GSO_2960_84, fields = additData_InHouse_GSO_2960_84, node = 'mr:additionalValues')
message(xmlObject <- InHouse_GSO_2960_84)

write_xml(InHouse_GSO_2960_84, 'www/MR_MRC/Para calibrantes/InHouse_GSO_2960_84.xml')



