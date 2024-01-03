# This file create the XML files of CRMs and RMs
library(dplyr)
library(xml2)

InHouse_2960_84 <- read_xml('<?xml version="1.0" encoding="UTF-8"?>
<MRC xmlns:si="https://ptb.de/si" xmlns:qudt="http://qudt.org/vocab/"/>')

InHouse_2960_84 %>% {
  xml_add_child(., 'administrativeData')
  xml_add_child(., 'certifiedValues')
  xml_add_child(., 'additionalValues')
}

InHouse_2960_84 %>% xml_child('administrativeData') %>% {
  xml_add_child(., 'name', 'In House 2960-84')
  xml_add_child(., 'batch', 12)
  (xml_add_child(., 'producer') %>% {
    xml_add_child(., 'name', 'Ural Scientific Research Institute for Metrology', lang = 'EN', country = 'RU')
    xml_add_child(., 'shortName', 'UNIIM')
    xml_add_child(., 'alternativeName', 'Уральский научно-исследовательский институт метрологии', lang = 'RU')
    xml_add_child(., 'url', 'https://uniim.ru/')})
  xml_add_child(., 'validUntil', '2024/08/30')}

InHouse_2960_84 %>% xml_child('additionalValues') %>% {
  (xml_add_child(., 'property', id = 'Prop1') %>% {
    xml_add_child(., 'qudt:quantitykind', 'MassFraction')
    (xml_add_child(., 'substance', name = 'EDTA disodium salt dihydrate') %>% {
      xml_add_child(., 'InChI', '1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6')
      xml_add_child(., 'InChiKey', 'FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')})
    xml_add_child(., 'matrix', 'High purity reagent')
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'MassFraction')
      xml_add_child(., 'si:value', 0.99874)
      xml_add_child(., 'si:unit', '\\gram\\gram\\tothe{-1}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.0007)
        xml_add_child(., 'si:coverageFactor', 1.983)
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
  (xml_add_child(., 'property', id = 'MolarMass') %>% {
    xml_add_child(., 'qudt:quantitykind', 'MolarMass')
    (xml_add_child(., 'substance', name = 'EDTA disodium salt dihydrate') %>% {
      xml_add_child(., 'InChI', '1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6')
      xml_add_child(., 'InChiKey', 'FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')})
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'MolarMass')
      xml_add_child(., 'si:value', 3.722368e+02)
      xml_add_child(., 'si:unit', '\\gram\\mole\\tothe{-1}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 6.332914e-03 * sqrt(3))
        xml_add_child(., 'si:coverageFactor', sqrt(3))
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
  (xml_add_child(., 'property', id = 'saltDensity') %>% {
    xml_add_child(., 'qudt:quantitykind', 'Density')
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'Density')
      xml_add_child(., 'si:value', 0.860)
      xml_add_child(., 'si:unit', '\\gram\\centi\\meter\\tothe{-3}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.005 * sqrt(3))
        xml_add_child(., 'si:coverageFactor', sqrt(3))
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
}
message(InHouse_2960_84)
write_xml(InHouse_2960_84, 'www/CertMRC/EDTA/InHouse_2960_84.xml')

