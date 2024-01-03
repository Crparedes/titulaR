# This file create the XML files of CRMs and RMs
library(dplyr)
library(xml2)

NIST_SRM928 <- read_xml('<?xml version="1.0" encoding="UTF-8"?>
<MRC xmlns:si="https://ptb.de/si" xmlns:qudt="http://qudt.org/vocab/"/>')

NIST_SRM928 %>% {
  xml_add_child(., 'administrativeData')
  xml_add_child(., 'certifiedValues')
  xml_add_child(., 'additionalValues')
}

NIST_SRM928 %>% xml_child('administrativeData') %>% {
  xml_add_child(., 'name', 'NIST SRM 928')
  xml_add_child(., 'batch')
  (xml_add_child(., 'producer') %>% {
    xml_add_child(., 'name', 'National Institute of Standards and Technology', lang = 'EN', country = 'US')
    xml_add_child(., 'shortName', 'NIST')
    xml_add_child(., 'alternativeName')
    xml_add_child(., 'url', 'https://www.nist.gov/')})
  xml_add_child(., 'validUntil', '2023/XX/XX')}

NIST_SRM928 %>% xml_child('certifiedValues') %>% 
  xml_add_child(., 'property', id = 'Prop1') %>% {
    xml_add_child(., 'qudt:quantitykind', 'MassFraction')
    (xml_add_child(., 'substance', name = 'Lead nitrate') %>% {
      xml_add_child(., 'InChI', '1S/2NO3.Pb/c2*2-1(3)4;/q2*-1;+2', version = '1.0.6')
      xml_add_child(., 'InChiKey', 'RLJMLMKIBZAXJO-UHFFFAOYSA-N', version = '1.0.6')})
    xml_add_child(., 'matrix', 'High purity reagent')
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'MassFraction')
      xml_add_child(., 'si:value', 1.0000)
      xml_add_child(., 'si:unit', '\\gram\\gram\\tothe{-1}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.0003)
        xml_add_child(., 'si:coverageFactor', 2)
        xml_add_child(., 'si:coverageProbability', 0.95)}})}

NIST_SRM928 %>% xml_child('additionalValues') %>% {
  (xml_add_child(., 'property', id = 'ionMassFraction') %>% {
    xml_add_child(., 'qudt:quantitykind', 'MassFraction')
    (xml_add_child(., 'substance', name = 'lead(2+)') %>% {
      xml_add_child(., 'InChI', '1S/Pb/q+2', version = '1.0.6')
      xml_add_child(., 'InChiKey', 'RVPVRDXYQKGNMQ-UHFFFAOYSA-N', version = '1.0.6')})
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'MassFraction')
      xml_add_child(., 'si:value', 207.209 / 331.219)
      xml_add_child(., 'si:unit', '\\gram\\gram\\tothe{-1}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.0003)
        xml_add_child(., 'si:coverageFactor', 2)
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
  (xml_add_child(., 'property', id = 'ionMolarMass') %>% {
    xml_add_child(., 'qudt:quantitykind', 'MolarMass')
    (xml_add_child(., 'substance', name = 'lead(2+)') %>% {
      xml_add_child(., 'InChI', '1S/Pb/q+2', version = '1.0.6')
      xml_add_child(., 'InChiKey', 'RVPVRDXYQKGNMQ-UHFFFAOYSA-N', version = '1.0.6')})
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'MolarMass')
      xml_add_child(., 'si:value', 207.209)
      xml_add_child(., 'si:unit', '\\gram\\mole\\tothe{-1}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.005 * sqrt(3))
        xml_add_child(., 'si:coverageFactor', sqrt(3))
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
  (xml_add_child(., 'property', id = 'saltDensity') %>% {
    xml_add_child(., 'qudt:quantitykind', 'Density')
    (xml_add_child(., 'si:real') %>% {
      xml_add_child(., 'si:label', 'Density')
      xml_add_child(., 'si:value', 4.53)
      xml_add_child(., 'si:unit', '\\gram\\centi\\meter\\tothe{-3}')
      xml_add_child(., 'si:expandedUnc') %>% {
        xml_add_child(., 'si:uncertainty', 0.05 * sqrt(3))
        xml_add_child(., 'si:coverageFactor', sqrt(3))
        xml_add_child(., 'si:coverageProbability', 0.95)}})})
}
message(NIST_SRM928)
write_xml(NIST_SRM928, 'www/CertMRC/Pb/NIST_SRM928.xml')

