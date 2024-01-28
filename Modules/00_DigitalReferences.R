canonicalURL <- 'https://siunits.stuchalk.domains.unf.edu/si/definition/'
SI_unit_nice <- function(unit = NULL, derived = FALSE, width = "84%") {
  if (missing(unit)) {
    unitCircle <- tags$a(href = canonicalURL, img(src = "SI_units.png", width = width), target = '_blank')
  } else {
    unitCircle <- tags$a(href = paste0(canonicalURL, ifelse(derived, 'derivedunit/', 'unit/'), unit),
                         img(src = paste0("SI_", unit, ".png"), width = width), target = '_blank')
  }
  return(unitCircle)
}

Substances <- list(
  Na2EDTA.2H2O = list(
    'mr:name' = 'EDTA disodium salt dihydrate',
    'mr:InChI' = c('1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6'),
    'mr:InChiKey' = c('FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')),
  PbNO3 = list(
    'mr:name' = 'Lead nitrate',
    'mr:InChI' = c('1S/2NO3.Pb/c2*2-1(3)4;/q2*-1;+2', version = '1.0.6'),
    'mr:InChiKey' = c('RLJMLMKIBZAXJO-UHFFFAOYSA-N', version = '1.0.6')),
  PbII = list(
    'mr:name' = 'lead(2+)',
    'mr:InChI' = c('1S/Pb/q+2', version = '1.0.6'),
    'mr:InChiKey' = c('RVPVRDXYQKGNMQ-UHFFFAOYSA-N', version = '1.0.6'))
)

RORs <- list(
  NIST = list(
    'mr:laboratoryName' = c('National Institute of Standards and Technology', lang = 'EN'),
    'mr:laboratoryShortName' = 'NIST',
    'mr:country' = 'US',
    'mr:laboratoryROR' = 'https://ror.org/05xpvk416',
    'mr:laboratoryURL' = 'https://www.nist.gov/'),
  UNIIM = list(
    'mr:laboratoryName' = c('Ural Scientific Research Institute for Metrology', lang = 'EN'),
    'mr:laboratoryShortName' = 'UNIIM',
    'mr:country' = 'RU',
    'mr:laboratoryAltName' = c('Уральский научно-исследовательский институт метрологии', lang = 'RU'),
    'mr:laboratoryROR' = '',
    'mr:laboratoryURL' = 'https://uniim.ru/')
)
