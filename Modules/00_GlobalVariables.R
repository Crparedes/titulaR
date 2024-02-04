## balanzass
#saveRDS(MT.XPE.205, file = paste0('www/balanzas/Mettler Toledo XPE 205 (', MT.XPE.205$date, ').rds'))
#saveRDS(MT.XPE.204, file = paste0('www/balanzas/Mettler Toledo XPE 204 (', MT.XPE.204$date, ').rds'))
#saveRDS(MT.XPE.504, file = paste0('www/balanzas/Mettler Toledo XPE 504 (', MT.XPE.504$date, ').rds'))
#saveRDS(MT.XP.56, file = paste0('www/balanzas/Mettler Toledo XP 56 (', MT.XP.56$date, ').rds'))
#saveRDS(MT.XP.2002, file = paste0('www/balanzas/Mettler Toledo XP 2002 (', MT.XP.2002$date, ').rds'))

balanzasPATH <- 'www/Balanzas/'
balanzasArchivos <- gsub('.rds', '', list.files(path = balanzasPATH))
balanzasList <- lapply(X = paste0(balanzasPATH, balanzasArchivos, '.rds'), FUN = readRDS)

names(balanzasList) <- balanzasArchivos #lapply(X = balanzasList, FUN = function(x) return(x$balanceID))
balanzasShow <- as.list(c(names(balanzasList)))
names(balanzasShow) <- c(balanzasArchivos)

TemperatureUnits <- list('°C' = '\\degreecelsius')
AtmosPressuUnits <- list('hPa' = '\\hecto\\pascal')
RelatiHumidUnits <- list('%' = '\\percent')
DensityUnits <- list('g/cm^3' = '\\gram\\centi\\meter\\tothe{-3}')
CobertureFactors <- list('(k = 1)' = 1, '(k = 1.96)' = 1.96, '(k = 2)' = 2)
Distributions <- list('Normal')

elemEspa <- list(lead = 'plomo', camdmium = 'cadmio', calcium = 'calcio')




## Personal
files <- gsub('', '', list.files(path = 'www/Personal/', pattern = 'xml', full.names = TRUE))
authPersons <- lapply(files, function (x) read_xml(x))
names(authPersons) <- sapply(authPersons, function (x) as_list(x)$respPerson$name)
names(authPersons) <- sapply(authPersons, function (x) xml_text(xml_child(x, search = './/name')))




DummyNumber <- c(0, 0.1234567899999999999)
demoData <- data.frame('Titrant' = c(4.5781, 4.5865, 4.5887, 4.5930, 4.5970,
                                     4.6009, 4.6046, 4.6097, 4.6143, 4.6180, 4.6268, 4.6357, rep(NA, 9)),
                       'Signal' = c(-199.8, -203.5, -204.9, -207.8, -211.4,
                                    -216.8, -224.3, -237.5, -246.2, -247.8, -249.5, -250.0, rep(NA, 9)),
                       'DerAppr' = 0)

voidData <- data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29)), 'DerAppr' = c(0.1, rep(NA, 29)))

  
