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
RelatiHumidUnits <- list('%' = '\\percent?')
CobertureFactors <- list('(k = 1)' = 1, '(k = 1.96)' = 1.96, '(k = 2)' = 2)
Distributions <- list('Normal')


## MRCs
namesMR_MRCs <-
  MRCs.ArchiveNames <- list(forEDTA = gsub('.xml', '', list.files(path = 'www/MR_MRC/Para EDTA', pattern = 'xml')),
                            forCali = gsub('.xml', '', list.files(path = 'www/MR_MRC/Para calibrantes', pattern = 'xml')))

MRCs.MassFraction <- list(Pb = list(c(round(207.209 / 331.219, 6), 3e-4/2)), # NIST SRM 928
                          EDTA = list(c(0.9986, 0.00015),# UNIM GSO 2960-84
                                      c(0.99874, 0.00038)))     # UNIM Caracterizado INM

MRC.At_MolWeigths <- list(Pb = list(c(207.209, 0.005)), # NIST SRM 928
                          EDTA = list(round(c(3.722368e+02, 6.332914e-03), 4),
                                      round(c(3.722368e+02, 6.332914e-03), 4))) # UNIM GSO 2960-84

MRC.densities     <- list(Pb = list(c(4.53, 0.05)), # NIST SRM 928 
                          EDTA = list(c(0.860, 0.005), c(0.860, 0.005))) # UNIM GSO 2960-84

MRC.ExpiricyDates <- list(Pb = list(as.Date('2026-01-01')), # NIST SRM 928 # Preguntar cuando abrieron el frasco
                          EDTA = list(as.Date('2020-08-30'), as.Date('2023-12-31'))) # UNIM GSO 2960-84

names(MRCs.MassFraction$Pb) <- names(MRC.At_MolWeigths$Pb) <- names(MRC.ExpiricyDates$Pb) <- names(MRC.densities$Pb) <- MRCs.ArchiveNames$Pb
names(MRCs.MassFraction$EDTA) <- names(MRC.At_MolWeigths$EDTA) <- names(MRC.ExpiricyDates$EDTA) <- names(MRC.densities$EDTA) <- MRCs.ArchiveNames$EDTA

ElementsAtomicMass <- list(Cd = c(112.414, 0.004/sqrt(3)),
                           Ca = c(40.078, 0.004/sqrt(3)))

## Personal
files <- gsub('', '', list.files(path = 'www/Personal/', pattern = 'xml', full.names = TRUE))
authPersons <- sapply(files, function (x) as_list(read_xml(x)))

names(authPersons) <- sapply(authPersons, function (x) x$data$name)

