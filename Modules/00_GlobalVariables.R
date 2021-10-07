## CalibCerts
#saveRDS(MT.XPE.205, file = paste0('www/calibCert/Mettler Toledo XPE 205 (', MT.XPE.205$date, ').rds'))
#saveRDS(MT.XPE.204, file = paste0('www/calibCert/Mettler Toledo XPE 204 (', MT.XPE.204$date, ').rds'))
#saveRDS(MT.XPE.504, file = paste0('www/calibCert/Mettler Toledo XPE 504 (', MT.XPE.504$date, ').rds'))
#saveRDS(MT.XP.56, file = paste0('www/calibCert/Mettler Toledo XP 56 (', MT.XP.56$date, ').rds'))
#saveRDS(MT.XP.2002, file = paste0('www/calibCert/Mettler Toledo XP 2002 (', MT.XP.2002$date, ').rds'))
CalibCertArchivos <- gsub('.rds', '', list.files(path = 'www/calibCert'))
CalibCertList <- lapply(X = paste0('www/calibCert/', CalibCertArchivos, '.rds'), FUN = readRDS)
names(CalibCertList) <- lapply(X = CalibCertList, FUN = function(x) return(x$balanceID))
CalibCertShow <- as.list(names(CalibCertList)); names(CalibCertShow) <- CalibCertArchivos

## MRCs
MRCs.ArchiveNames <- list(Pb = gsub('.pdf', '', list.files(path = 'www/CertMRC/Pb')),
                          EDTA = gsub('.pdf', '', list.files(path = 'www/CertMRC/EDTA')))

MRCs.MassFraction <- list(Pb = list(c(round(207.209 / 331.219, 6), 3e-4/2)), # NIST SRM 928
                          EDTA = list(c(0.9986, 3e-4/2)))     # UNIM GSO 2960-84
                          
MRC.At_MolWeigths <- list(Pb = list(c(207.209, 0.005)), # NIST SRM 928 # Decid'i no dividir por raiz de tres
                          EDTA = list(c(3.722368e+02, 6.332914e-03))) # UNIM GSO 2960-84

MRC.densities     <- list(Pb = list(c(4.53, 0.05)), # NIST SRM 928 # Decid'i no dividir por raiz de tres
                          EDTA = list(c(0.860, 0.005))) # UNIM GSO 2960-84

MRC.ExpiricyDates <- list(Pb = list(as.Date('2026-01-01')), # NIST SRM 928 # Preguntar cuando abrieron el frasco
                          EDTA = list(as.Date('2020-08-30'))) # UNIM GSO 2960-84
                          
names(MRCs.MassFraction$Pb) <- names(MRC.At_MolWeigths$Pb) <- names(MRC.ExpiricyDates$Pb) <- names(MRC.densities$Pb) <- MRCs.ArchiveNames$Pb
names(MRCs.MassFraction$EDTA) <- names(MRC.At_MolWeigths$EDTA) <- names(MRC.ExpiricyDates$EDTA) <- names(MRC.densities$EDTA) <- MRCs.ArchiveNames$EDTA
