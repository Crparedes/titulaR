#saveRDS(MT.XPE.205, file = paste0('www/calibCert/Mettler Toledo XPE 205 (', MT.XPE.205$date, ').rds'))
#saveRDS(MT.XPE.204, file = paste0('www/calibCert/Mettler Toledo XPE 204 (', MT.XPE.204$date, ').rds'))
#saveRDS(MT.XPE.504, file = paste0('www/calibCert/Mettler Toledo XPE 504 (', MT.XPE.504$date, ').rds'))
#saveRDS(MT.XP.56, file = paste0('www/calibCert/Mettler Toledo XP 56 (', MT.XP.56$date, ').rds'))
#saveRDS(MT.XP.2002, file = paste0('www/calibCert/Mettler Toledo XP 2002 (', MT.XP.2002$date, ').rds'))
CalibCertArchivos <- gsub('.rds', '', list.files(path = 'www/calibCert'))
CalibCertList <- lapply(X = paste0('www/calibCert/', CalibCertArchivos, '.rds'), FUN = readRDS)
names(CalibCertList) <- lapply(X = CalibCertList, FUN = function(x) return(x$balanceID))
CalibCertShow <- as.list(names(CalibCertList)); names(CalibCertShow) <- CalibCertArchivos