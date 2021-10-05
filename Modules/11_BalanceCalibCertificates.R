BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Trazabilidad Metrológica de las Balanzas')), 
      width = 9, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      uiOutput(ns('CalibCertPicker')),
          'Para solicitar la inclusión o la actualización de un certificado de calibración digital
          mande un correo a caparedes@inm.gov.co.', tags$hr(),
      tags$b('Visualizar certificados de calibración digitales:'),
        uiOutput(ns('CalibCertVisualizer')), 
        checkboxGroupInput(ns('CalibCertPlotPrint'), label = NULL,
                           choices = list('Información completa' = 'Complete',
                                          'Gráfico de error de indicación' = 'Plot'), 
                           inline = TRUE),
        verbatimTextOutput(ns('printCalibCert')), plotOutput(ns('plotCalibCert'))
      
  )
}

BalanceCalibCertServer <- function(input, output, session) {
  
  #saveRDS(MT.XPE.205, file = paste0('www/calibCert/Mettler Toledo XPE 205 (', MT.XPE.205$date, ').rds'))
  #saveRDS(MT.XPE.204, file = paste0('www/calibCert/Mettler Toledo XPE 204 (', MT.XPE.204$date, ').rds'))
  #saveRDS(MT.XPE.504, file = paste0('www/calibCert/Mettler Toledo XPE 504 (', MT.XPE.504$date, ').rds'))
  #saveRDS(MT.XP.56, file = paste0('www/calibCert/Mettler Toledo XP 56 (', MT.XP.56$date, ').rds'))
  #saveRDS(MT.XP.2002, file = paste0('www/calibCert/Mettler Toledo XP 2002 (', MT.XP.2002$date, ').rds'))
  CalibCertArchivos <- gsub('.rds', '', list.files(path = 'www/calibCert'))
  CalibCertPicker <- pickerInput(session$ns("CalibCertElected"), label = 'Seleccione los certificados de calibración de las balanzas que necesita:',
                                 choices = CalibCertArchivos, width = '100%',# inline = FALSE,
                                 multiple = TRUE,
                                 options = list(#`actions-box` = TRUE, size = 10, `deselect-all-text` = "Deseleccionar todos",
                                                #`select-all-text` = "Seleccionar todos", 
                                                `none-selected-text` = "(Revise la fecha del certificado)"))
  
  

  CalibCertList <- lapply(X = paste0('www/calibCert/', CalibCertArchivos, '.rds'), FUN = readRDS)
  names(CalibCertList) <- lapply(X = CalibCertList, FUN = balID)
  CalibCertVisualizer <- reactive(selectizeInput(session$ns('CalibCertPrint'), 
                                                 label = "Seleccione el certificado de calibracion:", 
                                                 multiple = FALSE,  width = '50%', choices = names(CalibCertList)))
  
  output$printCalibCert <- renderPrint(print(CalibCertList[[input$CalibCertPrint]]))
  output$CalibCertVisualizer <- renderUI(CalibCertVisualizer())
  output$CalibCertPicker <- renderUI(CalibCertPicker)
}