BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Trazabilidad Metrológica de las Balanzas')), 
      width = 9, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      uiOutput(ns('CalibCertPicker')),
      h6('Para solicitar la inclusión o la actualización de un certificado de calibración digital
          mande un correo a caparedes@inm.gov.co.'), tags$hr(),
      #tags$b('Visualizar certificados de calibración digitales:'),
      #  uiOutput(ns('CalibCertVisualizer')), 
      fluidRow(
        column(4, materialSwitch(ns('ShallprintTheCalibCert'), label = 'Mostrar certificado de calibración digital', 
                                 value = FALSE, status = "primary")),
        column(4, materialSwitch(ns('ShallplotTheCalibCert'), label = 'Mostrar gráfico de error de indicación', 
                                 value = FALSE, status = "primary"))),
      #checkboxGroupInput(ns('CalibCertPlotPrint'), label = NULL,
      #                   choices = list('Información completa' = 'Complete',
      #                                  'Gráfico de error de indicación' = 'Plot'), 
      #                   inline = TRUE),
      verbatimTextOutput(ns('printTheCalibCert')), 
      conditionalPanel(condition = "input.ShallplotTheCalibCert",
                       ns = ns,
                       plotOutput(ns('plotTheCalibCert'), width = '95%'))
  )
}

BalanceCalibCertServer <- function(input, output, session) {
  #saveRDS(MT.XPE.205, file = paste0('www/calibCert/Mettler Toledo XPE 205 (', MT.XPE.205$date, ').rds'))
  #saveRDS(MT.XPE.204, file = paste0('www/calibCert/Mettler Toledo XPE 204 (', MT.XPE.204$date, ').rds'))
  #saveRDS(MT.XPE.504, file = paste0('www/calibCert/Mettler Toledo XPE 504 (', MT.XPE.504$date, ').rds'))
  #saveRDS(MT.XP.56, file = paste0('www/calibCert/Mettler Toledo XP 56 (', MT.XP.56$date, ').rds'))
  #saveRDS(MT.XP.2002, file = paste0('www/calibCert/Mettler Toledo XP 2002 (', MT.XP.2002$date, ').rds'))
  CalibCertArchivos <- gsub('.rds', '', list.files(path = 'www/calibCert'))
  CalibCertList <- lapply(X = paste0('www/calibCert/', CalibCertArchivos, '.rds'), FUN = readRDS)
  names(CalibCertList) <- lapply(X = CalibCertList, FUN = balID)
  CalibCertShow <- as.list(names(CalibCertList)); names(CalibCertShow) <- CalibCertArchivos
  
  CalibCertPicker <- pickerInput(session$ns("CalibCertElected"), 
                                 label = 'Verifique los certificados de calibración de las balanzas que necesita:',
                                 choices = CalibCertShow, width = '100%',# inline = FALSE,
                                 multiple = FALSE,
                                 options = list(#`actions-box` = TRUE, size = 10, `deselect-all-text` = "Deseleccionar todos",
                                                #`select-all-text` = "Seleccionar todos", 
                                                `none-selected-text` = "Revise la vigencia del certificado (fecha)"))
  
  
  #CalibCertVisualizer <- reactive(selectizeInput(session$ns('CalibCertPrint'), 
  #                                               label = "Seleccione el certificado de calibracion:", 
  #                                               multiple = FALSE,  width = '50%', choices = names(CalibCertList)))
  
  #complete <- reactive('Complete' %in% input$CalibCertPlotPrint)
  plotTheCalibCert <- reactive({
    if (input$ShallplotTheCalibCert) plot(CalibCertList[[input$CalibCertElected]])
    })
  
  output$printTheCalibCert <- renderPrint({
    if (input$ShallprintTheCalibCert) {
      cat('Certificado de calibración digital balanza', input$CalibCertElected, '\n\n')
      print(CalibCertList[[input$CalibCertElected]])#, complete = complete())
    }
  })
  output$plotTheCalibCert  <- renderPlot(plotTheCalibCert())
  #output$CalibCertVisualizer <- renderUI(CalibCertVisualizer())
  output$CalibCertPicker <- renderUI(CalibCertPicker)
}