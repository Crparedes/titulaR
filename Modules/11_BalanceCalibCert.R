BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Trazabilidad Metrológica de las Balanzas')), 
      width = 9, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      #uiOutput(ns('CalibCertPicker')),
      pickerInput(ns("CalibCertElected"), 
                  label = 'Verifique la disponibilidad de los certificados de calibración de las balanzas que necesita:',
                  choices = CalibCertShow, width = '100%',# inline = FALSE,
                  multiple = FALSE, 
                  options = list(#`actions-box` = TRUE, size = 10, `deselect-all-text` = "Deseleccionar todos",
                    #`select-all-text` = "Seleccionar todos", 
                    `none-selected-text` = "Revise la vigencia del certificado (fecha)")),
      h6('Para solicitar la inclusión o la actualización de un certificado de calibración digital
          mande un correo a caparedes@inm.gov.co.'), tags$hr(),
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
  #return()
}