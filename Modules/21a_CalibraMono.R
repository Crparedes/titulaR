CalibraMonoUI <- function(id) {
  ns <- NS(id)
  tags$hr()
  fluidRow(
    column(1, tags$br()),
    column(6,
           radioButtons(ns('Elemento'), label = 'Especie en la disolucion calibrante: ', inline = TRUE, 
                        choices = list('Plomo (II)' = 'Pb', 'Cadmio (II)' = 'Cd', 'Calcio (II)' = 'Ca')),
           tags$div(
             id = "inlineBOT", style = 'font-size:12px', 
             pickerInput(ns("CalibCertMRC"), label = 'Balanza utilizada:_',
                         choices = CalibCertShow, width = '100%', selected = 'MT XPE 205', multiple = FALSE),
             numericInput(ns('MasaAlic'), value = 0, label = 'Masa de la alicuota:_')),
           tags$hr(),
           column(1), column(6, rHandsontableOutput(ns("TitData"), width = '100%'))),
    column(5, tags$br(), tags$br(), tags$br(), tags$br(), 
           column(12, align = 'center', plotOutput(ns('TitCurve'), width = '90%'))))
}

CalibraMonoServer <- function(input, output, session) {
  cleanTable <- data.frame('Masa titulante [g]' = rep(NA, 26),  'Potencial [mV]' = rep(NA, 26))
  
  TitCurve <- reactive(
    plot(1:4)
  )
  
  output$TitData <- renderRHandsontable(
    {rhandsontable(cleanTable, colHeaders = c('Masa de titulante [g]', 'Diferencia de potencial \n  [mV]'), readOnly = FALSE, 
                   fillHandle = list(direction = 'vertical', autoInsertRow = TRUE), ) %>% 
        hot_col(col = 1, type = 'numeric', format = "0.0000") %>% 
        hot_col(col = 2, type = 'numeric', format = "0.0") %>% 
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
      })
  output$TitCurve <- renderPlot(TitCurve())
}