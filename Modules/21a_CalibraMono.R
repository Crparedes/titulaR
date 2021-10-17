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
           column(1), column(6, rHandsontableOutput(ns("TableData"), width = "100%"),
                             rHandsontableOutput(ns("TitData"), width = '100%'))),
    column(5, tags$br(), tags$br(), tags$br(), tags$br(), 
           tags$b('Curva de titulacion:'), tags$br(), 
           fluidRow(column(12, align = 'center', plotOutput(ns('TitCurve'), width = '90%'))), tags$hr(), tags$br(),
           actionButton(ns('TermTit'), label = 'Terminar titulacion'), tags$hr(),
           conditionalPanel(
             condition = 'input.TermTit > 0', ns = ns,
             tags$div(id = "inlineBOT",
                      downloadButton(ns('DwnlResFile'), 'Descargar resultados individuales .res'),
                      #downloadButton(ns('DwnlPDFPartial'), 'Informe de resultados parcial (PDF)')
                      )
           )
           #uiOutput(ns('DescaResu'))
           ))
}

CalibraMonoServer <- function(input, output, session) {
  TableDat_0  <- reactiveValues(hot = data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29))))
  TableData <- reactive({
    DT <- NULL
    if (!is.null(input$TitData)) {
      DT <-  setDT(hot_to_r(input$TitData))
      TableDat_0[["hot"]]  <-  DT
    } else {
      if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
    }
    if (!is.null(DT)) {
      #rhandsontable(DT)
      rhandsontable(DT, colHeaders = c('Masa de titulante [g]', 'Diferencia de potencial \n  [mV]'), readOnly = FALSE, 
                    fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
        hot_col(col = 1, type = 'numeric', format = "0.0000") %>% 
        hot_col(col = 2, type = 'numeric', format = "0.0") %>% 
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  output$TitData <- renderRHandsontable(TableData())
  
  TitCurve <- reactive(
    plot(hot_to_r(input$TitData))
    #plotCurve(curve = df2t.curve(hot_to_r(input$TitData)))
  )
  #observeEvent(input$TitData, {
  #  test_df <- hot_to_r(input$TitData)
  #  print(test_df)
    # browser() # uncomment for debugging
  #})
  
  output$TitCurve <- renderPlot(TitCurve())
  output$DescaResu <- renderUI(DescaResu())
}