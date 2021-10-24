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
             numericInput(ns('MasaAlic'), value = 10, label = 'Masa de la alicuota:_')), # Cambiar a cero de nuevo!
           tags$hr(),
           conditionalPanel(
             condition = 'input.MasaAlic > 0', ns = ns,
             rHandsontableOutput(ns("TitData"), width = '100%')
           )),
    column(5, tags$br(), tags$br(), tags$br(), tags$br(), 
           tags$b('Curva de titulacion:'), tags$br(), 
           fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '90%'))), tags$hr(), tags$br(),
           actionButton(ns('TermTit'), label = 'Terminar titulacion'), tags$hr(),
           uiOutput(ns('TitulTerminada'))
           #conditionalPanel(
          #   condition = 'input.TermTit > 0', ns = ns,
          #   tags$div(id = "inlineBOT",
          #            downloadButton(ns('DwnlResFile'), 'Descargar resultados individuales .res'),
          #            #downloadButton(ns('DwnlPDFPartial'), 'Informe de resultados parcial (PDF)')
          #            )
           #)
          ### #uiOutput(ns('DescaResu'))
           ))
}

CalibraMonoServer <- function(input, output, session) {
  TableDat_0  <- reactiveValues(hot = data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29)), 'DerAppr' = c(0.1, rep(NA, 29))))
  TableData <- reactive({
    DT <- NULL
    
    if (!is.null(input$TitData)) {
      DT <- setDT(hot_to_r(input$TitData))
      TableDat_0[["hot"]]  <-  DT
    } else {#For initial data upload
      if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
    }
    if (!is.null(DT)) {
      nDat <- length(na.omit(DT$Signal))
      try(isolate(DT$DerAppr[1:nDat] <- c(NA, abs((DT$Signal[2:nDat] - DT$Signal[1:(nDat - 1)])/(DT$Titrant[2:nDat] - DT$Titrant[1:(nDat - 1)]))/100)))
      #rhandsontable(DT)
      rhandsontable(DT, colHeaders = c('Masa de titulante [g]', 'Diferencia de potencial \n  [mV]', 'Derivada aprox. abs. \n |d(m)/d(V)|'), 
                    readOnly = FALSE, fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
        hot_col(col = 1, type = 'numeric', format = "0.0000") %>% 
        hot_col(col = 2, type = 'numeric', format = "0.0") %>%  
        hot_col(col = 3, type = 'numeric', format = "0.000", readOnly = TRUE, halign = 'htRight') %>% 
        hot_validate_numeric(col = 1, min = 0, allowInvalid = TRUE) %>% 
        hot_heatmap(cols = 3, color_scale = c('#edf2f4', '#9caac6')) %>% 
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }
  })
  
  TitCurvDat <- reactive(hot_to_r(input$TitData))

    
  output$TitData <- renderRHandsontable(TableData())
  
  CleanDf <- eventReactive(input$TermTit, {
    dff <- as.data.frame(TitCurvDat()[(as.numeric(!is.na(TitCurvDat()$Titrant)) + as.numeric(!is.na(TitCurvDat()$Signal))) > 1, ])
    dff <- dff[dff[, 1] > 0, ]
    if(nrow(dff) > 3) {
      return(df2t.curve(df = dff, plot = FALSE))
    } else {
      return()
    }
  })
  
#  observeEvent(input$TermTit, {
 #   print(CleanDf())
  #})
  
  TitCurvePlot <- reactive(
    tryCatch(
      expr = {
        if(length(na.omit(TitCurvDat()$Titrant)) >= 3) {
          if(input$TermTit < 1) {
            plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0])
          #}
          } else {
            #if(length(na.omit(TitCurvDat()$Titrant)) < 7) {
            #  plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0],
            #       main = 'No se puede terminar la titulacion con menos de 8 datos!')
            #} else {
              plotCurve(curve = CleanDf(), xlab = 'Titulante [g]', ylab = 'Señal [mV]')
            #}
          }
        } else {
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("No hay suficientes datos..."), 
               cex = 1.6, col = "black")
        }}, 
      error = function(cond) {
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, paste("No hay datos..."), 
             cex = 1.6, col = "black")
             })
    #plotCurve(curve = df2t.curve(hot_to_r(input$TitData)))
  )
  #observeEvent(input$TitData, {
  #  test_df <- hot_to_r(input$TitData)
  #  print(test_df)
    # browser() # uncomment for debugging
  #})
  
  TitulTerminada <- eventReactive(input$TermTit, {
    if(length(na.omit(TitCurvDat()$Titrant)) < 7) {
      tags$b('No se puede terminar la titulacion con menos de 8 datos!')
    } else {
      tags$div(id = "inlineBOT",
               downloadButton(session$ns('DwnlResFile'), 'Descargar resultados individuales .res'),
               #downloadButton(session$ns('DwnlPDFPartial'), 'Informe de resultados parcial (PDF)')
      )
    }
  })
  
  
  output$TitCurvePlot <- renderPlot(TitCurvePlot())
  output$TitulTerminada <- renderUI(TitulTerminada())
  output$DescaResu <- renderUI(DescaResu())
}