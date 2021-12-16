# CalibraMonoUI <- function(id) {
#   ns <- NS(id)
#   tags$hr()
#   fluidRow(
#     #verbatimTextOutput(ns('test')),
#     column(1, tags$br()),
#     column(6, tags$br(), tags$br(), 
#            radioButtons(ns('Elemento'), label = 'Especie en la disolucion calibrante: ', inline = TRUE, 
#                         choices = list('Plomo (II)' = 'Pb', 'Cadmio (II)' = 'Cd', 'Calcio (II)' = 'Ca'), 
#                         selected = character(0)),
#            conditionalPanel(condition = 'input.Elemento == "Pb"', ns = ns, 
#                             tags$div(id = "inline", style = 'font-size:12px',
#                                      splitLayout(cellWidths = c("40%", "12%"),
#                                                  numericInput(ns('LeadAM'), label = 'Masa atomica del plomo [g mol$^{-1}$]: .', value = 207.20),
#                                                  numericInput(ns('u_LeadAM'), label = '\u00B1', value = 0.06)))),
#            tags$hr(),
#            tags$div(
#              id = "inlineTOP", style = 'font-size:12px', 
#              pickerInput(ns("CalibCertMRC"), label = 'Balanza utilizada: .',
#                          choices = CalibCertShow, width = '500px', selected = 'MT XPE 205', multiple = FALSE),
#              numericInput(ns('MasaAlic'), value = 10, label = 'Masa de la alicuota: .'),
#              textInput(ns('sampleID'), label = 'Identificacion muestra: .', value = 'Calibrante'),
#              textAreaInput(ns('dscrMuestra'), label = 'Observaciones:  .', rows = 2, 
#                            placeholder = '(Informacion adicional)', width = '100%')) , # Cambiar a cero de nuevo!
#            actionButton(ns('InitTit'), label = 'Iniciar nueva titulacion'), tags$hr(),
#            
#            tags$hr(),
#            conditionalPanel(
#              condition = 'input.MasaAlic > 0', ns = ns,
#              rHandsontableOutput(ns("TitData"), width = '100%')
#            )),
#     column(5, tags$br(), tags$br(), tags$br(), tags$br(), 
#            tags$b('Curva de titulacion:'), tags$br(), 
#            fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '90%'))), tags$hr(), tags$br(),
#            actionButton(ns('TermTit'), label = 'Terminar titulacion'), tags$hr(),
#            uiOutput(ns('TitulTerminada'))
#            ))
# }
# 
# CalibraMonoServer <- function(input, output, session, DisEDTA_MRC, IDUsuario) {
#   # https://www.ardata.fr/en/post/2019/07/01/dynamic-module-call/
#   rv <- reactiveValues(all_ui = list())
#   titRationMonoElem1 <- callModule(module = CalibraMonoIndividualServer, id = "titRationMonoElem1")
# 
#   TableDat_0  <- reactiveValues(hot = data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29)), 'DerAppr' = c(0.1, rep(NA, 29))))
#   TableData <- reactive({
#     DT <- NULL
#     
#     if (!is.null(input$TitData)) {
#       DT <- setDT(hot_to_r(input$TitData))
#       TableDat_0[["hot"]]  <-  DT
#     } else {#For initial data upload
#       if (!is.null(TableDat_0[["hot"]])) {DT <- TableDat_0[["hot"]]}
#     }
#     if (!is.null(DT)) {
#       nDat <- length(na.omit(DT$Signal))
#       try(isolate(DT$DerAppr[1:nDat] <- c(NA, abs((DT$Signal[2:nDat] - DT$Signal[1:(nDat - 1)])/(DT$Titrant[2:nDat] - DT$Titrant[1:(nDat - 1)]))/100)))
#       #rhandsontable(DT)
#       rhandsontable(DT, colHeaders = c('Masa de titulante [g]', 'Diferencia de potencial \n  [mV]', 'Derivada aprox. abs. \n |d(m)/d(V)|'), 
#                     readOnly = FALSE, fillHandle = list(direction = 'vertical', autoInsertRow = TRUE)) %>% 
#         hot_col(col = 1, type = 'numeric', format = "0.0000") %>% 
#         hot_col(col = 2, type = 'numeric', format = "0.0") %>%  
#         hot_col(col = 3, type = 'numeric', format = "0.000", readOnly = TRUE, halign = 'htRight') %>% 
#         hot_validate_numeric(col = 1, min = 0, allowInvalid = TRUE) %>% 
#         hot_heatmap(cols = 3, color_scale = c('#edf2f4', '#9caac6')) %>% 
#         hot_table(highlightCol = TRUE, highlightRow = TRUE)
#     }
#   })
#   
#   TitCurvDat <- reactive(hot_to_r(input$TitData))
# 
#     
#   output$TitData <- renderRHandsontable(TableData())
#   
#   CleanDf <- eventReactive(input$TermTit, {
#     dff <- as.data.frame(TitCurvDat()[(as.numeric(!is.na(TitCurvDat()$Titrant)) + as.numeric(!is.na(TitCurvDat()$Signal))) > 1, ])
#     dff <- dff[dff[, 1] > 0, ]
#     if(nrow(dff) > 3) {
#       return(df2t.curve(df = dff, plot = FALSE))
#     } else {
#       return()
#     }
#   })
#   
#   TitCurvePlot <- reactive(
#     tryCatch(
#       expr = {
#         if(length(na.omit(TitCurvDat()$Titrant)) >= 3) {
#           if(input$TermTit < 1) {
#             plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0])
#           } else {
#             plotCurve(curve = CleanDf(), xlab = 'Titulante [g]', ylab = 'Señal [mV]')
#           }
#         } else {
#           plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
#           text(x = 0.5, y = 0.5, paste("No hay suficientes datos..."), 
#                cex = 1.6, col = "black")
#         }}, 
#       error = function(cond) {
#         plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
#         text(x = 0.5, y = 0.5, paste("No hay datos..."), 
#              cex = 1.6, col = "black")
#              })
#   )
#   
#   TitulTerminada <- eventReactive(input$TermTit, {
#     if(length(na.omit(TitCurvDat()$Titrant)) < 7) {
#       tags$b('No se puede terminar la titulacion con menos de 8 datos!')
#     } else {
#       tags$div(
#         infoBox(
#           width = 12, title = tags$b('Resultados parciales de titulacion'), icon = icon("vials"), color = 'black', 
#           value = DisEDTA_MRC$infoDisMRC()),
#           #tags$html(tags$h5(DisEDTA_MRC$infoDisMRC() , 'Fraccion masica de la especie en la disolucion: ', tags$b(round(100, 3), '[mg kg$^{-1}]')), #tags$br(),
#           #                  tags$h6('Los calculos de incertidumbre se realizan cuando se combinen los resultados individuales de titulacion'))),
#         downloadButton(session$ns('DwnlResFile'), 'Descargar archivo .res con resultados individuales'), tags$hr(), tags$hr(),
#         actionButton(session$ns('Restarter'), 'Iniciar nueva titulacion', icon = icon("gear"))
#       )
#     }
#   })
#   
#   MasaEquiv <- reactive(try(EP.1stDer(curve = CleanDf())))
#   MasAtoElem <- reactive(ifelse(input$Elemento == 'Pb', c(input$LeadAM, input$u_LeadAM), ElementsAtomicMass[[input$Elemento]]))
#   ResParcial <- reactive(MasaEquiv() * DisEDTA_MRC$infoDisMRC()$`Concentracion [mmol/kg]` / input$MasaAlic * MasAtoElem()[1])
#   
#   summaryTitration <- reactive(
#     list(date = Sys.time(), Elemento = input$Elemento, DisEDTA = DisEDTA_MRC$infoDisMRC(), 
#          MasaMuestra = input$MasaAlic, TitCurvDat = TitCurvDat(),
#          dscrMuestra = input$dscrMuestra, 
#          MasAtoElem = MasAtoElem()))
#   
#   output$DwnlResFile <- downloadHandler(
#     filename = function() {paste0(Elemento, "_", input$sampleID, "_", format(Sys.time(), '%Y-%m-%d_%H-%M'), ".res")},
#     content = function(file) {saveRDS(summaryTitration(), file = file)}, contentType = NULL)
#   
#   output$TitCurvePlot <- renderPlot(TitCurvePlot())
#   output$TitulTerminada <- renderUI(TitulTerminada())
#   output$DescaResu <- renderUI(DescaResu())
#   
#   observeEvent(input$Restarter, {
#     # NADA DE LO QUE SIGUE FUNCIONA.,, COMO SE PODRIA GENERAR UNA PESTA:A NUEVA_ ESO SER"IA HASTA MEJOR PARA QUE NA=O SE BORREN DATOS SIN DESCARGAR...
#     #TitulTerminada <- reactive(tags$b('Nueva titulacion en proceso'))
#     #input$TermTit <- 0
#     #TableDat_0[["hot"]]  <- data.frame('Titrant' = c(0.0001, rep(NA, 29)),  'Signal' = c(0.1, rep(NA, 29)), 'DerAppr' = c(0.1, rep(NA, 29)))
#   }
#   )
# }