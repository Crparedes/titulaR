# EDTACombUI <- function(id) {
#   ns <- NS(id)
#   fluidRow(
#     #verbatimTextOutput(ns('test')),
#     column(3, 
#            tags$b('Importar archivos .tit de resultados individuales'),
#            fileInput(ns('TitFiles'), width = '100%', 
#                      label = NULL, multiple = TRUE, accept = '.tit'),
#            uiOutput(ns('buttonUpload')), tags$hr(),
#            uiOutput(ns('visualizacion')),
#            conditionalPanel(condition = 'input.visualizacion == "Comb"', ns = ns, uiOutput(ns('titFilesSelectComb'))),
#            conditionalPanel(condition = 'input.visualizacion == "Indi"', ns = ns, uiOutput(ns('titFilesSelectIndi'))), 
#            conditionalPanel(condition = 'input.buttonUpload > 0', ns = ns, 
#                             uiOutput(ns('Calcular'))),
#            tags$hr(),
#            #radioButtons(ns('independCriteria'), label = 'Criterio de independiencia', 
#            #             choices = list('Diferente dia' = 'dia', 'Diferente disolucion titulante' = 'DifDisTit'), 
#            #             inline = TRUE, selected = 'DifDisTit'),
#            verbatimTextOutput(ns('printed'))
#            ),
#     column(9, 
#            conditionalPanel(condition = 'input.visualizacion == "Comb"', ns = ns, 
#                             tags$b('Combinación de resultados'), tags$hr(),
#                             column(7, plotOutput(ns('plotCombinados'), width = '100%')),
#                             column(5, box(title = tags$b('Resumen de resultados combinados'), width = 12, status = 'primary',
#                                           tableOutput(ns('resultadosCombi'))))),
#            conditionalPanel(condition = 'input.visualizacion == "Indi"', ns = ns, 
#                             tags$b('Visualizacion de resultado individual'), tags$hr(),
#                             column(7, plotOutput(ns('plotIndiv'), width = '100%')),
#                             column(5, uiOutput(ns('resIndiv'))))#,
#            #uiOutput(ns('Cajas'))
#     ))
# }
# 
# EDTACombServer <- function(input, output, session, IDUsuario, brwzMDL) {
#   FileNames <- reactive(input$TitFiles$name)
#   
#   buttonUpload <- eventReactive(input$TitFiles,
#     ifelse(all(substr(FileNames(), start = 1, stop = 5) == 'EDTA.'),
#            return(actionButton(session$ns('buttonUpload'), label = tags$b('Subir archivos'))),
#            return(box(status = 'danger', width = 12, tags$b('Todos los archivos deben ser de muestras de EDTA. Intente nuevamente')))))
#   output$buttonUpload <- renderUI(buttonUpload())
#   
#   visualizacion <- eventReactive(input$buttonUpload, {
#     radioButtons(session$ns('visualizacion'), label = NULL,
#                  choices = list('Combinar los resultados' = 'Comb', 'Visualizar resultado individual' = 'Indi'))})
#   output$visualizacion <- renderUI(visualizacion())
#   
#   Calcular <- reactive(
#     ifelse(length(unique(substr(FileNames(), start = 1, stop = 3))) == 1,
#            return(actionButton(session$ns('Calcular'), label = tags$b('Mostrar/recalcular resultados'))),
#            return(NULL)))
#   output$Calcular <- renderUI(Calcular())
#   
#   
#   DataCompl <- reactiveValues()
#   observeEvent(input$TitFiles, {
#     #DataCompl <- reactiveValues()
#     for (i in 1:length(FileNames())) DataCompl[[FileNames()[i]]] <- readRDS(input$TitFiles[i, ]$datapath)
#     #browser()
#   })
#   
#   # DataComplList <- reactive({
#   #   x <- reactiveValuesToList(DataCompl)
#   #   x <- x[names(x) %in% FileNames()] # To clean old files
#   #   return(x)})
#   
#   titFilesSelectComb <- reactive({
#     x <- reactiveValuesToList(DataCompl)
#     x <- x[names(x) %in% FileNames()]
#     Fechas <- as.factor(unlist(sapply(x, 
#                                       function(x) {substr(as.character(x[[12]]), 
#                                                           start = nchar(as.character(x[[12]])) - 18, 
#                                                           stop = nchar(as.character(x[[12]])) - 3)})))
#     VecMomento <- as.factor(unlist(sapply(x, function(x) {x[[11]]})))
#     choices <- names(x)[order(VecMomento)]
#     #browser()
#     return(checkboxGroupInput(session$ns('titFilesSelectComb'), label = tags$b("Archivos a considerar:"), 
#                               choices = choices, selected = choices, width = '100%'))
#   })
#   output$titFilesSelectComb <- renderUI(titFilesSelectComb())
#   
#   DataCleanDF <- eventReactive(input$Calcular, {
#     #browser()
#     x <- reactiveValuesToList(DataCompl)
#     x <- x[names(x) %in% FileNames()]
#     DataTrimmedList <- x[names(x) %in% input$titFilesSelectComb] # To consider only selected files
#     VecMomento <- as.factor(unlist(sapply(DataTrimmedList, function(x) {x[[11]]})))
#     
#     VecMuestra <- unlist(sapply(DataTrimmedList, function(x) {x[[1]]}))[order(VecMomento)]
#     VecDescrip <- unlist(sapply(DataTrimmedList, function(x) {ifelse(!is.null(x[[6]][[3]]), x[[6]][[3]], x[[6]][[2]])}))[order(VecMomento)]
#     VecMasaAli <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[2]]})))[order(VecMomento)]
#     VecMasaEqi <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[3]]})))[order(VecMomento)]
#     VecFraccMa <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[4]]})))[order(VecMomento)]
#     VecFracUnc <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[5]]$prop[[3]]})))[order(VecMomento)]
#     VecFechas0 <- as.factor(unlist(sapply(DataTrimmedList, function(x) {substr(as.character(x[[12]]), start = 1, stop = 10)})))[order(VecMomento)]
#     #browser()
#     x <- data.frame(VecMuestra, VecDescrip, VecMasaAli, VecMasaEqi, VecFraccMa, VecFracUnc, VecFechas0,
#                     index = 1:length(VecFracUnc))
#     #browser()
#     return(x)
#   })
#   
#   resultadosCombi <- eventReactive(DataCleanDF(), {
#     AverageValue <- mean(DataCleanDF()$VecFraccMa)
#     IncertTipoB <- max(DataCleanDF()$VecFracUnc)
#     StandarDev <- sd(DataCleanDF()$VecFraccMa)
#     n_ind <- length(unique((DataCleanDF()$VecFechas0)))
#     IncertTipoA <- StandarDev/sqrt(n_ind)
#     IncertComb <- sqrt(IncertTipoB^2 + IncertTipoA^2)
#     return(data.frame('.' = c('Promedio de las mediciones', 'Incertidumbre tipo B', 'Desviacion estandar de las mediciones', 
#                               'Numero de datos', 'Numero de datos independientes (dia)', 'Incertidumbre tipo A', 'Incertidumbre combinada',
#                               'Incertidumbre expandida (k=2)'),
#                       'Valor' = as.character(c(round(c(AverageValue, IncertTipoB, StandarDev), 2), 
#                                                round(c(length((DataCleanDF()$VecFechas0)), n_ind)),
#                                                round(c(IncertTipoA, IncertComb, IncertComb * 2), 2))),
#                       'Unidades' = c(rep('%', 3), rep('', 2), rep('%', 3))))
#   })
#   
#   plotCombinados <- reactive({
#     p <- ggplot(data = DataCleanDF(), aes(x = index)) + theme_bw() + 
#       labs(y = expression(paste('Fraccion masica del elemento / ', 'mg k', g^{-1})), x = NULL) +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             axis.text.y = element_text(color = "black"),
#             #axis.ticks.x = element_blank(), 
#             axis.text.x = element_blank(), legend.title = element_blank()) +
#       scale_y_continuous(expand = c(0, 0.4), n.breaks = 8) +
#       #geom_hline(aes(yintercept = 0.999 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
#       geom_hline(aes(yintercept = 0.999 * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
#       geom_hline(aes(yintercept = mean(VecFraccMa)), linetype = 1, lwd = 0.5, col = 'gray60') +
#       geom_hline(aes(yintercept = 1.001 * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
#       #geom_hline(aes(yintercept = 1.001 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
#       geom_point(aes(y = VecFraccMa, color = VecFechas0)) + 
#       geom_errorbar(aes(ymin = VecFraccMa - VecFracUnc, ymax = VecFraccMa + VecFracUnc, color = VecFechas0), width = 0.4)
#     print(p)
#     #browser()
#     })
#   output$plotCombinados <- renderPlot(plotCombinados())
#   output$resultadosCombi <- renderTable(resultadosCombi())
#   
#   
#   titFilesSelectIndi <- reactive({
#     x <- reactiveValuesToList(DataCompl)
#     x <- x[names(x) %in% FileNames()]
#     Fechas <- as.factor(unlist(sapply(x, 
#                                       function(x) {substr(as.character(x[[12]]), 
#                                                           start = nchar(as.character(x[[12]])) - 18, 
#                                                           stop = nchar(as.character(x[[12]])) - 3)})))
#     VecMomento <- as.factor(unlist(sapply(x, function(x) {x[[12]]})))
#     choices <- names(x)[order(VecMomento)]
#     #browser()
#     return(radioButtons(session$ns('titFilesSelectIndi'), label = tags$b("Archivo:"), 
#                         choices = choices, selected = character(0), width = '100%'))
#     })
#   output$titFilesSelectIndi <- renderUI(titFilesSelectIndi())
#   
#   meanValues <- reactive(
#     NULL
#   )
#   
#   output$printed <- renderPrint({
#   #  req(input$TitFiles)
#   #  (length(input$TitFiles$name))
#   })
#   
# }