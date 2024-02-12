CombinaResultadosUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, Nlns(4), uiOutput(ns('brwz')),
           tags$h4(style = 'margin-left: 60px;', tags$b('Combinación de resultados individuales de titulación'))),
    column(
      width = 3, style = 'margin-left: 80px;', tags$br(),
      # fluidRow(
        # column(width = 3, SI_unit_nice('mole', width = "110%"), SI_unit_nice('kilogram', width = "110%")),
        # column(
          # width = 9,
          # tags$hr(),
      tags$b('Importe archivos XML de sesiones anteriores.'),
      tags$div(
        style = 'margin-left: 25px; margin-top:0px;',
        fileInput(ns('NewXML'), label = NULL, buttonLabel = 'Examinar...', multiple = TRUE, accept = '.xml', width = '100%'),
        uiOutput(ns('XmlCargados')))),
    column(
      width = 7, style = 'margin-left: 50px;', tags$br(),
      tags$br(),
      box(
        id = ns('FilesAvailable'), status = 'primary', title = tags$b('Resultados individuales'), width = 12, collapsible = TRUE, collapsed = FALSE,
        'Seleccione los resultados individuales de titulación para combinar marcando la casilla al inicio de cada fila.', tags$br(),
        'Solo puede combinar resultados de la misma sustancia.', tags$br(),
        tags$div(style = 'margin-left: 25px; margin-top:20px; border-style:groove;', rHandsontableOutput(ns("resultFiles"))),
        Nlns(), actionButton(ns('CombinArchivos'), label = tags$b('Combinar resultados individuales'), style = 'margin-left:25px;'))),
    column(11, hidden(tags$div(
      style = 'margin-left: 80px;',                         
      id = ns('combinedResults'), tags$hr(), tags$h4(tags$b('Resultados combinados')), Nlns(2),
      fluidRow(
        column(7, plotOutput(ns('plotCombinados'), width = '100%')),
        column(5, box(title = tags$b('Resumen de resultados combinados'), width = 12, status = 'primary',
                      tableOutput(ns('resultadosCombi'))), tags$br(),
               uiOutput(ns('DescDigit.SIBttn')), tags$hr(),
               uiOutput(ns('DescMatDarBttn')), uiOutput(ns('DescMatExcelBttn'))), tags$hr(), tags$br(), tags$hr(),
        column(12, uiOutput(ns('TablasPorDia'))))
    ))),
    
  )
}

CombinaResultadosServer <- function(id, session, devMode, demo, fecha, PartialTitrationResults) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
    observeEvent(input$brwz, browser())
    
    XmlCargados <- reactiveVal()
    observeEvent(input$NewXML, {
      if (!all(input$NewXML$type == 'text/xml') || is.error(lapply(input$NewXML$datapath, function(x) read_xml(x)))) {
        shinyalert(title = 'Error!', text = 'Todos los archivos deben ser formato XML.', type = 'error',
                   timer = 3000, showConfirmButton = FALSE)
      } else {
        uploadedFiles <- lapply((input$NewXML$datapath), function(x) read_xml(x))
        # browser()
        isTitRes <- sapply(uploadedFiles, function(x) length(xml_find_all(x, xpath = '//mr:titrationResult')))
        
        if (any(isTitRes == 0)) {#!all(solTypes %in% c('EstandarEDTA', 'MuestraCalib', 'EstandarPlomo', 'MuestraEDTA'))) {
          shinyalert(title = 'Error!', text = 'Al menos un archivo XML no contiene resultados de titulación.', type = 'error',
                     timer = 3000, showConfirmButton = FALSE)
        } else {
          PartialTitrationResults$results <- append(PartialTitrationResults$results, lapply(uploadedFiles, function(x) {return(reactive(x))}))
          XmlCargados(tags$div(
            id = session$ns('filesCargados'),
            HTML('Se cargó la información de los siguientes archivos de resultados:<p align = "left"><ul>',
                           paste0(sapply(uploadedFiles, function(x) {
                             return(paste0('<li><b>', xml_text(xml_find_all(x, xpath = '//mr:coreData/mr:solutionSource')), ':</b> ',
                                           xml_text(xml_find_all(x, xpath = '//mr:coreData/mr:dateTime')), '</li>'))
                           }), collapse = ''))))
          shinyjs::show("filesCargados")}}
    })
    output$XmlCargados <- renderUI(XmlCargados())
    
    
    values <- reactiveValues()
    DF <- reactive({
      n <- length(PartialTitrationResults$results)
      if (n == 0) {
        return(data.frame('.' = FALSE, Disolucion = '', Material = '', Fecha = '', Susbtancia = '', Valor = '', Unidades = ''))
      } else {
        return(data.frame(
          '.' = FALSE,
          Disolucion = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:additionalInfo/mr:intermediateSolution/mr:solutionID')))}),
          Material = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:coreData/mr:solutionSource')))}),
          Fecha = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:coreData/mr:dateTime')))}),
          Sustancia = sapply(PartialTitrationResults$results, function (x) {
            return(elemEspa[[xml_text(xml_child(x(), search = 'mr:titrationResult/mr:substance/mr:name'))]])}),
          Valor = sapply(PartialTitrationResults$results, function (x) {
            return(round(xml_double(xml_child(x(), search = 'mr:titrationResult/si:real/si:value')), 3))}),
          Unidades = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:titrationResult/si:real/si:unit')))})))
      }
    })
    
    ## Handsontable
    observe({
      if (!is.null(input$resultFiles)) {
        DF = hot_to_r(input$resultFiles)
      } else {
        if (is.null(values[["DF"]])) {DF <- DF()} else {DF <- values[["DF"]]}
      }
      values[["DF"]] <- DF()
    })
    
    output$resultFiles <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = TRUE, stretchH = "all", rowHeaders = NULL, selectCallback = TRUE)%>%
        hot_col(2:6, readOnly = TRUE) %>%
        hot_cols(colWidths = c(20, 150, 150, 150, 75, 75, 190)) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)})
    
    files2Combine <- reactiveVal()
    observeEvent(input$CombinArchivos, {
      if (sum(hot_to_r(input$resultFiles)['.']) < 2) {
        shinyalert(title = 'Error!', text = 'Seleccione al menos dos archivos de resultados para combinar.',
                   type = 'error', timer = 3000, showConfirmButton = FALSE)
      } else {
        SelectedXMLs <- PartialTitrationResults$results[which(hot_to_r(input$resultFiles)['.'] == TRUE)]
        Substances <- sapply(SelectedXMLs, function (x) {
          return(elemEspa[[xml_text(xml_child(x(), search = 'mr:titrationResult/mr:substance/mr:name'))]])})
        if (length(unique(Substances)) != 1) {
          shinyalert(title = 'Error!', text = 'Solo se pueden combinar archivos de resultados de la misma especie química.',
                     type = 'error', timer = 3000, showConfirmButton = FALSE)
        } else {
          js$collapse(session$ns("FilesAvailable"))
          shinyjs::hide("filesCargados")
          shinyjs::show("combinedResults")
          files2Combine(SelectedXMLs)
        }
      }
    })
    
    # x <- reactive({
    # VecMasaAli <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[2]]]})))[order(VecMomento)]
    #   VecMasaEqi <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[3]]]})))[order(VecMomento)]
    #   VecFraccMa <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[4]]]})))[order(VecMomento)]
    #   VecFracUnc <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[5]]]$prop[[3]]})))[order(VecMomento)]
    #   VecFechas0 <- as.factor(unlist(sapply(DataTrimmedList, function(x) {substr(as.character(x[[pos[6]]]), start = 1, stop = 10)})))[order(VecMomento)]
    #   #browser()
    #   x <- data.frame(VecElement, VecMuestra, VecDescrip, VecMasaAli, VecMasaEqi, VecFraccMa, VecFracUnc, VecFechas0,
    #                   index = 1:length(VecFracUnc))
    
    # DF <- data.frame('.' = 1:length(files2Combine()))
    #   DF$ResInd <- lapply(files2Combine(), function (x) {
    #     People <- xml_text(xml_child(x(), search = 'mr:coreData/respPerson/name'))
    #     Date <- xml_text(xml_child(x(), search = 'mr:coreData/mr:dateTime'))
    #     
    #     Vunits <- GetValueEstandUncert(xml_child(x(), search = 'mr:titrationResult/si:real'))$Units
    #     
    #     # xml_child(files2Combine()[[1]](), search = 'mr:coreData')
    #     
    #   })
    #   if (length(unique(Vunits)) != 1) return() else Vunits <- Vunits[1]
      # 
      
    # })
    
    ResultadosElect <- reactive({
      
    })
    output$ResultadosElect <- renderUI(ResultadosElect())
    
    
    #   x <- reactiveValuesToList(DataCompl)
    #   x <- x[names(x) %in% FileNames()]
    #   DataTrimmedList <- x[names(x) %in% input$titFilesSelectComb] # To consider only selected files
    #   VecMomento <- as.factor(unlist(sapply(DataTrimmedList, function(x) {x[[pos[1]]]})))
    #   
    #   VecElement <- ifelse(especie == 'EDTA', rep(NA, length(VecMomento)),
    #                        ifelse(especie == 'Elem', unlist(sapply(DataTrimmedList, function(x) {x[[2]]}))[order(VecMomento)], NULL))
    #   VecMuestra <- unlist(sapply(DataTrimmedList, function(x) {x[[1]]}))[order(VecMomento)]
    #   VecDescrip <- ifelse(especie == 'EDTA', 
    #                        unlist(sapply(DataTrimmedList, function(x) {ifelse(!is.null(x[[6]][[3]]), x[[6]][[3]], x[[6]][[2]])}))[order(VecMomento)],
    #                        ifelse(especie == 'Elem', unlist(sapply(DataTrimmedList, function(x) {x[[11]]}))[order(VecMomento)], NULL))
    #   VecMasaAli <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[2]]]})))[order(VecMomento)]
    #   VecMasaEqi <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[3]]]})))[order(VecMomento)]
    #   VecFraccMa <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[4]]]})))[order(VecMomento)]
    #   VecFracUnc <- as.numeric(unlist(sapply(DataTrimmedList, function(x) {x[[pos[5]]]$prop[[3]]})))[order(VecMomento)]
    #   VecFechas0 <- as.factor(unlist(sapply(DataTrimmedList, function(x) {substr(as.character(x[[pos[6]]]), start = 1, stop = 10)})))[order(VecMomento)]
    #   #browser()
    #   x <- data.frame(VecElement, VecMuestra, VecDescrip, VecMasaAli, VecMasaEqi, VecFraccMa, VecFracUnc, VecFechas0,
    #                   index = 1:length(VecFracUnc))
    #   #browser()
    #   return(x)
    # })
    # 
    # DescMatDarBttn <- eventReactive(DataCleanDF(), {
    #   downloadButton(session$ns('DescMatDar'), label = tags$b('Descargar matriz de resultados en RDS'))})
    # output$DescMatDarBttn <- renderUI(DescMatDarBttn())
    # output$DescMatDar <- downloadHandler(
    #   filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.rds')},
    #   content = function(file) {saveRDS(DataCleanDF(), file = file)}, contentType = NULL)
    # 
    # DescMatExcelBttn <- eventReactive(DataCleanDF(), {
    #   downloadButton(session$ns('DescMatExcel'), label = tags$b('Descargar matriz de resultados en Excel'))})
    # output$DescMatExcelBttn <- renderUI(DescMatExcelBttn())
    # output$DescMatExcel <- downloadHandler(
    #   filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xlsx')},
    #   content = function(file) {write_xlsx(x = DataCleanDF(), path = file, format_headers = TRUE)}, contentType = NULL)
    # 
    # DescDigit.SIBttn <- eventReactive(DataCleanDF(), {
    #   downloadButton(session$ns('DescDigit.SI'), label = tags$b('Descargar resultados en SI Digital (XML)'))})
    # output$DescDigit.SIBttn <- renderUI(DescDigit.SIBttn())
    # output$DescDigit.SI <- downloadHandler(
    #   filename = function() {paste0("MatrizResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xlsx')},
    #   content = function(file) {write_xlsx(x = DataCleanDF(), path = file, format_headers = TRUE)}, contentType = NULL)
    # 
    # 
    # 
    # resultadosCombi <- eventReactive(DataCleanDF(), {
    #   AverageValue <- mean(DataCleanDF()$VecFraccMa)
    #   IncertTipoB <- max(DataCleanDF()$VecFracUnc)
    #   StandarDev <- sd(DataCleanDF()$VecFraccMa)
    #   n_ind <- length(unique((DataCleanDF()$VecFechas0)))
    #   IncertTipoA <- StandarDev/sqrt(n_ind)
    #   IncertComb <- sqrt(IncertTipoB^2 + IncertTipoA^2)
    #   LevTest <- tryCatch(leveneTest(VecFraccMa ~ VecFechas0, data = DataCleanDF()), error = function(e) 'no aplica')
    #   return(data.frame('.' = c('Promedio de las mediciones', 'Incertidumbre tipo B', 'Desviacion estandar de las mediciones', 
    #                             'Numero de datos', 'Numero de datos independientes (dia)', 'Incertidumbre tipo A', 'Incertidumbre combinada',
    #                             'Incertidumbre expandida (k=2)', 'Valor p homogeneidad de varianzas (Levene)'),
    #                     'Valor' = as.character(c(round(c(AverageValue, IncertTipoB, StandarDev), 3), 
    #                                              round(c(length((DataCleanDF()$VecFechas0)), n_ind)),
    #                                              round(c(IncertTipoA, IncertComb, IncertComb * 2), 3),
    #                                              ifelse(n_ind > 1, round(LevTest$`Pr(>F)`[1], 4), LevTest))),
    #                     'Unidades' = c(rep(unidad, 3), rep('', 2), rep(unidad, 3), '')))
    # })
    # 
    # plotCombinados <- reactive({
    #   if (especie == 'EDTA') {
    #     ylab <- expression(paste('Fracción masica de EDTA / g ', ' ', g^{-1}, ' (%)'))
    #   } else {
    #     if (especie == 'Elem') {
    #       ylab <- expression(paste('Fracción masica del elemento / ', 'mg k', g^{-1}))
    #     }
    #   }
    #   
    #   p <- ggplot(data = DataCleanDF(), aes(x = index)) + theme_bw() + 
    #     labs(y = ylab, x = NULL) +
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #           axis.text.y = element_text(color = "black"),
    #           #axis.ticks.x = element_blank(), 
    #           axis.text.x = element_blank(), legend.title = element_blank()) +
    #     scale_y_continuous(expand = c(0, 0.4), n.breaks = 8) +
    #     #geom_hline(aes(yintercept = 0.999 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
    #     geom_hline(aes(yintercept = (1 - tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
    #     geom_hline(aes(yintercept = mean(VecFraccMa)), linetype = 1, lwd = 0.5, col = 'gray60') +
    #     geom_hline(aes(yintercept = (1 + tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
    #     #geom_hline(aes(yintercept = 1.001 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
    #     geom_point(aes(y = VecFraccMa, color = VecFechas0)) + 
    #     geom_errorbar(aes(ymin = VecFraccMa - VecFracUnc, ymax = VecFraccMa + VecFracUnc, color = VecFechas0), width = 0.4)
    #   #browser()
    #   print(p)
    # })
    # output$plotCombinados <- renderPlot(plotCombinados())
    # output$resultadosCombi <- renderTable(resultadosCombi())
    # 
    # unidad <- dplyr::case_when(especie == 'EDTA' ~ 'g / g (%)', especie == 'Elem' ~ 'mg / kg')
    # 
    # TablasPorDia <- eventReactive(DataCleanDF(), {
    #   x <- list()
    #   for (i in unique((DataCleanDF()$VecFechas0))) {
    #     j <- i
    #     DayRes <- which(DataCleanDF()$VecFechas0 == j)
    #     datFram <- data.frame(Archivo = row.names(DataCleanDF())[DayRes],
    #                           Resultado = DataCleanDF()$VecFraccMa[DayRes], 
    #                           '.' = rep(unidad, length(DayRes)))
    #     nDat <- nrow(datFram)
    #     temp <- box(title = tags$b(paste0('Resumen de resultados del ', j)), status = 'primary', collapsible = TRUE, collapsed = FALSE,
    #                 column(5, #renderTable(isolate(datFram), digits = 3),
    #                        tags$h4('Promedio del día:', tags$b(round(mean(datFram$Resultado), 3), unidad), tags$br(),
    #                                'Desviación estándar relativa del día:', tags$b(round(sd(datFram$Resultado)/mean(datFram$Resultado)*100, 3), '%'))),
    #                 column(7, tags$h4('Valor p prueba de normalidad de Shapiro-Wilk:', 
    #                                   tags$b(tryCatch(signif(shapiro.test(datFram$Resultado)$p.value, 3), 
    #                                                   error = function(e) 'No se puede calcular para menos de tres datos')), 
    #                                   tags$br(),
    #                                   'Valores p de las pruebas de datos anómalos de Grubbs:', tags$br(),
    #                                   tryCatch(
    #                                     tags$h4('  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 10)$p.value, 3)),
    #                                             ' para el valor más ', 
    #                                             ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'alto.', 'bajo.')),
    #                                     error = function(e) 'No se puede calcular para menos de tres datos'),
    #                                   tryCatch(
    #                                     tags$h4('  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 11)$p.value, 3)),
    #                                             ' para un valor a cada extremo.', tags$br(),
    #                                             '  · ', tags$b(signif(grubbs.test(datFram$Resultado, type = 20)$p.value, 3)), 
    #                                             ' para los dos valores más ', 
    #                                             ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'altos.', 'bajos.')),
    #                                     error = function(e) ''))
    #                 ))
    #     x <- c(x, temp)
    #   }
    #   return(x)
    # })
    # output$TablasPorDia <- renderUI(TablasPorDia())
    # 
    # 
    # titFilesSelectIndi <- reactive({
    #   x <- reactiveValuesToList(DataCompl)
    #   x <- x[names(x) %in% FileNames()]
    #   pos <- dplyr::case_when(especie == 'EDTA' ~ c(12, 11),
    #                           especie == 'Elem' ~ c(13, 13))
    #   Fechas <- as.factor(unlist(sapply(x, 
    #                                     function(x) {substr(as.character(x[[pos[1]]]), 
    #                                                         start = nchar(as.character(x[[pos[1]]])) - 18, 
    #                                                         stop = nchar(as.character(x[[pos[1]]])) - 3)})))
    #   VecMomento <- as.factor(unlist(sapply(x, function(x) {x[[pos[2]]]})))
    #   choices <- names(x)[order(VecMomento)]
    #   #browser()
    #   return(radioButtons(session$ns('titFilesSelectIndi'), label = tags$b("Archivo:"), 
    #                       choices = choices, #selected = character(0), 
    #                       width = '100%'))
    # })
    # output$titFilesSelectIndi <- renderUI(titFilesSelectIndi())
    # 
    # meanValues <- reactive(
    #   NULL
    # )
    # 
    # output$printed <- renderPrint({
    #   #  req(input$TitFiles)
    #   #  (length(input$TitFiles$name))
    # })
  })
}
