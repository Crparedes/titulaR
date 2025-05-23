CombinaResultadosUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, Nlns(4), uiOutput(ns('brwz')),
           tags$h4(style = 'margin-left: 60px;', tags$b('Combinación de resultados de titulación'))),
    column(
      width = 2, style = 'margin-left: 80px;', tags$br(),
      tags$b('Importar archivos de resultados de sesiones anteriores (.xml)'),
      tags$div(
        style = 'margin-left: 25px; margin-top:0px;',
        fileInput(ns('NewXML'), label = NULL, buttonLabel = 'Examinar...', multiple = TRUE, accept = '.xml', width = '100%'),
        uiOutput(ns('XmlCargados')))),
    column(
      width = 9, style = 'margin-left: 30px;', tags$br(),
      box(
        id = ns('FilesAvailable'), status = 'primary', title = 'Resultados individuales', width = 12, collapsible = TRUE, collapsed = FALSE,
        'Seleccione los resultados individuales de titulación marcando la casilla al inicio de cada fila.', tags$br(),
        'Solo puede combinar resultados de la misma sustancia.', tags$br(),
        tags$div(style = 'margin-left: 25px; margin-top:20px; border-style:groove;', rHandsontableOutput(ns("resultFiles"))),
        Nlns(), actionButton(ns('CombinArchivos'), label = tags$b('Promediar resultados'), style = 'margin-left:45px;')),
      ),
    column(11, hidden(tags$div(
      style = 'margin-left: 80px;', id = ns('combinedResults'), tags$hr(), #tags$h4(tags$b('Resultado')), Nlns(2),
      fluidRow(
        column(6, plotOutput(ns('plotCombinados'), width = '100%')),
        column(5, box(title = NULL, width = 12, status = 'primary',
                      tableOutput(ns('resultadosCombi')), tags$hr(),
                      uiOutput(ns('DFexplan')),
                      tags$hr(),
                      tags$ul(
                        tags$li(downloadLink(ns("DownResultadoXML"), label = tags$b('Descargar información del resultado (formato XML)'))),
                        tags$li(downloadLink(ns("ResumenRDS"), label = ('Descargar resumen de resultados individuales (Archivo R)'))),
                        tags$li(downloadLink(ns("ResumenExcel"), label = ('Descargar resumen de resultados individuales (Archivo Excel)'))))
                      )),
        column(12, tags$hr(), uiOutput(ns('TablasPorDia'))))
    )))
    
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
          # resultsIDs <- sapply(uploadedFiles, function(x) length(xml_find_all(x, xpath = '//mr:titrationResult')))
          # ONLY ENTRY THOSE FILES NOT IN THE APP
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
        return(data.frame('.' = FALSE,  Sustancia = '', Material = '', ID.Resultado = '', Analista = '', Valor = '', Unidades = ''))
      } else {
        return(data.frame(
          '.' = FALSE,
          Sustancia = sapply(PartialTitrationResults$results, function (x) {
            return(elemEspa[[xml_text(xml_child(x(), search = 'mr:titrationResult/mr:substance/mr:name'))]])}),
          Material = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:coreData/mr:solutionSource')))}),
          ID.Resultado = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:coreData/mr:resultID')))}),
          Analista = sapply(PartialTitrationResults$results, function (x) {
            return(xml_text(xml_child(x(), search = 'mr:coreData/respPerson/name')))}),
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
        hot_cols(colWidths = c(20, 75, 100, 250, 100, 70, 180)) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)})
    
    files2Combine <- reactiveVal()
    observeEvent(input$CombinArchivos, {
      SimpleData <- hot_to_r(input$resultFiles)
      if (sum(SimpleData['.']) < 2) {
        shinyalert(title = 'Error!', text = 'Seleccione al menos dos archivos de resultados para combinar.',
                   type = 'error', timer = 3000, showConfirmButton = FALSE)
      } else {
        
        SelectedXMLs <- PartialTitrationResults$results[which(SimpleData['.'] == TRUE)]
        Substances <- sapply(SelectedXMLs, function (x) {
          return(elemEspa[[xml_text(xml_child(x(), search = 'mr:titrationResult/mr:substance/mr:name'))]])})
        ResultIDs <- sapply(SelectedXMLs, function (x) {
          return(xml_text(xml_child(x(), search = 'mr:coreData/mr:resultID')))})
        ResValues <- sapply(SelectedXMLs, function (x) {
          return(xml_text(xml_child(x(), search = 'mr:titrationResult/si:real/si:value')))})
        ResUnits <- sapply(SelectedXMLs, function (x) {
          return(xml_text(xml_child(x(), search = 'mr:titrationResult/si:real/si:unit')))})
        if (length(unique(Substances)) != 1) {
          shinyalert(title = 'Error!', text = 'Solo se pueden combinar archivos de resultados de la misma especie química.',
                     type = 'error', timer = 3000, showConfirmButton = FALSE)
        } else {
          if (length(unique(ResultIDs)) < sum(SimpleData['.']) && length(unique(ResValues)) < sum(SimpleData['.']) ) {
            shinyalert(title = 'Error!', text = 'Existe un archivo duplicado!<br>
                       Verifique el ID de los resultados para combinar e inténtelo nuevamente.',
                       type = 'error', timer = 3000, showConfirmButton = FALSE)
          } else {
            if (length(unique(ResUnits)) > 1) {
              shinyalert(title = 'Error!', text = 'El aplicativo aún no soporta conversión de unidades<br>
                       Las unidades de los resultados de medición deben ser todas iguales.',
                         type = 'error', timer = 3000, showConfirmButton = FALSE)
            } else {
              js$collapse(session$ns("FilesAvailable"))
              shinyjs::hide("filesCargados")
              shinyjs::show("combinedResults")
              files2Combine(SelectedXMLs)
            }
          }
        }
      }
    })
    
    DataCleanDF <- reactive({
      VecElement <- sapply(files2Combine(), function (x) {
        return(xml_text(xml_child(x(), search = 'mr:titrationResult/mr:substance/mr:name')))})
      VecMuestra <- sapply(files2Combine(), function (x) {
        return(xml_text(xml_child(x(), search = 'mr:coreData/mr:resultID')))})
      VecDescrip <- sapply(files2Combine(), function (x) {
        return(xml_text(xml_child(x(), search = 'mr:coreData/mr:solutionSource')))})
      VecMasaAli <- sapply(files2Combine(), function (x) {
        return(xml_double(xml_child(x(), search = 'mr:additionalInfo/mr:measurementData/mr:titrationAliquot/si:real/si:value')))})
      VecMasaEqi <- sapply(files2Combine(), function (x) {
        return(xml_double(xml_child(x(), search = 'mr:additionalInfo/mr:measurementData/mr:titrationFinalMass/si:real/si:value')))})
      VecFraccMa <- sapply(files2Combine(), function (x) {
        return(xml_double(xml_child(x(), search = 'mr:titrationResult/si:real/si:value')))})
      VecUnits <- sapply(files2Combine(), function (x) {
        return(xml_text(xml_child(x(), search = 'mr:titrationResult/si:real/si:unit')))})
      VecFracUnc <- sapply(files2Combine(), function (x) {
        return(xml_double(xml_child(x(), search = 'mr:titrationResult/si:real/si:expandedUnc/si:uncertainty')))})
      
      VecFechas0 <- sapply(files2Combine(), function (x) {
        return(substr(xml_text(xml_child(x(), search = 'mr:coreData/mr:dateTime')), 1, 10))})
      
      x <- data.frame(VecElement, VecMuestra, VecDescrip, VecMasaAli, VecMasaEqi, VecFraccMa, VecUnits,
                      VecFracUnc, VecFechas0, index = 1:length(VecMasaAli))
      return(x)
    })
    
    ResultadosElect <- reactive({
      
    })
    output$ResultadosElect <- renderUI(ResultadosElect())
    
    # Combinaci'on de resultados
    AverageValue  <- reactive(mean(DataCleanDF()$VecFraccMa))
    IncertTipoB <- reactive(max(DataCleanDF()$VecFracUnc))
    StandarDev  <- reactive(sd(DataCleanDF()$VecFraccMa))
    n_ind       <- reactive(length(DataCleanDF()))
    output$DFexplan <- renderUI(tags$div(
      style = 'font-size:12px;',
      'Los grados de libertad se calculan con la relación de Welch-Satterthwaite utilizando', n_ind(),
      '- 1 grados de libertad para el aporte de incertidumbre tipo A y 200 grados de libertad para el aporte de incertidumbre tipo B.'))
    IncertTipoA <- reactive(StandarDev()/sqrt(n_ind()))
    nu_A <- reactive(n_ind() - 1)
    IncertComb  <- reactive(sqrt(IncertTipoB()^2 + IncertTipoA()^2))
    nu_eff <- reactive(round(IncertComb()^4/(IncertTipoB()^4/200 + IncertTipoA()^4/nu_A())))
    k_fact <- reactive(round(qt(p = 0.975, df = nu_eff()), 4))
    
    resultadosCombi <- eventReactive(DataCleanDF(), {
      d1 <- decimals(signif(IncertComb(), 3))
      
      LevTest <- tryCatch(round(leveneTest(VecFraccMa ~ VecFechas0, data = DataCleanDF())$`Pr(>F)`[1], 4), error = function(e) 'no aplica')
      return(data.frame('.' = c('Promedio de las mediciones', 'Incertidumbre tipo B', 'Desviacion estandar de las mediciones',
                                'Numero de datos', 'Dias de medicion', 'Incertidumbre tipo A', 'Incertidumbre combinada',
                                'Incertidumbre expandida', 'Factor de cobertura (95 %)', 'Grados de libertad efectivos', 'Homocedasticidad entre días'),
                        'Valor' = as.character(c(round(AverageValue(), d1), signif(c(IncertTipoB(), StandarDev()), 3),
                                                 length((DataCleanDF()$VecFechas0)),
                                                 length(unique(DataCleanDF()$VecFechas0)),
                                                 signif(c(IncertTipoA(), IncertComb(), IncertComb() * k_fact()), 3), k_fact(), nu_eff(),
                                                 LevTest)),
                        'Unidades' = c(rep(DataCleanDF()$VecUnits[1], 3), '', '\\day', rep(DataCleanDF()$VecUnits[1], 3),  '', '',
                                       '(Valor p prueba de Levene)')))
    })
    output$resultadosCombi <- renderTable(resultadosCombi())

    plotCombinados <- reactive({
      ylab <- paste('Fracción masica / ', DataCleanDF()$VecUnits[1])
      tol <- 0.005
      p <- ggplot(data = DataCleanDF(), aes(x = index)) + theme_bw() +
        labs(y = ylab, x = NULL) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.y = element_text(color = "black"),
              #axis.ticks.x = element_blank(),
              axis.text.x = element_blank(), legend.title = element_blank()) +
        scale_y_continuous(expand = c(0.4, 0), n.breaks = 8) +
        #geom_hline(aes(yintercept = 0.999 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
        geom_hline(aes(yintercept = (1 - tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
        geom_hline(aes(yintercept = mean(VecFraccMa)), linetype = 1, lwd = 0.5, col = 'gray60') +
        geom_hline(aes(yintercept = (1 + tol) * mean(VecFraccMa)), linetype = 2, lwd = 0.5, col = 'gray60') +
        #geom_hline(aes(yintercept = 1.001 * mean(VecFraccMa)), linetype = 4, lwd = 0.5, col = 'red1') +
        geom_point(aes(y = VecFraccMa, color = VecFechas0)) +
        geom_errorbar(aes(ymin = VecFraccMa - VecFracUnc, ymax = VecFraccMa + VecFracUnc, color = VecFechas0), width = 0.4)
      #browser()
      print(p)
    })
    output$plotCombinados <- renderPlot(plotCombinados())

    # Resultados en XML
    CompleteResultsXML <- eventReactive(DataCleanDF(), {
      SamplesIDs <- unique(sapply(files2Combine(), function (x) {
        return(xml_text(xml_child(x(), search = 'mr:coreData/mr:solutionSource')))}))
      if(length(SamplesIDs) > 1) SamplesIDs <- paste0(SamplesIDs, collapse = ', ')
      
      xmlObject <- initiateResultsXML()
      addDataToMRXML(xmlObject, list('mr:sampleID' = SamplesIDs), node = 'mr:coreData')
      addDataToMRXML(xmlObject, list('mr:dateTime' = iso8601(fecha = fecha())), node = 'mr:coreData')
      xml_add_child(xml_child(xmlObject, search = 'mr:coreData'), xml_child(files2Combine()[[1]](), search = 'mr:coreData/respPerson'))
      
      
      xml_child(xmlObject, search = 'mr:measurementResult') %>% {
        xml_add_child(., .value = xml_child(files2Combine()[[1]](), search = 'mr:titrationResult//mr:substance'))
        xml_add_child(., .value = SiRealXML(
          quantityTypeQUDT = 'MassFraction', value = AverageValue(),
          units = xml_text(xml_child(files2Combine()[[1]](), search = 'mr:titrationResult//si:real//si:unit')),
          uncert = IncertComb() * k_fact(), covFac = k_fact(), covProp = 0.95, distribution = 't Student'))
      }
      xml_child(xmlObject, search = 'mr:additionalInfo') %>% {
        xml_add_child(., .value = 'mr:individualResults') %>% {
          sapply(files2Combine(), function (x) {
            xml_add_child(., xml_child(x(), search = 'mr:coreData/mr:resultID')) %>%
              xml_add_child(., xml_child(x(), search = 'mr:titrationResult/si:real'))
            return()
          })
      }}
      
      return(xmlObject)
    })
    
    # Descarga de archivos
    output$ResumenRDS <- downloadHandler(
      filename = function() {paste0("ResumenResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.rds')},
      content = function(file) {saveRDS(DataCleanDF(), file = file)}, contentType = NULL)

    output$ResumenExcel <- downloadHandler(
      filename = function() {paste0("ResumenResultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xlsx')},
      content = function(file) {write_xlsx(x = DataCleanDF(), path = file, format_headers = TRUE)}, contentType = NULL)

    output$DownResultadoXML <- downloadHandler(
      filename = function() {paste0("Resultados_", fecha(), format(Sys.time(), '_%H-%M'), '.xml')},
      content = function(file) {write_xml(x = CompleteResultsXML(), file)})

    
    # Tablas por d'ia
    TablasPorDia <- eventReactive(DataCleanDF(), {
      x <- list()
      unidad <- DataCleanDF()$VecUnits[1]
      k <- 1
      for (i in unique(DataCleanDF()$VecFechas0)) {
        j <- i
        DayRes <- which(DataCleanDF()$VecFechas0 == j)
        datFram <- data.frame(Archivo = row.names(DataCleanDF())[DayRes],
                              Resultado = DataCleanDF()$VecFraccMa[DayRes],
                              '.' = rep(unidad, length(DayRes)))
        nDat <- nrow(datFram)
        x[[k]] <- box(
          title = tags$b(paste0('Resumen de resultados del día ', j)), status = 'primary', collapsible = TRUE, collapsed = FALSE, width = 4,
          'Promedio:', tags$b(signif(mean(datFram$Resultado), 7), unidad), tags$br(),
          'Desviación estándar relativa:', tags$b(round(sd(datFram$Resultado)/mean(datFram$Resultado)*100, 4), '%'),
          tags$hr(),
          tags$b('Normalidad de los datos'), tags$br(), tags$ul(tags$li(
            'Prueba Shapiro-Wilk, valor p ', 
            tags$b(tryCatch(pred(signif(shapiro.test(datFram$Resultado)$p.value, 3)),
                            error = function(e) 'No disponible para menos de tres datos')))),
          tags$hr(),
          tags$b('Ausencia de datos anómalos'), tags$br(), 'Pruebas de Grubbs', tags$br(),
          tags$ul(
            tryCatch(
              tags$li(
                'Valor p para el valor más ',
                ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'alto: ', 'bajo: '),
                tags$b(pred(signif(grubbs.test(datFram$Resultado, type = 10)$p.value, 3)))),
              error = function(e) 'No disponible para menos de tres datos'),
            tryCatch(
              tags$li(
                'Valor p para los dos valores de los extremos:',
                tags$b(pred(signif(grubbs.test(datFram$Resultado, type = 11)$p.value, 3)))),
              error = function(e) ''),
            tryCatch(
              tags$li(
                'Valor p para los dos valores más ',
                ifelse(word(grubbs.test(datFram$Resultado, type = 10)$alternative, 1) == 'highest', 'altos: ', 'bajos: '),
                tags$b(pred(signif(grubbs.test(datFram$Resultado, type = 20)$p.value, 3)))),
              error = function(e) '')
          )
        )
        k <- k + 1
      }
      return(x)
    })
    output$TablasPorDia <- renderUI(TablasPorDia())
  })
}
