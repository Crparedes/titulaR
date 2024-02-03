TitIndividualUI <- function(id, demo, title, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b('Titulación de ', explan), Nlns(2),
    fluidRow(
      column(
        5,
        tags$div(
          id = 'inline', style = 'font-size:12px; margin-left:25px', 
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasaAlic'), label = ReqField('Masa de la alícuota / g'), value = ifelse(demo, 10.0552, 0)),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasaEDTA0'), label = NonReqField('Masa inicial de titulante / g', 5), value = 0),
          conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns, Nlns(),
                           rHandsontableOutput(ns("TitData"), width = '100%')))),
      column(
        7, #tags$b('Curva de titulación:'),
        fluidRow(
          column(12, uiOutput(ns('SummaryIndivTitr'))),
          column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'), tags$br()),
          column(9, offset = 2, disabled(actionButton(ns('TermTit'), label = 'Terminar titulación')), tags$hr()),
          column(width = 2, SI_unit_nice('mole', width = "97%"), SI_unit_nice('kilogram', width = "97%")),
          column(width = 10, downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML del resultado individual'), tags$br(),
                 '(El resultado de medición se obtiene combinando resultados individuales)',
                 Nlns(2), htmlOutput(ns('InfoTitXML')))),
        )
    )
  )
}

TitIndividualServer <- function(id, devMode, demo, reagKey, analyst, balanza, fecha, StanDisol, SampDisol, type = 'calibrante') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())
    
    horaInicio <- eventReactive(input$MasaAlic, iso8601(fecha = fecha()))
    horaFinal  <- eventReactive(input$TermTit, iso8601(fecha = fecha()))
    element <- reactive(xml_text(xml_find_all(SampDisol(), xpath = '//mr:substance//mr:name')))
    
    TblDt_0 <- reactive(if(demo()) return(demoData) else return(voidData))
    TableDat_0  <- reactiveValues(hot = TblDt_0())
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
        rhandsontable(DT, colHeaders = c('Masa de titulante\n (\\gram)', 'Diferencia de potencial \n  (\\mili\\volt)', 'Derivada \n |d(m)/d(E)|'), 
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
    
    observe({req(TitCurvDat()); if(length(na.omit(TitCurvDat()$Titrant)) >= 7) enable('TermTit')})
    
    CleanDf <- eventReactive(input$TermTit, {
      dff <- as.data.frame(TitCurvDat()[(as.numeric(!is.na(TitCurvDat()$Titrant)) + as.numeric(!is.na(TitCurvDat()$Signal))) > 1, ])
      dff <- dff[dff[, 1] > 0, ]
      dff[, 1] <- dff[, 1] + input$MasaEDTA0
      if(nrow(dff) > 3) {
        return(df2t.curve(df = dff, plot = FALSE))
      } else {
        return()
      }
    })
    
    # observeEvent(input$TermTit, { # https://stackoverflow.com/questions/54652364/r-shiny-automatically-start-download
    #   if (is.numeric(ResParcial())) {
    #     runjs(paste0("$('#", number(), "-DwnlResFile')[0].click();"))
    #   }
    # })
  
    TitCurvePlot <- reactive(
      tryCatch(
        expr = {
          if(length(na.omit(TitCurvDat()$Titrant)) >= 3) {
            if(input$TermTit < 1) {
              plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0])
            } else {
              plotCurve(curve = CleanDf(), xlab = 'Titulante [g]', ylab = 'Señal [mV]')
            }
          } else {
            plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
            text(x = 0.5, y = 0.5, paste("Esperando datos de titulación..."), 
                 cex = 1.6, col = "black")
          }}, 
        error = function(cond) {
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.5, paste("Esperando datos de titulación..."), 
               cex = 1.6, col = "black")
        })
    )
    output$TitCurvePlot <- renderPlot(TitCurvePlot())
    
    MasaEquiv <- eventReactive(input$TermTit, {try(EP.1stDer(curve = CleanDf()))})
    ConcStanSolut <- reactive(GetValueEstandUncert(StanDisol(),  property = 'AmountOfSubstancePerUnitMass'))
    AtMasSampElem <- reactive(GetValueEstandUncert(SampDisol(),  property = 'MolarMass', node = 'mr:property'))
    MassRatioSamp <- reactive(GetValueEstandUncert(SampDisol(),  property = 'MassRatio', node = 'mr:property'))
    
    
    # SampDisol
    ResParcial <- reactive(MasaEquiv() * ConcStanSolut()$ValUnc[1] / input$MasaAlic * AtMasSampElem()$ValUnc[1])
    ResParcUnc <- reactive(
      propagate(expr = expression((Meq - Mbln) * Cedta / Mali * Mato),
                data = cbind(Meq = c(convMass(balanza(), reading = MasaEquiv()),
                                     sqrt(2) * uncertConvMass(balanza(), reading = MasaEquiv(), d = 0.1, d.units = 'mg')),
                             Mbln = c(0, 0.0028/(2*sqrt(3))),
                             Cedta = ConcStanSolut()$ValUnc,
                             Mali = c(convMass(balanza(), reading = input$MasaAlic),
                                      uncertConvMass(balanza(), reading = input$MasaAlic)),
                             Mato = AtMasSampElem()$ValUnc),
                second.order = FALSE, do.sim = FALSE))
    
    ResParcialSource <- reactive(MasaEquiv() * ConcStanSolut()$ValUnc[1] / input$MasaAlic * AtMasSampElem()$ValUnc[1] / MassRatioSamp()$ValUnc[1])
    ResParcUncSource <- reactive(
      propagate(expr = expression((Meq - Mbln) * Cedta / Mali * Mato / MassRatioSamp),
                data = cbind(Meq = c(convMass(balanza(), reading = MasaEquiv()),
                                     sqrt(2) * uncertConvMass(balanza(), reading = MasaEquiv(), d = 0.1, d.units = 'mg')),
                             Mbln = c(0, 0.0028/(2*sqrt(3))),
                             Cedta = ConcStanSolut()$ValUnc,
                             Mali = c(convMass(balanza(), reading = input$MasaAlic),
                                      uncertConvMass(balanza(), reading = input$MasaAlic)),
                             Mato = AtMasSampElem()$ValUnc,
                             MassRatioSamp = MassRatioSamp()$ValUnc),
                second.order = FALSE, do.sim = FALSE))
    
    InfoTitXML <- eventReactive(MasaEquiv(), {
      xmlObject <- initiateTitrationXML()
      titResList <- list()
      addInfList <- list('mr:timeISO8601start' = horaInicio(),
                         'mr:timeISO8601end' = horaFinal())
      
      xml_child(xmlObject, search = 'mr:titrationResult') %>% {
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:solutionID'))
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:solutionSource'))
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:property//mr:substance'))
      }
      xml_child(xmlObject, search = 'mr:additionalInfo') %>% xml_add_child(., .value = analyst())
      
      addDataToMRXML(xmlObject, titResList, node = 'mr:titrationResult')
      addDataToMRXML(xmlObject, addInfList, node = 'mr:additionalInfo')
      return(xmlObject)
    })
    
    observeEvent(req(InfoTitXML()), {
      enable('downlXMLlink')
      withCallingHandlers({
        shinyjs::html("InfoTitXML", "")
        message(InfoTitXML())},
        message = function(m) {
          shinyjs::html(id = "InfoTitXML",
                        html = paste0('<textarea rows = 40 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLlink <-  downloadHandler(
        filename = function() {paste0(gsub(pattern = ' ', replacement = '_', 'NAME', fixed = FALSE), ".xml")},
        content = function(file) {write_xml(InfoTitXML(), file)})
    })

    
    SummaryIndivTitr <- eventReactive(input$TermTit, {
      if(length(na.omit(TitCurvDat()$Titrant)) < 6) {
        tags$b('No se puede terminar la titulación con menos de 7 datos.')
      } else {
        infoBox(
          width = 12, title = tags$b(style = 'font-size:13px', 'Resultado individual (parcial)'),
          icon = icon("vials"), color = 'black', fill = FALSE,
          subtitle = tags$div(
            style = 'font-size:12px',
            'Fracción de', elemEspa[[element()]], 'en la disolución titulada:',
            tags$b(style = 'margin-left:1px;', round(ResParcial(), 3), '\u00B1', signif(ResParcUnc()$prop[3], 3), ' mg/kg (k=1)'), Nlns(1), 
            'Fracción de', elemEspa[[element()]], 'en la muestra original:',
            tags$b(style = 'margin-left:1px;', round(ResParcialSource(), 3), '\u00B1', signif(ResParcUncSource()$prop[3], 3), ' mg/kg (k=1)')))
      }
    })
    output$SummaryIndivTitr <- renderUI(SummaryIndivTitr())
    output$DescaResu <- renderUI(DescaResu())
    
    
    
    # return(list('infoDisMRC' = infoDisMRC))
  })
}
                           