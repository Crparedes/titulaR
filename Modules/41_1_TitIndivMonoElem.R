TitIndivMonoElemUI <- function(id, demo, title, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b('Titulación de ', explan), Nlns(2),
    conditionalPanel(
      'input.TermTit == 0', ns = ns,
      fluidRow(
        column(
          5,
          tags$div(
            id = 'inline', style = 'font-size:12px; margin-left:25px', 
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, align = 'left',
                             ns('MasaAlic'), label = ReqField('Masa de la alícuota / g'), value = ifelse(demo, rnorm(1, 10.0552, 10.0552*0.0015), 0)),
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, align = 'left',
                             ns('MasaEDTA0'), label = NonReqField('Masa inicial de titulante / g', 5), value = 0),
            conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns, Nlns(),
                             rHandsontableOutput(ns("TitData"), width = '100%')))),
        column(
          7, #tags$b('Curva de titulación:'),
          fluidRow(
            column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'), tags$br()),
            column(9, offset = 2, disabled(actionButton(ns('TermTit'), label = 'Terminar titulación'))),
            column(12, tags$hr()),
            column(width = 2, SI_unit_nice('mole', width = "97%"), SI_unit_nice('kilogram', width = "97%")),
            # column(width = 10, downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML del resultado individual'), tags$br(),
            #        actionLink(ns("showBudget"), label = 'Mostrar presupuesto de incertidumbre'), tags$br(),
            #        tags$div(style = 'font-size:11px;', '(Combine varios resultados individuales para obtener un resultado de medición)', tags$br(),
            #                 tags$div(style = 'font-size:12px;', htmlOutput(ns('InfoTitXML')))))
          )))
    ),
    conditionalPanel(
      'input.TermTit > 0', ns = ns,
      fluidRow(
        column(width = 1, SI_unit_nice('mole', width = "120%"), SI_unit_nice('kilogram', width = "120%")),
        column(width = 11, #align = 'center', 
               uiOutput(ns('SummaryIndivTitr')),
               # plotOutput(ns('TitCurvePlot2'), width = '80%'),
               tags$hr(), uiOutput(ns('uncertBudget')), tags$hr(), htmlOutput(ns('InfoTitXML')))
      )
    )
    
  )
}

TitIndivMonoElemServer <- function(id, devMode, demo, reagKey, analyst, balanza, fecha, StanDisol, SampDisol, type = 'calibrante') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())
    
    horaInicio <- eventReactive(input$MasaAlic, iso8601(fecha = fecha()))
    resultID <- reactive(paste0('Result_', xml_text(xml_child(SampDisol(), 'mr:coreData/mr:solutionID')), '_', horaInicio()))
    
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
              plot(TitCurvDat()$Signal[TitCurvDat()$Titrant != 0] ~ TitCurvDat()$Titrant[TitCurvDat()$Titrant != 0],
                   xlab = 'Masa de titulante / g', ylab = 'Diferencia de potencial / mV')
            } else {
              plotCurve(curve = CleanDf(), xlab = 'Masa de titulante / g', ylab = 'Diferencia de potencial / mV')
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
    output$TitCurvePlot2 <- renderPlot(TitCurvePlot())
    
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
      
      addDataToMRXML(xmlObject, list('mr:resultID' = resultID()), node = 'mr:coreData')
      xml_child(xmlObject, search = 'mr:coreData') %>% {
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:solutionSource'))
        xml_add_child(., .value = analyst())
      }
      addDataToMRXML(xmlObject, list('mr:dateTime' = horaFinal()), node = 'mr:coreData')
    
      xml_child(xmlObject, search = 'mr:titrationResult') %>% {
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:property//mr:substance'))
        xml_add_child(., .value = SiRealXML(
          quantityTypeQUDT = 'massFraction', value = ResParcUncSource()$prop[[1]], units = '\\milli\\gram\\kilo\\gram\\tothe{-1}',
          uncert = ResParcUncSource()$prop[[3]], covFac = 1))
      }
      xml_child(xmlObject, search = 'mr:additionalInfo') %>% {
        xml_add_child(., .value = 'mr:intermediateSolution')
        xml_add_child(., .value = 'mr:referenceSolution')
      }
      xml_child(xmlObject, search = 'mr:additionalInfo//mr:intermediateSolution') %>% {
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:solutionID'))
        xml_add_child(., .value = CopySiRealFromXML(SampDisol(), 'MassRatio'))
        xml_add_child(., .value = SiRealXML(
          quantityTypeQUDT = 'massFraction', value = ResParcUnc()$prop[[1]], units = '\\milli\\gram\\kilo\\gram\\tothe{-1}',
          uncert = ResParcUnc()$prop[[3]], covFac = 1))
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:timeISO8601'))
      }
      xml_child(xmlObject, search = 'mr:additionalInfo//mr:referenceSolution') %>% {
        xml_add_child(., .value = xml_child(StanDisol(), search = 'mr:coreData//mr:solutionID'))
        xml_add_child(., .value = xml_child(StanDisol(), search = 'mr:coreData//mr:CRM'))
        xml_add_child(., .value = xml_child(StanDisol(), search = 'mr:property//mr:substance'))
        xml_add_child(., .value = CopySiRealFromXML(StanDisol(), 'AmountOfSubstancePerUnitMass'))
        xml_add_child(., .value = xml_child(SampDisol(), search = 'mr:coreData//mr:timeISO8601'))
      }
      message(xmlObject)
      # addDataToMRXML(xmlObject, addInfList, node = 'mr:additionalInfo')
      return(xmlObject)
    })

    
    SummaryIndivTitr <- eventReactive(input$TermTit, {
      if(length(na.omit(TitCurvDat()$Titrant)) < 6) {
        tags$b('No se puede terminar la titulación con menos de 7 datos.')
      } else {
        d1 <- decimals(signif(ResParcUnc()$prop[3], 3))
        d2 <- decimals(signif(ResParcUncSource()$prop[3], 3))
        return(infoBox(
          width = 12, title = tags$b(style = 'font-size:13px', 'Resultado individual (parcial)'),
          icon = icon("vials"), color = 'black', fill = FALSE,
          subtitle = tags$div(
            style = 'font-size:13px',
            fluidRow(
              column(5,
                'Fracción de', elemEspa[[element()]], 'en la disolución titulada:', tags$br(),
                tags$b(style = 'margin-left:10px;', round(ResParcial(), d1), '\u00B1',
                       ReqField(signif(ResParcUnc()$prop[3], 3), 1), ' mg/kg (k=1)'), Nlns(2), 
                'Fracción de', elemEspa[[element()]], 'en la muestra original:',  tags$br(),
                tags$b(style = 'margin-left:10px;', round(ResParcialSource(), d2), '\u00B1',
                       ReqField(signif(ResParcUncSource()$prop[3], 3), 1), ' mg/kg (k=1)'), tags$br(), tags$br(),
                tags$div(
                  style = 'font-size:10px',
                  ReqField('', 1),
                  'No incluye el error asociado a la determinación del punto final de titulación.
                  Este aporte es significativo y se estima con la repetibilidad de las mediciones (Incertidumbre tipo A).'),
                tags$hr(),
                tags$ul(
                  tags$li(downloadLink(session$ns("downlXMLlink"), label = tags$b('Descargar XML del resultado'))),
                  tags$li(actionLink(session$ns("showBudget"), label = ('Ver presupuesto de incertidumbre'))),
                  tags$li(actionLink(session$ns("showXMLfile"), label = ('Ver informacion completa del resultado'))))
              ),
              column(6, plotOutput(session$ns('TitCurvePlot2'), width = '100%'))
            ))))
      }
    })
    output$SummaryIndivTitr <- renderUI(SummaryIndivTitr())
    
    uncertBudget <- eventReactive(input$showBudget, {
      tagList(
        tags$b('Ecuación del modelo:'), tags$br(),
        '$$ \\nu_{metal} = \\frac{(m_{eq} - m_{bln} \\cdot c_{std}}{m_{ali}} \\cdot M_{metal} \\cdot
        \\frac{1}{r_{mass}}$$',
        Nlns(), tags$b('Presupuesto de incertidumbre:'),
        tags$div(
          style = 'margin-left:15px;margin-right:10px;font-size:12px;', 
          withMathJax(),
          withMathJax(tableOutput(session$ns("tableUncert"))),
          'donde \\(\\nu_{metal}\\) es la fracción másica del metal en la disolución original, \\(m_{eq}\\) es la masa de equivalencia de titulante,
          \\(m_{bln}\\) es la masa de titulante para un blanco de muestra, \\(c_{std}\\) es la concentración de EDTA en la disolución estándar,
          \\(m_{ali}\\) es la masa de la alícuota, \\(M_{metal}\\) es el peso atómico del metal, y 
          \\(r_{mass}\\) es la relación de masa de la disolución original en la disolución que se titula (en caso de titular muestras diluidas gravimétricamente).'),
        tags$hr())
    })
    output$uncertBudget <- renderUI(uncertBudget())
    output$tableUncert <- renderTable({
      units <- c("\\milli\\gram\\kilo\\gram\\tothe{-1}", "\\gram", "\\gram", ConcStanSolut()$Units, "\\gram", 
                 ConcStanSolut()$Units, MassRatioSamp()$Units)
      tab <- data.frame(Valor = as.character(signif(c(ResParcUncSource()$prop[[1]], ResParcUncSource()$data[1, ]), 9)),
                        u_std = as.character(signif(c(ResParcUncSource()$prop[[3]], ResParcUncSource()$data[2, ]), 4)),
                        Unidades = units, Aporte = c(NA, paste((round(diag(ResParcUncSource()$rel.contr)*100, 3)), '%', sep = ' ')))
      rownames(tab) <- c("\\(nu_{metal}\\)", "\\(m_{eq}\\)", "\\(m_{bln}\\)", "\\(c_{std}\\)", "\\(m_{ali}\\)", "\\(M_{metal}\\)", "\\r_{mass}\\)")
      tab
    },
    include.rownames = TRUE, include.colnames = TRUE)
    
    observeEvent(req(input$showXMLfile), {
      withCallingHandlers({
        shinyjs::html("InfoTitXML", "")
        message(InfoTitXML())},
        message = function(m) {
          shinyjs::html(id = "InfoTitXML",
                        html = paste0('<b>Información de la titulación:</b><br>
                                      <textarea rows = 40 style = "width: 95%; margin-left:20px;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLlink <-  downloadHandler(
        filename = function() {paste0(gsub(' ', '_', resultID(), fixed = FALSE), ".xml")},
        content = function(file) {write_xml(InfoTitXML(), file)})
    })

    return(InfoTitXML)
  })
}
                           