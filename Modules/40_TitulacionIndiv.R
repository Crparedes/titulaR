TitIndividualUI <- function(id, demo, title, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b('Titulación de ', explan), tags$hr(), Nlns(1),
    fluidRow(
      column(
        6,
        tags$div(
          id = 'inline', style = 'font-size:12px; margin-left:25px', 
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasaAlic'), label = ReqField('Masa de la alícuota / g'), value = ifelse(demo, 10.0552, 0)),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasaEDTA0'), label = NonReqField('Masa inicial de titulante / g', 4), value = 0),
          conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns, tags$hr(),
                           rHandsontableOutput(ns("TitData"), width = '100%')))),
      column(
        6, tags$b('Curva de titulación:'),
        fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'))), tags$br(),
        disabled(actionButton(ns('TermTit'), label = 'Terminar titulación')), tags$br(),
        infoBoxOutput(ns("summary")),
        fluidRow(
          column(width = 1, SI_unit_nice('kilogram', width = "97%")),
          column(width = 11, downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML de la titulación'),
                 Nlns(2), htmlOutput(ns('InfoTitXML')))),
        uiOutput(ns('TitulTerminada')))
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
    
    TitulTerminada <- eventReactive(input$TermTit, {
      if(length(na.omit(TitCurvDat()$Titrant)) < 6) {
        tags$b('No se puede terminar la titulación con menos de 7 datos!')
      } else {
        tags$div(
          tags$hr(),
          tags$b('Resultados de la titulación:'), tags$br(),
          tags$div(style = 'font-size:12px', 
                   infoBox(
                     width = 12, title = tags$b('Fracción másica del elemento en la alícuota: '), icon = icon("vials"), color = 'black', 
                     value = tags$div(tags$b(round(ResParcial(), 3), ' [mg/kg]')#, renderPrint(summaryTitration())
                     ), 
                     subtitle = 'Exporte y guarde el archivo de resultados individuales.´'), tags$br(), tags$br(),
                   downloadButton(session$ns('DwnlResFile'), label = tags$b('Descargar archivo .tit')), tags$br(), tags$br())
        )
      }
    })
    MasaEquiv <- eventReactive(input$TermTit, {try(EP.1stDer(curve = CleanDf()))})
    MasAtoElem <- reactive(ifelse(Elemento == 'Pb', LeadAM, ElementsAtomicMass[[Elemento]][1]))
    u_MasAtoElem <- reactive(ifelse(Elemento == 'Pb', u_LeadAM, ElementsAtomicMass[[Elemento]][2]))
    ResParcial <- reactive(MasaEquiv() * DisEDTA_MRC$infoDisMRC()$`Concentración [mmol/kg]` / input$MasaAlic * MasAtoElem())
    ResParcUnc <- reactive(propagate(expr = expression((Meq - Mbln) * Cedta / Mali * Mato),
                                     data = cbind(Meq = c(convMass(CalibCertList[[BalanzaMonoelemTit]], reading = MasaEquiv()),
                                                          sqrt(2) * uncertConvMass(CalibCertList[[BalanzaMonoelemTit]], reading = MasaEquiv(), 
                                                                                   d = 0.1, d.units = 'mg')),
                                                  Mbln = c(0, 0.0028/(2*sqrt(3))),
                                                  Cedta = c(DisEDTA_MRC$infoDisMRC()$`Concentración [mmol/kg]`,
                                                            DisEDTA_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`),
                                                  Mali = c(convMass(CalibCertList[[BalanzaMonoelemTit]], reading = input$MasaAlic),
                                                           uncertConvMass(CalibCertList[[BalanzaMonoelemTit]], reading = input$MasaAlic)),
                                                  Mato = c(MasAtoElem(), u_MasAtoElem())),
                                     second.order = FALSE, do.sim = FALSE
    ))
    
    summaryTitration <- reactive(
      list(Muestra = sampleID, Elemento = Elemento, MasaAlicuota = input$MasaAlic, 
           MasaEquiv = MasaEquiv(), 'Fracción másica [mg/kg]' = ResParcial(), 'Incertidumbre estandar' = ResParcUnc(), 
           'Disolución titulante' = DisEDTA_MRC$infoDisMRC(),
           'Certificado calibración balanza' = CalibCertList[[BalanzaMonoelemTit]],
           Analista = IDUsuario(),#[1], correoAnalista = IDUsuario()[2],
           TitCurvDat = TitCurvDat()[complete.cases(TitCurvDat()), ],
           dscripMuestra = dscrMuestraMonoelemTit, 
           MasAtoElem = c(MasAtoElem(), u_MasAtoElem()),
           Inicio = horaInicio(),
           Final = horaFinal()
      ))
    
    output$DwnlResFile <- downloadHandler(
      filename = function() {paste0(Elemento, "_", sampleID, ".", number, "_", isolate(horaInicio()), ".tit")},
      content = function(file) {saveRDS(summaryTitration(), file = file)}, contentType = NULL)
    
    output$TitCurvePlot <- renderPlot(TitCurvePlot())
    output$TitulTerminada <- renderUI(TitulTerminada())
    output$DescaResu <- renderUI(DescaResu())
    return(list('Titulación' = summaryTitration#, 
                #'Exito' = !is.null(is.numeric(ResParcial()))
    ))
    
    # return(list('infoDisMRC' = infoDisMRC))
  })
}
                           