SolidSampleUI <- function(id, demo, title, reagent, reagKey, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b(paste0('Nueva disolucion muestra de ', reagKey)), tags$br(), explan, tags$br(), tags$br(),
    tags$div(
      id = 'inline', style = 'font-size:12px; margin-left:25px', 
      textInput(ns('DisolID'), label = h5(tags$b(ReqField('ID disolución', 4))), width = '300px',
                value = paste(gsub('-', '', fecha), title, sep = '_')),
      radioButtons(ns('Reagent'), label = h5(tags$b(ReqField('Reactivo', 11))),
                   choices = list('Sal disódica dihidratada de EDTA' = 'Na2EDTA.2H2O')),
      tags$hr(),
      splitLayout(
        cellWidths = c("38%", "10%", "38%"),
        tags$div(
          id = "inline", style = 'margin-left:25px', 
          h5(tags$b('Masa del solido')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRec1'), label = ReqField('Masa del recipiente / g:'), value = ifelse(demo, 0.976601, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasSample1'), label = ReqField('Masa del reactivo / g:'), value = ifelse(demo, 0.30042, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSample1'), label = ReqField('Masa conjunta / g:'), value = ifelse(demo, 1.27705, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaSAMPLE'))),
        tags$div(),
        tags$div(
          id = "inline",
          h5(tags$b('Masa final de la disolución')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRec2'), label = ReqField('Masa del recipiente / g:'), value = ifelse(demo, 16.77169, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, 
                           ns('MasDis1'), label = ReqField('Masa final disolución / g:'), value = ifelse(demo, 80.02288, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRecDis1'), label = ReqField('Masa conjunta / g:'), value = ifelse(demo, 96.79455, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaDisSAMPLE')))),
      tags$hr(),
      SiRealInputUI(ns('DensiDisol'), name = ReqField('Densidad de la disolución'),
                    x0 = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0)), 
                    u0 = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)), units = DensityUnits,
                    decimalPlaces = 3),
      tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución')), Nlns(3)),
    fluidRow(
      column(width = 2, SI_unit_nice('mole', width = "95%"), SI_unit_nice('kilogram', width = "95%")),
      column(width = 10, disabled(downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML de la disolución muestra')),
             Nlns(2), htmlOutput(ns('InfoDisXML'))))
  )
}

SolidSampleServer <- function(id, devMode, demo, reagKey, reagForm, balanza, analyst, fecha, ambient, solutionType) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
    observeEvent(input$brwz, browser())
    
    DensiDisol <- SiRealInputServer('DensiDisol', devMode = devMode, quantityTypeQUDT = 'Density')
    observe({
      req(balanza, analyst, input$DisolID, input$MasRec1, input$MasSample1, input$MasRecSample1, input$MasRec2, input$MasDis1, input$MasRecDis1)
      if (input$MasRec1 * input$MasSample1 * input$MasRecSample1 * input$MasRec2 * input$MasDis1 * input$MasRecDis1 > 0) enable('buttonCalc')
    })
    
    derMassSample <- reactive(input$MasRecSample1 - input$MasSample1 - input$MasRec1)
    masSample <- reactive(mean(input$MasSample1, input$MasRecSample1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    derMassSAMPLE <- reactive(input$MasRecSAMPLE1 - input$MasSAMPLE1 - input$MasRec1)
    masSAMPLE <- reactive(mean(input$MasSAMPLE1, input$MasRecSAMPLE1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    DisolDensi <- reactive(GetValueEstandUncert(req(DensiDisol())))
    airDensity <- reactive(GetValueEstandUncert(req(ambient()), 'Density'))
    
    convMassSample <- reactive(c(convMass(calibCert = balanza(), reading = masSample(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masSample(), units = 'g')))
    DensitSample <- reactive(GetValueEstandUncert(SiRealXML(SI.list = densities$Na2EDTA.2H2O['si:real']), 'Density'))
    
    BuoySample <- reactive(c(MABC(rho = DensitSample()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DensitSample()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DensitSample()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE, plot = FALSE)))
    
    convMassDis <- reactive(c(convMass(calibCert = balanza(), reading = masDis(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masDis(), units = 'g')))
    BuoyDis <- reactive(c(MABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DisolDensi()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE)))
    
    factorDilucion <- eventReactive(input$buttonCalc, {
      xx <- propagate(
        expr = expression((convMassSample * BuoySample) / (convMassDis * BuoyDis)),
        data = cbind(convMassSample = convMassSample(), BuoySample = BuoySample(), 
                     convMassDis = convMassDis(), BuoyDis = BuoyDis()),
        do.sim = FALSE)
      xx <- SiRealXML(quantityTypeQUDT = 'MassRatio', value = signif(xx$prop[[1]], 8),
                      units = '\\gram\\gram\\tothe{-1}', uncert =  signif(xx$prop[[3]], 5), covFac = 1)
      return(xx)
    })
    
    # Messages
    deriMasaSAMPLE <- eventReactive(input$MasRecSAMPLE1, 
                                 div(style = 'font-size:11px', 'Deriva de la balanza:', signif(derMassSAMPLE() * 1000, 2), ' mg'))
    deriMasaDisSAMPLE <- eventReactive(input$MasRecDis1,
                                    div(style = 'font-size:11px', 'Deriva de la balanza:', signif(derMassDis() * 1000, 2), ' mg'))
    output$deriMasaSAMPLE <- renderUI(deriMasaSAMPLE())
    output$deriMasaDisSAMPLE <- renderUI(deriMasaDisSAMPLE())
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML()
      AdminList <- list('mr:solutionType' = solutionType, 'mr:timeISO8601' = iso8601(fecha(), niceHTML = FALSE))
      PropeList <- list('mr:substance' = Substances[reagForm])
      
      addDataToMRXML(xmlObject, AdminList, node = 'mr:coreData')
      addDataToMRXML(xmlObject, PropeList, node = 'mr:property')
      xml_child(xmlObject, search = 'mr:coreData') %>% xml_add_child(., .value = analyst())
      xml_child(xmlObject, search = 'mr:property') %>% xml_add_child(., .value = factorDilucion())
      return(xmlObject)
    })
    
    observeEvent(input$buttonCalc, {
      enable('downlXMLlink')
      withCallingHandlers({
        shinyjs::html("InfoDisXML", "")
        message(DisolucionXML())},
        message = function(m) {
          shinyjs::html(id = "InfoDisXML",
                        html = paste0('<textarea rows = 40 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLlink <-  downloadHandler(
        filename = function() {paste0(gsub(pattern = ' ', replacement = '_', input$DisolID, fixed = FALSE), ".xml")},
        content = function(file) {write_xml(DisolucionXML(), file)})
    })
    
    return(DisolucionXML)
  })
}