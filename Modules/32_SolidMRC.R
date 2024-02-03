SolidMRCUI <- function(id, demo, title, reagent, reagKey, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b(paste0('Nueva disolucion estándar de ', reagKey)), tags$br(), 
    paste0('Patrón para titular muestras de ', explan), 
    tags$br(), tags$br(),
    tags$div(
      id = 'inline', style = 'font-size:12px; margin-left:25px', 
      textInput(ns('DisolID'), label = h5(tags$b(ReqField('ID disolución', 8))), width = '300px',
                value = paste(gsub('-', '', fecha), title, sep = '_')),
      uiOutput(ns('MRCtoUse')),
      tags$hr(),
      splitLayout(
        cellWidths = c("38%", "10%", "38%"),
        tags$div(
          id = "inline", style = 'margin-left:25px', 
          h5(tags$b('Masa del solido')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRec1'), label = ReqField('Masa del recipiente / g:'), value = ifelse(demo, 0.976601, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasMRC1'), label = ReqField('Masa del MRC / g:'), value = ifelse(demo, 0.30042, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecMRC1'), label = ReqField('Masa conjunta / g:'), value = ifelse(demo, 1.27705, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaMRC'))),
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
          uiOutput(ns('deriMasaDisMRC')))),
      tags$hr(),
      SiRealInputUI(ns('DensiDisol'), name = ReqField('Densidad de la disolución'),
                    x0 = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0)), 
                    u0 = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)), units = DensityUnits,
                    decimalPlaces = 3),
      tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución')), Nlns(3)),
    fluidRow(
      column(width = 2, SI_unit_nice('mole', width = "95%"), SI_unit_nice('kilogram', width = "95%")),
      column(width = 10, disabled(downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML de la disolución estándar')),
             Nlns(2), htmlOutput(ns('InfoDisXML'))))
  )
}

SolidMRCServer <- function(id, devMode, demo, reagKey, reagForm, balanza, analyst, materiales, fecha, ambient, solutionType) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())
    choicesMateriales <- reactive(sapply(materiales, function(x) as_list(x)[[1]]$administrativeData$name))
    
    MRCtoUse <- reactive(pickerInput(
      session$ns("MRCtoUse"), width = "fit", selected = ifelse(demo(), choicesMateriales()[[1]], ''), multiple = TRUE, inline = TRUE,
      label = h5(tags$b(ReqField("MRC de partida"))), choices = choicesMateriales(),
      options = list(`max-options` = 1, `none-selected-text` = "(Ver módulo Materiales de referencia)")))
    output$MRCtoUse <- renderUI(MRCtoUse())
    # browser()
    SolidMRC <- reactive({
      req(input$MRCtoUse)
      materiales[[which(sapply(materiales, function (x) as_list(x)[[1]]$administrativeData$name) == input$MRCtoUse)]]})
    
    # ListSolidMRC <- reactive(as_list(SolidMRC()))
      
    DensiDisol <- SiRealInputServer('DensiDisol', devMode = devMode, quantityTypeQUDT = 'Density')
    
    observe({
      req(balanza, analyst, input$DisolID, input$MRCtoUse, input$MasRec1, input$MasMRC1, input$MasRecMRC1, input$MasRec2, input$MasDis1, input$MasRecDis1)
      if (input$MasRec1 * input$MasMRC1 * input$MasRecMRC1 * input$MasRec2 * input$MasDis1 * input$MasRecDis1 > 0) enable('buttonCalc')
    })
    
    MassFrMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), property = 'MassFraction', node = 'mr:additionalValues'))
    MolWeiMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), property = 'MolarMass', node = 'mr:additionalValues'))
    DensitMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), property = 'Density', node = 'mr:additionalValues'))
    
    derMassMRC <- reactive(input$MasRecMRC1 - input$MasMRC1 - input$MasRec1)
    masMRC <- reactive(mean(input$MasMRC1, input$MasRecMRC1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    DisolDensi <- reactive(GetValueEstandUncert(req(DensiDisol())))
    airDensity <- reactive(GetValueEstandUncert(req(ambient()), 'Density'))
    
    
    convMassMRC <- reactive(c(convMass(calibCert = balanza(), reading = masMRC(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masMRC(), units = 'g')))
    BuoyMRC <- reactive(c(MABC(rho = DensitMRC()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DensitMRC()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DensitMRC()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE, plot = FALSE)))
    
    convMassDis <- reactive(c(convMass(calibCert = balanza(), reading = masDis(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masDis(), units = 'g')))
    BuoyDis <- reactive(c(MABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DisolDensi()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE)))
    
    DisConc <- eventReactive(input$buttonCalc, {
      xx <- propagate(
        expr = expression(convMassMRC * MassFrMRC * BuoyMRC / (MolWeiMRC * convMassDis * BuoyDis) * 1000000),
        data = cbind(convMassMRC = convMassMRC(), MassFrMRC = MassFrMRC()$ValUnc, BuoyMRC = BuoyMRC(), 
                     MolWeiMRC = MolWeiMRC()$ValUnc, convMassDis = convMassDis(), BuoyDis = BuoyDis()),
        do.sim = FALSE)
      xx <- SiRealXML(quantityTypeQUDT = 'AmountOfSubstancePerUnitMass', value = signif(xx$prop[[1]], 8),
                      units = '\\milli\\mole\\kilo\\gram\\tothe{-1}', uncert =  signif(xx$prop[[3]], 3), covFac = 1)
      return(xx)
    })
    
    
    # Messages
    deriMasaMRC <- reactive(div(style = 'font-size:11px', 'Deriva de la balanza: ', round(derMassMRC() * 1000, 2), ' mg'))
    deriMasaDisMRC <- reactive(div(style = 'font-size:11px', 'Deriva de la balanza: ', round(derMassDis() * 1000, 2), ' mg'))
    
    output$deriMasaMRC <- renderUI(deriMasaMRC())
    output$deriMasaDisMRC <- renderUI(deriMasaDisMRC())
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML()
      AdminList <- list('mr:solutionType' = solutionType,
                        'mr:solutionID' = input$DisolID,
                        'mr:CRM' = input$MRCtoUse,
                        'mr:timeISO8601' = iso8601(fecha(), niceHTML = FALSE))
      PropeList <- list('mr:substance' = Substances[[reagForm]])

      addDataToMRXML(xmlObject, AdminList, node = 'mr:coreData')
      addDataToMRXML(xmlObject, PropeList, node = 'mr:property')
      xml_child(xmlObject, search = 'mr:coreData') %>% xml_add_child(., .value = analyst())
      xml_child(xmlObject, search = 'mr:property') %>% xml_add_child(., .value = DisConc())
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
                           