  CalibSampleUI <- function(id, demo, title, fecha, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b(paste0('Nueva disolucion calibrante monoelemental.')), Nlns(), #explan, tags$br(), tags$br(),
    tags$div(
      id = 'inline', style = 'font-size:12px; margin-left:25px',
      textInput(ns('DisolID'), label = h5(tags$b(ReqField('ID disolución', 4))), width = '300px',
                value = paste(gsub('-', '', fecha), title, sep = '_')),
      textInput(ns('Nombre'), label = h5(tags$b(ReqField('Nombre', 4))), width = '300px',
                value = ifelse(demo, 'MRC-INM-023-1', ''), placeholder = '(e.g. código MRC, unidad, lote)')),
    tags$div(
      id = 'inline', style = 'font-size:12px; margin-left:25px',
      pickerInput(ns('Elemento'), label = h5(tags$b(ReqField('Elemento', 3, 6))), multiple = FALSE, inline = TRUE, width = 'fit',
                  choices = list('Plomo' = 'PbII', 'Cadmio' = 'CdII',  'Calcio' = 'CaII')),
      pickerInput(ns('MolarMassOpt'), label = NULL, multiple = FALSE,  inline = TRUE, width = 'fit',
                  choices = list('Peso atómico estándar' = 'IUPAC', 'Ingresar peso atómico' = 'Otro')),
      pickerInput(ns('DilutionOpt'), label = NULL, multiple = FALSE,  inline = TRUE, width = 'fit',
                  choices = list('Calibrante sin diluír' = 'noDil', 'Dilución gravimétrica' = 'Dil'))),
      
      conditionalPanel(
        'input.DilutionOpt == "Dil"', ns = ns, 
        tags$div(
          id = "inline", style = 'margin-left:25px',
          tags$hr(), tags$b('Dilución gravimétrica de la muestra:', style = 'margin-left:-15px'),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRec1'), label = ReqField('Masa del recipiente / g:'),
                           value = ifelse(demo, 10.00000, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasSample1'), label = ReqField('Masa del calibrante / g:'),
                           value = ifelse(demo, 1.00006, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSample1'), label = ReqField('Masa del recipiente con calibrante / g:'),
                           value = ifelse(demo, 11.00001, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSample1'), label = ReqField('Masa del disolvente / g:'),
                           value = ifelse(demo, 9.00002, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSample1'), label = ReqField('Masa del recipiente con disolución / g:'),
                           value = ifelse(demo, 20.0000, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaSample')))),
    
        uiOutput(ns('ShowMolarMass')),
        tags$hr(),
        tags$hr(),
        tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución')), Nlns(3),
      fluidRow(
        column(width = 2, SI_unit_nice('mole', width = "95%"), SI_unit_nice('kilogram', width = "95%")),
        column(width = 10, disabled(downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML de la disolución muestra')),
               Nlns(2), htmlOutput(ns('InfoDisXML'))))
    )
}

CalibSampleServer <- function(id, devMode, demo, balanza, analyst, fecha, ambient, solutionType) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
    observeEvent(input$brwz, browser())
    
    # OtroMasaMolar <- reactive({
    #   if(input$MasaMolarOpt == 'Otro') {
    #     
    #     MasaMolarInputUI <- SiRealInputUI(session$ns('MasaMolar'), name = 'Valor de masa molar del elemento')))
    #   return(MasaMolarInputUI)
    #   }
    # })
    
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
      xx <- SiRealXML(quantityTypeQUDT = 'MassFraction', value = signif(xx$prop[[1]], 8),
                      units = '\\gram\\gram\\tothe{-1}', uncert =  signif(xx$prop[[3]], 5), covFac = 1)
      return(xx)
    })
    
    # Messages
    deriMasaSAMPLE <- eventReactive(input$MasRecSAMPLE1, 
                                 div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassSAMPLE() * 1000, 2), ' / mg'))
    deriMasaDisSAMPLE <- eventReactive(input$MasRecDis1,
                                    div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassDis() * 1000, 2), ' / mg'))
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