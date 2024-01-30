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
      pickerInput(ns('DilutionOpt'), label = NULL, multiple = FALSE,  inline = TRUE, width = 'fit',
                  choices = list('Calibrante sin diluír' = 'noDil', 'Dilución gravimétrica' = 'Dil')),
      pickerInput(ns('MolarMassOpt'), label = NULL, multiple = FALSE,  inline = TRUE, width = 'fit',
                  choices = list('Peso atómico estándar' = 'IUPAC', 'Ingresar peso atómico manualmente' = 'Otro'))),
      
      conditionalPanel(
        'input.DilutionOpt == "Dil"', ns = ns, tags$hr(), tags$b('Dilución gravimétrica de la muestra:', style = 'margin-left:20px'),
        tags$div(
          id = "inline", style = 'margin-left:45px; font-size:12px;', 
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRec1'), label = ReqField('Masa del recipiente / g:'),
                           value = ifelse(demo, 10.00000, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasSample1'), label = ReqField('Masa del calibrante / g:'),
                           value = ifelse(demo, 1.00006, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSample1'), label = ReqField('Masa del recipiente con calibrante / g:'),
                           value = ifelse(demo, 11.00001, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaAlicuota')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasDisolv1'), label = ReqField('Masa del disolvente / g:'),
                           value = ifelse(demo, 9.00002, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSolution1'), label = ReqField('Masa del recipiente con disolución / g:'),
                           value = ifelse(demo, 20.0000, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaDisolucion')))),
    conditionalPanel('input.MolarMassOpt == "Otro"', ns = ns, tags$hr(), uiOutput(ns('ShowMolarMass'))),
    tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución', style = 'margin-left:45px')), Nlns(3),
    
    fluidRow(
        column(width = 2, SI_unit_nice('mole', width = "95%"), SI_unit_nice('kilogram', width = "95%")),
        column(width = 10, disabled(downloadLink(ns("downlXMLlink"), label = 'Descargar archivo XML de la disolución muestra')),
               Nlns(2), htmlOutput(ns('InfoDisXML'))))
    )
}

CalibSampleServer <- function(id, devMode, demo, balanza, analyst, fecha, ambient, solutionType) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())
    
    InitMolarMass <- reactive(IUPAC2019AW[[input$Elemento]])
    
    ShowMolarMass <- reactive({
      if (input$MolarMassOpt == 'Otro') {
        MasaMolarInputUI <- SiRealInputUI(session$ns('MasaMolar'),
                                          name = tags$h5(tags$b('Peso atómico del elemento'), style = 'margin-left:20px'),
                                          colWid = c(2, 2, 3, 4),
                                          x0 = InitMolarMass()[1], u0 = InitMolarMass()[2], units = '\\gram\\mole\\tothe{-1}')
        return(MasaMolarInputUI)
      }
    })
    output$ShowMolarMass <- renderUI(ShowMolarMass())
    observe(isolate(SiRealInputServer(id = 'MasaMolar', devMode = devMode, quantityTypeQUDT = 'MolarMass')))
    # OtroMasaMolar <- reactive({
    #   if(input$MasaMolarOpt == 'Otro') {
    #     
        
    #   }
    # })
    
    observe({
      req(analyst, input$Nombre)
      bolean <- TRUE
      # if (input$DilutionOpt == 'Dil') {
      #   bolean <- FALSE
      #   req(balanza)
      #   if (input$MasRec1 * input$MasSample1 * input$MasRecSample1 * input$MasRec2 * input$MasDis1 *
      #       input$MasDisolv1 * input$MasRecSolution1 == 0) bolean <- FALSE
      # }
      # if (!isTruthy(balanza))
      # if (input$MolarMassOpt == "Otro") {
      #   if (input$MasRec1 * input$MasSample1 * input$MasRecSample1 * input$MasRec2 * input$MasDis1 *
      #       input$MasDisolv1 * input$MasRecSolution1 == 0) bolean <- FALSE
      # }
      
      if (bolean) enable('buttonCalc')
    })
    
    derMassSample <- reactive(input$MasRecSample1 - input$MasSample1 - input$MasRec1)
    masSample <- reactive(mean(input$MasSample1, input$MasRecSample1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    derMassSAMPLE <- reactive(input$MasRecSAMPLE1 - input$MasSAMPLE1 - input$MasRec1)
    masSAMPLE <- reactive(mean(input$MasSAMPLE1, input$MasRecSAMPLE1 - input$MasRec1))
    deriMasaDisolucion <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    # Messages
    deriMasaSAMPLE <- eventReactive(input$MasRecSAMPLE1, 
                                 div(style = 'font-size:11px', 'Deriva en la medición de masa de la alicuota: ', signif(derMassSAMPLE() * 1000, 2), ' / mg'))
    deriMasaDisSAMPLE <- eventReactive(input$MasRecDis1,
                                    div(style = 'font-size:11px', 'Deriva en la medición de masa de la disolucion: ', signif(derMassDis() * 1000, 2), ' / mg'))
    output$deriMasaSAMPLE <- renderUI(deriMasaSAMPLE())
    output$deriMasaDisSAMPLE <- renderUI(deriMasaDisSAMPLE())
    
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
    
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML()
      AdminList <- list('mr:solutionType' = solutionType, 'mr:timeISO8601' = iso8601(fecha(), niceHTML = FALSE))
      # PropeList <- list('mr:substance' = Substances[reagForm])
      
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