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
                  choices = list('Plomo' = 'Pb', 'Cadmio' = 'Cd',  'Calcio' = 'Ca')),
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
          uiOutput(ns('deriMasaSAMPLE')), tags$hr(),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasDisolv1'), label = ReqField('Masa del disolvente / g:'),
                           value = ifelse(demo, 9.00002, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecSolution1'), label = ReqField('Masa del recipiente con disolución / g:'),
                           value = ifelse(demo, 20.0000, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaDisSAMPLE')), tags$hr(),
          uiOutput(ns('showFactorDilucion')))),
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
    otroMolarMass <- SiRealInputServer(id = 'MasaMolar', devMode = devMode, quantityTypeQUDT = 'MolarMass')
    
    MolarMass <- reactive({
      if(input$MolarMassOpt == 'Otro') return(otroMolarMass())
      return(SiRealXML(quantityTypeQUDT = 'MolarMass', value = InitMolarMass()[1], units = '\\gram\\mole\\tothe{-1}',
                       uncert = InitMolarMass()[2], covFac = 1, distribution = 'normal'))
    })
    

    observe({
      req(analyst, input$Nombre)
      bolean <- TRUE
      if (bolean) enable('buttonCalc')
    })
    
    derMassSample <- reactive(input$MasRecSample1 - input$MasSample1 - input$MasRec1)
    masSample <- reactive(mean(input$MasSample1, input$MasRecSample1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecSolution1 - input$MasDisolv1 - input$MasSample1 - input$MasRec1)
    masDis <- reactive(mean(input$MasRecSolution1 - input$MasRec1, input$MasSample1 + input$MasDisolv1))
    
    # Messages
    deriMasaSAMPLE <- reactive(div(style = 'font-size:11px', 'Deriva en la medición de masa de la alicuota: ', signif(derMassSample() * 1000, 2), ' / mg'))
    deriMasaDisSAMPLE <- reactive(div(style = 'font-size:11px', 'Deriva en la medición de masa de la disolucion: ', signif(derMassDis() * 1000, 2), ' / mg'))
    output$deriMasaSAMPLE <- renderUI(deriMasaSAMPLE())
    output$deriMasaDisSAMPLE <- renderUI(deriMasaDisSAMPLE())
    
    convMassSample <- reactive(c(convMass(calibCert = balanza(), reading = masSample(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masSample(), units = 'g')))
    
    convMassDis <- reactive(c(convMass(calibCert = balanza(), reading = masDis(), units = 'g'),
                              uncertConvMass(calibCert = balanza(), reading = masDis(), units = 'g')))
    
    
    
    
    
    factorDilucion <- eventReactive(input$buttonCalc, {
      if (input$DilutionOpt == "Dil") {
        xx <- propagate(
          expr = expression(convMassSample / convMassDis),
          data = cbind(convMassDis = convMassDis(), convMassSample = convMassSample()),
          do.sim = FALSE)
        xx <- SiRealXML(quantityTypeQUDT = 'MassRatio', value = signif(xx$prop[[1]], 8),
                        units = '\\gram\\gram\\tothe{-1}', uncert =  signif(xx$prop[[3]], 5), covFac = 1)
      } else {
        xx <- SiRealXML(quantityTypeQUDT = 'MassRatio', value = 1, units = '\\gram\\gram\\tothe{-1}', uncert =  0, covFac = 1)
      }
      return(xx)
    })
    
    showFactorDilucion <- reactive(tags$div(tags$b(paste0(
      'Factor de dilución 1:', signif((input$MasSample1 + input$MasDisolv1) / input$MasSample1, 1)))))
    
    output$showFactorDilucion <- renderUI(showFactorDilucion())
    
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML()
      AdminList <- list('mr:solutionType' = solutionType, 
                        'mr:timeISO8601' = iso8601(fecha(), niceHTML = FALSE))
      PropeList <- list('mr:substance' = Substances[[input$Elemento]])
      
      addDataToMRXML(xmlObject, AdminList, node = 'mr:coreData')
      addDataToMRXML(xmlObject, PropeList, node = 'mr:property')
      xml_child(xmlObject, search = 'mr:coreData') %>% xml_add_child(., .value = analyst())
      xml_child(xmlObject, search = 'mr:property') %>% {
        xml_add_child(., .value = MolarMass())
        xml_add_child(., .value = factorDilucion())}
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