SolidMRCUI <- function(id, demo, title, reagent, reagKey, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b(paste0('Nueva disolucion de ', reagKey)), tags$br(), 
    paste0('Estandar para titular muestras de ', explan), 
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
                           ns('MasRec1'), label = 'Masa del recipiente [g]: .', value = ifelse(demo, 0.976601, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasMRC1'), label = 'Masa del MRC [g]: .', value = ifelse(demo, 0.30042, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecMRC1'), label = 'Masa conjunta [g]: .', value = ifelse(demo, 1.27705, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaMRC'))),
        tags$div(),
        tags$div(
          id = "inline",
          h5(tags$b('Masa final de la disolución')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRec2'), label = 'Masa del recipiente [g]: .', value = ifelse(demo, 16.77169, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, 
                           ns('MasDis1'), label = 'Masa final disolución [g]: .', value = ifelse(demo, 80.02288, 0), align = 'left', minimumValue = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRecDis1'), label = 'Masa conjunta [g]: .', value = ifelse(demo, 96.79455, 0), align = 'left', minimumValue = 0),
          uiOutput(ns('deriMasaDisMRC')))),
      tags$hr(),
      SiRealInputUI(ns('DensiDisol'), name = 'Densidad de la disolución', 
                    x0 = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0)), 
                    u0 = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)), units = DensityUnits,
                    decimalPlaces = 3),
      tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución')), Nlns(3)),
    fluidRow(
      column(width = 2, img(src = "SI_mol.png", width = "95%")),
      column(width = 10, tags$br(), uiOutput(ns("downlXMLlink")), htmlOutput(ns('InfoDisXML'))))
  )
}

SolidMRCServer <- function(id, devMode, demo, reagKey, reagForm, balanza, analyst, materiales, fecha, ambient) {
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
      req(balanza, analyst, input$DisolID, input$MRCtoUse, input$MasRec1, input$MasMRC1,
          input$MasRecMRC1, input$MasRec2, input$MasDis1, input$MasRecDis1)
      if (input$MasRec1 * input$MasMRC1 * input$MasRecMRC1 * input$MasRec2 * input$MasDis1 * input$MasRecDis1 > 0) enable('buttonCalc')
    })
    
    MassFrMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), 'MassFraction'))
    MolWeiMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), 'MolarMass'))
    DensitMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), 'Density'))
    
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
                      units = '\\milli\\mol\\kilo\\gram\\tothe{-1}', uncert =  signif(xx$prop[[2]], 5), covFac = 1)
      return(xx)
    })
    
    
    # infoDisMRC <- eventReactive(input$buttonCalc, {
    #   if (input$SourceOption == "daCapo") {
    #     if (!is.na(DisConc()$prop[[1]] > 0) && !is.na(DisConc()$prop[[3]] > 0)) {
    #       return(list('MRC empleado' = input$MRCElected,
    #                   'Fecha de vencimiento MRC' = dateMRC(),
    #                   'Especie ' = reagKey,
    #                   'Concentración [mmol/kg]' = signif(DisConc()$prop[[1]], 7),
    #                   'Incertidumbre [mmol/kg]' = signif(DisConc()$prop[[3]], 4),
    #                   'Persona responsable' = data.frame(Nombre = IDUsuario()[1],
    #                                                      Correo = IDUsuario()[2]),
    #                   'Fecha de preparación' = fecha(),
    #                   'PropagateCompleto' = DisConc()))
    #     } else {
    #       return('Los datos ingresados no son validos!')
    #     }
    #   } else {
    #     dataFile <- readRDS(input$DisFile$datapath)
    #     if (dataFile['Especie '] != reagKey) {
    #       return(rbind('ERROR!!! ERROR!!! ERROR!!!', 
    #                    'Por favor ingrese una disolución de la especie apropiada' ))
    #     } else {
    #       return(dataFile)
    #     }
    #     
    #   }})
    #paste0(signif(DisConc()$prop[1], 5), signif(DisConc()$prop[3], 3), collapse = ' \u00b1 '))})
    
    
    # InfoMrcBox <- reactive({
    #   box(title = div(style = 'font-size:14px', 
    #                   ifelse(dateMRC() > fecha(), 'Resumen de información del MRC (vigente):', 'Resumen de información del MRC (VENCIDO):')), 
    #       width = 12, collapsible = TRUE, collapsed = TRUE,
    #       status = ifelse(dateMRC() > fecha(), 'success', 'danger'),
    #       div(style = 'font-size:12px',
    #           tags$b('Fecha de vencimiento:'), dateMRC(), tags$br(),
    #           tags$b('Fracción masica de ', reagKey, ':'), MassFrMRC()[1], '\u00B1', MassFrMRC()[2], tags$br(),
    #           tags$b('Masa molar de ', reagKey, ':'), MolWeiMRC()[1], '\u00B1', MolWeiMRC()[2], 'g mol', tags$sup('-1'),tags$br(),
    #           tags$b('Densidad estimada del MRC:'), DensitMRC()[1], '\u00B1', DensitMRC()[2], 'g cm', tags$sup('-3')))
    # })
    # 
    # InfoDisBox <- eventReactive(input$buttonCalc, {
    #   trigger <- TRUE
    #   #printedStuff <- ifelse()
    #   box(title = div(style = 'font-size:14px', 'Información de la disolución:'),
    #       width = 12, collapsible = TRUE, collapsed = FALSE,
    #       status = 'primary',#ifelse(trigger, 'success', 'danger'),
    #       renderPrint(tryCatch(infoDisMRC(),
    #                            error = function(cond) {'Los datos ingresados no son validos!'})),
    #       if(input$SourceOption == "daCapo") {downloadButton(session$ns('DwnlDisFile'), 'Descargar archivo .dis')})
    # })
    # 
    # output$MRC_CertiFile <- renderUI(fileDwnHTML())
    # output$InfoMrcBox <- renderUI(InfoMrcBox())
    # output$InfoDisBox <- renderUI(InfoDisBox())
    # output$CalibCertDis <- renderUI(CalibCertDis())
    # output$DwnlDisFile <- downloadHandler(
    #   filename = function() {paste0("Disolucion_MRC_", reagKey, "_", paste0(fecha(), format(Sys.time(), '_%H-%M')), ".dis")}, 
    #   content = function(file) {saveRDS(infoDisMRC(), file = file)}, contentType = NULL)
    
    # Messages
    deriMasaMRC <- reactive(div(style = 'font-size:11px', 'La deriva en la medición de masa es ', round(derMassMRC() * 1000, 2), ' [mg]'))
    deriMasaDisMRC <- reactive(div(style = 'font-size:11px', 'La deriva en la medición de masa es ', round(derMassDis() * 1000, 2), ' [mg]'))
    
    output$deriMasaMRC <- renderUI(deriMasaMRC())
    output$deriMasaDisMRC <- renderUI(deriMasaDisMRC())
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML()
      AdminList <- list('mr:solutionType' = 'Reference', 'mr:timeISO8601' = iso8601(fecha(), niceHTML = FALSE))
      PropeList <- list('mr:substance' = Substances[reagForm])

      addDataToMRXML(xmlObject, AdminList, node = 'mr:coreData')
      addDataToMRXML(xmlObject, PropeList, node = 'mr:property')
      xml_child(xmlObject, search = 'mr:coreData') %>% xml_add_child(., .value = analyst())
      xml_child(xmlObject, search = 'mr:property') %>% xml_add_child(., .value = DisConc())
      return(xmlObject)
    })
    
    
    
    observeEvent(input$buttonCalc, {
      withCallingHandlers({
        shinyjs::html("InfoDisXML", "")
        message(DisolucionXML())},
        message = function(m) {
          shinyjs::html(id = "InfoDisXML",
                        html = paste0('<textarea rows = 40 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
    })
    
    
    
    # return(list('infoDisMRC' = infoDisMRC))
  })
}
                           