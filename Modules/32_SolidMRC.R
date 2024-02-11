SolidMRCUI <- function(id, demo, title, reagent, reagKey, fecha, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    conditionalPanel(
      'input.buttonCalc == 0', ns = ns,
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
                             ns('MasMRC1'), label = ReqField('Masa del MRC / g:'),
                             value = ifelse(demo, ifelse(reagKey == 'EDTA', 0.30042, 0.20012), 0), align = 'left', minimumValue = 0),
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                             ns('MasRecMRC1'), label = ReqField('Masa conjunta / g:'),
                             value = ifelse(demo, ifelse(reagKey == 'EDTA', 1.27705, 1.17673), 0), align = 'left', minimumValue = 0),
            uiOutput(ns('deriMasaMRC'))),
          tags$div(),
          tags$div(
            id = "inline",
            h5(tags$b('Masa final de la disolución')),
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                             ns('MasRec2'), label = ReqField('Masa del recipiente / g:'), value = ifelse(demo, 16.77169, 0), align = 'left', minimumValue = 0),
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, 
                             ns('MasDis1'), label = ReqField('Masa final disolución / g:'), 
                             value = ifelse(demo, ifelse(reagKey == 'EDTA', 80.02288, 125.0013), 0), align = 'left', minimumValue = 0),
            autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                             ns('MasRecDis1'), label = ReqField('Masa conjunta / g:'), 
                             value = ifelse(demo, ifelse(reagKey == 'EDTA', 96.79455, 141.7712), 0), align = 'left', minimumValue = 0),
            uiOutput(ns('deriMasaDisMRC')))),
        tags$hr(),
        SiRealInputUI(ns('DensiDisol'), name = ReqField('Densidad de la disolución'),
                      x0 = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0)), 
                      u0 = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)), units = DensityUnits,
                      decimalPlaces = 3),
        tags$hr(), disabled(actionButton(ns('buttonCalc'), label = 'Crear disolución')), Nlns(3))
    ),
    fluidRow(
      column(width = 2, SI_unit_nice('mole', width = "95%"), SI_unit_nice('kilogram', width = "95%")),
      column(width = 10,
             conditionalPanel(
               'input.buttonCalc > 0', ns = ns, 
               uiOutput(ns('SummarySolution')), tags$hr(), tags$hr(),
               uiOutput(ns('uncertBudget')), tags$hr(), htmlOutput(ns('InfoDisXML'))))
    )
  )
}

SolidMRCServer <- function(id, devMode, demo, reagKey, reagForm, balanza, analyst, materiales, fecha, ambient, solutionType, InChiKey) {
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
    
    MassFrMRC <- reactive(GetValueEstandUncert(req(xmlObject = SolidMRC()), property = 'MassFraction', InChiKey = InChiKey))
    MolWeiMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), property = 'MolarMass', InChiKey = InChiKey))
    DensitMRC <- reactive(GetValueEstandUncert(req(SolidMRC()), property = 'Density', node = 'mr:additionalValues'))
    
    derMassMRC <- reactive(input$MasRecMRC1 - input$MasMRC1 - input$MasRec1)
    masMRC <- reactive(mean(input$MasMRC1, input$MasRecMRC1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    DisolDensi <- reactive(GetValueEstandUncert(req(DensiDisol())))
    airDensity <- reactive(GetValueEstandUncert(req(ambient()), 'Density'))
    
    
    convMassMRC <- reactive(c(convMass(calibCert = balanza(), reading = masMRC(), units = 'g'),
                              sqrt(uncertConvMass(calibCert = balanza(), reading = masMRC(), units = 'g')^2 +
                                     (derMassMRC()/sqrt(12))^2)))
    BuoyMRC <- reactive(c(MABC(rho = DensitMRC()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DensitMRC()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DensitMRC()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE, plot = FALSE)))
    
    convMassDis <- reactive(c(convMass(calibCert = balanza(), reading = masDis(), units = 'g'),
                              sqrt(uncertConvMass(calibCert = balanza(), reading = masDis(), units = 'g')^2 +
                                     (derMassDis()/sqrt(12))^2)))
    BuoyDis <- reactive(c(MABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1]),
                          uncertMABC(rho = DisolDensi()$ValUnc[1], rho_air = airDensity()$ValUnc[1], 
                                     u_rho = DisolDensi()$ValUnc[2], u_rho_air = airDensity()$ValUnc[2], printRelSD = FALSE)))
    
    DisConcProp <- eventReactive(input$buttonCalc, {
      propagate(
        expr = expression(convMassMRC * BuoyMRC * MassFrMRC / (MolWeiMRC * convMassDis * BuoyDis) * 1000000),
        data = cbind(convMassMRC = convMassMRC(), BuoyMRC = BuoyMRC(), MassFrMRC = MassFrMRC()$ValUnc, 
                     MolWeiMRC = MolWeiMRC()$ValUnc, convMassDis = convMassDis(), BuoyDis = BuoyDis()),
        do.sim = FALSE)
    })
    
    DisConc <- reactive(SiRealXML(
      quantityTypeQUDT = 'AmountOfSubstancePerUnitMass', value = signif(DisConcProp()$prop[[1]], 8),
      units = '\\milli\\mole\\kilo\\gram\\tothe{-1}', uncert =  signif(DisConcProp()$prop[[3]], 3), covFac = 1))
    
    
    SummarySolution <- eventReactive(input$buttonCalc, {
      c_std <- GetValueEstandUncert(DisConc())
      d1 <- decimals(signif(c_std$ValUnc[2], 3))
      return(infoBox(
        width = 12, title = tags$b(style = 'font-size:13px', paste0('Nueva disolución estándar creada')),
        icon = icon("fill-drip"), color = 'black', fill = FALSE,
        subtitle = tags$div(
          tags$table(
            style = "width:100%; font-size:13px; margin-left:20px; vertical-align:top",
            tags$tr(
              tags$th('Concentración:'),
              tags$th(round(c_std$ValUnc[1], d1), '\u00B1', signif(c_std$ValUnc[2], 3), ' mmol/kg (k=1)')
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Especie:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                      Substances[[reagForm]]$`mr:name`, tags$br(),
                      'InChi Key ', InChiKey, tags$br(),
                      tags$a(href = paste0('https://pubchem.ncbi.nlm.nih.gov/#query=', InChiKey), 
                             style = 'color:#0072bd;',
                             tags$html('Ver sustancia en', img(src = "PubChem.png", height = '19px')), target = '_blank'))
            )),
          tags$hr(),
          tags$ul(
            tags$li(downloadLink(session$ns("downlXMLlink"), label = tags$b('Descargar archivo XML de la disolución'))),
            tags$li(actionLink(session$ns("showBudget"), label = ('Mostrar presupuesto de incertidumbre'))),
            tags$li(actionLink(session$ns("showXMLfile"), label = ('Mostrar informacion completa de la disolución')))))))
    })
    output$SummarySolution <- renderUI(SummarySolution())
    
    uncertBudget <- eventReactive(input$showBudget, {
      tagList(
        tags$b('Ecuación del modelo:'), tags$br(),
        '$$ c_{std} = \\frac{m_{MRC} \\cdot B_{MRC} \\cdot x_{specie}}{M_{specie} \\cdot m_{solution} \\cdot B_{solution}}$$',
        Nlns(), tags$b('Presupuesto de incertidumbre:'),
        tags$div(
          style = 'margin-left:15px;margin-right:10px;font-size:12px;', 
          withMathJax(),
          withMathJax(tableOutput(session$ns("tableUncert"))),
          'donde \\(c_{std}\\) es la concentración de la especie en la disolución estándar, \\(m_{MRC}\\) es la masa convencional del MRC,
          \\(B_{MRC}\\) es la corrección por flotabilidad del MRC, \\(x_{specie}\\) es la fracción másica de la especie en el MRC,
          \\(M_{specie}\\) es la masa molar de la especie, \\(m_{solution}\\) es la masa convencional de la disolución, y 
          \\(B_{solution}\\) es la corrección por flotabilidad de la disolución.'),
        tags$hr())
    })
    output$uncertBudget <- renderUI(uncertBudget())
    output$tableUncert <- renderTable({
      c_std <- GetValueEstandUncert(DisConc())
      units <- c(c_std$Units, "\\gram", "\\gram\\gram\\tothe{-1}", MassFrMRC()$Units, MolWeiMRC()$Units, "\\gram", "\\gram\\gram\\tothe{-1}")
      tab <- data.frame(Valor = as.character(signif(c(c_std$ValUnc[1], DisConcProp()$data[1, ]), 9)),
                        u_std = as.character(signif(c(c_std$ValUnc[2], DisConcProp()$data[2, ]), 4)),
                        Unidades = units, Aporte = c(NA, paste((round(diag(DisConcProp()$rel.contr)*100, 3)), '%', sep = ' ')))
      rownames(tab) <- c("\\(c_{std}\\)", "\\(m_{MRC}\\)", "\\(B_{MRC}\\)", "\\(x_{specie}\\)", 
                         "\\(M_{specie}\\)", "\\(m_{solution}\\)", "\\(B_{solution}\\)")
      tab
    },
    include.rownames = TRUE,
    include.colnames = TRUE)
    
    # Messages
    deriMasaMRC <- reactive(div(style = 'font-size:11px', 'Deriva de masa: ', round(derMassMRC() * 1000, 2), ' mg'))
    deriMasaDisMRC <- reactive(div(style = 'font-size:11px', 'Deriva de masa: ', round(derMassDis() * 1000, 2), ' mg'))
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
    
    
    
    observeEvent(input$showXMLfile, {
      withCallingHandlers({
        shinyjs::html("InfoDisXML", "")
        message(DisolucionXML())},
        message = function(m) {
          shinyjs::html(id = "InfoDisXML",
                        html = paste0('<b>Información de la disolución:</b><br>
                                      <textarea rows = 40 style = "width: 95%; margin-left:20px;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLlink <-  downloadHandler(
        filename = function() {paste0(gsub(pattern = ' ', replacement = '_', input$DisolID, fixed = FALSE), ".xml")},
        content = function(file) {write_xml(DisolucionXML(), file)})
    })
    
    return(DisolucionXML)
  })
}
                           