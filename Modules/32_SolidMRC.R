SolidMRCUI <- function(id, title, reagent, reagKey, explan, nu = FALSE) {
  ns <- NS(id)
  tabPanel(
    title = tags$b(title), uiOutput(ns('brwz')),
    tags$b(paste0('Nueva disolucion de ', reagKey)), tags$br(), 
    paste0('Estandar para titular muestras de ', explan), 
    tags$br(), tags$br(),
    tags$div(
      style = 'font-size:12px; margin-left:25px', 
      pickerInput(
        ns("MRCtoUse"), width = "fit", selected = NULL, multiple = TRUE, inline = TRUE,
        label = h5(tags$b(ReqField("MRC de partida"))), choices = as.list(namesMR_MRCs$forEDTA),
        options = list(`max-options` = 1, `none-selected-text` = "(Lista de MRCs en el módulo de materiales de referencia)")),
      tags$hr(),
      splitLayout(
        cellWidths = c("38%", "10%", "38%"),
        tags$div(
          id = "inline", style = 'margin-left:25px', 
          h5(tags$b('Masa del solido')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRec1'), label = 'Masa del recipiente [g]: .', value = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5, 
                           ns('MasMRC1'), label = 'Masa del MRC [g]: .', value = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 5,
                           ns('MasRecMRC1'), label = 'Masa conjunta [g]: .', value = 0),
          uiOutput(ns('deriMasaMRC'))),
        tags$div(),
        tags$div(
          id = "inline",
          h5(tags$b('Masa final de la disolución')),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRec2'), label = 'Masa del recipiente [g]: .', value = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4, 
                           ns('MasDis1'), label = 'Masa final disolución [g]: .', value = 0),
          autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, decimalPlaces = 4,
                           ns('MasRecDis1'), label = 'Masa conjunta [g]: .', value = 0),
          uiOutput(ns('deriMasaDisMRC')))),
      tags$hr(),
      SiRealInputUI(ns('DensiDisol'), name = 'Densidad de la disolución', 
                    x0 = ifelse(reagKey == 'EDTA', 1.000, ifelse(reagKey == 'Pb', 1.007, 0)), 
                    u0 = ifelse(reagKey == 'EDTA', 0.004, ifelse(reagKey == 'Pb', 0.006, 0)), units = DensityUnits,
                    decimalPlaces = 3),
      tags$hr(), actionButton(ns('buttonCalc'), label = 'Crear disolución'), Nlns(3)),
    fluidRow(
      column(width = 2, img(src = "D-SI.png", width = "95%")),
      column(width = 10, tags$br(), uiOutput(ns("downlXMLlink")), htmlOutput(ns('InfoDisXML'))))
  )
}

SolidMRCServer <- function(id, devMode, reagKey, IDUsuario, fecha) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())
    # browser()
    
    DensiDisol <- SiRealInputServer('DensiDisol', devMode = devMode)
    
    fileDwnHTML <- reactive(a(href = paste0('CertMRC/', reagKey, '/', input$MRCElected, '.pdf'),
                              "Descargar certificado ", download = NA, target = "_blank"))
    dateMRC <- reactive(MRC.ExpiricyDates[[reagKey]][[input$MRCElected]])
    MassFrMRC <- reactive(MRCs.MassFraction[[reagKey]][[input$MRCElected]])
    MolWeiMRC <- reactive(MRC.At_MolWeigths[[reagKey]][[input$MRCElected]])
    DensitMRC <- reactive(MRC.densities[[reagKey]][[input$MRCElected]])
    
    
    
    
    derMassMRC <- reactive(input$MasRecMRC1 - input$MasMRC1 - input$MasRec1)
    masMRC <- reactive(mean(input$MasMRC1, input$MasRecMRC1 - input$MasRec1))
    derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
    masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
    
    
    CalibCertDis <- reactive(tags$div(id = "inline", style = 'font-size:12px', 
                                      pickerInput(session$ns("CalibCertDis"), 
                                                  label = 'Balanza utilizada: .',
                                                  choices = CalibCertShow, selected = input$CalibCertMRC, width = '100%', multiple = FALSE)))
    
    convMassMRC <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g'),
                              uncertConvMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g')))
    BuoyMRC <- reactive(c(MABC(rho = DensitMRC()[1], rho_air = DensitAir()[1]),
                          uncertMABC(rho = DensitMRC()[1], rho_air = DensitAir()[1], 
                                     u_rho = DensitMRC()[2], u_rho_air = DensitAir()[2], printRelSD = FALSE)))
    
    convMassDis <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g'),
                              uncertConvMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g')))
    BuoyDis <- reactive(c(MABC(rho = input$DensitDis, rho_air = DensitAir()[1]),
                          uncertMABC(rho = input$DensitDis, rho_air = DensitAir()[1], 
                                     u_rho = input$u_DensitDis, u_rho_air = DensitAir()[2], printRelSD = FALSE)))
    
    DisConc <- reactive({
      #xx <- 
      propagate(expr = expression(convMassMRC * MassFrMRC * BuoyMRC / (MolWeiMRC * convMassDis * BuoyDis) * 1000000),
                data = cbind(convMassMRC = convMassMRC(), MassFrMRC = MassFrMRC(), BuoyMRC = BuoyMRC(), 
                             MolWeiMRC = MolWeiMRC(), convMassDis = convMassDis(), BuoyDis = BuoyDis()),
                do.sim = FALSE)
      #return(xx$prop[c(1, 3)])
    })
    
    
    infoDisMRC <- eventReactive(input$buttonCalc, {
      if (input$SourceOption == "daCapo") {
        if (!is.na(DisConc()$prop[[1]] > 0) && !is.na(DisConc()$prop[[3]] > 0)) {
          return(list('MRC empleado' = input$MRCElected,
                      'Fecha de vencimiento MRC' = dateMRC(),
                      'Especie ' = reagKey,
                      'Concentración [mmol/kg]' = signif(DisConc()$prop[[1]], 7),
                      'Incertidumbre [mmol/kg]' = signif(DisConc()$prop[[3]], 4),
                      'Persona responsable' = data.frame(Nombre = IDUsuario()[1],
                                                         Correo = IDUsuario()[2]),
                      'Fecha de preparación' = fecha(),
                      'PropagateCompleto' = DisConc()))
        } else {
          return('Los datos ingresados no son validos!')
        }
      } else {
        dataFile <- readRDS(input$DisFile$datapath)
        if (dataFile['Especie '] != reagKey) {
          return(rbind('ERROR!!! ERROR!!! ERROR!!!', 
                       'Por favor ingrese una disolución de la especie apropiada' ))
        } else {
          return(dataFile)
        }
        
      }})
    #paste0(signif(DisConc()$prop[1], 5), signif(DisConc()$prop[3], 3), collapse = ' \u00b1 '))})
    
    
    InfoMrcBox <- reactive({
      box(title = div(style = 'font-size:14px', 
                      ifelse(dateMRC() > fecha(), 'Resumen de información del MRC (vigente):', 'Resumen de información del MRC (VENCIDO):')), 
          width = 12, collapsible = TRUE, collapsed = TRUE,
          status = ifelse(dateMRC() > fecha(), 'success', 'danger'),
          div(style = 'font-size:12px',
              tags$b('Fecha de vencimiento:'), dateMRC(), tags$br(),
              tags$b('Fracción masica de ', reagKey, ':'), MassFrMRC()[1], '\u00B1', MassFrMRC()[2], tags$br(),
              tags$b('Masa molar de ', reagKey, ':'), MolWeiMRC()[1], '\u00B1', MolWeiMRC()[2], 'g mol', tags$sup('-1'),tags$br(),
              tags$b('Densidad estimada del MRC:'), DensitMRC()[1], '\u00B1', DensitMRC()[2], 'g cm', tags$sup('-3')))
    })
    
    InfoDisBox <- eventReactive(input$buttonCalc, {
      trigger <- TRUE
      #printedStuff <- ifelse()
      box(title = div(style = 'font-size:14px', 'Información de la disolución:'),
          width = 12, collapsible = TRUE, collapsed = FALSE,
          status = 'primary',#ifelse(trigger, 'success', 'danger'),
          renderPrint(tryCatch(infoDisMRC(),
                               error = function(cond) {'Los datos ingresados no son validos!'})),
          if(input$SourceOption == "daCapo") {downloadButton(session$ns('DwnlDisFile'), 'Descargar archivo .dis')})
    })
    
    output$MRC_CertiFile <- renderUI(fileDwnHTML())
    output$InfoMrcBox <- renderUI(InfoMrcBox())
    output$InfoDisBox <- renderUI(InfoDisBox())
    output$CalibCertDis <- renderUI(CalibCertDis())
    output$DwnlDisFile <- downloadHandler(
      filename = function() {paste0("Disolucion_MRC_", reagKey, "_", paste0(fecha(), format(Sys.time(), '_%H-%M')), ".dis")}, 
      content = function(file) {saveRDS(infoDisMRC(), file = file)}, contentType = NULL)
    
    # Messages
    deriMasaMRC <- eventReactive(input$MasRecMRC1, 
                                 div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassMRC() * 1000, 2), ' [mg]'))
    deriMasaDisMRC <- eventReactive(input$MasRecDis1,
                                    div(style = 'font-size:11px', 'La deriva en la medición de masa es ', signif(derMassDis() * 1000, 2), ' [mg]'))
    
    output$deriMasaMRC <- renderUI(deriMasaMRC())
    output$deriMasaDisMRC <- renderUI(deriMasaDisMRC())
    
    DisolucionXML <- eventReactive(input$buttonCalc, {
      xmlObject <- initiateSolutionXML(id)
      AdminList <- list('solution:type' = 'Reference')
      PropeList <- list(
        'solution:substance' = list(
          'solution:name' = 'EDTA disodium salt dihydrate',
          'solution:InChI' = c('1S/C10H16N2O8.2Na.2H2O/c13-7(14)3-11(4-8(15)16)1-2-12(5-9(17)18)6-10(19)20;;;;/h1-6H2,(H,13,14)(H,15,16)(H,17,18)(H,19,20);;;2*1H2', version = '1.0.6'),
          'solution:InChiKey' = c('FXKZPKBFTQUJBA-UHFFFAOYSA-N', version = '1.0.6')))
      addDataToMRXML(xmlObject, AdminList, node = 'solution:administrativeData')
      addDataToMRXML(xmlObject, PropeList, node = 'solution:propertyValues')
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
    
    
    
    return(list('infoDisMRC' = infoDisMRC))
  })
}
                           