PreparaDisolucioUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      12, Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: 60px;',
              tags$b('Disoluciones estandar y disoluciones muestra para las titulaciones'))),
    column(
      width = 6, style = 'margin-left: 80px;', 
      tags$b('Titulacion de calibrantes monoelementales'), tags$br(),
      actionButton(
        ns('NewEDTAStdSol'), width = '35%',
        'Crear disolución estándar de EDTA'),
      actionButton(
        ns('NewCaliSamSol'), width = '35%',
        'Crear disolución muestra del elemento'), tags$br(), tags$br(),
      tags$b('Titulacion de la sal de EDTA'), tags$br(),
      actionButton(
        ns('NewLeadStdSol'), width = '35%',
        'Crear disolución estándar de plomo'),
      actionButton(
        ns('NewEDTASamSol'), width = '35%',
        'Crear disolución muestra de EDTA')),
    column(
      width = 4, style = 'margin-left: 60px;',
      tags$b('Disoluciones preparadas en el pasado'), tags$br(),
      'Cargue los archivos XML con la información de un material de referencia faltante, 
      indique el uso que tiene el material y opima el botón', tags$u('Cargar.'), Nlns(),
      fileInput(ns('NewMrXml'), label = NULL, multiple = FALSE, accept = '.xml', width = '90%'),
      uiOutput(ns('CargarMrXml'))),
    column(11,  style = 'margin-left: 80px;', tags$hr()),
    column(
      width = 6, style = 'margin-left: 80px;', 
      tabBox(
        id = ns('NewSolutions'), width = 12, side = 'right',
        fluidRow(
          column(1, img(src = "D-SI.png", width = "50px")),
          column(
            11,
            fluidRow(
              column(6, 
                tags$div(
                  id = "inline",
                  uiOutput(ns('balanzasPicker')),
                  pickerInput(
                    ns("Analista"), label = ReqField('Analista', 2),
                    choices = names(authPersons), width = '100%',# inline = FALSE,
                    multiple = TRUE, selected = NULL,
                    options = list(
                      `max-options` = 1, `none-selected-text` = "(Personal con autorizaciones)")))),
              column(6, uiOutput(ns('datosAnalista')))),
            conditionalPanel(
              condition = 'input.balanzasUse.length > 0 && input.Analista.length > 0', ns = ns,
              tags$hr(),
              tags$div(
                id = "inline", style = 'font-size:12px; margin-left:20px; margin-right:20px',
                tags$b('Condiciones ambientales', style = 'margin-left:-20px'),
                fluidRow(
                  column( # Valores
                    3, 
                    autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                     ns('Temp'), label = NonReqField('Temperatura:'), value = 18),
                    autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                     ns('BarPres'), label = NonReqField('Presion:'), value = 750),
                    autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                     ns('relHum'), label = NonReqField('Humedad:'), value = 45)),
                  column( # Incertidumbres
                    2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                        ns('u_Temp'), label = '\u00B1', value = 2), 
                    autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                     ns('u_BarPres'), label = '\u00B1', value = 2),
                    autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                     ns('u_relHum'), label = '\u00B1', value = 3)),
                  column( # Unidades
                    2, selectInput(ns('units_Temp'), label = NULL, choices = TemperatureUnits),
                    selectInput(ns('units_BarPres'), label = NULL, choices = AtmosPressuUnits),
                    selectInput(ns('units_BarPres'), label = NULL, choices = RelatiHumidUnits)),
                  column( # Factores cobertura
                    2, selectInput(ns('covFac_Temp'), label = NULL, choices = CobertureFactors),
                    selectInput(ns('covFac_BarPres'), label = NULL, choices = CobertureFactors),
                    selectInput(ns('covFac_BarPres'), label = NULL, choices = CobertureFactors)),
                  column( # Distribucion
                    2, selectInput(ns('Distri_Temp'), label = NULL, choices = Distributions),
                    selectInput(ns('Distri_BarPres'), label = NULL, choices = Distributions),
                    selectInput(ns('Distri_BarPres'), label = NULL, choices = Distributions))
                )
              )
            )
          ),
          tags$hr())
    )),
    column(
      width = 4, style = 'margin-left: 60px;')
      
  )
}


PreparaDisolucioServer <- function(id, devMode, balanzas) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    EDTA_STD_solutions <- reactiveValues()
    observeEvent(input$NewEDTAStdSol, {
      req(input$NewEDTAStdSol > 0)
      tabName <- isolate(paste0('EstandarEDTA.', input$NewEDTAStdSol))
      isolate(SolidMRCServer(id = tabName, devMode = devMode,
                             IDUsuario = 'IDUsuario', fecha = 'fecha'))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = tabName, reagent = 'EDTA', reagKey = 'EDTA',
          explan = 'calibrantes monoelementales.'))
    })
    observeEvent(input$NewCaliSamSol, {
      req(input$NewCaliSamSol > 0)
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = tabPanel(
          title = tags$b(paste0('MuestraCalib.', input$NewCaliSamSol)),
          tags$hr(),
          tags$div(id = "inline", style = 'font-size:12px', tags$hr()))
      )
    })
    
    balanzasPicker <- reactive({
      #if (is.null(input$balanzasElected)) return('(Seleccione o cargue la información de al menos una balanza)')
      pickerInput(
        session$ns("balanzasUse"), label = ReqField('Balanza', 3),
        choices = balanzasList, width = '100%',# inline = FALSE,
        multiple = TRUE, selected = NULL,
        options = list(
          `max-options` = 1,
          `none-selected-text` = "(Módulo balanzas)"))
    })
    output$balanzasPicker <- renderUI(balanzasPicker())
    
    Analista <- reactive({
      req(input$Analista)
      authPersons[[input$Analista]]
    })
    datosAnalista <- eventReactive(input$Analista, ignoreNULL = TRUE, ignoreInit = TRUE, {
      tags$div(
        Nlns(2),
        tags$a(href = gsub('www/', '', list.files(path = 'www/Personal/', pattern = input$Analista, full.names = TRUE)),
               'Descargar XML analista', download = NA, target = "_blank"),
        spcs(3), tags$a(href = Analista()$data$orcid, img(src = "ORCID.png", width = "25", height = "25"), target = "_blank"),
        spcs(3), tags$a(href = Analista()$inst$ror, img(src = "ROR.png", width = "25", height = "25"), target = "_blank")
      )
    })
    output$datosAnalista <- renderUI(datosAnalista())
    
    SelectMRC <- reactive(
      pickerInput(
        session$ns("MRCtoView"), width = "100%",
        label = "Seleccione un material de referencia para visualizar la información",
        choices = list(
          `Para caracterizar EDTA` = as.list(namesMR_MRCs$forEDTA),
          `Para caracterizar disoluciones calibrantes` = as.list(namesMR_MRCs$forCali)),
        selected = NULL, multiple = TRUE,
        options = list(`max-options` = 1, `none-selected-text` = "(Ninguno seleccionado)")
      ))
    output$SelectMRC <- renderUI(SelectMRC())
    
    observeEvent(input$MRCtoView, {
      fileXML <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE,
                            pattern = paste0(input$MRCtoView, '.xml'))
      
      filePDF <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE,
                            pattern = paste0(input$MRCtoView, '.pdf'))
      withCallingHandlers({
        shinyjs::html("printTheMrXML", "")
        message(read_xml(fileXML))},
        message = function(m) {
          shinyjs::html(id = "printTheMrXML",
                        html = paste0('<textarea cols = 75, rows = 40>',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLBttn <- renderUI(tags$div(
        a(href = gsub('www/', '', fileXML), 'Descargar archivo XML', 
          download = NA, target = "_blank"), tags$br(),
        a(href = gsub('www/', '', filePDF), 'Descargar certificado o reporte en PDF', 
          download = NA, target = "_blank")))
    })
    
  })
  return()
}