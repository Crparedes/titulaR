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
            uiOutput(ns('balanzasPicker')),
            fluidRow(
              column(5, tags$div(id = "inline", pickerInput(
                ns("Analista"), label = ReqField('Analista', 2), inline = TRUE, width = 'fit',
                choices = names(authPersons), multiple = TRUE, selected = NULL,
                options = list(`max-options` = 1, `none-selected-text` = "(Personal con autorizaciones)")))),
              column(7, uiOutput(ns('datosAnalista')))),
            conditionalPanel(
              condition = 'input.Analista.length > 0', ns = ns,
              tags$div(
                id = "inline", style = 'font-size:12px; margin-left:20px;',
                box(
                  title = tags$b(style = 'font-size: 13px;', 'Condiciones ambientales'), id = 'condAmbiBox',
                  width = 12, collapsible = TRUE, collapsed = FALSE,
                  style = '.box-header .box-title {}',
                  tags$b('Temperatura'),
                  fluidRow(
                    style = 'margin-left:10px;',
                    column(3, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('Temp'), label = NULL, value = 18)),
                    column(3, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('u_Temp'), label = '\u00B1', value = 2)),
                    column(3, selectInput(ns('units_Temp'), label = NULL, choices = TemperatureUnits))),
                  fluidRow(
                    column(4, offset = 2, selectInput(ns('covFac_Temp'), label = 'Factor de cobertura', choices = CobertureFactors)),
                    column(3, selectInput(ns('Distri_Temp'), label = 'Distribución', choices = Distributions))),
                  
                  tags$hr(), tags$b('Presión barométrica'),
                  fluidRow(
                    style = 'margin-left:10px;',
                    column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('BarPres'), label = NULL, value = 750)),
                    column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('u_BarPres'), label = '\u00B1', value = 2)),
                    column(2, selectInput(ns('units_BarPres'), label = NULL, choices = AtmosPressuUnits))),
                  fluidRow(
                    column(4, offset = 2, selectInput(ns('covFac_BarPres'), label = 'Factor de cobertura', choices = CobertureFactors)),
                    column(3, selectInput(ns('Distri_BarPres'), label = 'Distribución', choices = Distributions))),
                  
                  tags$hr(), tags$b('Humedad relativa'),
                  fluidRow(
                    style = 'margin-left:10px;',
                    column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('relHum'), label = NULL, value = 45)),
                    column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                               ns('u_relHum'), label = '\u00B1', value = 3)),
                    column(2, selectInput(ns('units_relHum'), label = NULL, choices = RelatiHumidUnits))),
                  fluidRow(
                    column(4, offset = 2, selectInput(ns('covFac_relHum'), label = 'Factor de cobertura', choices = CobertureFactors)),
                    column(3, selectInput(ns('Distri_relHum'), label = 'Distribución', choices = Distributions)))
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


PreparaDisolucioServer <- function(id, devMode, dateTime, balanzas, materiales) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    
    balanzasPicker <- reactive({
      balanceChioces <- sapply(balanzas(), function (x) x$balanceID)
      if (length(balanceChioces) == 0) {
        return(tags$div(
          style = 'color:red;', 'Vaya al módulo de', tags$b('Balanzas,'),
          'y seleccione o cargue la información de al menos una balanza)', tags$hr()))}
      pickerInput(
        session$ns("balanzasUse"), label = ReqField('Balanza', 3), inline = TRUE, width = 'fit',
        choices = sapply(balanzas(), function (x) x$balanceID),
        multiple = TRUE, selected = NULL,
        options = list(`max-options` = 1, `none-selected-text` = "(Módulo balanzas)"))
    })
    output$balanzasPicker <- renderUI(balanzasPicker())
    
    Analista <- reactive({
      req(input$Analista)
      authPersons[[input$Analista]]
    })
    datosAnalista <- eventReactive(input$Analista, ignoreNULL = TRUE, ignoreInit = TRUE, {
      tags$div(
        tags$a(href = gsub('www/', '', list.files(path = 'www/Personal/', pattern = input$Analista, full.names = TRUE)),
               'Descargar XML analista', download = NA, target = "_blank"),
        spcs(3), tags$a(href = Analista()$data$orcid, img(src = "ORCID.png", width = "25", height = "25"), target = "_blank"),
        spcs(3), tags$a(href = Analista()$inst$ror, img(src = "ROR.png", width = "25", height = "25"), target = "_blank")
      )
    })
    output$datosAnalista <- renderUI(datosAnalista())
    
    
    EDTA_STD_solutions <- reactiveValues()
    observeEvent(input$NewEDTAStdSol, {
      req(input$NewEDTAStdSol > 0)
      js$collapse("condAmbiBox")
      tabName <- isolate(paste0('EstandarEDTA.', input$NewEDTAStdSol))
      isolate(SolidMRCServer(id = tabName, devMode = devMode, IDUsuario = 'IDUsuario', fecha = 'fecha'))
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
    

    
  })
  return()
}