PreparaDisolucioUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      12, Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: 60px;', tags$b('Disoluciones estandar y disoluciones muestra para las titulaciones'))),
    column(
      width = 4, style = 'margin-left: 80px;',
      tags$b('Titulacion de calibrantes monoelementales'), tags$br(),
      spcs(10), actionLink(ns('NewEDTAStdSol'), icon = icon("fill-drip"), 'Crear disolución estándar de EDTA'), tags$br(),
      spcs(10), actionLink(ns('NewCaliSamSol'), icon = icon("fill-drip"), 'Crear muestra de disolución monoelemental'), tags$br(), tags$br(),
      tags$b('Titulacion de la sal de EDTA'), tags$br(),
      spcs(10), actionLink(ns('NewLeadStdSol'), icon = icon("fill-drip"), 'Crear disolución estándar de plomo'), tags$br(),
      spcs(10), actionLink(ns('NewEDTASamSol'), icon = icon("fill-drip"), 'Crear disolución muestra de EDTA'),
      tags$hr(), Nlns(),
      tags$b('Disoluciones preparadas en el pasado'),
      tags$div(
        style = 'margin-left: 60px;',
        'Cargue los archivos XML con la información de un material de referencia faltante, 
        indique el uso que tiene el material y opima el botón', tags$u('Cargar.'), Nlns(),
        fileInput(ns('NewMrXml'), label = NULL, multiple = FALSE, accept = '.xml', width = '90%'),
        uiOutput(ns('CargarMrXml')))
      ),
    conditionalPanel(
      'input.NewEDTAStdSol > 0 || input.NewCaliSamSol > 0 || input.NewLeadStdSol > 0 || input.NewEDTASamSol > 0', ns = ns,
      column(
        width = 6, style = 'margin-left: 100px;', 
        tags$h5(tags$b('Nuevas disoluciones'), style = 'margin-left: -40px;'),
        tags$div(
          id = "inline", style = 'font-size:12px; margin-left:60px;',
          shinydashboardPlus::box(
            status = 'black', 
            title = tags$b(style = 'font-size: 13px;', 'Condiciones ambientales'), id = ns('condAmbiBox'),
            width = 12, collapsible = TRUE, collapsed = TRUE, AmbiDensAireUI(ns('AmbiDensAireSolutions')))),
        balanzasPickerUI(ns('SolPrep')), AnalystPickerUI(ns('Analyst')),
        Nlns(2),
        tabBox(title = NULL, id = ns('NewSolutions'), width = 12, side = 'right')))
  )
}


PreparaDisolucioServer <- function(id, devMode, balanzas, materiales) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    balanzasUse <- balanzasPickerServer('SolPrep', devMode, balanzas)
    Analyst <- AnalystPickerServer('Analyst')
    
    
    AmbiDensAire <- AmbiDensAireServer('AmbiDensAireSolutions', devMode = devMode)
    
    EDTA_STD_solutions <- reactiveValues()
    observeEvent(input$NewEDTAStdSol, {
      req(input$NewEDTAStdSol > 0)
      if(!input$condAmbiBox$collapsed) updateBox('condAmbiBox', 'toggle')
      tabName <- isolate(paste0('EstandarEDTA_', input$NewEDTAStdSol))
      isolate(SolidMRCServer(id = tabName, devMode = devMode, IDUsuario = 'IDUsuario', fecha = 'fecha'))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = session$ns(tabName), title = tabName, reagent = 'EDTA', reagKey = 'EDTA',
          explan = 'calibrantes monoelementales.'))
    })

    

    
  })
  return()
}