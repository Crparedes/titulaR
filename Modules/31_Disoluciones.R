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
    column(
      width = 6, style = 'margin-left: 100px;',
      shinydashboardPlus::box(status = 'black', title = tags$b(style = 'font-size: 13px;', 'Condiciones ambientales'), id = ns('condAmbiBox'),
                              width = 12, collapsible = TRUE, collapsed = FALSE, AmbiDensAireUI(ns('AmbiDensAireSolutions'))),
      conditionalPanel(
        'input.NewEDTAStdSol > 0 || input.NewCaliSamSol > 0 || input.NewLeadStdSol > 0 || input.NewEDTASamSol > 0', ns = ns,  
        # tags$h4(tags$b('Nuevas disoluciones'), style = 'margin-left: -40px;'),
        tags$div(
          id = "inline", style = 'font-size:12px; margin-left:60px;',
          ),
        balanzasPickerUI(ns('SolPrep')), AnalystPickerUI(ns('analyst')), Nlns(1),
        tabBox(title = NULL, id = ns('NewSolutions'), width = 12, side = 'right')))
  )
}


PreparaDisolucioServer <- function(id, devMode, demo, balanzas, materiales, fecha) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    balanzaUsed <- balanzasPickerServer(id = 'SolPrep', devMode = devMode, demo = demo, balanzas = balanzas)
    analyst <- AnalystPickerServer('analyst', devMode = devMode, demo = demo)
    
    
    AmbiDensAire <- AmbiDensAireServer('AmbiDensAireSolutions', devMode = devMode, fecha = fecha)
    
    StandardSampleSolutions <- reactiveValues(solutions = list())
    
    observeEvent(input$NewEDTAStdSol, {
      req(input$NewEDTAStdSol > 0)
      solutionType <- 'EstandarEDTA'
      tabName <- isolate(paste0(solutionType, '_', input$NewEDTAStdSol))
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(SolidMRCServer(id = tabName, devMode = devMode, reagKey = 'EDTA', reagForm = 'Na2EDTA.2H2O', materiales = materiales$forCalibrantes,
                                    demo = demo, analyst = analyst, balanza = balanzaUsed, fecha = fecha, ambient = AmbiDensAire,
                                    solutionType = solutionType))))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()), reagent = 'EDTA', reagKey = 'EDTA',
          explan = 'calibrantes monoelementales.'))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) {
        updateBox('condAmbiBox', action = 'toggle')
      } 
    })
    ### Disoluci'on calibrante monoelemental
    observeEvent(input$NewCaliSamSol, {
      req(input$NewCaliSamSol > 0)
      solutionType <- 'MuestraCalib'
      tabName <- isolate(paste0(solutionType, '_', input$NewCaliSamSol))
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(CalibSampleServer(id = tabName, devMode = devMode, demo = demo, analyst = analyst, balanza = balanzaUsed,
                                       fecha = fecha, ambient = AmbiDensAire, solutionType = solutionType))))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = CalibSampleUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha())#,
          # explan = 'Para asignar valor de fracción másica en el reactivo sólido.'
      ))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) {
        updateBox('condAmbiBox', action = 'toggle')
      } 
    })

    observeEvent(input$NewLeadStdSol, {
      req(input$NewLeadStdSol > 0)
      solutionType <- 'EstandarPlomo'
      tabName <- isolate(paste0(solutionType, '_', input$NewEDTAStdSol))
      
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(SolidMRCServer(id = tabName, devMode = devMode, reagKey = 'Pb', reagForm = 'PbNO3', materiales = materiales$forEDTA,
                                    demo = demo, analyst = analyst, balanza = balanzaUsed, fecha = fecha, ambient = AmbiDensAire,
                                    solutionType = solutionType))))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()), reagent = 'Pb', reagKey = 'Pb',
          explan = 'disoluciones de EDTA.'))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) {
        updateBox('condAmbiBox', action = 'toggle')
      } 
    })
    
    observeEvent(input$NewEDTASamSol, {
      req(input$NewEDTASamSol > 0)
      solutionType <- 'MuestraEDTA'
      tabName <- isolate(paste0(solutionType, '_', input$NewEDTASamSol))
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(SolidSampleServer(id = tabName, devMode = devMode, reagKey = 'EDTA', reagForm = 'Na2EDTA.2H2O',
                                       demo = demo, analyst = analyst, balanza = balanzaUsed, fecha = fecha, ambient = AmbiDensAire,
                                       solutionType = solutionType))))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidSampleUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()), reagent = 'EDTA', reagKey = 'EDTA',
          explan = 'Para asignar valor de fracción másica en el reactivo sólido.'))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) {
        updateBox('condAmbiBox', action = 'toggle')
      } 
    })
    

    
  })
  return()
}