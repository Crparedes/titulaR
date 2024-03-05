PreparaDisolucioUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      12, Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: 60px;', tags$b('Disoluciones estandar y disoluciones muestra para las titulaciones')), tags$br()),
    column(
      width = 5,  style = 'margin-left: 80px;',
      tags$b('Disoluciones para la caracterización de calibrantes monoelementales'), Nlns(1),
      spcs(10), actionLink(ns('NewEDTAStdSol'), icon = icon("fill-drip"), 'Nueva disolución estándar de EDTA'), tags$br(),
      spcs(10), actionLink(ns('NewCaliSamSol'), icon = icon("fill-drip"), 'Nueva muestra de disolución monoelemental'), Nlns(),
      tags$b('Disoluciones para la  caracterización de la sal de EDTA'), Nlns(1),
      spcs(10), actionLink(ns('NewLeadStdSol'), icon = icon("fill-drip"), 'Nueva disolución estándar de plomo'), tags$br(),
      spcs(10), actionLink(ns('NewEDTASamSol'), icon = icon("fill-drip"), 'Nueva disolución muestra de EDTA'), Nlns()),
    column(
      width = 4,
      tags$b('Importar archivos de información de disoluciones (.xml)'), Nlns(),
      tags$div(
        style = 'margin-left: 30px;',
        fileInput(ns('NewXML'), label = NULL, buttonLabel = 'Examinar...', multiple = TRUE, accept = '.xml', width = '90%'),
        uiOutput(ns('XmlCargados')))),
    column(12, tags$hr(), tags$hr()),
    column(
      width = 8, style = 'margin-left: 120px;',
      # shinydashboardPlus::
        box(id = ns('condAmbiBox'), status = 'primary', 
            title = tags$b(style = 'font-size: 14px;', 'Condiciones ambientales para la preparación de disoluciones'),
            width = 12, collapsible = TRUE, collapsed = FALSE, AmbiDensAireUI(ns('AmbiDensAireSolutions'))),
      conditionalPanel(
        'input.NewEDTAStdSol > 0 || input.NewCaliSamSol > 0 || input.NewLeadStdSol > 0 || input.NewEDTASamSol > 0', ns = ns,  
        # tags$h4(tags$b('Nuevas disoluciones'), style = 'margin-left: -40px;'),
        tags$div(
          id = "inline", style = 'font-size:12px; margin-left:60px;',
          ), #Nlns(1),
        tabBox(title = NULL, id = ns('NewSolutions'), width = 12, side = 'right',
               tags$div(balanzasPickerUI(ns('SolPrep')), AnalystPickerUI(ns('analyst')), tags$hr()))))
  )
}


PreparaDisolucioServer <- function(id, devMode, demo, balanzas, materiales, fecha, StandardSampleSolutions) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    balanzaUsed <- balanzasPickerServer(id = 'SolPrep', devMode = devMode, demo = demo, balanzas = balanzas)
    analyst <- AnalystPickerServer('analyst', devMode = devMode, demo = demo)
    
    
    AmbiDensAire <- AmbiDensAireServer('AmbiDensAireSolutions', devMode = devMode, fecha = fecha)
    
    # StandardSampleSolutions <- reactiveValues(solutions = list())
    XmlCargados <- reactiveVal()
    observeEvent(input$NewXML, {
      # browser()
      if (!all(input$NewXML$type == 'text/xml') || is.error(lapply(input$NewXML$datapath, function(x) read_xml(x)))) {
        shinyalert(title = 'Error!', text = 'Todos los archivos deben ser formato XML.', type = 'error',
                   timer = 3000, showConfirmButton = FALSE)
      } else {
        uploadedFiles <- lapply((input$NewXML$datapath), function(x) read_xml(x))
        solTypes <- sapply(uploadedFiles, function(x) xml_text(xml_find_all(x, xpath = '//mr:solutionType')))
        if (!all(solTypes %in% c('EstandarEDTA', 'MuestraCalib', 'EstandarPlomo', 'MuestraEDTA'))) {
          shinyalert(title = 'Error!', text = 'Al menos un archivo XML no contiene información de disoluciones.', type = 'error',
                     timer = 3000, showConfirmButton = FALSE)
        } else {
          StandardSampleSolutions$solutions <- append(StandardSampleSolutions$solutions, 
                                                      lapply(uploadedFiles, function(x) {return(reactive(x))}))
          XmlCargados(HTML('Se cargó la información de las siguientes disoluciones:<p align = "left"><ul>',
                           paste0(sapply(uploadedFiles, function(x) {
                             return(paste0('<li><b>', xml_text(xml_find_all(x, xpath = '//mr:solutionType')), ':</b> ',
                                           xml_text(xml_find_all(x, xpath = '//mr:solutionID')), '</li>'))
                           }), collapse = '')))
          # shinyalert(
          #   title = NULL, type = 'success', html = TRUE, timer = 1e4, showConfirmButton = FALSE,
          #   text = paste0(
          #     'Se cargó la información de las siguientes disoluciones:<br><br><p align = "left"><ul>',
          #     paste0(sapply(uploadedFiles, function(x) {
          #       return(paste0('<li><b>', xml_text(xml_find_all(x, xpath = '//mr:solutionType')), ':</b> ',
          #                     xml_text(xml_find_all(x, xpath = '//mr:solutionID')), '</li>'))
          #     }), collapse = ''),
          #     '</ul><br><br>Presione <b>ESC</b> para cerrar el recuadro.</p>'))
        }
      }
    })
    output$XmlCargados <- renderUI(XmlCargados())
    
    observeEvent(input$NewEDTAStdSol, {
      req(input$NewEDTAStdSol > 0)
      solutionType <- 'EstandarEDTA'
      tabName <- isolate(paste0(solutionType, '_', input$NewEDTAStdSol))
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(SolidMRCServer(id = tabName, devMode = devMode, reagKey = 'EDTA', reagForm = 'Na2EDTA.2H2O',
                                    materiales = materiales$forCalibrantes,
                                    demo = demo, analyst = analyst, balanza = balanzaUsed, fecha = fecha, ambient = AmbiDensAire,
                                    solutionType = solutionType, InChiKey = 'FXKZPKBFTQUJBA-UHFFFAOYSA-N'))))
      
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()), reagent = 'EDTA', reagKey = 'EDTA',
          explan = 'calibrantes monoelementales.'))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) js$collapse(session$ns("condAmbiBox")) 
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
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) js$collapse(session$ns("condAmbiBox"))
    })

    observeEvent(input$NewLeadStdSol, {
      req(input$NewLeadStdSol > 0)
      solutionType <- 'EstandarPlomo'
      tabName <- isolate(paste0(solutionType, '_', input$NewLeadStdSol))
      
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, 
        list(isolate(SolidMRCServer(id = tabName, devMode = devMode, reagKey = 'Pb', reagForm = 'PbNO3', materiales = materiales$forEDTA,
                                    demo = demo, analyst = analyst, balanza = balanzaUsed, fecha = fecha, ambient = AmbiDensAire,
                                    solutionType = solutionType, InChiKey = 'WABPQHHGFIMREM-UHFFFAOYSA-N'))))
      appendTab(
        inputId = 'NewSolutions', select = TRUE, 
        tab = SolidMRCUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()), reagent = 'Pb', reagKey = 'Pb',
          explan = 'disoluciones de EDTA.'))
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) js$collapse(session$ns("condAmbiBox")) 
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
      
      if(sum(c(input$NewEDTAStdSol, input$NewCaliSamSol, input$NewLeadStdSol, input$NewEDTASamSol)) == 1) js$collapse(session$ns("condAmbiBox"))
    })
    

    
  })
  # return(reactive(lapply(StandardSampleSolutions$solutions, function(x) return(x()))))
}