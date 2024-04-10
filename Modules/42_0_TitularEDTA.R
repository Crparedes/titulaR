TitularEDTAUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, Nlns(4), uiOutput(ns('brwz')), tags$h4(style = 'margin-left: 60px;', tags$b('Titulación de disoluciones calibrantes monoelementales'))),
    column(
      width = 3, style = 'margin-left: 80px;',
      balanzasPickerUI(ns('TitMonoelem')), tags$br(), AnalystPickerUI(ns('Analyst')), tags$br(),
      solutionPickerUI(ns('StanDisol')), tags$br(),
      # actionLink(ns('verDisolEstand'), ' Ver informacion de la disolución estándar'), Nlns(),
      solutionPickerUI(ns('SampDisol')),  tags$hr(),
      # actionLink(ns('verDisolSample'), ' Ver informacion de la disolución muestra'), tags$hr(),
      disabled(actionButton(ns('NewTit'), label = tags$b('Nueva titulación'), style = 'margin-left:40px;'))),
    column(width = 8,
           conditionalPanel(
             'input.NewTit > 0', ns = ns, tabBox(
               title = NULL, id = ns('Titrations'), width = 12, side = 'left',
               tabPanel(
                 title = tags$b('Disoluciones'), 
                 fluidRow(column(width = 1, SI_unit_nice('mole', width = "100%"), SI_unit_nice('kilogram', width = "100%")),
                          column(width = 10, tags$h5(tags$b('Disolución estándar de ion plomo')), ShowSolutionUI(id = ns('Estandar')),
                                 '.', tags$hr(),
                                 tags$h5(tags$b('Disolución de la muestra de la sal de EDTA')), ShowSolutionUI(id = ns('Muestra'))))
    ))))
  )
}

TitularEDTAServer <- function(id, devMode, demo, balanzas, solutions, fecha, EstandarLead, MuestraEDTA, PartialTitrationResults) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
    observeEvent(input$brwz, browser())
    
    balanzaUsed <- balanzasPickerServer(id = 'TitMonoelem', devMode = devMode, demo = demo, balanzas = balanzas, inline = FALSE, width = '300px')
    Analyst <- AnalystPickerServer('Analyst', devMode = devMode, demo = demo, showData = FALSE, inline = FALSE, width = '300px')
    
    StanDisol <- solutionPickerServer('StanDisol', devMode = devMode, demo = demo, solutions = EstandarLead,
                                      label = 'Disolución estándar de ion plomo')
    SampDisol <- solutionPickerServer('SampDisol', devMode = devMode, demo = demo, solutions = MuestraEDTA,
                                      label = 'Disolución de la muestra de la sal de EDTA')
    
    observeEvent(req(balanzaUsed(), Analyst(), StanDisol(), SampDisol()), {
      if (is.error(c(balanzaUsed(), Analyst(), StanDisol(), SampDisol()))) disable('NewTit') else enable('NewTit')
    })
    
    isolate(ShowSolutionServer(id = 'Estandar', devMode = devMode, demo = demo, solution = StanDisol, type = 'SolidMRC'))
    isolate(ShowSolutionServer(id = 'Muestra', devMode = devMode, demo = demo, solution = SampDisol, type = 'CalibSample'))
    
    IndivTitrResult <- reactiveValues()
    observeEvent(input$NewTit, {
      req(input$NewTit > 0)
      if (!is.error(c(balanzaUsed(), Analyst(), StanDisol(), SampDisol()))) {
        tabName <- isolate(paste0('Titulación_', input$NewTit))
        element <- xml_text(xml_find_all(SampDisol(), xpath = '//mr:substance//mr:name'))
        PartialTitrationResults$results <- append(
          PartialTitrationResults$results, 
          list(isolate(TitIndivMonoElemServer(
            id = tabName, devMode = devMode, demo = demo, analyst = Analyst, balanza = balanzaUsed, fecha = fecha,
            StanDisol = StanDisol, SampDisol = SampDisol))))
        
        appendTab(
          inputId = 'Titrations', select = TRUE, 
          tab = TitIndivMonoElemUI(
            id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()),
            explan = tagList('disolución monoelemental: ', elemEspa[[element]])))
      }
    })
    
    
  })
    
}