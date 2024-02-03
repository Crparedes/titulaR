TitularMonoelemtUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, Nlns(4), uiOutput(ns('brwz')),
           tags$h4(style = 'margin-left: 60px;', tags$b('Titulación de disoluciones calibrantes monoelementales'))),
    column(
      width = 3, style = 'margin-left: 80px;',
      balanzasPickerUI(ns('TitMonoelem')), tags$br(), AnalystPickerUI(ns('Analyst')), tags$br(),
      solutionPickerUI(ns('StanDisol')), tags$br(), solutionPickerUI(ns('SampDisol')), tags$hr(),
      actionButton(ns('NewTit'), label = tags$b('Nueva titulación'), style = 'margin-left:40px;')),
    conditionalPanel('input.NewTit > 0', ns = ns,
                     column(width = 8, Nlns(), tabBox(title = NULL, id = ns('Titrations'), width = 12, side = 'right')))
    
    # )
  
  #uiOutput(ns('pestana'))
  # tabPanel(title = tags$b(paste0('Tit.', sub("monoElemTit", "", id, fixed = TRUE))), 
    # column(6, tags$br(),
    #        tags$b('Datos de la titulación:'), tags$br(),
    #        tags$div(id = "inline", style = 'font-size:12px', uiOutput(ns('MasaAlic')), uiOutput(ns('MasaEDTA0'))), tags$br(), 
    #        uiOutput(ns('brwz')),
    #        tags$br(),
    #        conditionalPanel(condition = 'input.MasaAlic > 0', ns = ns,
    #                         rHandsontableOutput(ns("TitData"), width = '100%'))),
    # column(6, tags$br(),
    #        tags$b('Curva de titulación:'), tags$br(), 
    #        fluidRow(column(12, align = 'center', plotOutput(ns('TitCurvePlot'), width = '80%'))), tags$br(),
    #        actionButton(ns('TermTit'), label = 'Terminar titulación'), tags$br(),
    #        uiOutput(ns('TitulTerminada'))
    #        )
  )
}

TitularMonoelemtServer <- function(id, devMode, demo, balanzas, solutions, fecha, EstandarEDTA, MuestraCalib) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
      if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
    observeEvent(input$brwz, browser())
    
    balanzaUsed <- balanzasPickerServer(id = 'TitMonoelem', devMode = devMode, demo = demo, balanzas = balanzas, inline = FALSE, width = '300px')
    Analyst <- AnalystPickerServer('Analyst', devMode = devMode, demo = demo, showData = FALSE, inline = FALSE, width = '300px')
    
    StanDisol <- solutionPickerServer('StanDisol', devMode = devMode, demo = demo, solutions = EstandarEDTA,
                                      label = 'Disolución estándar de EDTA')
    SampDisol <- solutionPickerServer('SampDisol', devMode = devMode, demo = demo, solutions = MuestraCalib,
                                      label = 'Muestra disolución monoelemental')
    
    observe({
      req(balanzaUsed(), Analyst(), StanDisol(), SampDisol())
      enable('NewTit')
    })
    
    IndivTitrResult <- reactiveValues()
    observeEvent(input$NewTit, {
      req(input$NewTit > 0)
      
      tabName <- isolate(paste0('Titulación_', input$NewTit))
      element <- xml_text(xml_find_all(SampDisol(), xpath = '//mr:substance//mr:name'))
      isolate(TitIndividualServer(id = tabName, devMode = devMode, demo = demo, analyst = Analyst, balanza = balanzasUse, fecha = fecha,
                                  StanDisol, SampDisol))
      appendTab(
        inputId = 'Titrations', select = TRUE, 
        tab = TitIndividualUI(
          id = session$ns(tabName), demo = isolate(demo()), title = tabName, fecha = isolate(fecha()),
          explan = tagList('disolución monoelemental: ', tags$em(element))))
    })
    
    
  })
    
}