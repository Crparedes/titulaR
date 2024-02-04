BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4, style = 'margin-left: 80px;', Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: -20px;', tags$b('Informacion de calibracion de balanzas')),
       tags$br(),
      fluidRow(
        column(3, SI_unit_nice('kilogram', width = "110%")),
        column(9, uiOutput(ns('balanzasElected')))),
      tags$hr(),
      tags$b('Cargue los archivos con los certificados que le hagan falta.'), Nlns(),
      tags$div(
        style = 'margin-left: 40px;',
        'El botón', tags$b('Cargar'), 'se habilita solo si sube archivos compatibles.', tags$br(),
        'Por el momento, titulaR solo puede procesar archivos RDS creados con el',
        tags$a(href = 'https://crparedes.shinyapps.io/masscor/', 'aplicativo del paquete masscor.', target = '_blank'), Nlns(),
        fileInput(ns('NewDCC1'), label = NULL, buttonLabel = 'Examinar...', multiple = TRUE, accept = '.rds', width = '100%'),  
        '(Verifique de los nuevos certificados aparecen en el recuadro de visualización)')
    ),
    
    column(
      6,  style = 'margin-left: 80px;', Nlns(7), 
      box(
        title = tags$b('Visualizador'), width = 12, status = 'primary', collapsible = FALSE,
        uiOutput(ns('balanzasPicker')), tags$br(),
        splitLayout(
          materialSwitch(ns('ShallprintThebalanzas'), label = 'Ver información del certificado', right = TRUE,
                         value = FALSE, status = "primary"),
          materialSwitch(ns('ShallplotThebalanzas'), label = 'Gráfico de error de indicación', right = TRUE,
                         value = TRUE, status = "primary")),
        tags$hr(),
        plotOutput(ns('plotThebalanzas'), width = '95%'),
        verbatimTextOutput(ns('printThebalanzas'))
      )
    )
  )
}

BalanceCalibCertServer <- function(id, devMode, demo, BalanzasReVa) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    balanzasElected <- reactive(checkboxGroupInput(
      session$ns("balanzasElected"), width = '100%', selected = ifelse(demo(), 'Mettler Toledo XPE 205 (2023-07-18)', ''),
      label = tags$b('Seleccione los certificados de calibración de las balanzas que necesita:'),
      choices = balanzasShow))
    output$balanzasElected <- renderUI(balanzasElected())
    
    balanzasPicker <- reactive({
      if (is.null(input$balanzasElected)) return('(Seleccione o cargue la información de al menos una balanza)')
      pickerInput(
        session$ns("balanzasShowData"), label = 'Seleccione una balanza',
        choices = input$balanzasElected, width = '100%',# inline = FALSE,
        multiple = TRUE, selected = ifelse(demo(), 'Mettler Toledo XPE 205 (2023-07-18)', ''),
        options = list(
          `max-options` = 1,
          `none-selected-text` = "(Revise la vigencia del certificado de calibración - fecha)"))
    })
    output$balanzasPicker <- renderUI(balanzasPicker()) 
    
    plotThebalanzas <- reactive({
      if (input$ShallplotThebalanzas && !is.null(input$balanzasShowData)) {
        plot(balanzasList[[input$balanzasShowData]])
      } 
    })
    output$plotThebalanzas  <- renderPlot(plotThebalanzas())
    
    output$printThebalanzas <- renderPrint({
      if (input$ShallprintThebalanzas && !is.null(input$balanzasShowData)) {
        cat('Certificado de calibración digital balanza', input$balanzasShowData, '\n\n')
        print(balanzasList[[input$balanzasShowData]])#, complete = complete())
      }
    })
    
    observeEvent(input$balanzasElected, {BalanzasReVa$DCC <- lapply(input$balanzasElected, function(x) balanzasList[[x]])})
    
    # return(reactive(lapply(input$balanzasElected, function(x) balanzasList[[x]])))
  })
}