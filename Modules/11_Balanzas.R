BalanceCalibCertUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4, style = 'margin-left: 80px;', Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: -20px;', tags$b('Informacion de calibracion de balanzas')),
      checkboxGroupInput(
        ns("balanzasElected"), width = '100%',
        label = 'Seleccione los certificados de calibración de las balanzas que necesita:',
        choices = balanzasShow),
      tags$hr(),
      tags$b('Cargue los archivos con los certificados que le hagan falta.'), tags$br(),
      'Estos archivos se crean con el ',
      tags$a(href = 'https://crparedes.shinyapps.io/masscor/', 'aplicativo del paquete masscor.'),
      tags$br(),
      'Suba los archivos individualmente en las siguientes cajas y asegurese de opimir el botón',
      tags$u('Cargar'),
      'y que la información de sus balanzas se puede visualizar correctamente', Nlns(),
      splitLayout(
        fileInput(ns('NewDCC1'), label = NULL, multiple = FALSE, accept = '.rds', width = '90%'),
        uiOutput(ns('CargarDCC1'))),
      splitLayout(
        fileInput(ns('NewDCC2'), label = NULL, multiple = FALSE, accept = '.rds', width = '90%'),
        uiOutput(ns('CargarDCC2')))
    ),
    
    column(
      6,  style = 'margin-left: 80px;', Nlns(7), 
      box(
        title = NULL, width = 12, status = 'primary', collapsible = FALSE,
        tags$b('Visualizador'), uiOutput(ns('balanzasPicker')),
        splitLayout(
          materialSwitch(ns('ShallprintThebalanzas'), label = 'Información del certificado de calibración',
                         value = FALSE, status = "primary"),
          materialSwitch(ns('ShallplotThebalanzas'), label = 'Gráfico de error de indicación', 
                         value = TRUE, status = "primary")),
        tags$hr(),
        plotOutput(ns('plotThebalanzas'), width = '95%'),
        verbatimTextOutput(ns('printThebalanzas'))
      )
    )
  )
}

BalanceCalibCertServer <- function(id, devMode) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    
    balanzasPicker <- reactive({
      if (is.null(input$balanzasElected)) return('(Seleccione o cargue la información de al menos una balanza)')
      pickerInput(
        session$ns("balanzasShowData"), label = 'Seleccione una balanza',
        choices = input$balanzasElected, width = '100%',# inline = FALSE,
        multiple = TRUE, selected = NULL,
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
    
    return(reactive(lapply(input$balanzasElected, function(x) balanzasList[[x]])))
  })
}