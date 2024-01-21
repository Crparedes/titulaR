SiRealXML <- function(quantityTypeQUDT, value, units, uncert, covFac, distribution) {
  return(addDataToMRXML(
    read_xml('<si:real xmlns:si="https://ptb.de/si"/>'),
    list('si:quantityTypeQUDT' = quantityTypeQUDT, 'si:value' = value, 'si:unit' = units,
         'si:expandedUnc' = list('si:uncertainty' = uncert, 'si:coverageFactor' = covFac, 'si:coverageProbability' = distribution))))}

SiRealInputUI <- function(id, name, x0, u0, units, decimalPlaces = 3) {
  ns <- NS(id)
  tags$div(
    id = "inline", style = 'font-size:12px;', uiOutput(ns('brwz')),
    tags$b(name),
    fluidRow(
      style = 'margin-left:10px;',
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 inputId = ns('value'), label = NULL, value = x0, decimalPlaces = decimalPlaces)),
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 inputId = ns('uncert'), label = '\u00B1', value = u0, decimalPlaces = decimalPlaces)),
      column(2, selectInput(ns('units'), label = NULL, choices = units))),
    fluidRow(
      column(4, offset = 2, selectInput(ns('covFac'), label = 'Factor de cobertura', choices = CobertureFactors)),
      column(3, selectInput(ns('distribution'), label = 'Distribución', choices = Distributions))),
  )
}

SiRealInputServer <- function(id, devMode, quantityTypeQUDT = '') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa subsubmodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    
    SiReal <- reactive(SiRealXML(quantityTypeQUDT, input$value, input$units, input$uncert, input$covFac, input$distribution))
    
    return(SiReal)
  })
}