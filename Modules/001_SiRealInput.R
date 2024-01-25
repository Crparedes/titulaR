SiRealXML <- function(quantityTypeQUDT, value, units, uncert, covFac, covProp, distribution) {
  return(addDataToMRXML(
    read_xml('<si:real xmlns:si="https://ptb.de/si"/>'),
    list('si:quantityTypeQUDT' = quantityTypeQUDT, 'si:value' = value, 'si:unit' = units,
         'si:expandedUnc' = list(
           'si:uncertainty' = uncert, 'si:coverageFactor' = covFac,
           'si:coverageProbability' = covProp, 'si:distribution' = distribution))))}

SiRealInputUI <- function(id, name, x0, u0, units, decimalPlaces = 3) {
  ns <- NS(id)
  tags$div(
    id = "inline", style = 'font-size:12px;', uiOutput(ns('brwz')),
    tags$b(name),
    fluidRow(
      style = 'margin-left:10px;',
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 inputId = ns('value'), label = NULL, value = x0, decimalPlaces = decimalPlaces)),
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('uncert'), label = NonReqField('\u00B1', 3), value = u0, decimalPlaces = decimalPlaces)),
      column(2, selectInput(ns('units'), label = NULL, choices = units)),
      column(5, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('covFac'), label = NonReqField('Factor k'), value = 1.96, decimalPlaces = 2),
             autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                              inputId = ns('covProp'), label = NonReqField('Probabilidad'), value = 0.95, decimalPlaces = 2),
             selectInput(ns('distribution'), label = NonReqField('Distribución'), choices = Distributions))),
  )
}

SiRealInputServer <- function(id, devMode, quantityTypeQUDT = '') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa subsubmodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    SiReal <- reactive(SiRealXML(quantityTypeQUDT, input$value, input$units, input$uncert,
                                 input$covFac, input$covProp, input$distribution))
    
    
    
    return(SiReal)
  })
}