SiRealXML <- function(quantityTypeQUDT = NULL, value = NULL, units = NULL,
                      uncert = NULL, covFac = NULL, covProp = NULL, distribution = 'normal', SI.list = NULL) {
  SiRealXML <- read_xml('<si:real xmlns:si="https://ptb.de/si" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>')
  if(!missing(SI.list)) {
    addDataToMRXML(SiRealXML, SI.list)
  } else {
    if(missing(covProp)) covProp <- round(pnorm(covFac) - pnorm(-covFac), 3)
    addDataToMRXML(SiRealXML, list(
      'si:quantityTypeQUDT' = quantityTypeQUDT, 'si:value' = value, 'si:unit' = units,
      'si:expandedUnc' = list(
        'si:uncertainty' = uncert, 'si:coverageFactor' = covFac,
        'si:coverageProbability' = covProp, 'si:distribution' = distribution)))
  }
  return(SiRealXML)
}

SiRealInputUI <- function(id, name, x0, u0, units, decimalPlaces = 3, colWid = c(2, 2, 2, 5)) {
  ns <- NS(id)
  tags$div(
    id = "inline", style = 'font-size:12px;', uiOutput(ns('brwz')),
    tags$b(name),
    fluidRow(
      style = 'margin-left:10px;',
      column(colWid[1], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 inputId = ns('value'), label = NULL, value = x0, decimalPlaces = decimalPlaces)),
      column(colWid[2], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('uncert'), label = NonReqField('\u00B1', 3), value = u0, decimalPlaces = decimalPlaces)),
      column(colWid[3], selectInput(ns('units'), label = NULL, choices = units)),
      column(colWid[4], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('covFac'), label = NonReqField('Factor k'), value = 1.96, decimalPlaces = 2),
             uiOutput(ns('covProp')),
             selectInput(ns('distribution'), label = NonReqField('Distribución'), choices = Distributions))),
  )
}

SiRealInputServer <- function(id, devMode, quantityTypeQUDT = '') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa subsubmodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    covProp <- reactive({
      autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                       inputId = session$ns('covProp'), label = NonReqField('Probabilidad'), value = pnorm(input$covFac) - pnorm(-input$covFac),
                       decimalPlaces = 3)
    })
    output$covProp <- renderUI(covProp())
    
    SiReal <- reactive(SiRealXML(quantityTypeQUDT, input$value, input$units, input$uncert,
                                 input$covFac, input$covProp, input$distribution))
    
    
    
    return(SiReal)
  })
}