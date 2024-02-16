SiRealInputUI <- function(id, name, x0, u0, units, decimalPlaces = 3, colWid = c(2, 2, 2, 4)) {
  ns <- NS(id)
  tags$div(
    id = "inline", style = 'font-size:12px;', uiOutput(ns('brwz')),
    fluidRow(
      style = 'margin-left:80px;',
      tags$h5(tags$b(name), style = 'margin-left:-40px;'),
      uiOutput(ns('SIlogo')),
      column(colWid[1], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 inputId = ns('value'), label = NULL, value = x0, decimalPlaces = decimalPlaces)),
      column(colWid[2], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('uncert'), label = NonReqField('\u00B1', 3), value = u0, decimalPlaces = decimalPlaces)),
      column(colWid[3], selectInput(ns('units'), label = NULL, choices = units)),
      column(colWid[4], autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE, align = 'left', minimumValue = 0,
                                 inputId = ns('covFac'), label = NonReqField('Factor_k', 10), value = 1.96, decimalPlaces = 2),
             uiOutput(ns('covProp')),
             selectInput(ns('distribution'), label = NonReqField('Distribución'), choices = Distributions))),
  )
}

SiRealInputServer <- function(id, devMode, quantityTypeQUDT = '',
                              SIdigRef = FALSE, unit = NULL, derived = FALSE, width = '90%', SIalign = 'right', SIcol = 1) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(tags$hr(), actionButton(session$ns('brwzInsideModule'), tags$b('Pausa ingreso datos D-SI')))})
    observeEvent(input$brwzInsideModule, browser())
    
    SIlogo <- reactive({
      if (SIdigRef) return(column(width = SIcol, style = 'margin-left:-90px;', 
                                  lapply(1:length(SIdigRef), function(x) return(SI_unit_nice(unit[x], derived[x], width = width)))))
    })
    output$SIlogo <- renderUI(SIlogo())
    
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