solutionPickerUI <- function(id) {
  ns <- NS(id)
  tags$div(uiOutput(ns('brwz')), uiOutput(ns('solutionPicker')))
}

solutionPickerServer <- function(id, devMode, demo, solutions, label, inline = FALSE, width = '300px') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(tags$hr(), actionButton(session$ns('brwzInsideModule'), tags$b('Pausa picker')))})
    observeEvent(input$brwzInsideModule, browser())
    
    solutionPicker <- reactive({
      if(length(solutions()) == 0) {
        return(div(
          tags$b(ReqField(label)), tags$br(),
          tags$div(style = 'color:red;', 'Vaya al módulo de', icon('fill-drip'), tags$b('Preparación disoluciones,'),
                   'y genere o cargue información de al menos una', tags$u(label), tags$br(), tags$br())
        ))
      }
      solNames <- sapply(solutions(), function(x) {return(xml_text(xml_find_all(x(), xpath = '//mr:solutionID')))})
      solPosit <- 1:length(solNames)
      names(solPosit) <- solNames
      pickerInput(
        session$ns("solution"), label = ReqField(label), inline = inline, width = width,
        choices = solPosit, multiple = TRUE, selected = NULL,
        options = list(`max-options` = 1, `none-selected-text` = "(Módulo Preparación disoluciones)"))
    })
    output$solutionPicker <- renderUI(solutionPicker())
    
    solution <- reactive({
      req(input$solution)
      solutions()[[input$solution]]
    })
    return(solution)
  })
}