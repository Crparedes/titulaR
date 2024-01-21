balanzasPickerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('balanzasPicker'))
}

balanzasPickerServer <- function(id, devMode, balanzas) {
  moduleServer(id, function(input, output, session) {
    balanzasPicker <- reactive({
      balanceChioces <- sapply(balanzas(), function (x) x$balanceID)
      if (length(balanceChioces) == 0) {
        return(tags$div(style = 'color:red;', tags$hr(), 'Vaya al módulo de', icon('certificate'), tags$b('Balanzas,'),
                        'y seleccione o cargue la información de al menos una balanza)', tags$hr()))}
      pickerInput(
        session$ns("balanzasUse"), label = ReqField('Balanza', 3), inline = TRUE, width = 'fit', multiple = TRUE, selected = NULL,
        choices = sapply(balanzas(), function (x) x$balanceID), options = list(`max-options` = 1, `none-selected-text` = "(Módulo balanzas)"))
    })
    output$balanzasPicker <- renderUI(balanzasPicker())
    
    balanzasUse <- reactive(input$balanzasUse)
  return(balanzasUse)
  })
}