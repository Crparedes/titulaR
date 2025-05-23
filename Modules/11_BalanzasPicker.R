balanzasPickerUI <- function(id) {
  ns <- NS(id)
  tags$div(uiOutput(ns('brwz')), uiOutput(ns('balanzasPicker')))
}

balanzasPickerServer <- function(id, devMode, demo, balanzas, inline = TRUE, width = 'fit') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(tags$hr(), actionButton(session$ns('brwzInsideModule'), tags$b('Pausa picker')))})
    observeEvent(input$brwzInsideModule, browser())
    
    balanzasPicker <- reactive({
      balanceChioces <- sapply(balanzas$DCC, function (x) x$balanceID)
      if (length(balanceChioces) == 0) {
        return(tags$div(ReqField(tags$b('Balanza')), tags$br(), 
                        tags$div(style = 'color:red;', 'Vaya al módulo de', icon('certificate'), tags$b('Balanzas,'),
                                 'y seleccione (o cargue) la información de al menos una balanza.', tags$br(), tags$br())))
      }
      pickerInput(
        session$ns("balanzaUsed"), label = ReqField('Balanza', 3), inline = inline, width = width, multiple = FALSE,
        # selected = ifelse(demo(), 'BALANZA METTLER TOLEDO XPE 205', ''), 
        choices = sapply(balanzas$DCC, function (x) x$balanceID), options = list(`max-options` = 1, `none-selected-text` = "(Módulo balanzas)"))
    })
    output$balanzasPicker <- renderUI(balanzasPicker())
    
    balanzaUsed <- reactive({
      req(input$balanzaUsed)
      balanzas$DCC[[which(sapply(balanzas$DCC, function (x) x$balanceID) == input$balanzaUsed)]]})
  return(balanzaUsed)
  })
}