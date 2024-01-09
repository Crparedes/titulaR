GENERIC.Server <- function(input, output, session, devMode) {
  output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar módulo'))))
  observeEvent(input$brwz, browser())
}

