AnalystPickerUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, pickerInput(
      ns("Analista"), label = ReqField('Analista', 2), inline = TRUE, width = 'fit',
      choices = names(authPersons), multiple = TRUE, selected = NULL,
      options = list(`max-options` = 1, `none-selected-text` = "(Personal con autorizaciones)"))),
    column(6, uiOutput(ns('datosAnalista')))
  )
}

AnalystPickerServer <- function(id, devMode, balanzas) {
  moduleServer(id, function(input, output, session) {
    Analista <- reactive({
      req(input$Analista)
      authPersons[[input$Analista]]
    })
    datosAnalista <- eventReactive(input$Analista, ignoreNULL = TRUE, ignoreInit = TRUE, {
      tags$div(
        tags$a(href = gsub('www/', '', list.files(path = 'www/Personal/', pattern = input$Analista, full.names = TRUE)),
               tags$b('XML analista'), download = NA, target = "_blank"),
        spcs(3), tags$a(href = Analista()$data$orcid, img(src = "ORCID.png", width = "25", height = "25"), target = "_blank"),
        spcs(3), tags$a(href = Analista()$inst$ror, img(src = "ROR.png", width = "25", height = "25"), target = "_blank")
      )
    })
    output$datosAnalista <- renderUI(datosAnalista())
  return(Analista)
  })
}