AnalystPickerUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, uiOutput(ns('AnalistaPicker'))),
    column(6, uiOutput(ns('datosAnalista')))
  )
}

AnalystPickerServer <- function(id, devMode, demo, showData = TRUE, inline = TRUE, width = 'fit') {
  moduleServer(id, function(input, output, session) {
    Analista <- reactive({
      req(input$Analista)
      authPersons[[input$Analista]]
    })
    
    AnalistaPicker <- reactive(pickerInput(
      session$ns("Analista"), label = ReqField('Analista', 2), inline = inline, width = width,
      choices = names(authPersons), multiple = TRUE, selected = ifelse(demo(), 'Cristhian Paredes', ''),
      options = list(`max-options` = 1, `none-selected-text` = "(Personal con autorizaciones)")))
    output$AnalistaPicker <- renderUI(AnalistaPicker())

    datosAnalista <- eventReactive(input$Analista, ignoreNULL = TRUE, ignoreInit = TRUE, {
      tags$div(
        tags$a(href = gsub('www/', '', list.files(path = 'www/Personal/', pattern = input$Analista, full.names = TRUE)),
               tags$b(ifelse(showData, 'XML analista', '')), download = NA, target = "_blank"),
        spcs(3), tags$a(href = Analista()$data$orcid, img(src = "ORCID.png", width = "25", height = "25"), target = "_blank"),
        spcs(3), tags$a(href = Analista()$inst$ror, img(src = "ROR.png", width = "25", height = "25"), target = "_blank")
      )
    })
    output$datosAnalista <- renderUI(datosAnalista())
  return(Analista)
  })
}