MaterialesRefereUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6, style = 'margin-left: 80px;', Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: -20px;',
              tags$b('Materiales de referencia para la preparacion de disoluciones')),
      uiOutput(ns('SelectMRC')),
      box(
        title = NULL, width = 12, status = 'primary', collapsible = FALSE,
        fluidRow(
          column(width = 2, img(src = "D-SI.png", width = "100%")),
          column(width = 10, tags$br(), uiOutput(ns("downlXMLlink"))), 
          column(width = 12, tags$br(), htmlOutput(ns('printTheMrXML')))
        )
      )
    ),
    
    column(
      4, style = 'margin-left: 120px;', Nlns(6), 
      tags$b('Cargar información de nuevos materiales de referencia.'), tags$br(),
      'Seleccione un archivo XML con la información de un material de referencia faltante, 
      indique el uso que tiene el material y opima el botón', tags$u('Cargar.'), Nlns(),
      fileInput(ns('NewMrXml'), label = NULL, multiple = FALSE, accept = '.xml', width = '90%'),
      uiOutput(ns('CargarMrXml'))
    )
  )
}


MaterialesRefereServer <- function(id, devMode) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    
    SelectMRC <- reactive(
      pickerInput(
        session$ns("MRCtoView"), width = "100%",
        label = "Ver o descargar la información de un material de referencia cargado en el aplicativo",
        choices = list(
          `Para caracterizar EDTA` = as.list(namesMR_MRCs$forEDTA),
          `Para caracterizar disoluciones calibrantes` = as.list(namesMR_MRCs$forCali)),
        selected = NULL, multiple = TRUE,
        options = list(`max-options` = 1, `none-selected-text` = "(Ninguno seleccionado)")
      ))
    output$SelectMRC <- renderUI(SelectMRC())
    
    observeEvent(input$MRCtoView, {
      fileXML <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE,
                            pattern = paste0(input$MRCtoView, '.xml'))
      
      filePDF <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE,
                            pattern = paste0(input$MRCtoView, '.pdf'))
      withCallingHandlers({
        shinyjs::html("printTheMrXML", "")
        message(read_xml(fileXML))},
        message = function(m) {
          shinyjs::html(id = "printTheMrXML",
                        html = paste0('<textarea rows = 40 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$downlXMLlink <- renderUI(tags$div(
        a(href = gsub('www/', '', fileXML), 'Descargar archivo XML', 
          download = NA, target = "_blank"), tags$br(),
        a(href = gsub('www/', '', filePDF), 'Descargar certificado o reporte en PDF', 
          download = NA, target = "_blank")))
    })
    
    return()
  })
}