MaterialesRefereUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6, style = 'margin-left: 80px;', Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: -20px;',
              tags$b('Materiales de referencia para la preparacion de disoluciones')),
      tags$b("Ver o descargar la información de un material de referencia"),
      radioButtons(ns('MrXmlViewType'), label = NULL, 
                   choices = list('Para titular disoluciones calibrantes' = 'forCalibrantes',
                                  'Para titular EDTA' = 'forEDTA')),
      uiOutput(ns('SelectMRC')),
      box(
        title = NULL, width = 12, status = 'primary', collapsible = FALSE,
        fluidRow(
          column(width = 2, img(src = "SI_mol.png", width = "100%")),
          column(width = 10, tags$br(), downloadLink(ns('DescMrXml'), label = "Descargar archivo XML")),#uiOutput(ns("downlXMLlink"))), 
          column(width = 12, tags$br(), htmlOutput(ns('printTheMrXML')))
        )
      )
    ),
    
    column(
      4, style = 'margin-left: 120px;', Nlns(6), 
      tags$b('Cargar información de nuevos materiales de referencia.'), tags$br(),
      'Seleccione un archivo XML con la información de un material de referencia faltante, 
      indique el uso que tiene el material y opima el botón', tags$b('Cargar.'), Nlns(),
      fileInput(ns('NewMrXml'), label = NULL, buttonLabel = 'Examinar...',#placeholder = '',
                multiple = FALSE, accept = '.xml', width = '90%'),
      uiOutput(ns('CargarMrXml'))
    )
  )
}


MaterialesRefereServer <- function(id, devMode) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa modulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    ReferenceMaterials <- reactiveValues(
      forCalibrantes = lapply(list.files(path = 'www/MR_MRC/Para calibrantes', pattern = 'xml', full.names = TRUE), read_xml),
      forEDTA = lapply(list.files(path = 'www/MR_MRC/Para EDTA', pattern = 'xml', full.names = TRUE), read_xml))
    
    SelectMRC <- reactive(tags$div(
      pickerInput(
        session$ns("MRCtoView"), width = "100%", label = NULL,
        choices = lapply(ReferenceMaterials[[input$MrXmlViewType]], function(x) {as_list(x)[[1]]$administrativeData$name[[1]]}),
        selected = NULL, multiple = TRUE,
        options = list(`max-options` = 1, `none-selected-text` = "Seleccione un MRC"))))
    output$SelectMRC <- renderUI(SelectMRC())
    
    observeEvent(input$MRCtoView, {
      index <- which(lapply(ReferenceMaterials[[input$MrXmlViewType]], function(x) {as_list(x)[[1]]$administrativeData$name[[1]]}) == input$MRCtoView)
      browser()
      # fileXML <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE, pattern = paste0(input$MRCtoView, '.xml'))
      # filePDF <- list.files('www/MR_MRC', recursive = TRUE, full.names = TRUE, pattern = paste0(input$MRCtoView, '.pdf'))
      
      withCallingHandlers({
        shinyjs::html("printTheMrXML", "")
        message(ReferenceMaterials[[input$MrXmlViewType]][[index]])},
        message = function(m) {
          shinyjs::html(id = "printTheMrXML",
                        html = paste0('<textarea rows = 40 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
      
      output$DescMrXml <-  downloadHandler(
        filename = function() {paste0(gsub(pattern = ' ', replacement = '_', input$MRCtoView, fixed = FALSE), ".xml")},
        content = function(file) {write_xml(ReferenceMaterials[[input$MrXmlViewType]][[index]], file)})
        
      # output$downlXMLlink <- renderUI(downloadLink(session$ns('DescMrXml'), label = "Descargar archivo XML"))
        #tags$div(
        #a(href = gsub('www/', '', fileXML), 'Descargar archivo XML', 
        #  download = NA, target = "_blank")#, tags$br(),
        #a(href = gsub('www/', '', filePDF), 'Descargar certificado o reporte en PDF', 
        #  download = NA, target = "_blank")
        #)
      # )
    })
    
    CargarMrXml <- eventReactive(input$NewMrXml, ignoreInit = TRUE, {
      if (!grepl('.xml', input$NewMrXml$name, fixed = TRUE)) {
        return(tags$div(style = 'color:red;', tags$b('Escoja un archivo XML válido!')))
      } else {
        return(tags$div(
          style = 'margin-left:60px;',
          radioButtons(session$ns('MrXmlUploadType'), label = 'Uso del MRC:', 
                       choices = list('Para titular disoluciones calibrantes' = 'forCalibrantes',
                                      'Para titular EDTA' = 'forEDTA')),
          actionButton(session$ns('CargarMrXml'), label = 'Cargar')))
      }
    })
    output$CargarMrXml <- renderUI(CargarMrXml())
    
    observeEvent(input$CargarMrXml, {
      ReferenceMaterials[[input$MrXmlUploadType]] <- append(ReferenceMaterials[[input$MrXmlUploadType]], values = list(read_xml(input$NewMrXml$datapath)))
    })
    
    return(ReferenceMaterials)
  })
}