MaterialesRefereUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    
    column(
      12, Nlns(4), uiOutput(ns('brwz')),
      tags$h4(style = 'margin-left: 60px;', tags$b('Materiales de referencia para la preparacion de disoluciones')), tags$br()),
    column(
      width = 4, style = 'margin-left: 80px', #Nlns(10), 
      tags$b('Cargar archivos XML de materiales de referencia'), Nlns(),
      tags$div(
        style = 'margin-left: 40px;',
        'Indique el uso que tiene cada material y presione el botón para cargar la información al aplicativo.',
        fileInput(ns('NewMrXml'), label = NULL, buttonLabel = 'Examinar...', multiple = FALSE, accept = '.xml', width = '100%'),
        uiOutput(ns('UsoMrXml')),
        'Verifique que los archivos quedan disponibles en el recuadro de visualización.'),
      uiOutput(ns('NewMrXml')),
      uiOutput(ns('CargarMrXml'))
    ),
    column(
      width = 7, style = 'margin-left: 40px;', 
      tags$b("Materiales de referencia cargados en el aplicativo"), Nlns(),
      awesomeRadio(ns('MrXmlViewType'), label = NULL, selected = 'forEDTA', width = '100%', inline = TRUE, status = 'primary',
                   choices = list('Para caracterizar disoluciones monoelementales' = 'forCalibrantes',
                                  'Para caracterizar sales de EDTA' = 'forEDTA')),
      uiOutput(ns('SelectMRC')),
      box(
        title = tags$b('Visualizador'), width = 12, status = 'primary', collapsible = FALSE,
        fluidRow(
          column(width = 2, SI_unit_nice('mole', width = "100%")),
          column(width = 10, disabled(downloadLink(ns('DescMrXml'), label = "Descargar archivo XML de material de referencia")),
                 Nlns(2), tags$div(style = 'font-size:12px;', htmlOutput(ns('printTheMrXML'))))
        )
      )
    )
    
    
  )
}


MaterialesRefereServer <- function(id, devMode, demo) {
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
        multiple = !demo(),
        options = list(`max-options` = 1, `none-selected-text` = "Seleccione un MRC"))))
    output$SelectMRC <- renderUI(SelectMRC())
    

    observeEvent(input$MRCtoView, {
      req(input$MRCtoView)
      enable('DescMrXml')
      
      index <- which(lapply(ReferenceMaterials[[input$MrXmlViewType]], function(x) {
        as_list(x)[[1]]$administrativeData$name[[1]]}) == input$MRCtoView)
      withCallingHandlers({
        shinyjs::html("printTheMrXML", "")
        message(ReferenceMaterials[[input$MrXmlViewType]][[index]])},
        message = function(m) {
          shinyjs::html(id = "printTheMrXML",
                        html = paste0('<textarea rows = 100 style = "width: 100%;">',
                                      m$message, '</textarea>'), add = FALSE)})
      output$DescMrXml <-  downloadHandler(
        filename = function() {paste0(gsub(pattern = ' ', replacement = '_', input$MRCtoView, fixed = FALSE), ".xml")},
        content = function(file) {write_xml(ReferenceMaterials[[input$MrXmlViewType]][[index]], file)})
    })
    
    UsoMrXml <- eventReactive(input$NewMrXml, ignoreInit = TRUE, {
      if (!grepl('.xml', input$NewMrXml$name, fixed = TRUE)) {
        return(tags$div(style = 'color:red;', tags$b('Escoja un archivo XML válido!')))
      } else {
        return(tags$div(
          style = 'margin-left:60px;',
          tags$b('Archivo:'), input$NewMrXml$name,
          radioButtons(session$ns('MrXmlUploadType'), label = 'Uso del MRC:', 
                       choices = list('Para titular disoluciones calibrantes' = 'forCalibrantes',
                                      'Para titular EDTA' = 'forEDTA'))))
      }
    })
    output$UsoMrXml <- renderUI(UsoMrXml())
    
    observeEvent(input$CargarMrXml, {
      ReferenceMaterials[[input$MrXmlUploadType]] <- append(ReferenceMaterials[[input$MrXmlUploadType]], values = list(read_xml(input$NewMrXml$datapath)))
      
    })
    
    return(ReferenceMaterials)
  })
}