ShowSolutionUI <- function(id) {
  ns <- NS(id)
  tags$div(uiOutput(ns('SummarySolution'))   )#, tags$hr(), htmlOutput(ns('InfoDisXML')))
}

ShowSolutionServer <- function(id, devMode, demo, solution) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())

    SummarySolution <- reactive({
      xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionType'))
      ID <- xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionID'))
      InChiKey <- xml_text(xml_child(solution(), search = 'mr:property//mr:substance//mr:InChiKey'))
      
      c_std <- GetValueEstandUncert(xml_children(xml_children(solution())[2])[2])
      d1 <- decimals(signif(c_std$ValUnc[2], 3))
      return(infoBox(
        width = 12, title = tags$b(style = 'font-size:13px', 'ID disolución:', ID),
        icon = icon("fill-drip"), color = 'black', fill = FALSE,
        subtitle = tags$div(

          tags$table(
            style = "width:100%; font-size:13px; margin-left:20px; vertical-align:top",
            tags$tr(
              tags$th('Concentración:'),
              tags$th(round(c_std$ValUnc[1], d1), '\u00B1', signif(c_std$ValUnc[2], 3), ' mmol/kg (k=1)')
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Especie:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                      xml_text(xml_child(solution(), search = 'mr:property//mr:substance//mr:name')), tags$br(),
                      'InChi Key ', InChiKey, tags$br(),
                      tags$a(href = paste0('https://pubchem.ncbi.nlm.nih.gov/#query=', InChiKey),
                             style = 'color:#0072bd;',
                             tags$html('Ver sustancia en', img(src = "PubChem.png", height = '19px')), target = '_blank'))
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:1.7em;', 'Fecha de preparación:'),
              tags$th(style = 'vertical-align:top;padding-top:1.7em;',
                      xml_text(xml_child(solution(), search = 'mr:coreData//mr:dateTime')))
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.7em;', 'Persona responsable:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                      xml_text(xml_child(solution(), search = 'mr:coreData//respPerson//name')), spcs(3),
                      tags$a(href = xml_text(xml_child(solution(), search = 'mr:coreData//respPerson//orcid')),
                             img(src = "ORCID.png", width = "15", height = "15"), target = "_blank"))
            )
            
          ),
          tags$hr())))
    })
    output$SummarySolution <- renderUI(SummarySolution())
    
    observe({
      withCallingHandlers({
        shinyjs::html("InfoDisXML", "")
        message(solution())},
        message = function(m) {
          shinyjs::html(id = "InfoDisXML",
                        html = paste0('<b>Información de la titulación:</b><br>
                                      <textarea rows = 40 style = "width: 95%; margin-left:20px;">',
                                      m$message, '</textarea>'), add = FALSE)})
    })
    
  })
}
                           