ShowSolutionUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('brwz'))
  tags$div(uiOutput(ns('SummarySolution'))   )#, tags$hr(), htmlOutput(ns('InfoDisXML')))
}

ShowSolutionServer <- function(id, devMode, demo, solution, type = 'SolidMRC') {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(
    if(devMode()) return(actionButton(session$ns('brwz'), label = tags$b('Pausar submódulo'))))
    observeEvent(input$brwz, browser())

    SummarySolution <- reactive({
      xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionType'))
      ID <- xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionID'))
      InChiKey <- xml_text(xml_child(solution(), search = 'mr:property//mr:substance//mr:InChiKey'))
      
      
      if (type == 'SolidMRC') {
        c_std <- GetValueEstandUncert(xml_child(solution(), search = 'mr:property//si:real'))
        d1 <- decimals(signif(c_std$ValUnc[2], 3))
        Solutioninfo <- tagList(
          tags$tr(
            tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Concentración:'),
            tags$th(style = 'vertical-align:top;padding-top:0.5em;', 
                    round(c_std$ValUnc[1], d1), '\u00B1', signif(c_std$ValUnc[2], 3), ' mmol/kg (k=1)')
          ),
          tags$tr(
            tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'MRC de partida:'),
            tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                    xml_text(xml_child(solution(), search = 'mr:coreData//mr:CRM')))
          )
        )
      } else {
        if (type == 'CalibSample') {
          f_dil <- GetValueEstandUncert(solution(),  property = 'MassRatio', node = 'mr:property')
          d1 <- decimals(signif(f_dil$ValUnc[2], 3))
          m_mas <- GetValueEstandUncert(solution(),  property = 'MolarMass', node = 'mr:property')
          d2 <- decimals(signif(m_mas$ValUnc[2], 3))
          Solutioninfo <- tagList(
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Material de partida:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                      xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionSource')))
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Factor de dilución:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 
                      round(f_dil$ValUnc[1], d1), '\u00B1', signif(f_dil$ValUnc[2], 3), ' g/g (k=1)')
            ),
            tags$tr(
              tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Masa molar del elemento:'),
              tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                      round(m_mas$ValUnc[1], d2), '\u00B1', signif(m_mas$ValUnc[2], 3), ' g/mol (k=1)')
            )
          )
        } else {
          if (type == 'EDTASample') {
            f_dil <- GetValueEstandUncert(solution(),  property = 'MassRatio', node = 'mr:property')
            d1 <- decimals(signif(f_dil$ValUnc[2], 3))
            m_mas <- GetValueEstandUncert(solution(),  property = 'MolarMass', node = 'mr:property')
            d2 <- decimals(signif(m_mas$ValUnc[2], 3))
            Solutioninfo <- tagList(
              tags$tr(
                tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Material de partida:'),
                tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                        xml_text(xml_child(solution(), search = 'mr:coreData//mr:solutionSource')))
              ),
              tags$tr(
                tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Factor de dilución:'),
                tags$th(style = 'vertical-align:top;padding-top:0.5em;', 
                        round(f_dil$ValUnc[1], d1), '\u00B1', signif(f_dil$ValUnc[2], 3), ' g/g (k=1)')
              ),
              tags$tr(
                tags$th(style = 'vertical-align:top;padding-top:0.5em;', 'Masa molar del elemento:'),
                tags$th(style = 'vertical-align:top;padding-top:0.5em;',
                        round(m_mas$ValUnc[1], d2), '\u00B1', signif(m_mas$ValUnc[2], 3), ' g/mol (k=1)')
              )
            )
          }
      }
      
      return(infoBox(
        width = 12, title = tags$b(style = 'font-size:13px', 'ID disolución:', ID),
        icon = icon("fill-drip"), color = 'black', fill = FALSE,
        subtitle = tags$div(
          tags$table(
            style = "width:100%; font-size:13px; margin-left:20px; vertical-align:top",
            tags$tr(
              tags$th(style = 'vertical-align:top', 'Especie:'),
              tags$th(style = 'vertical-align:top', 
                      xml_text(xml_child(solution(), search = 'mr:property//mr:substance//mr:name')), tags$br(),
                      'InChi Key ', InChiKey, tags$br(),
                      tags$a(href = paste0('https://pubchem.ncbi.nlm.nih.gov/#query=', InChiKey),
                             style = 'color:#0072bd;',
                             tags$html('Ver sustancia en', img(src = "PubChem.png", height = '19px')), target = '_blank'))
            ),
            Solutioninfo,
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
          tags$hr())
      ))
    })
    output$SummarySolution <- renderUI(SummarySolution())
  })
}
                           