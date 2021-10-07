SolidMRCUI <- function(id, reagent, reagKey, explan, nu = FALSE) {
  ns <- NS(id)
  box(title = div(style = 'font-size:18px;valign="bottom"', tags$b('MRC de ', reagent)), 
      width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      h5(explan),
      radioButtons(ns('SourceOption'), label = '¿Qué desea hacer?', 
                   choices = list('Crear disolución nueva utilizando un MRC' = 'daCapo',
                                  "Subir un archivo '.dis' generado anteriormente" = 'archivo')),
      tags$hr(),
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo"',
        ns = ns,
        fluidRow(
          column(8, pickerInput(ns("MRCElected"), label = 'Seleccione el MRC:',
                                choices = MRCs.ArchiveNames[[reagKey]], width = '100%',# inline = FALSE,
                                multiple = FALSE, selected = NULL)),
          column(4, tags$br(), uiOutput(ns("MRC_CertiFile")))),
        uiOutput(ns('InfoMrcBox')),
        box(title = div(style = 'font-size:14px', 'Preparacion de la disolucion'), width = 12, collapsible = TRUE, collapsed = TRUE, status = 'primary',
            tags$b('Masa del MRC'),
            numericInput(ns('MasRec1'), label = div(style = 'font-size:12px', 'Masa del recipiente donde se pesara el MRC [g]:'), value = 0),
            numericInput(ns('MasMRC1'), label = div(style = 'font-size:12px', 'Masa del MRC adicionado [g]:'), value = 0),
            numericInput(ns('MasRecMRC1'), label = div(style = 'font-size:12px', 'Masa conjunta del recipiente y el MRC [g]:'), value = 0))),
      conditionalPanel(
        condition = 'input.SourceOption == "archivo"',
        ns = ns,
        fileInput(ns('DisFile'), label = 'Escoja el archivo', multiple = FALSE, accept = '.dis'),
        uiOutput(ns('InfoDisBox')))
  )
}

SolidMRCServer <- function(input, output, session, reagKey) {
  fileDwnHTML <- reactive(a(href = paste0('CertMRC/', reagKey, '/', input$MRCElected, '.pdf'),
                            "Descargar certificado ", download = NA, target = "_blank"))
  dateMRC <- reactive(MRC.ExpiricyDates[[reagKey]][[input$MRCElected]])
  MassFrMRC <- reactive(MRCs.MassFraction[[reagKey]][[input$MRCElected]])
  MolWeiMRC <- reactive(MRC.At_MolWeigths[[reagKey]][[input$MRCElected]])
  DensitMRC <- reactive(MRC.densities[[reagKey]][[input$MRCElected]])
  
  InfoMrcBox <- reactive({
    
    box(title = div(style = 'font-size:14px', 
                    ifelse(dateMRC() > Sys.Date(), 'Resumen de informacion del MRC (vigente):', 'Resumen de informacion del MRC (VENCIDO):')), 
        width = 12, collapsible = TRUE, collapsed = TRUE,
        status = ifelse(dateMRC() > Sys.Date(), 'success', 'danger'),
        div(style = 'font-size:12px',
            tags$b('Fecha de vencimiento:'), dateMRC(), tags$br(),
            tags$b('Fraccion masica de ', reagKey, ':'), MassFrMRC()[1], '\u00B1', MassFrMRC()[2], tags$br(),
            tags$b('Masa molar de ', reagKey, ' que aplica para el material:'), MolWeiMRC()[1], '\u00B1', MolWeiMRC()[2], 'g mol$^{-1}$',tags$br(),
            tags$b('Densidad estimada del MRC:'), DensitMRC()[1], '\u00B1', DensitMRC()[2], 'g cm$^{-3}$'))
  })
  
  output$MRC_CertiFile <- renderUI(fileDwnHTML())
  output$InfoMrcBox <- renderUI(InfoMrcBox())
  
  
  # Disoluci'on cargada
  InfoDisBox <- reactive(box(title = div(style = 'font-size:13px', 'Informacion de la disolucion')))
  output$InfoDisBox <- renderUI(InfoDisBox())
  return()
}