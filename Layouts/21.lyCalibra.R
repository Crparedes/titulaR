CalibraMonoLy <- fluidRow(
  #tags$br(), tags$br(),
    column(12, 
      tabBox(title = div(style = 'font-size:21px', tags$b('Fraccion masica de iones metalicos en disoluciones calibrantes monoelementales')), 
             width = 12, side = 'right', #status = 'primary', 
             tabPanel(
               title = tags$b('Titulaciones'),
               fluidRow(
                 #verbatimTextOutput(ns('test')),
                 column(1, tags$br()),
                 column(6, tags$br(),
                        radioButtons('Elemento', label = 'Especie en la disolucion calibrante: ', inline = TRUE, 
                                     choices = list('Plomo (II)' = 'Pb', 'Cadmio (II)' = 'Cd', 'Calcio (II)' = 'Ca'), 
                                     selected = 'Cd'),#character(0)),
                        conditionalPanel(
                          condition = 'input.Elemento == "Pb"',
                          tags$div(
                            id = "inline", style = 'font-size:12px',
                            splitLayout(cellWidths = c("40%", "12%"),
                                        numericInput('LeadAM', label = 'Masa atomica del plomo [g mol$^{-1}$]: .', value = 207.20),
                                        numericInput('u_LeadAM', label = '\u00B1', value = 0.06))
                          )
                        ),
                        # tags$hr(),
                        tags$div(
                          id = "inlineTOP", style = 'font-size:12px', 
                          textInput('sampleID', label = 'Identificacion muestra: .', value = 'Calibrante'),
                          textAreaInput('dscrMuestraMonoelemTit', label = 'Observaciones:  .', rows = 2, 
                                        placeholder = '(Informacion adicional)', width = '100%'),
                          pickerInput("BalanzaMonoelemTit", label = 'Balanza utilizada: .',
                                      choices = CalibCertShow, width = '500px', selected = 'MT XPE 205', multiple = FALSE),
                        ), 
                        tags$br(),  
                        actionButton('MonoElemInitTit', label = 'Iniciar nueva titulacion'),
                        tags$hr()
               ),
               column(11,
                      column(1, tags$br()),
                      tabBox(id = 'monoElemTabBox', width = 10, side = 'left')
               )
             )),       
             tabPanel(title = tags$b('Combinación de resultados'),
                      CalibraMonoCombUI('CalibraMonoComb1'))
      )
      )
)
