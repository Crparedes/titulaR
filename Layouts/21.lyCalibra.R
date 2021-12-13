CalibraMonoLy <- fluidRow(
  #tags$br(), tags$br(),
    column(12, 
      tabBox(title = div(style = 'font-size:21px', tags$b('Fraccion masica de iones metalicos en disoluciones calibrantes monoelementales')), 
             width = 12, side = 'right', #status = 'primary', 
             tabPanel(title = tags$b('Titular una muestra'),
                      CalibraMonoUI('CalibraMono1')),
             tabPanel(title = tags$b('Combinacion de resultados'),
                      CalibraMonoCombUI('CalibraMonoComb1'))
        
          

      )
      )
)
