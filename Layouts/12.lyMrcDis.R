MrcDisLy <- fluidRow(
    column(12, 
      box(title = div(style = 'font-size:23px', tags$b('MRCs y disoluciones para complejometrías')), 
          width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
            #h4("Esta aplicación se utiliza para los cálculos de los siguientes protocolos de medicion:", tags$br(),
            #   tags$ul(
            #     tags$li(tags$b("Fracción másica de iones plomo, cadmio y calcio, en disoluciones 
            #                    calibrantes monoelementales, por titulación complejométrica con EDTA.")),
            #     tags$li(tags$b("Fracción másica de EDTA en la sal disódica dihidratada del ácido 
            #                    etilendiamonitetracético, por titulación complejométrica con ion plomo."))
            #   ), tags$br(),
            #   "Para iniciar debe verificar que los certificados de calibración digitales de las balanzas que utilizará 
            #   se encuentren en el aplicativo.", tags$br(), "Luego debe ir a la opción ", tags$b(icon("fill-drip"), "MRCs y disoluciones"), 
            #   " para cargar la información de los materiales de referencia certificados y de las disoluciones que preparó a partir de estos.", tags$br(), 
            #   "Finalmente debe navegar hasta el módulo correspondiente al tipo de titulación que necesita y seguir las instrucciones correspondientes.", 
            #   tags$br(), tags$br(), "Los certificados de calibración digitales de las balanzas son los estipulados en el paquete de R ", 
            #   tags$b(a("masscor", href = "https://CRAN.R-project.org/package=masscor"), "."))),
      fluidRow(
        column(4,
               SolidMRCUI('ModuloDisolucionEDTA', reagent = 'EDTA - sal disodica dihidratada', reagKey = 'EDTA',
                 explan = 'Para asignar valor de fraccion masica de iones metalicos en disoluciones calibrantes monoelementales'
                 #o para realizar aseguramiento de la calidad de los resultados de fraccion masica de EDTA en la sal disodica dihidratada'
                 )),
        column(4,
               SolidMRCUI('ModuloDisolucionPbNO3.2', reagent = 'Nitrato de plomo', reagKey = 'Pb',
                 explan = 'Para asignar valor de fraccion masica de EDTA en la sal disodica dihidratada'
                 #o para realizar aseguramiento de la calidad de los resultados de fraccion masica de iones metalicos en disoluciones 
                 #calibrantes monoelementales'
                 )),
        
        column(4,
               LiquidMRCUI('ModuloDilucionCobre', reagent = 'Disolucion calibrante de cobre', reagKey = 'Cu',
                           explan = 'Para usar como indicadora en las titulaciones complejometricas por retroceso'),
               LiquidMRCUI('ModuloDilucionZinc', reagent = 'Disolucion calibrante de zinc', reagKey = 'Zn',
                           explan = 'Para usar como disolucion titulante en las titulaciones complejometricas por retroceso')
        ))
      ),
      box(title = NULL, width = 12, h6('Para solicitar la inclusión de más MRCs por favor mande un correo a caparedes@inm.gov.co.'))
    )
)
