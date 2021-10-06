inicioLy <- fluidRow(
    column(12, 
      box(title = div(style = 'font-size:25px', tags$b('Introducción')), width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
            h4("Esta aplicación se utiliza para los cálculos de los siguientes protocolos de medicion:", tags$br(),
               tags$ul(
                 tags$li(tags$b("Fracción másica de iones plomo, cadmio y calcio, en disoluciones 
                                calibrantes monoelementales, por titulación complejométrica con EDTA.")),
                 tags$li(tags$b("Fracción másica de EDTA en la sal disódica dihidratada del ácido 
                                etilendiamonitetracético, por titulación complejométrica con ion plomo."))
               ), tags$br(),
               "Para iniciar debe verificar que los certificados de calibración digitales de las balanzas que utilizará 
               se encuentren en el aplicativo.", tags$br(), "Luego debe ir a la opción ", tags$b(icon("fill-drip"), "MRCs y disoluciones"), 
               " para cargar la información de los materiales de referencia certificados y de las disoluciones que preparó a partir de estos.", tags$br(), 
               "Finalmente debe navegar hasta el módulo correspondiente al tipo de titulación que necesita y seguir las instrucciones correspondientes.", 
               tags$br(), tags$br(), "Los certificados de calibración digitales de las balanzas son los estipulados en el paquete de R ", 
               tags$b(a("masscor", href = "https://CRAN.R-project.org/package=masscor"), "."))),
      BalanceCalibCertUI('BalanceCalibCert'),
      box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Configuracion')), 
          width = 3, status = 'primary', collapsible = TRUE, collapsed = TRUE,
      ))
)
