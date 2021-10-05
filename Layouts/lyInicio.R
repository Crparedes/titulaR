inicioLy <- fluidRow(
    column(12, 
      box(title = div(style = 'font-size:25px', tags$b('Introducción')), width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
        fluidRow(
          column(9,
            h4("Esta aplicación complementa los protocolos de medicion...")))),
      box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Certificados de calibracion de balanzas')), 
          width = 6, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          ),
      box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Configuracion')), 
          width = 6, status = 'primary', collapsible = TRUE, collapsed = TRUE,
      ))
)
