inicioLy <- fluidRow(
  column(
    12, 
    box(
      title = div(style = 'font-size:25px', tags$b('Introducción')),
      width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      column(
        10,
        h5("Esta aplicación trata los datos de medición para la caracterización de Materiales de Referencia
        Certificados (MRC) por medio de titulaciones gravimétricas utilizando los siguientes protocolos de medicion:",
           tags$br(),
           tags$ul(
             tags$li(tags$b("M-03-L-14-P-007"),
                     "Determinación de Fracción Másica (Pureza) de la Sal Disódica Dihidratada del
                     Ácido Etilendiaminotetracético por Titulación Complejométrica Gravimétrica"),
             tags$li(tags$b("M-03-L-14-P-008"),
                     "Determinación de Fracción Másica de Iones Plomo, Cadmio y Calcio, en Disoluciones
                     Calibrantes Monoelementales, por Titulación Complejométrica con EDTA")),
           tags$br(), tags$br(),
           
           'Este aplicativo incorpora el uso del',
           tags$a(href = 'https://zenodo.org/records/10230771', 'Esquema del SI Digital'),
           'para el almacenamiento y la transferencia de datos metrológicos. 
           Este esquema utiliza archivos en formato XML y se desarrolló desde el 
           Programa Europeo Metrológico para la Innovación y la Investigación (EMPIR).',
           tags$br(), tags$hr(),
           'El aplicativo', tags$b('titulaR'), 'se utiliza de la siguiente manera:',
           tags$ol(
             tags$li("En esta página, en el recuadro", tags$b('Inicio'),
                     "diligencie la información de la persona responsable y de la institución correspondiente."),
             tags$li("En el recuadro", tags$b('Trazabilidad Metrológica de Balanzas'),
                     "verifique que se encuentra cargada la información de los certificados de calibración
                     de las balanzas que utilizará, o cargue los archivos correspondientes que hagan falta.",
                     tags$br(), 'Estos archivos puede generarlos desde la',
                     tags$a(href = 'https://crparedes.shinyapps.io/masscor/', 'GUI del paquete masscor.')),
             tags$hr(),
             tags$li("Diríjase a la opción ", tags$b(icon("fill-drip"), "MRCs y disoluciones"),
                     "Ubique el tipo de disolución que necesita (disolución patrón o disolución muestra)
                     y siga las instrucciones del módulo para cargar o crear la información correspondiente."),
             tags$li("Navegue hasta la opción del tipo de titulación que realizará y siga las instrucciones
                     del módulo para generar resultados individuales de la muestra de interés.
                     Descargue los archivos de resultados de  cada titulación."),
             tags$li("Cuando termine las titulaciones, combine lo archivos XML de resultados individuales
                     usando la pestaña de combinación de resultados del mismo módulo en el que generó los
                     resultados individuales.")),
           tags$br(), tags$br())),
      column(2, img(src = "D-SI.png", width="180", height="130"))),
    
      box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Inicio')), 
          width = 5, status = 'primary', collapsible = TRUE, collapsed = FALSE,#height = '300px',
          tags$b("Información general:"),
          tags$div(id = "inline", 
                   dateInput('Fecha', label = 'Fecha: .', language = 'es'),
                   textInput("nombre", label = "Responsable: .", width = "300px", value = 'Cristhian Paredes'),
                   textInput("correo", label = "Correo: .", value = "caparedes@inm.gov.co"),
                   textInput(
                     "orcid", 
                     label = tags$div(
                       tags$a(href="https://orcid.org/", img(src = "ORCID.png", width="30", height="30")), 
                       " ORCID: ."),
                     value = "https://orcid.org/0000-0001-7049-9597"),
                  textInput("instit", label = "Institución: .", value = "Instituto Nacional de Metrología"),
                  textInput(
                    "ror", 
                    label = tags$div(
                      tags$a(href="https://ror.org/", img(src = "ROR.png", width="30", height="30")), 
                      " ROR: ."),
                    value = "https://ror.org/028s91538")),
          
                   # select
          tags$br(), 
          checkboxInput('BalanzVerif', label = 'Está disponible el certificado de calibración de la balanza', value = TRUE, width = '100%'), 
          tags$hr(), 
          conditionalPanel('input.BalanzVerif', actionButton('Start1', tags$b('Comenzar')))
      ),
      BalanceCalibCertUI('BalanceCalibCert'),
    )
)
