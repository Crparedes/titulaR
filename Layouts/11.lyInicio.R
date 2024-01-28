inicioLy <- fluidRow(
  tags$hr(), #tags$hr(), #h2(HTML(spcs(5)), tags$b('Herramientas para la validación de métodos')),
  column(
    width = 10, offset = 1, Nlns(3),
    tags$h4(style = 'margin-left: -20px;',
            tags$b('Introducción')),
    h5("Esta aplicación automatiza el tratamiento de los datos de medición para la caracterización de Materiales de Referencia
       Certificados (MRC) por medio de titulaciones gravimétricas, utilizando los siguientes protocolos de medicion:",
       Nlns(),
       tags$ul(
         tags$li(tags$b("M-03-L-14-P-007"), tags$br(),
                 "Determinación de Fracción Másica (Pureza) de la Sal Disódica Dihidratada del
                     Ácido Etilendiaminotetracético por Titulación Complejométrica Gravimétrica"),
         tags$li(tags$b("M-03-L-14-P-008"), tags$br(),
                 "Determinación de Fracción Másica de Iones Plomo, Cadmio y Calcio, en Disoluciones
                     Calibrantes Monoelementales, por Titulación Complejométrica con EDTA")),
       Nlns(2), tags$hr(),
       fluidRow(
         column(2, SI_unit_nice(width = '75%')),
         column(
           6, 'Este aplicativo utiliza el',
           tags$b(tags$a(href = 'https://zenodo.org/records/10230771', 'Esquema del SI Digital (D-SI)', target = '_blank')),
           'para el manejo de datos metrológicos utilizando archivos en formato XML para el almacenamiento y la transferencia
           de la información.', Nlns(),
           'El', tags$b(tags$a(href = 'https://zenodo.org/records/10230771', 'Esquema del SI Digital (D-SI)', target = '_blank')),
           'busca que la información metrológica cumpla con los ',
           tags$b(tags$a(href = 'https://www.go-fair.org/fair-principles/', 'Principios FAIR:', target = '_blank')),
           Nlns(),
           tags$ul(
             tags$li(tags$i(tags$b('F', .noWS = c('after')), 'indability'), spcs(6), '(Datos encontrables)'),
             tags$li(tags$i(tags$b('A', .noWS = c('after')), 'ccessability'), spcs(2), '(Datos accesibles)'),
             tags$li(tags$i(tags$b('I', .noWS = c('after')), 'nteroperability'), '(Datos interoperables)'),
             tags$li(tags$i(tags$b('R', .noWS = c('after')), 'eusability'), spcs(5), '(Datos reusables)')),
           Nlns(),
           'El aplicativo usa la ',
           tags$a(href = 'http://qudt.org/', tags$html('Ontología', img(src = "QUDT.png", height = '14px')), target = '_blank'),
           'para describir tipos de cantidades.', tags$br(),
           tags$div(style = 'font-size:8px', '(Ver tema de discusión en ',
                    tags$a(href = 'https://gitlab1.ptb.de/d-ptb/d-si/xsd-d-si/-/issues/35', 'Repositorio GitLab',
                           target = '_blank', .noWS = 'after'), ')'))),
       tags$hr(), Nlns(3),
       'El aplicativo tiene por los siguientes módulos:',
       tags$ul(
         tags$br(),
         # tags$li("Diligencie la información general en el recuadro que se muestra abajo a la derecha."), tags$br(),
         tags$li(
           actionLink('tabsCertMass', label = tags$b(icon("certificate"), 'Balanzas')),
           tags$br(),
           "Contiene la información de los certificados de calibración de las balanzas y permite
           cargar nuevos archivos de información de calibración creados con el ",
           tags$a(href = 'https://crparedes.shinyapps.io/masscor/', 'aplicativo del paquete masscor.', target = '_blank')),
         tags$li(
           actionLink('tabsCertMRCs', label = tags$b(icon("certificate"), "Materiales de referencia")), tags$br(),
           "Muestra la información de los archivos .xml de los materiales de referencia (RM) y materiales de 
           referencia certificados (MRC) que se utilizan en los procedimientos de medición."),
         tags$li(
           actionLink('tabsSolution', label = tags$b(icon("fill-drip"), "Preparación disoluciones")),tags$br(),
           "Permite crear o cargar archivos .xml con la información de disoluciones estándar
           y disoluciones de muestras."),
         tags$li(
           actionLink('tabsMonoelem', label = tags$b(icon("bong"), "Titular disolución calibrante")), tags$br(),
           "Titulaciones de disoluciones calibrantes monoelementales y 
           generación de archivos de resultados individuales (en formato .xml)."),
         tags$li(
           actionLink('tabsEDTAsalt', label = tags$b(icon("stroopwafel"), "Titular sal de EDTA")), tags$br(),
           "Titulaciones de disoluciones de la sal disódica dihidratada de EDTA y 
           generación de archivos de resultados individuales (en formato .xml)."),
         tags$li(
           actionLink('tabsSummResu', label = tags$b(icon("compass"), "Combinación resultados")), tags$br(),
           "Combinación de archivos de resultados individuales.")
       )
    )
  )
  
  # box(title = div(style = 'font-size:20px;valign="bottom"', tags$b('Inicio')), 
  #     width = 5, status = 'primary', collapsible = TRUE, collapsed = FALSE,#height = '300px',
  #     tags$b("Información general:"),
  #     tags$div(id = "inline", 
  #              dateInput('Fecha', label = 'Fecha: .', language = 'es'),
  #              textInput("nombre", label = "Responsable: .", width = "300px", value = 'Cristhian Paredes'),
  #              textInput("correo", label = "Correo: .", value = "caparedes@inm.gov.co"),
  #              textInput(
  #                "orcid", 
  #                label = tags$div(
  #                  tags$a(href="https://orcid.org/", img(src = "ORCID.png", width="30", height="30")), 
  #                  " ORCID: ."),
  #                value = "https://orcid.org/0000-0001-7049-9597"),
  #              textInput("instit", label = "Institución: .", value = "Instituto Nacional de Metrología"),
  #              textInput(
  #                "ror", 
  #                label = tags$div(
  #                  tags$a(href="https://ror.org/", img(src = "ROR.png", width="30", height="30")), 
  #                  " ROR: ."),
  #                value = "https://ror.org/028s91538")),
  #     
  #     # select
  #     tags$br(), 
  #     checkboxInput('BalanzVerif', label = 'Está disponible el certificado de calibración de la balanza', value = TRUE, width = '100%'), 
  #     tags$hr(), 
  #     conditionalPanel('input.BalanzVerif', actionButton('Start1', tags$b('Comenzar')))
  # ),
  # BalanceCalibCertUI('BalanceCalibCert'),
)

