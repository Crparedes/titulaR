inicioLy <- fluidRow(
  tags$hr(), #tags$hr(), #h2(HTML(spcs(5)), tags$b('Herramientas para la validación de métodos')),
  column(
    width = 10, offset = 1, Nlns(1),
    tags$h4(style = 'margin-left: -20px;',
            tags$b('Introducción')),
    h5("Esta aplicación automatiza el tratamiento de los datos de medición para la caracterización de Materiales de Referencia
       Certificados (MRC) por medio de titulaciones gravimétricas complejométricas.", tags$br(),
       "Procedimientos de medición implementados:",
       Nlns(),
       tags$ul(
         tags$li(tags$b("M-03-L-14-P-007: "), #tags$br(),
                 "Determinación de Fracción Másica (Pureza) de la Sal Disódica Dihidratada del
                     Ácido Etilendiaminotetracético por Titulación Complejométrica Gravimétrica"), tags$br(),
         tags$li(tags$b("M-03-L-14-P-008"), #tags$br(),
                 "Determinación de Fracción Másica de Iones Plomo, Cadmio y Calcio, en Disoluciones
                     Calibrantes Monoelementales, por Titulación Complejométrica con EDTA")),
       Nlns(2), tags$hr(),
       fluidRow(
         column(2, tags$a(href = 'https://www.bipm.org/en/publications/si-brochure', 
                          img(src = "SI_units.png", width = '85%', alt = 'SI brochure'),
                          target = '_blank')),
         column(
           8, 'Este aplicativo utiliza el',
           tags$b(tags$a(href = 'https://zenodo.org/records/10230771', 'Esquema del SI Digital (D-SI)', target = '_blank')),
           'para el manejo de datos metrológicos, utilizando archivos en formato XML para el almacenamiento y la transferencia
           de la información.', Nlns(),
           'El', tags$b('Esquema del SI Digital (D-SI)'),# tags$a(href = 'https://zenodo.org/records/10230771', '', target = '_blank')
           'busca que la información metrológica cumpla con los ',
           tags$b(tags$a(href = 'https://www.go-fair.org/fair-principles/', 'Principios FAIR:', target = '_blank')),
           Nlns(),
           tags$ul(
             tags$li(tags$i(tags$b('F', .noWS = c('after')), 'indability'), spcs(6), '(Datos encontrables)'),
             tags$li(tags$i(tags$b('A', .noWS = c('after')), 'ccessability'), spcs(2), '(Datos accesibles)'),
             tags$li(tags$i(tags$b('I', .noWS = c('after')), 'nteroperability'), '(Datos interoperables)'),
             tags$li(tags$i(tags$b('R', .noWS = c('after')), 'eusability'), spcs(5), '(Datos reusables)')),
           Nlns(), tags$hr(), Nlns(),
           'La información metrológica generada en la app incorpora además los siguientes identificadores digitales: ', Nlns(),
           tags$ul(
             tags$li(
               tags$a(href = 'http://qudt.org/', style = 'color:#00428c;',
                      tags$html(img(src = "QUDT.png", height = '17px'),
                                tags$b('- Quantities, Units, Dimensions, and Types Ontology,')), target = '_blank'),
               'para describir tipos de cantidades.', 
               tags$div(style = 'font-size:8px', '(De acuerdo con el',
                        tags$a(href = 'https://gitlab1.ptb.de/d-ptb/d-si/xsd-d-si/-/issues/35', 'tema de discusión en GitLab',
                               target = '_blank', .noWS = 'after'), ')')),
             tags$br(),
             tags$li(
               tags$a(href = 'https://www.inchi-trust.org/', style = 'color:#14602f;',
                      tags$html(img(src = "InChI.png", height = '27px'),
                                tags$b('- IUPAC International Chemical Identifier,')), target = '_blank'),
               'para relacionar sustancias químicas'),
             tags$br(),
             tags$li(
               tags$a(href = 'https://orcid.org/', style = 'color:#a6ce39;',
                      tags$html(img(src = "ORCIDLong.png", height = '18px'),
                                tags$b('- Open Researcher and Contributor ID,')), target = '_blank'),
               'para relacionar personas responsables de ejecutar una etapa del análisis.')),
           tags$br(),
       )),
       tags$hr(), 
       conditionalPanel('input.partesAplicativo == 0', actionLink('partesAplicativo', 'Descripción de módulos del aplicativo...')),
       conditionalPanel(
         'input.partesAplicativo > 0',               
         tags$b('El aplicativo tiene por los siguientes módulos:'),
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

