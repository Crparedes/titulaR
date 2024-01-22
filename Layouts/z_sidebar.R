customSidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 110px;}"), # font-size: larger
  width = 300, withMathJax(),
  sidebarMenu(
    id = "tabs", # tags$br(), tags$br(),
    menuItem(("Inicio"), tabName = "tabsInicio", icon = icon("info-circle")),
    tags$hr(),
    tags$b(HTML(spcs(2), 'Trazabilidad metrológica')),
    menuItem(("Balanzas"), tabName = "tabsCertMass", icon = icon("certificate")),
    menuItem(("Materiales de referencia"), tabName = "tabsCertMRCs", icon = icon("certificate")), 
    menuItem(("Preparación disoluciones"), tabName = "tabsSolution", icon = icon("fill-drip")),
    tags$hr(),
    tags$b(HTML(spcs(2), 'Complejometrías')),
    menuItem(("Titular disolución calibrante"), tabName = "tabsMonoElem", icon = icon("bong")),
    menuItem(("Titular sal de EDTA"), tabName = "tabsEDTAsalt", icon = icon("stroopwafel")),
    tags$hr(),
    # tags$b(HTML('&ensp;'), 'Ácido-Base'),
    # menuItem("", tabName = "Acido1", icon = icon("bong")),
    
    menuItem(("Combinación de resultados"), tabName = "tabsSummResu", icon = icon("compass")),
    menuItem("Curva de titulación genérica", tabName = "tabsGenerica", icon = icon("bong")),
    Nlns(2), uiOutput('dateTimeISO8601'), Nlns(2),
    conditionalPanel('input.Desarrollador',
                     div(id = 'inline', style = 'font-size:12px;',
                         tags$hr(), spcs(5), dateInput('fecha', label = NULL, language = 'es')),
                     # timeInput("Hora", "Modificar hora: (hh:mm:ss)", value = Sys.time()),
                     actionButton('brwz', label = tags$b('Pausar aplicativo'), width = '70%')),
    materialSwitch('Desarrollador', 'devel', status = 'primary')
  ))