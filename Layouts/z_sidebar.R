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
    tags$hr(), 
    menuItem(("Combinación de resultados"), tabName = "tabsSummResu", icon = icon("compass")),
    Nlns(2),
    menuItem("Curva de titulación genérica", tabName = "tabsGenerica", icon = icon("bong")),
    Nlns(5),
    materialSwitch('Desarrollador', 'devel', status = 'primary'),
    uiOutput('brwz')
  ))