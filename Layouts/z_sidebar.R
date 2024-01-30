# i18n <- Translator$new(translation_json_path = "translation.json")
# i18n$set_translation_language("es") # here you select the default translation to display

customSidebar <- dashboardSidebar(
  tags$style(".left-side, .main-sidebar {padding-top: 110px;}"), # font-size: larger
  width = 320,
  sidebarMenu(
    id = "tabs", # tags$br(), tags$br(),
    menuItem(#i18n$t("Inicio"),
             'Inicio',
             tabName = "tabsInicio", icon = icon("info-circle")),
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
    splitLayout(
      materialSwitch('Demo', 'Demo', value = TRUE, status = 'primary'),
      disabled(materialSwitch('Desarrollador', 'Devel', value = FALSE, status = 'primary')))
  ))