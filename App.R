rm(list = ls())
gc()        

# Devel
library(shiny)
library(shinydashboard)
library(dashboardthemes) #https://cran.r-project.org/web/packages/dashboardthemes/vignettes/using_dashboardthemes.html
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs) #to use hidden
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(shinyalert)
library(rhandsontable)
library(data.table)
library(outliers)
library(FrF2)
library(purrr) # map-like functional programing (?)
library(units)
library(masscor)
# library(CIAAWconsensus)
# install.packages("devtools")
library(xml2)
# library(XML) # to use XML::xmlCleanNamespaces()
library(dplyr)
library(stringr)

library(propagate)
# library(shinyTime)

# library(shinydashboardPlus)

# library(shiny.i18n)
# i18n <- Translator$new(translation_json_path = "translation.json")
# i18n$set_translation_language("es") # here you select the default translation to display

# rm(print.xml_document, print.xml_node)

source('D_SI_xml/0_createMRXML.R')

# Por lo general, los módulos_UI son llamados desde las funciones de layouts
functiFiles <- list.files(path = "Functions", full.names = TRUE)
titRatFiles <- list.files(path = "titRation", full.names = TRUE)
moduleFiles <- list.files(path = "Modules", full.names = TRUE)
layoutFiles <- list.files(path = "Layouts", full.names = TRUE)
sapply(c(functiFiles, titRatFiles, moduleFiles, layoutFiles), source)

numInput <- function(...) {autonumericInput(..., digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE)}

ui <- fluidPage(
  # shiny.i18n::usei18n(i18n),
  
  dashboardPage(header = customHeader, sidebar = customSidebar, body = customBody,
                  title = "titulaR - Instituto Nacional de Metrología") #customStuff in ./Layouts
)

server <- function(input, output, session) {
print.xml_document <- print.xml_node <- function (x) message(x)

  # shinyalert(title = 'Advertencia', text = , "Este aplicativo está en desarrollo", showConfirmButton = FALSE,
  #            closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE, type = "info", timer = 7500)
  # 
  devMode <- reactive(input$Desarrollador)
  observeEvent(input$brwz, browser())
  
  demo <- reactive(input$Demo)
  fecha <- reactive(input$fecha)
  dateTimeISO8601 <- reactive({invalidateLater(1000); iso8601(fecha(), niceHTML = TRUE)})
  output$dateTimeISO8601 <- renderUI(dateTimeISO8601())
  
  BalanzasReVa <- reactiveValues(DCC = list())
  StandardSampleSolutions <- reactiveValues(solutions = list())
  PartialTitrationResults <- reactiveValues(results = list())
  
  observeEvent(demo(), {
    if(demo()) {
      StandardSampleSolutions$solutions <- append(
        StandardSampleSolutions$solutions, lapply(list.files('www/DemoFiles/Solutions/', full.names = TRUE), function(x) reactive(read_xml(x))))
      BalanzasReVa$DCC <- append(BalanzasReVa$DCC, list(balanzasList[[4]]))
      PartialTitrationResults$results <- append(
        PartialTitrationResults$results, lapply(list.files('www/DemoFiles/Results/', full.names = TRUE), function(x) reactive(read_xml(x))))
  }})
  
  
  BalanceCalibCertServer('Balanzas', devMode = devMode, demo = demo, BalanzasReVa = BalanzasReVa)
  MateReferDC <- MaterialesRefereServer('MateRefe', devMode = devMode, demo = demo)
  PreparaDisolucioServer(
    'Solution', devMode = devMode, balanzas = BalanzasReVa, materiales = MateReferDC, fecha = fecha, demo = demo,
    StandardSampleSolutions = StandardSampleSolutions)
  
  
  SolOrder <- reactive({
    solTypes <- sapply(StandardSampleSolutions$solutions, function(x) {
      if(!is.error(x())) {
        return(xml_text(xml_find_all(x(), xpath = '//mr:solutionType')))
      } else {return(NULL)}
    })
    list(EstandarEDTA = which(solTypes == 'EstandarEDTA'), MuestraCalib = which(solTypes == 'MuestraCalib'),
         EstandarPlomo = which(solTypes == 'EstandarPlomo'), MuestraEDTA = which(solTypes == 'MuestraEDTA'))})
  
  TitMonoelem <- TitularMonoelemtServer(
    'MonoElem', devMode = devMode,  demo = demo,  fecha = fecha, balanzas = BalanzasReVa,
    EstandarEDTA = reactive(StandardSampleSolutions$solutions[SolOrder()$EstandarEDTA]),
    MuestraCalib = reactive(StandardSampleSolutions$solutions[SolOrder()$MuestraCalib]),
    PartialTitrationResults = PartialTitrationResults)
  
  CombinaResultadosServer('Combina', devMode = devMode, demo = demo, fecha = fecha, PartialTitrationResults = PartialTitrationResults)
  
  
  observeEvent(input$tabsCertMass, updateTabItems(session, "tabs", 'tabsCertMass'))
  observeEvent(input$tabsCertMRCs, updateTabItems(session, "tabs", 'tabsCertMRCs'))
  observeEvent(input$tabsSolution, updateTabItems(session, "tabs", 'tabsSolution'))
  observeEvent(input$tabsMonoElem, updateTabItems(session, "tabs", 'tabsMonoElem'))
  observeEvent(input$tabsEDTAsalt, updateTabItems(session, "tabs", 'tabsEDTAsalt'))
  observeEvent(input$tabsSummResu, updateTabItems(session, "tabs", 'tabsSummResu'))
  
}

onStop(function() {rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv); gc()})
  
shinyApp(ui = ui, server = server, enableBookmarking = "url")
