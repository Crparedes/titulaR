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


library(xml2)
library(dplyr)
library(stringr)
# library(shinyTime)

# How many users are connected to my Shiny application? https://stackoverflow.com/questions/47728208
users <- reactiveValues(count = 0)



# Por lo general, los módulos_UI son llamados desde las funciones de layouts
functiFiles <- list.files(path = "Functions", full.names = TRUE)
titRatFiles <- list.files(path = "titRation", full.names = TRUE)
moduleFiles <- list.files(path = "Modules", full.names = TRUE)
layoutFiles <- list.files(path = "Layouts", full.names = TRUE)
sapply(c(functiFiles, titRatFiles, moduleFiles, layoutFiles), source)

numInput <- function(...) {autonumericInput(..., digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE)}

ui <- function(request) {
  withMathJax()
  dashboardPage(header = customHeader, sidebar = customSidebar, body = customBody,
                  title = "titulaR - Instituto Nacional de Metrología") #customStuff in ./Layouts
    
}

server <- function(input, output, session) {
  # shinyalert(title = 'Advertencia', text = , "Este aplicativo está en desarrollo", showConfirmButton = FALSE,
  #            closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE, type = "info", timer = 7500)
  # 
  
  devMode <- reactive(input$Desarrollador)
  observeEvent(input$brwz, browser())
  
  BalanzasDCC <- BalanceCalibCertServer('Balanzas', devMode = devMode)
  MateReferDC <- MaterialesRefereServer('MateRefe', devMode = devMode)
  DisolInfoPC <- PreparaDisolucioServer(
    'Disoluci', devMode = devMode, dateTime = dateTimeISO8601, balanzas = BalanzasDCC, materiales = MateReferDC)
  
  observeEvent(input$tabsCertMass, updateTabItems(session, "tabs", 'tabsCertMass'))
  observeEvent(input$tabsCertMRCs, updateTabItems(session, "tabs", 'tabsCertMRCs'))
  observeEvent(input$tabsSolution, updateTabItems(session, "tabs", 'tabsSolution'))
  observeEvent(input$tabsMonoElem, updateTabItems(session, "tabs", 'tabsMonoElem'))
  observeEvent(input$tabsEDTAsalt, updateTabItems(session, "tabs", 'tabsEDTAsalt'))
  observeEvent(input$tabsSummResu, updateTabItems(session, "tabs", 'tabsSummResu'))
  
  
  dateTimeISO8601 <- reactive({
    invalidateLater(1000)
    tm <- as.POSIXlt(Sys.time())
    if (devMode()) tm <- as.POSIXct(sub(Sys.Date(), input$Fecha, Sys.time()))
    tm_iso8601 <- sub('(+[0-9]{2})([0-9]{2}$)','\\1:\\2', strftime(tm, "%Y-%m-%dT%H:%M:%S%z") , fixed=FALSE)
  })
  output$dateTimeISO8601 <- renderUI(tags$div(
    style = 'font-size:12px;', spcs(5),
    tags$a(href = 'https://www.iso.org/iso-8601-date-and-time-format.html', tags$b('ISO 8601:'), target = '_blank'),
    dateTimeISO8601()))
  
}

onStop(function() {rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv); gc()})
  
shinyApp(ui = ui, server = server, enableBookmarking = "url")
