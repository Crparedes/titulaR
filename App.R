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


library(xml2)
library(dplyr)
library(stringr)

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
  output$brwz <- renderUI(if(devMode()) actionButton(inputId = 'brwz', label = tags$b('Pausar aplicativo'), width = '70%'))
  observeEvent(input$brwz, browser())
  
  BalanzasDCC <- BalanceCalibCertServer('Balanzas', devMode = devMode)
  MateReferDC <- MaterialesRefereServer('MateRefe', devMode = devMode)
  DisolInfoPC <- PreparaDisolucioServer('Disoluci', devMode = devMode, balanzas = BalanzasDCC, materiales = MateReferDC)
  
  observeEvent(input$tabsCertMass, updateTabItems(session, "tabs", 'tabsCertMass'))
  observeEvent(input$tabsCertMRCs, updateTabItems(session, "tabs", 'tabsCertMRCs'))
  observeEvent(input$tabsSolution, updateTabItems(session, "tabs", 'tabsSolution'))
  observeEvent(input$tabsMonoElem, updateTabItems(session, "tabs", 'tabsMonoElem'))
  observeEvent(input$tabsEDTAsalt, updateTabItems(session, "tabs", 'tabsEDTAsalt'))
  observeEvent(input$tabsSummResu, updateTabItems(session, "tabs", 'tabsSummResu'))
  
  
  
}

onStop(function() {rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv); gc()})
  
shinyApp(ui = ui, server = server, enableBookmarking = "url")
