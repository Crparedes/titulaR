rm(list=ls())
library(shiny)
library(shinydashboard)
library(dashboardthemes) #https://cran.r-project.org/web/packages/dashboardthemes/vignettes/using_dashboardthemes.html
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs) #to use hidden
library(ggplot2) #Grammar of graphics
library(ggfortify)
library(rhandsontable)
library(data.table)
library(masscor)
# icon("flask")

# Por lo general, los módulos_UI son llamados desde las funciones de`` layouts
pack_titRation  <- with(list(pt = 'Package/R/'), paste0(pt, list.files(path = pt)))
modules         <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt))) # El primer m'odulo es el de variables globales  
layouts         <- with(list(pt = 'Layouts/'), paste0(pt, list.files(path = pt))) # functions in the client side
sapply(c(pack_titRation, modules, layouts), source)

ui <- function(request) {
  withMathJax()
  dashboardPage(header = customHeader, sidebar = customSidebar, body = customBody,
                title = "titRation - Instituto Nacional de Metrología de Colombia") #customStuff in ./Layouts
}

server <- function(input, output, session) {
  formatP  <- reactive(input$Format)
  dimensP  <- reactive(c(input$plotsW, input$plotsH) / 25.4 * 1.6)
  
  callModule(module = BalanceCalibCertServer, id = 'BalanceCalibCert')
  callModule(module = SolidMRCServer, id = 'ModuloDisolucionEDTA', reagKey = 'EDTA')
  callModule(module = SolidMRCServer, id = 'ModuloDisolucionPbNO3.2', reagKey = 'Pb')
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionCobre', reagKey = 'Cu')
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionZinc', reagKey = 'Zn')
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
