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
library(propagate)
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
  IDUsuario  <- reactive(c(input$nombre, input$correo))
  
  callModule(module = BalanceCalibCertServer, id = 'BalanceCalibCert')
  DisEDTA_MRC <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionEDTA', reagKey = 'EDTA', IDUsuario = IDUsuario)
  DisPb_MRC   <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionPbNO3.2', reagKey = 'Pb', IDUsuario = IDUsuario)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionCobre', reagKey = 'Cu', IDUsuario = IDUsuario)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionZinc', reagKey = 'Zn', IDUsuario = IDUsuario)
  
  MonoElemTitrations <- reactiveValues(allTabs = list())
  observeEvent(input$MonoElemInitTit, {
    req(input$MonoElemInitTit > 0)
    callModule(module = CalibraMonoIndividualServer, id = input$MonoElemInitTit,
               Elemento = input$Elemento, LeadAM = input$LeadAM, u_LeadAM = input$u_LeadAM,
               sampleID = input$sampleID, dscrMuestraMonoelemTit = input$dscrMuestraMonoelemTit, 
               BalanzaMonoelemTit = input$BalanzaMonoelemTit,
               DisEDTA_MRC = input$DisEDTA_MRC, IDUsuario = input$IDUsuario)
    prependTab(inputId = 'monoElemTabBox', CalibraMonoIndividualUI(id = input$MonoElemInitTit), select = TRUE)
  })
  
  #callModule(module = CalibraMonoServer, id = 'CalibraMono1', DisEDTA_MRC = DisEDTA_MRC, IDUsuario)
  callModule(module = CalibraMonoCombServer, id = 'CalibraMonoComb1', IDUsuario)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
