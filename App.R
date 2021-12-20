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
  useShinyjs()
  dashboardPage(header = customHeader, sidebar = customSidebar, body = customBody,
                title = "titRation - Instituto Nacional de Metrología de Colombia") #customStuff in ./Layouts
}

server <- function(input, output, session) {
  #observeEvent(input$brwz, browser())
  # Inicializaci'on
  IDUsuario  <- reactive(c(input$nombre, input$correo))
  observeEvent(input$Start1, updateTabItems(inputId = 'tabs', selected = 'MRC_DisTab'))
  callModule(module = BalanceCalibCertServer, id = 'BalanceCalibCert')
  
  # Disoluciones de MRCs
  DisEDTA_MRC <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionEDTA', reagKey = 'EDTA', IDUsuario = IDUsuario)
  DisPb_MRC   <- callModule(module = SolidMRCServer, id = 'ModuloDisolucionPbNO3.2', reagKey = 'Pb', IDUsuario = IDUsuario)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionCobre', reagKey = 'Cu', IDUsuario = IDUsuario)
  callModule(module = LiquidMRCServer, id = 'ModuloDilucionZinc', reagKey = 'Zn', IDUsuario = IDUsuario)
  
  CalibMonoDelDia <- reactiveValues()
  # Titulaciones disoluciones calibrantes monoelementales
  observeEvent(input$MonoElemInitTit, {
    req(input$MonoElemInitTit > 0)
    Elemento <- reactive(input$Elemento)
    LeadAM <- reactive(input$LeadAM)
    u_LeadAM <- reactive(input$u_LeadAM) 
    sampleID <- reactive(input$sampleID)
    dscrMuestraMonoelemTit <- reactive(input$dscrMuestraMonoelemTit) 
    BalanzaMonoelemTit <- reactive(input$BalanzaMonoelemTit)
    MonoElemNumber <- reactive(input$MonoElemInitTit)
    # browser()
    CalibMonoDelDia[[paste0(Elemento(), "_", sampleID(), ".", as.character(isolate(MonoElemNumber())), "_", format(Sys.time(), '%Y-%m-%d_%H-%M'), ".tit")]] <- 
      callModule(module = CalibraMonoIndividualServer, id = paste0('monoElemTit', input$MonoElemInitTit),
                 Elemento = Elemento, LeadAM = LeadAM, u_LeadAM = u_LeadAM,
                 sampleID = sampleID, dscrMuestraMonoelemTit = dscrMuestraMonoelemTit, 
                 BalanzaMonoelemTit = BalanzaMonoelemTit,
                 DisEDTA_MRC = DisEDTA_MRC, IDUsuario = IDUsuario, number = isolate(MonoElemNumber))
    prependTab(inputId = 'monoElemTabBox', CalibraMonoIndividualUI(id = paste0('monoElemTit', input$MonoElemInitTit)), select = TRUE)
    
  })
  
  
  output$PrintDisEDTA <- renderUI(
    box(width = 12, title = tags$b('Disolucion titulante de EDTA'),  color = 'black',
        renderPrint(
          tryCatch(rbind(
            paste0('MRC: ', DisEDTA_MRC$infoDisMRC()$`MRC empleado`, ': '),
            paste0(' Concentración: ', signif(DisEDTA_MRC$infoDisMRC()$`Concentracion [mmol/kg]`, 6), ' \u00B1 ',
                   signif(DisEDTA_MRC$infoDisMRC()$`Incertidumbre [mmol/kg]`, 2), ' [mmol/kg]'),
            paste0(' Preparación: ', DisEDTA_MRC$infoDisMRC()$`Fecha de preparacion`, '.')),
            error = function(cond) {rbind('Sin informacion de la disolucion titulante.', 'Dirijase al modulo de MRCs y disoluciones.')}))))
  
  callModule(module = CalibraMonoCombServer, id = 'CalibraMonoComb1', IDUsuario = IDUsuario) 
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
