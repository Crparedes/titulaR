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
# icon("flask")

# Por lo general, los módulos_UI son llamados desde las funciones de layouts
pack_titRation  <- with(list(pt = 'Package/R/'), paste0(pt, list.files(path = pt)))
modules         <- with(list(pt = 'Modules/'), paste0(pt, list.files(path = pt)))
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
  matrixInputDF <- eventReactive(input$GenerarInputDF,
                                 matrix(c(0, rep(NA, input$NrowsDF * input$NcolsDF - 2), 0.001),
                                        nrow = input$NrowsDF, ncol = input$NcolsDF))

  TrnsDt0  <- eventReactive(input$GenerarInputDF, {reactiveValues(hot = data.table(matrixInputDF()))})
  
  TrnsDt <- eventReactive(input$GenerarInputDF, {
    DT <- NULL
    if (!is.null(input$TrnsDt)) {
      DT <-  setDT(hot_to_r(input$TrnsDt))
      TrnsDt0()[["hot"]]  <-  DT
    } else {
      if (!is.null(TrnsDt0()[["hot"]])) {DT <- TrnsDt0()[["hot"]]}
      }
    if (!is.null(DT)) {
      rhandsontable(DT)
      }
    })

  output$TrnsDt <- renderRHandsontable(TrnsDt())
  TrnsDtEx <- reactive({
    req(input$TrnsDt)#if(is.null(input$TrnsDt)) return(NULL)
    DF <- hot_to_r(input$TrnsDt)
    return(as.data.frame(DF[, !is.na(DF[1, ]), with = FALSE]))})

  datSeries <- list(data = TrnsDtEx, descr = reactive(input$dataDescrip))

}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
