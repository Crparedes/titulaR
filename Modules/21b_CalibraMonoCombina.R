CalibraMonoCombUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    #verbatimTextOutput(ns('test')),
    column(1, tags$br()),
    column(6, tags$br(), tags$br(), 
           fileInput(ns('ResFiles'), width = '100%', 
                     label = 'Escoja los archivos de resultados individuales', multiple = TRUE, accept = '.res')
    ))
  
}

CalibraMonoCombServer <- function(input, output, session) {
  
}