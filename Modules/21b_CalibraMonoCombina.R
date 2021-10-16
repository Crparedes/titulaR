CalibraMonoCombUI <- function(id) {
  ns <- NS(id)
  numericInput(ns('asd'), value = 3, label = 'asd')
}

CalibraMonoCombServer <- function(input, output, session) {
  
}