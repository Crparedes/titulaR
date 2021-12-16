library(shiny)
library(slickR)

carousel_ui <- function(id){
  ns <- NS(id)
  slickROutput(ns("slickr"), width="100%")
}

carousel_module <- function(input, output, session) {
  output$slickr <- renderSlickR({
    imgs <- list.files("~/Desktop/imgs", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
}

my_tab <- function(input,output,session,parent_session,tab_element,tab_name){
  
  ns = session$ns
  
  appendTab(inputId = "test_tabs",
            tabPanel(
              title = tab_name,
              value = tab_name,
              carousel_ui(ns("carousel")) # Operating in the parent session so explicitly supply the namespace
            ),
            session = parent_session
  )
  
  updateTabsetPanel(parent_session, "test_tabs", selected = tab_name) # Refer to test_tabs from the parent namespace
  
  # Need to update the carousel every time the user clicks on a tab
  # Else the carousel is only updated on the latest tab created
  
  observeEvent(tab_element(),{
    req(tab_element())
    
    if(tab_element() == tab_name){
      cat("Running\n")
      callModule(carousel_module,"carousel")# This module knows the namespace so no need to supply the namespace
    }
  })
  
}

ui <- fluidPage(  
  tabsetPanel(id = "test_tabs",
              tabPanel(
                title = "First tab",
                value = "page1",
                fluidRow(textInput('new_tab_name', 'New tab name'),
                         actionButton('add_tab_button','Add'))
              )
  )
)


server <- function(input, output, session) {
  
  tab_list <- NULL
  
  observeEvent(input$add_tab_button,{
    
    tab_title <- input$new_tab_name
    callModule(my_tab,tab_title,session,reactive(input$test_tabs),input$new_tab_name)
    
  })
}

shinyApp(ui, server)
