library(shiny)
library(shinyjs)

# function which checks the data; returns TRUE or FALSE
checkData <- function(dat){
  TRUE
}

# function which transforms the data; returns NULL if check not TRUE
processData <- function(dat){
  if(checkData(dat)){
    # do something with dat
    names(dat) <- toupper(names(dat)) # for our example
    return(dat)
  }else{
    return(NULL)
  }
}

ui <- fluidPage(
  useShinyjs(),
  conditionalPanel(
    "true", # always hide the download button
    downloadButton("downloadData")
  ),
  actionButton("check", "Download")
)

server <- function(input, output, session){
  
  dat <- mtcars
  
  finalData <- reactiveVal() # to store the processed data
  observeEvent(input$check, {
    if(!is.null(df <- processData(dat))){
      finalData(df)
      runjs("$('#downloadData')[0].click();")
    }else{
      # something which throws an alert message "invalid data" 
      # (eg with shinyBS::createAlert or shinyWidgets::sendSweetAlert)
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(finalData(), file)
    }
  )
}

shinyApp(ui, server)