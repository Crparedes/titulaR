AmbiDensAireUI <- function(id) {
  ns <- NS(id)
  box(
    title = tags$b(style = 'font-size: 13px;', 'Condiciones ambientales'), id = 'condAmbiBox',
    width = 12, collapsible = TRUE, collapsed = FALSE,
    style = '.box-header .box-title {}',
    tags$b('Temperatura'),
    fluidRow(
      style = 'margin-left:10px;',
      column(3, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('Temp'), label = NULL, value = 18)),
      column(3, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('u_Temp'), label = '\u00B1', value = 2)),
      column(3, selectInput(ns('units_Temp'), label = NULL, choices = TemperatureUnits))),
    fluidRow(
      column(4, offset = 2, selectInput(ns('covFac_Temp'), label = 'Factor de cobertura', choices = CobertureFactors)),
      column(3, selectInput(ns('Distri_Temp'), label = 'Distribución', choices = Distributions))),
    
    tags$hr(), tags$b('Presión barométrica'),
    fluidRow(
      style = 'margin-left:10px;',
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('BarPres'), label = NULL, value = 750)),
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('u_BarPres'), label = '\u00B1', value = 2)),
      column(2, selectInput(ns('units_BarPres'), label = NULL, choices = AtmosPressuUnits))),
    fluidRow(
      column(4, offset = 2, selectInput(ns('covFac_BarPres'), label = 'Factor de cobertura', choices = CobertureFactors)),
      column(3, selectInput(ns('Distri_BarPres'), label = 'Distribución', choices = Distributions))),
    
    tags$hr(), tags$b('Humedad relativa'),
    fluidRow(
      style = 'margin-left:10px;',
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('relHum'), label = NULL, value = 45)),
      column(2, autonumericInput(digitGroupSeparator = " ", decimalCharacter = ".", modifyValueOnWheel = FALSE,
                                 ns('u_relHum'), label = '\u00B1', value = 3)),
      column(2, selectInput(ns('units_relHum'), label = NULL, choices = RelatiHumidUnits))),
    fluidRow(
      column(4, offset = 2, selectInput(ns('covFac_relHum'), label = 'Factor de cobertura', choices = CobertureFactors)),
      column(3, selectInput(ns('Distri_relHum'), label = 'Distribución', choices = Distributions)))
  )
}


AmbiDensAireServer <- function(id, devMode) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa submodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    DensitAir <- reactive(c(airDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1),
                            uncertAirDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                                             u_Temp = input$u_Temp1, u_p = input$u_BarPres1,
                                             u_h = input$u_relHum1, printRelSD = FALSE)))
    
    NiceDensitAir <- reactive(tags$div(style = 'font-size:11px',
                                       'Densidad local del aire (CIMP2007): ', 
                                       signif(DensitAir()[1], 7), ' \u00B1 ', signif(DensitAir()[2], 3), '[g cm', tags$sup('-3'), ']'))
    output$NiceDensitAir <- renderUI(NiceDensitAir())

    

    
  })
  return()
}