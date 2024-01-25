AmbiDensAireUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('brwz')),
    fluidRow(
      column(3, offset = 1, img(src = "SI_Kelvin.png", width = "90%")),
      column(3, img(src = "SI_pascal.png", width = "90%")),
      column(3, img(src = "SI_mol.png", width = "90%"))),
    tags$hr(),
    SiRealInputUI(ns('Temperatura'), name = 'Temperatura', 20, 2, TemperatureUnits), tags$hr(),
    SiRealInputUI(ns('PressionBar'), name = 'Presión barométrica', 750, 3, AtmosPressuUnits), tags$hr(),
    SiRealInputUI(ns('HumedadRela'), name = 'Humedad relativa', 45, 3, RelatiHumidUnits),
    uiOutput(ns('NiceDensitAir'))
  )
}


AmbiDensAireServer <- function(id, devMode) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa submodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    Temperatura <- SiRealInputServer('Temperatura', devMode = devMode, quantityTypeQUDT = 'Temperature')
    PressionBar <- SiRealInputServer('PressionBar', devMode = devMode, quantityTypeQUDT = 'AmbientPressure')
    HumedadRela <- SiRealInputServer('HumedadRela', devMode = devMode, quantityTypeQUDT = 'RelativeHumidity')
    
    DensitAir <- reactive({
      Temp <- xml_double(xml_find_all(Temperatura(), '//si:value'))
      p <- xml_double(xml_find_all(PressionBar(), '//si:value'))
      h <- xml_double(xml_find_all(HumedadRela(), '//si:value'))
      
      u_Temp <- xml_double(xml_find_all(Temperatura(), '//si:expandedUnc/si:uncertainty')) /
        xml_double(xml_find_all(Temperatura(), '//si:expandedUnc/si:coverageFactor'))
      u_p <- xml_double(xml_find_all(PressionBar(), '//si:expandedUnc/si:uncertainty')) /
        xml_double(xml_find_all(Temperatura(), '//si:expandedUnc/si:coverageFactor'))
      u_h <- xml_double(xml_find_all(HumedadRela(), '//si:expandedUnc/si:uncertainty')) /
        xml_double(xml_find_all(Temperatura(), '//si:expandedUnc/si:coverageFactor'))
      
      unitsENV <- c(
        case_when(
          xml_text(xml_find_all(Temperatura(), '//si:unit')) == '\\degreecelsius' ~ 'deg.C'),
        case_when(
          xml_text(xml_find_all(PressionBar(), '//si:unit')) == '\\hecto\\pascal' ~ 'hPa'),
        case_when(
          xml_text(xml_find_all(HumedadRela(), '//si:unit')) == '\\percent' ~ '%'))
      
      
      return(c(airDensity(Temp = Temp, p = p, h = h, unitsENV = unitsENV),
               uncertAirDensity(model = 'CIMP2007', Temp = Temp, p = p, h = h, u_Temp = u_Temp, u_p = u_p, u_h = u_h,
                                unitsENV = unitsENV, plot = FALSE, printRelSD = FALSE)))
    })
    
    NiceDensitAir <- reactive(tags$div(style = 'background-color:#ebebeb;',
                                       'Densidad local del aire (CIMP2007): ', signif(DensitAir()[1], 7),
                                       ' \u00B1 ', signif(DensitAir()[2], 3), '[g cm', tags$sup('-3'), ']. '))
    output$NiceDensitAir <- renderUI(NiceDensitAir())
    
    AmbientCondInfo <- reactive({
      initiateAmbienteXML('Disoluciones')
    })
    return(AmbientCondInfo)
  })
}