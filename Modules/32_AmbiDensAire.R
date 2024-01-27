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
      Temp <- GetValueEstandUncert(Temperatura())
      p <- GetValueEstandUncert(PressionBar())
      h <- GetValueEstandUncert(HumedadRela())
      
      unitsENV <- c(case_when(Temp$Units == '\\degreecelsius' ~ 'deg.C'),
                    case_when(p$Units == '\\hecto\\pascal' ~ 'hPa'),
                    case_when(h$Units == '\\percent' ~ '%'))
      
      return(c(airDensity(Temp = Temp[[1]][1], p = p[[1]][1], h = h[[1]][1], unitsENV = unitsENV),
               uncertAirDensity(model = 'CIMP2007', Temp = Temp[[1]][1], p = p[[1]][1], h = h[[1]][1],
                                u_Temp = Temp[[1]][2], u_p = p[[1]][2], u_h = u[[1]][2],
                                unitsENV = unitsENV, plot = FALSE, printRelSD = FALSE)))
    })
    
    NiceDensitAir <- reactive(tags$div(style = 'background-color:#ebebeb;',
                                       'Densidad local del aire (CIMP2007): ', signif(DensitAir()[1], 7),
                                       ' \u00B1 ', signif(DensitAir()[2], 3), '[g cm', tags$sup('-3'), ']. '))
    output$NiceDensitAir <- renderUI(NiceDensitAir())
    
    AmbientCondInfo <- reactive({
      XX <- initiateAmbienteXML('Disoluciones')
      xml_add_child(.x = xml_child(XX, 'mr:ambientConditions'), .value = Temperatura())
      xml_add_child(.x = xml_child(XX, 'mr:ambientConditions'), .value = PressionBar())
      xml_add_child(.x = xml_child(XX, 'mr:ambientConditions'), .value = HumedadRela())
    })
    return(AmbientCondInfo)
  })
}