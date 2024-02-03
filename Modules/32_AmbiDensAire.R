AmbiDensAireUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('brwz')),
    # fluidRow(
      # column(3, offset = 1, SI_unit_nice('kelvin')),
      # column(3, SI_unit_nice('pascal', derived = TRUE)),
      # column(3, SI_unit_nice('mole'))),
    SiRealInputUI(ns('Temperatura'), name = ReqField('Temperatura'), 20, 2, TemperatureUnits), tags$hr(),
    SiRealInputUI(ns('PressionBar'), name = ReqField('Presión barométrica'), 750, 3, AtmosPressuUnits), tags$hr(),
    SiRealInputUI(ns('HumedadRela'), name = ReqField('Humedad relativa'), 45, 3, RelatiHumidUnits), tags$hr(), uiOutput(ns('NiceDensitAir')),
  )
}


AmbiDensAireServer <- function(id, devMode, fecha) {
  moduleServer(id, function(input, output, session) {
    output$brwz <- renderUI(if(devMode()) {
      tags$div(actionButton(session$ns('brwzInsideModule'), tags$b('Pausa submodulo')), tags$hr())})
    observeEvent(input$brwzInsideModule, browser())
    
    Temperatura <- SiRealInputServer('Temperatura', devMode = devMode, quantityTypeQUDT = 'Temperature',
                                     SIdigRef = TRUE, unit = 'kelvin', SIalign = 'left', width = '330%', SIcol = 1)
    PressionBar <- SiRealInputServer('PressionBar', devMode = devMode, quantityTypeQUDT = 'AmbientPressure',
                                     SIdigRef = TRUE, unit = 'pascal', derived = TRUE, SIalign = 'left', width = '330%', SIcol = 1)
    HumedadRela <- SiRealInputServer('HumedadRela', devMode = devMode, quantityTypeQUDT = 'RelativeHumidity',
                                     SIdigRef = TRUE, unit = 'mole', SIalign = 'left', width = '330%', SIcol = 1)
    
    DensitAir <- reactive({
      Temp <- GetValueEstandUncert(Temperatura())
      p <- GetValueEstandUncert(PressionBar())
      h <- GetValueEstandUncert(HumedadRela())
      
      unitsENV <- c(case_when(Temp$Units == '\\degreecelsius' ~ 'deg.C'),
                    case_when(p$Units == '\\hecto\\pascal' ~ 'hPa'),
                    case_when(h$Units == '\\percent' ~ '%'))
      
      DensitAir <- SiRealXML(
        quantityTypeQUDT = 'Density', 
        value = signif(digits = 7, airDensity(Temp = Temp[[1]][1], p = p[[1]][1], h = h[[1]][1], unitsENV = unitsENV)), 
        units = '\\gram\\centi\\meter\\tothe{-3}',
        uncert = signif(digits = 3, uncertAirDensity(model = 'CIMP2007', Temp = Temp[[1]][1], p = p[[1]][1], h = h[[1]][1],
                                                     u_Temp = Temp[[1]][2], u_p = p[[1]][2], u_h = h[[1]][2],
                                                     unitsENV = unitsENV, plot = FALSE, printRelSD = FALSE)),
        covFac = 1, distribution = 'normal')
      
      return(DensitAir)
    })
    
    NiceDensitAir <- reactive(tags$div(style = 'background-color:#ebebeb;',
                                       'Densidad local del aire (Fórmula CIMP2007): ', GetValueEstandUncert(DensitAir())$ValUnc[1], 
                                       ' \u00B1 ', GetValueEstandUncert(DensitAir())$ValUnc[2], '/ g cm', tags$sup('-3'), ' '))
    output$NiceDensitAir <- renderUI(NiceDensitAir())
    
    AmbientCondInfo <- reactive({
      ambientXML <- read_xml(genericHeading('mr:ambiente'))
      ambientXML %>% {
        xml_add_child(., 'mr:airDensity') %>% xml_add_child(., .value = DensitAir()) 
        xml_add_child(., 'mr:ambientConditions') %>% {
          xml_add_child(., .value = Temperatura())
          xml_add_child(., .value = PressionBar())
          xml_add_child(., .value = HumedadRela())}
        xml_add_child(., 'mr:timeISO8601', iso8601(fecha(), niceHTML = FALSE))}
      return(ambientXML)
    })
    return(AmbientCondInfo)
  })
}