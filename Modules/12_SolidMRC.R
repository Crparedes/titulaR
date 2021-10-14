SolidMRCUI <- function(id, reagent, reagKey, explan, nu = FALSE) {
  ns <- NS(id)
  box(title = div(style = 'font-size:18px;valign="bottom"', tags$b('MRC de ', reagent)), 
      width = 12, status = 'primary', collapsible = TRUE, collapsed = FALSE,
      h5(explan),
      radioButtons(ns('SourceOption'), label = '¿Qué desea hacer?', 
                   choices = list('Crear disolución nueva utilizando un MRC' = 'daCapo',
                                  "Subir un archivo '.dis' generado anteriormente" = 'archivo')),
      tags$hr(),
      conditionalPanel(
        condition = 'input.SourceOption == "daCapo"',
        ns = ns,
        fluidRow(
          column(8, pickerInput(ns("MRCElected"), label = 'Seleccione el MRC:',
                                choices = MRCs.ArchiveNames[[reagKey]], width = '100%',# inline = FALSE,
                                multiple = FALSE, selected = NULL)),
          column(4, tags$br(), uiOutput(ns("MRC_CertiFile")))),
        uiOutput(ns('InfoMrcBox')),
        box(title = div(style = 'font-size:14px', 'Preparacion de la disolucion'), 
            width = 12, collapsible = TRUE, collapsed = TRUE, status = 'primary',
            tags$b('Condiciones ambientales'),
            numericInput(ns('Temp1'), label = div(style = 'font-size:12px', 'Temperatura ambiente [$^o$C]:'), value = 18),
            numericInput(ns('BarPres1'), label = div(style = 'font-size:12px', 'Presion barometrica [hPa]:'), value = 540),
            numericInput(ns('relHum1'), label = div(style = 'font-size:12px', 'Humedad relativa [%]:'), value = 45), tags$hr(),
            tags$b('Masa del MRC'),
            pickerInput(ns("CalibCertMRC"), label = 'Balanza para medir la masa del solido:',
                        choices = CalibCertShow, width = '100%', selected = 'MT XPE 205', multiple = FALSE),
            numericInput(ns('MasRec1'), label = div(style = 'font-size:12px', 'Masa del recipiente donde pesara el MRC [g]:'), value = 0),
            numericInput(ns('MasMRC1'), label = div(style = 'font-size:12px', 'Masa del MRC adicionado [g]:'), value = 0.372),
            numericInput(ns('MasRecMRC1'), label = div(style = 'font-size:12px', 'Masa conjunta del recipiente y el MRC [g]:'), value = 0),
            uiOutput(ns('deriMasaMRC')), tags$hr(),
            tags$b('Masa final de la disolucion'),
            uiOutput(ns('CalibCertDis')),
            numericInput(ns('MasRec2'), 
                         label = div(style = 'font-size:12px', 'Masa del recipiente donde llevara la disolucion a masa final [g]:'), value = 0),
            numericInput(ns('MasDis1'), label = div(style = 'font-size:12px', 'Masa final de la disolucion [g]:'), value = 100),
            numericInput(ns('MasRecDis1'), label = div(style = 'font-size:12px', 'Masa conjunta del recipiente y la disolucion [g]:'), value = 0),
            numericInput(ns('DensitDis'), label = div(style = 'font-size:12px', 'Densidad de la disolucion [g cm$^{-3}$]:'), value = 1),
            numericInput(ns('u_DensitDis'), 
                         label = div(style = 'font-size:12px', 'Incertidumbre en la densidad de la disolucion [g cm$^{-3}$]:'), value = 0.1),
            uiOutput(ns('deriMasaDisMRC')))),
      conditionalPanel(
        condition = 'input.SourceOption == "archivo"',
        ns = ns, tags$b('Esto aun esta pendiente de implementacion'),
        fileInput(ns('DisFile'), label = 'Escoja el archivo', multiple = FALSE, accept = '.dis')),
      tags$hr(), 
      actionButton(ns('buttonCalc'), label = 'Calcular concentracion'), tags$br(), tags$br(), 
      uiOutput(ns('InfoDisBox'))#,
      #actionButton(ns('DwnlDisFile'), label = 'Descargar archivo .dis')
  )
}

SolidMRCServer <- function(input, output, session, reagKey) {
  fileDwnHTML <- reactive(a(href = paste0('CertMRC/', reagKey, '/', input$MRCElected, '.pdf'),
                            "Descargar certificado ", download = NA, target = "_blank"))
  dateMRC <- reactive(MRC.ExpiricyDates[[reagKey]][[input$MRCElected]])
  MassFrMRC <- reactive(MRCs.MassFraction[[reagKey]][[input$MRCElected]])
  MolWeiMRC <- reactive(MRC.At_MolWeigths[[reagKey]][[input$MRCElected]])
  DensitMRC <- reactive(MRC.densities[[reagKey]][[input$MRCElected]])
  
  DensitAir <- reactive(c(airDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1),
                          uncertAirDensity(Temp = input$Temp1, p = input$BarPres1, h = input$relHum1, 
                                           u_Temp = 0.3, u_p = 0.78, u_h = 1.7, printRelSD = FALSE)))
  
  
  derMassMRC <- reactive(input$MasRecMRC1 - input$MasMRC1 - input$MasRec1)
  masMRC <- reactive(mean(input$MasMRC1, input$MasRecMRC1 - input$MasRec1))
  derMassDis <- reactive(input$MasRecDis1 - input$MasDis1 - input$MasRec2)
  masDis <- reactive(mean(input$MasDis1, input$MasRecDis1 - input$MasRec2))
  
  
  CalibCertDis <- reactive(pickerInput(session$ns("CalibCertDis"), label = 'Balanza para medir la masa de la disolucion:',
                                       choices = CalibCertShow, selected = input$CalibCertMRC, width = '100%', multiple = FALSE))
  
  convMassMRC <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertMRC]], reading = masMRC(), units = 'g')))
  BuoyMRC <- reactive(c(MABC(rho = DensitMRC()[1], rho_air = DensitAir()[1]),
                        uncertMABC(rho = DensitMRC()[1], rho_air = DensitAir()[1], 
                                   u_rho = DensitMRC()[2], u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  convMassDis <- reactive(c(convMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g'),
                            uncertConvMass(calibCert = CalibCertList[[input$CalibCertDis]], reading = masDis(), units = 'g')))
  BuoyDis <- reactive(c(MABC(rho = input$DensitDis, rho_air = DensitAir()[1]),
                        uncertMABC(rho = input$DensitDis, rho_air = DensitAir()[1], 
                                   u_rho = input$u_DensitDis, u_rho_air = DensitAir()[2], printRelSD = FALSE)))
  
  DisConc <- reactive({
    #xx <- 
    propagate(expr = expression(convMassMRC * MassFrMRC * BuoyMRC / (MolWeiMRC * convMassDis * BuoyDis) * 1000),
                    data = cbind(convMassMRC = convMassMRC(), MassFrMRC = MassFrMRC(), BuoyMRC = BuoyMRC(), 
                                 MolWeiMRC = MolWeiMRC(), convMassDis = convMassDis(), BuoyDis = BuoyDis()),
                    do.sim = FALSE)
    #return(xx$prop[c(1, 3)])
  })
  
  
  infoDisMRC <- eventReactive(input$buttonCalc, {
    list('MRC empleado' = input$MRCElected,
         'Fecha de vencimiento MRC' = dateMRC(),
         'Especie ' = reagKey,
         'Concentracion [mmol/kg]' = signif(DisConc()$prop[1], 6),
         'Incertidumbre [mmol/kg]' = signif(DisConc()$prop[3], 3))})
         #paste0(signif(DisConc()$prop[1], 5), signif(DisConc()$prop[3], 3), collapse = ' \u00b1 '))})
  
  
  InfoMrcBox <- reactive({
    box(title = div(style = 'font-size:14px', 
                    ifelse(dateMRC() > Sys.Date(), 'Resumen de informacion del MRC (vigente):', 'Resumen de informacion del MRC (VENCIDO):')), 
        width = 12, collapsible = TRUE, collapsed = TRUE,
        status = ifelse(dateMRC() > Sys.Date(), 'success', 'danger'),
        div(style = 'font-size:12px',
            tags$b('Fecha de vencimiento:'), dateMRC(), tags$br(),
            tags$b('Fraccion masica de ', reagKey, ':'), MassFrMRC()[1], '\u00B1', MassFrMRC()[2], tags$br(),
            tags$b('Masa molar de ', reagKey, ' que aplica para el material:'), MolWeiMRC()[1], '\u00B1', MolWeiMRC()[2], 'g mol$^{-1}$',tags$br(),
            tags$b('Densidad estimada del MRC:'), DensitMRC()[1], '\u00B1', DensitMRC()[2], 'g cm$^{-3}$'))
  })
  
  InfoDisBox <- eventReactive(input$buttonCalc, {
    trigger <- TRUE
    #printedStuff <- ifelse()
    box(title = div(style = 'font-size:14px', 'Informacion de la disolucion:'),
        width = 12, collapsible = TRUE, collapsed = FALSE,
        status = ifelse(trigger, 'success', 'danger'),
        renderPrint(tryCatch(infoDisMRC(),
                             error = function(cond) {'Los datos ingresados no son validos!'})),
        downloadButton(session$ns('DwnlDisFile'), 'Descargar archivo .dis'))#infoDisMRC()[1], ' \u00B1 ', infoDisMRC()[3])
  })
  output$InfoMrcBox <- renderUI(InfoMrcBox())
  output$InfoDisBox <- renderUI(InfoDisBox())
  output$CalibCertDis <- renderUI(CalibCertDis())
  output$DwnlDisFile <- downloadHandler(filename = function() {paste0("Disolucion_MRC_", reagKey, "_", Sys.Date(), ".dis")}, 
                                        content = function(file) {saveRDS(infoDisMRC(), file = file)}, contentType = NULL)
  # output$DwnlDisFile <- renderUI(DwnlDisFile())
  return()
}