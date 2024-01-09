customHeader <- dashboardHeader(
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 100px}"),
      tags$style(".main-header .logo {height: 100px}"),
      # tags$style(type = "text/css", 
      #            paste0("a {color: ", BackSidebar, ";}
      #            a:hover {color: ", BackHeaderButtons, ";}"))
    ),
    
    titleWidth = 1920,
    title = tags$div(HTML(
      '<table text-align = left cellspacing = -20 cellPadding=0>
      <tr><th rowspan = 2>', spcs(5),
      '<a id = "logo" href = "http://www.inm.gov.co"  target = ”_blank” 
      title = "titulaR - Instituto Nacional de Metrología" data-height = "60">
      <img src = "INMgrayCorto.png" height = "80" alt = "INM de Colombia" style = "margin-top: 5px">
      </a>', spcs(5),
      '</th> 
      <th  height = "80"><h1 style="LINE-HEIGHT:5px; color: #dddddd; margin-bottom: -5px; font-size:45px;">
          <b>Aplicativo titulaR</b>
          <b style="LINE-HEIGHT:5px; color: #dddddd; font-size:14px;">v.0.8.1</b>
    </h1></th></tr>
    <tr><th><h3 style="LINE-HEIGHT:0px; color: #dddddd; margin-top: 1px;">
    Soporte para las titulaciones gravimétricas de la <b>SMQB - INM</b>
    </h3></th></tr>
    </table>'))
)
