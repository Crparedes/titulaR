customBody <- dashboardBody(
  tags$head(tags$style(
    HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
          .shiny-notification {position:fixed; top: calc(50% - 150px); left: calc(50% - 150px); 
                               height: auto !important; opacity:0.98; margin-right:500px}
          .btn-box-tool {color: #001848; font-size: 15px}
')
  )), 
  #shinyDashboardThemes(theme = "blue_gradient"),
  customTheme,... = # Look for it in Layouts/aa_ ...
  tags$style(tags$style(HTML('
        /* ligth-blue */
        .bg-light-blue {background-color: #34B1C9!important;}
    ')) #This is not working
    #type = 'text/css', 
    #'.bg-light-blue {background-color: #34B1C9!important; }'
  ),
  withMathJax(),
  tabItems(
    tabItem(tabName = "inicio", inicioLy),
    tabItem(tabName = "estDescri", h2("Titulaciòn de disoluciones calibrantes monoelementales")),
    tabItem(tabName = "bibliogr", h2("Bibliografía"))
  )
)
