GENERIC.UI <- function(id, IntID = 1, value0 = 10) {
  ns <- NS(id)
  fluidRow(
    tags$br(),
    column(width = 12, h3(HTML(spcs(3)), tags$b('Titulo')), uiOutput(ns('brwz'))),
    tags$div(
      style = 'margin-left: 50px;',
      column(
        11,
        tags$ol(
          h5(tags$li('Primera opt:')),
          h5(tags$li('Cuando haya diligenciado los campos haga click en el siguiente botón:')),
          uiOutput(ns('TerminarPlan')),
          conditionalPanel(
            condition = 'input.TerminarPlan > 0', ns = ns,
            h5(tags$li('op.')),
            uiOutput(ns('downloadPlan1'))
          )
          #uiOutput(ns('arranca'))
        )
      )
    )
  )
}