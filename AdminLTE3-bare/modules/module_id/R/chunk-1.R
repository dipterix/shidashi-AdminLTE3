library(shiny)
library(shidashi)
ui <- function(){

  fluidPage(
    fluidRow(
      column(
        width = 12L,

        # remember to add ns, which is given as shiny::NS("module_id")
        plotOutput(ns("plot"))
      )
    )
  )

}

server_chunk_1 <- function(input, output, session, ...){

  event_data <- register_session_events()

  output$plot <- renderPlot({
    theme <- get_theme(event_data)
    set.seed(1)
    par(
      bg = theme$background, fg = theme$foreground,
      col.main = theme$foreground,
      col.axis = theme$foreground,
      col.lab = theme$foreground
    )
    hist(rnorm(1000))
  })

}
