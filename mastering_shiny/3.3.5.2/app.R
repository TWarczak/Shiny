library(shiny)

ui <- fluidPage(
    fluidRow(
        column(width = 6, plotOutput("plot1")),
        column(width = 6, plotOutput("plot2"))
    )
)
server <- function(input, output, session) {
    output$plot1 <- renderPlot(plot(1:5))
    output$plot2 <- renderPlot(plot(1:5))
}

shinyApp(ui, server)
