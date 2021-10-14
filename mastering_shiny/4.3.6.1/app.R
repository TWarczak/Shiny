library(shiny)
library(reactlog)

reactlog_enable()

ui <- fluidPage(
    theme = shinythemes::shinytheme("superhero"),
    textInput("name", "What's your name?"),
    textOutput("greeting")
)

server1 <- function(input, output, server) {
    output$greeting <- renderText(paste0("Hello ", input$name))
}

server2 <- function(input, output, server) {
    output$greeting <- renderText(paste0("Hello ", input$name))
}

server3 <- function(input, output, session) {
    string <- reactive(paste0("Hello ", input$name, "!"))
    output$greeting <- renderText(string())
}

shinyApp(ui, server3)
