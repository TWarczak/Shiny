library(shiny)
#options(shiny.reactlog = TRUE)
#reactlogShow(time = FALSE)

ui <- fluidPage(
    theme = shinythemes::shinytheme("superhero"),
    numericInput("a", "a", 2),
    numericInput("b", "b", 4),
    numericInput("c", "c", 8),
    numericInput("d", "d", 3),
    numericInput("x1", "x1", 1),
    numericInput("x2", "x2", 5),
    numericInput("x3", "x3", 10),
    numericInput("y1", "y1", 100),
    numericInput("y2", "y2", 1000),
    textOutput("f"),
    textOutput("z"))

server1 <- function(input, output, session) {
    c <- reactive(input$a + input$b)
    e <- reactive(c() + input$d)
    output$f <- renderText(e())
}

server2 <- function(input, output, session) {
    x <- reactive(input$x1 + input$x2 + input$x3)
    y <- reactive(input$y1 + input$y2)
    output$z <- renderText(x() / y())
}

server3 <- function(input, output, session) {
    d <- reactive(c() ^ input$d)
    a <- reactive(input$a * 10)
    c <- reactive(b() / input$c)
    b <- reactive(a() + input$b)
    output$z <- renderText(d())
    # ((2*10 + 4) / 8)^3 = 27
}

shinyApp(ui, server3)
