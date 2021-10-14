prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
    theme = shinythemes::shinytheme("superhero"),
    fluidRow(column(8, selectInput("code", "Product",
                                   choices = setNames(products$prod_code, products$title),
                                   width = "100%")),
             column(2, selectInput("y", "Y axis", c("rate", "count"))),
             column(2, numericInput("rows", "Number of Rows", min = 1, max = 10, value = 5))),
    fluidRow(column(4, tableOutput("diag")),
             column(4, tableOutput("body_part")),
             column(4, tableOutput("location"))),
    fluidRow(column(12, plotOutput("age_sex"))),
    fluidRow(column(2, actionButton("prev_story", "Previous story")),
             column(2, actionButton("next_story", "Next story")),
             column(8, textOutput("narrative"))))

count_top <- function(df, var, n = 7) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    max_no_rows <- reactive(max(length(unique(selected()$diag)),
                                length(unique(selected()$body_part)),
                                length(unique(selected()$location))))
    observeEvent(input$code, {
        updateNumericInput(session, "rows", max = max_no_rows())
    })
    table_rows <- reactive(input$rows - 1)
    output$diag <- renderTable(count_top(selected(), diag, n = table_rows()), width = "100%")
    output$body_part <- renderTable(count_top(selected(), body_part, n = table_rows()), width = "100%")
    output$location <- renderTable(count_top(selected(), location, n = table_rows()), width = "100%")
    summary <- reactive({selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(y = "Estimated number of injuries") +
                ggdark::dark_theme_minimal(15)
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
                labs(y = "Injuries per 10,000 people") +
                ggdark::dark_theme_minimal(15)
        }
    })
    max_no_stories <- reactive(length(selected()$narrative))
    story <- reactiveVal(1)
    observeEvent(input$code, {
        story(1)
    })
    observeEvent(input$next_story, {
        story((story() %% max_no_stories()) + 1)
    })
    observeEvent(input$prev_story, {
        story(((story() - 2) %% max_no_stories()) + 1)
    })
    output$narrative <- renderText({
        selected()$narrative[story()]
    })
}

shinyApp(ui, server)
