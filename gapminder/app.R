library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("Life Expectation vs. GDP Per Capita"),
    theme = shinytheme("superhero"),
    shinythemes::shinytheme("paper"),
    #3.Update Layout (UI)
    sidebarLayout(
        sidebarPanel(
            #1.Add input (UI)
            selectInput("continent","Select Continent", choices = unique(gapminder$continent)),
            sliderInput("year", "Select Year", min = 1952, max = 2007, value = 1992, step = 5 )
        ),
        mainPanel(    
            tabsetPanel(
                #2.Add output (UI/Server)
                tabPanel("Plot", plotOutput("plot")),
                tabPanel("Table", DT::DTOutput('table'))
                )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot <- renderPlot({
        ggplot(gapminder %>% filter(continent == input$continent & year == input$year),
               aes(x = gdpPercap, y = lifeExp)) + geom_point()})
    output$table <- DT::renderDT({
        gapminder %>% 
            filter(continent == input$continent & year == input$year)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
