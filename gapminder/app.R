library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(gapminder)

ui <- fluidPage(
    
    titlePanel("Exploracion de Datos, base Gapminder"),
    theme = shinytheme("yeti"),
    #shinythemes::themeSelector(),
    
    #3.Update Layout (UI)
    sidebarLayout(
        sidebarPanel(
    
                    #1.Add input (UI)
                    selectInput("continent","Select Continent", choices = unique(gapminder$continent)),
                    sliderInput("year", "Select Year", min = 1952, max = 2007, value = 1992, step = 5 )),
        
        mainPanel(    
            tabsetPanel(
                        #2.Add output (UI/Server)
                        tabPanel("Plot", plotOutput("plot_1"), plotOutput("plot_2")),
                        tabPanel("Table", DT::DTOutput('table')) ))
        )
    )

server <- function(input, output) {
    
    data_reactive <- reactive({ gapminder %>% filter(continent == input$continent & year == input$year) })
    
    output$plot_1 <- renderPlot({ ggplot(data_reactive()) + 
                                    geom_point(mapping =aes(x=gdpPercap,y=lifeExp,color=country, size=pop)) })
    
    output$plot_2 <- renderPlot({ data_reactive() %>% arrange(desc(lifeExp)) %>%
            ggplot(mapping = aes(x = country, y = lifeExp, fill = country)) +
            geom_col(position = "dodge", show.legend = FALSE) + 
            coord_flip() })
    
    output$table <- DT::renderDT({ data_reactive() %>% group_by(continent, year) %>% summarise(freq = n(),
                                                                                               promedio = round(mean(lifeExp)))  })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
