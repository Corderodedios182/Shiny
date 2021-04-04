library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(gapminder)
library(reactlog)
library(plotly)
library(gapminder)


ui <- fluidPage(
    
    titlePanel("Exploración de Datos, base Gapminder"),
    theme = shinytheme("yeti"),
    #shinythemes::themeSelector(),
    
    #3.Update Layout (UI)
    sidebarLayout(
     sidebarPanel(
         #1.Add input (UI)
         
         checkboxGroupInput(
             inputId = "continent",
             label = "Seleccion de continentes", 
             choices = unique(gapminder$continent),
             selected = "Americas" ),
         pickerInput(
             inputId = "year",
             label = "Seleccion de años", 
             choices = unique(gapminder$year),
             multiple = TRUE ,
             selected = 2007)
         ),

     mainPanel(    
         tabsetPanel(
         #2.Add output (UI/Server)
         tabPanel("Graficas", plotOutput("plot_1"), plotOutput("plot_2")),
         tabPanel("Tablas", DT::dataTableOutput('table')) ))
     )
    )

server <- function(input, output) {
    
    data_reactive <- reactive({ gapminder %>% filter(continent %in% input$continent & year %in% input$year) })

    output$plot_1 <- renderPlot({ 
                                    
                                  validate(need((input$year != '') & (input$continent != ''), "Puedes explorar otros continentes y años"))
                                  
                                  data_reactive() %>% 
                                  ggplot( aes(gdpPercap, lifeExp, size = pop, color=continent)) +
                                  geom_point() +
                                  ggtitle("Ingreso Percapita, Esperanza de Vida y población") + 
                                  theme_bw()  })
    
    output$plot_2 <- renderPlot({ 
        
                                  validate(need((input$year != '') & (input$continent != ''), "Puedes explorar otros continentes y años"))
        
                                  data_reactive() %>% 
                                  ggplot(aes(x=factor(year),y=lifeExp, fill=continent)) +
                                  geom_boxplot() +
                                  geom_jitter(width=0.1,alpha=0.2) +
                                  xlab("Year")+ 
                                  facet_wrap(~continent,ncol = 4) +
                                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                                  ggtitle("Esperanza de Vida paises del Continente") })
    
    output$table <- DT::renderDataTable(DT::datatable({ data_reactive() %>% 
                                                        group_by(country, continent, year) %>%
                                                        summarize(esperanza_de_vida = ceiling(mean(lifeExp)), 
                                                                  poblacion = round(mean(pop),0), 
                                                                  ingresos = round(mean(gdpPercap)))  })) 
    }

# Run the application 
shinyApp(ui = ui, server = server)
