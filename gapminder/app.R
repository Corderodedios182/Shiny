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
             selected = 2007),
         textInput(
             inputId = 'name',
             'Mostrando Evento Oberve'),
         actionButton("show_reactivo",
                      "Boton Reactivo"),
         actionButton("show_observe",
                      "Boton ObserveEvent")
         ),

     mainPanel(    
         tabsetPanel(
         #2.Add output (UI/Server)
         tabPanel("Graficas", textOutput("show_isolate"),
                              textOutput("show_observe"),
                              textOutput("show_reactivo"),
                              plotOutput("plot_1"),
                              plotOutput("plot_2")),
         tabPanel("Tablas", DT::dataTableOutput('table')) ))
     )
    )

server <- function(input, output) {
    #https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent/53016939#53016939
    #observe, monitorea en cualquier momento todos los valores reactivos en su entorno, no devuelve ningun valor para su uso en el codigo
    observe({ showNotification(paste("Mostrando Evento Observe ", input$name)) })
    
    #ObserveEvent, es similar a observe a diferencia que necesita un disparador.
    observeEvent(input$show_observe, {
        showModal(modalDialog(paste("Hola ", input$name))) } )
    
    #eventeReactive, Cree una variable , con un disparador definido similar a observeEvent. Use esto cuando desee una variable reactiva que se evalúe debido a un disparador en lugar de cuando se llama.
    mensaje_reactive <- eventReactive(input$show_reactivo, {
        input$continent })
    
    #reactive, permite al usuario monitorear el estado de una entrada u otra variable cambiante y devolver el valor para usar en otra parte del código. 
    data_reactive <- reactive({ gapminder %>% filter(continent %in% input$continent & year %in% input$year) })

    #isolate
    output$show_isolate <- renderText({ paste("Aislando un Valor con Isolate, ",isolate({input$continent}), input$year, sep = ",") })
    
    output$show_reactivo <- renderText({
        msn_reactive <- mensaje_reactive() #Usando los datos de EventReactive
        paste("Boton Reactivo  ", msn_reactive , ", ")
        
    })
    
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
