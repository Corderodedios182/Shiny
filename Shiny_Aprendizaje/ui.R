library(lubridate)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(gapminder)
library(reactlog)
library(plotly)
library(gapminder)
library(leaflet)
library(wordcloud)
library(wordcloud2)
library(tm)

my_css <- "
#download_data {
  /* Change the background color of the download button
     to orange. */
  background: blue;

  /* Change the text size to 20 pixels. */
  font-size: 15px;
}

#table {
  /* Change the text color of the table to red. */
  color: purple;
}
"

ui <- fluidPage(
  
  titlePanel(h1(strong(em("Exploración de Datos, base Gapminder")))),
  theme = shinytheme("simplex"),
  #shinythemes::themeSelector(),
  tags$style(my_css),
  
  #3.Update Layout (UI)
  sidebarLayout(
    sidebarPanel(
      strong("Filtro y Botones de la aplicación"),
      #1.Add input (UI)
      checkboxGroupInput(
        inputId = "continent",
        label = "Seleccion de continentes", 
        choices = levels(gapminder$continent),
        selected = "Americas" ),
      pickerInput(
        inputId = "year",
        label = "Seleccion de años", 
        choices = unique(gapminder$year),
        multiple = TRUE ,
        selected = 2007),
      checkboxInput("fit", "Mostrar Grafica coloreada por País", FALSE),
      downloadButton(outputId = "download_data", label = "Descargar información"),
      
      textInput(
        inputId = 'name',
        'Mostrando Evento Oberve'),
      actionButton("show_reactivo",
                   "Boton Reactivo"),
      actionButton("show_observe",
                   "Boton ObserveEvent"),
      actionButton('show_about', 'About'),
      sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
      dateRangeInput('date_range', 'Select Date', "2010-01-01", "2019-12-01"),
      
      fileInput("file", "Selecciona un archivo"),
      checkboxInput("you_text", "Colocar tu texto: ", FALSE),
      textAreaInput("text", "Colocar texto: ", rows = 10)
      
    ),
    
    mainPanel(
      em(h2("Datos y Graficas interactivas")),
      tabsetPanel(
        #2.Add output (UI/Server)
        tabPanel("Graficas", textOutput("show_isolate"),
                 textOutput("show_observe"),
                 textOutput("show_reactivo"),
                 plotlyOutput("plot_1"),
                 plotlyOutput("plot_2")),
        tabPanel("Tablas", DT::dataTableOutput('table')),
        tabPanel("Mapa", leaflet::leafletOutput('map')),
        tabPanel("Nube de Palabras", wordcloud2Output(outputId = "cloud")  )))
  )
)
