create_wordcloud <- function(data, num_words = 100, background = "white") {
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background)
}

#La lectura de archivos es mas eficiente fuera de server
mass_shootings <- read.csv("data/mass-shootings.csv")

server <- function(input, output) {
  #https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent/53016939#53016939
  #observe, monitorea en cualquier momento todos los valores reactivos en su entorno, no devuelve ningun valor para su uso en el codigo
  observe({ showNotification(paste("Mostrando Evento Observe ", input$name)) })
  
  #ObserveEvent, es similar a observe a diferencia que necesita un disparador.
  observeEvent(input$show_observe, {
    showModal(modalDialog(paste("Hola ", input$name))) } )
  
  observeEvent(input$show_about, {
    showModal(modalDialog("This data was compiled by Mother Jones, nonprofit founded in 1976. Originally covering cases from 1982-2012, this database has since been expanded numerous times to remain current.", title = 'About'))
  })
  
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
  
  rval_mass_shootings <- reactive({
    
    mass_shootings$date <- mdy(mass_shootings$date)
    
    mass_shootings %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        fatalities >= input$nb_fatalities)
  })
  
  
  output$plot_1 <- renderPlotly({ 
    
    ggplotly({
      
      validate(need((input$year != '') & (input$continent != ''), "Puedes explorar otros continentes y años"))
      
      g_1 <- data_reactive() %>% 
        ggplot( aes(gdpPercap, lifeExp, size = pop, colour=continent)) +
        geom_point() +
        ggtitle("Ingreso Percapita, Esperanza de Vida y población") + 
        theme_bw()
      
      g_2 <- data_reactive() %>% 
        ggplot( aes(gdpPercap, lifeExp, size = pop, colour=country)) +
        geom_point() +
        facet_wrap(~continent) + 
        ggtitle("Ingreso Percapita, Esperanza de Vida y población") + 
        theme_bw()
      
      if (input$fit) { g_2 } else { g_1 } })
    
  })       
  
  output$plot_2 <- renderPlotly({ 
    
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
  output$map <- leaflet::renderLeaflet({
    rval_mass_shootings() %>%
      leaflet() %>% 
      setView( -98.58, 39.82, zoom = 5) %>% 
      addTiles() %>% 
      addCircleMarkers(
        popup = ~summary, radius = ~sqrt(fatalities)*3,
        fillColor = 'red', color = 'red', weight = 1
      ) })
  
  output$download_data <- downloadHandler(
    
    filename = "gapminder_app.csv",
    content = function(file){
      write.csv(data_reactive(), file, row.names = FALSE) })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    
    if(input$you_text) { create_wordcloud( as.character(input$text) ) } 
    else { create_wordcloud(data = as.character(input_file()), num_words = input$num, background = input$col) }
  })
  
}
