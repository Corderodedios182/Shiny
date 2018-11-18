library(shinydashboard)
library(shiny)

#Es el esqueleto, funciones basicas.

#En la primer parte de defininen las variables header, sidebar, body. Las cuales sirvern para el UI (funcion dashbordpage())
#En la segunda parte esta el UI
#Tercera parte el server, ver la relacion que tiene con las variables del UI


#########################
#Creacion del encabezado#
#########################
header <- dashboardHeader(dropdownMenuOutput("task_menu")) #Recordatorios en la parte superior derecha, se liga en la parte del server "output$task_menu"

#######################
#Creacion del Siderbar#
#######################

sidebar <- dashboardSidebar( #Funcion dashboardSidebar; Parte lateral izquierda del dashboard
  sidebarMenu( #Funcion siderbarMenu; permite contener a las funciones menuItem, menuSubItem
    menuItem("Datos", #Funcion menuItem; Genera los iconos en la barra lateral izquierda.
             tabName = "data"),
    menuItem("Graficas",
             tabName = "dashboard"),
    
    sliderInput(#Funcion sliderInput; Crea una barra vertical.
      inputId = "height", #
      label = "Barra lateral",
      min = 1,
      max = 100,
      value = 50),
    selectInput(inputId = "name",#Funcion selectInput(Es una funcion reactive); Filtro 
                label = "Filtros",
                choices = mtcars$cyl)
  ),
  actionButton("click","Boton dinamico") #Boton dinamico
)

#####################
#Creacion del Cuerpo#
#####################

body <- dashboardBody(
  
  tags$head( #Estilo CSS, mejora la vista como por ejemplo el acomodo de las palabras y el formato de letra
    tags$style(
      HTML('h3 {
           font-weight:bold;
      }')
    )
  ),
  
  textOutput("name"),#Funcion textOuput, ligada  en la parte del server como; output$name ocupando renderText().
  # Create a tabBox
  tabItems(
    tabItem( #Funcion tabItem,genera una caja con etiquetas, 
      tabName = "dashboard", #se relaciona con los parametros de la funcion menuItem
      tabBox( #Formato de la caja y las etiquetas
        title = "Caja con etiquetas se define con la funcion tabBox",
        tabPanel("Etiqueta 1"),
        tabPanel("Etiqueta 2")
      )
    ),
    tabItem(
      tabName = "data",
      tabBox(
        title = "Se puede definir por hoja",
        tabPanel("Etiqueta 3"),
        tabPanel("Etiqueta 4")
      )
    )
  ),
  valueBoxOutput("click_box"), #Resultado del Boton Dinamico, se define tambien en server output$click_box
  
  fluidRow(#Panel de control con diseno basado en fila
    #Row 1
    box(
      width = 10,
      title = "Podemos definir cuadros con texto, en fluidRow",
      "Star Wars"),
    # Row 2
    box(
      width = 12,
      title = "Se puede jugar con el formato",
      "Se puede colorear, viendo el parametro status = danger , success, info ver mas ?validStatues",
      status = "danger" #Se puede colocar un status ?validStatues
    ),
    #Row 3, column 1
    column(width = 6,
           infoBox(
             width = NULL,
             title = "Caja 1, se define en fluidRow",
             subtitle = "Se pueden mostrar graficas",
             icon = icon("star")
           )),
    #Row 3, column 2
    column(width = 6,
           infoBox(
             width = NULL,
             title = "Regular Box, Row 2, Column 1",
             subtitle = "Gimm those Star Wars",
             color = "yellow"
           ))
  ),

  tableOutput("table") #Salida de la tabla reactiva
)


# Funcion dashboadPage; Crea una pagina de panel de control para usar una aplicacion Shiny
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,skin = "green")

server <- function(input, output,session) {
  output$name <- renderText({ #Funcion renderText(Funcion de tipo render, ligada con selecInput tal que inputId = "name"), se necesita una funcion de salida en este caso textOutput
    input$name
  })
  reactive_starwars_data <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = "http://s3.amazonaws.com/assets.datacamp.com/production/course_6225/datasets/starwars.csv",
    readFunc = function(filePath) { 
      read.csv(url(filePath))
    }
  )
  output$table <- renderTable({ #Formato del archivo reactivo_starwas_data
    reactive_starwars_data()
  })
  
  task_data <- data.frame(text = c("A","B","C"), value = c(10,20,30)) #Tabla con los recordatorios
  
  output$task_menu <- renderMenu({ #Recordatorios dinamicos apartir del data.frame task_data
    tasks <- apply(task_data, 1, function(row) {
      taskItem(text = row[["text"]],
               value = row[["value"]])
    })
    
    dropdownMenu(type = "tasks", .list = tasks,
                 notificationItem(
                   text = "The International Space Station is overhead!",
                   icon = icon("rocket") #Podemos colocar iconos, podemos ver mas ejemplos (http://fontawesome.io/icons/,http://fontawesome.io/examples/ ,https://getbootstrap.com/components/)
                 ))
    
  })
    
  output$click_box <- renderValueBox({ #Salida del boton dinamico
      valueBox(
        input$click, 
        "Reacciona al presionar el Boton Dinamico"
      )
    })
}

shinyApp(ui, server)





