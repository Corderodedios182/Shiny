library(shinydashboard)
library(shiny)

#Creacion del encabezado
header <- dashboardHeader(dropdownMenuOutput("task_menu")) #Recordatorios en la parte superior derecha

#Creacion del Siderbar
sidebar <- dashboardSidebar( #Funcion dashboardSidebar; Parte izquierda del dashboard
  sidebarMenu( #Funcion siderbarMenu; permite contener a las funciones menuItem, menuSubItem
    menuItem("Data", #Funcion menuItem; Genera los botones 
             tabName = "data"),
    menuItem("Dashboard",
             tabName = "dashboard"),
    
    sliderInput(#Funcion sliderInput; Barra lateral.
      inputId = "height",
      label = "Height",
      min = 66,
      max = 264,
      value = 264),
    selectInput(inputId = "name",#Funcion selectInput(Es una funcion reactive); Filtro 
                label = "Name",
                choices = mtcars$cyl)
  ),
  actionButton("click","Update click box") #Boton dinamico
)


#Creacion del Cuerpo
body <- dashboardBody(
  
  tags$head( #Estilo CSS, mejora la vista como por ejemplo el acomodo de las palabras y el formato de letra
    tags$style(
      HTML('h3 {
           font-weight:bold;
      }')
    )
  ),
  
  textOutput("name"),#Funcion textOuput, ligada  con renderText la cual va a arrojar el resultado que se le coloque a renderText
  # Create a tabBox
  tabItems(
    tabItem( #Funcion tabItem,genera una caja con etiquetas, 
      tabName = "dashboard", #se relaciona con los parametros de la funcion menuItem
      tabBox( #Formato de la caja y las etiquetas
        title = "International Space Station Fun Facts",
        tabPanel("Fun Fact 1"),
        tabPanel("Fun Fact 2")
      )
    ),
    tabItem(
      tabName = "data",
      tabBox(
        title = "International Space Station Fun Facts",
        tabPanel("Fun Fact 3"),
        tabPanel("Fun Fact 4")
      )
    )
  ),
  valueBoxOutput("click_box"), #Resultado del Boton Update click box
  
  fluidRow(#Panel de control con diseño basado en fila
    #Row 1
    box(
      width = 10,
      title = "Regular Box, Row 1",
      "Star Wars"),
    # Row 2
    box(
      width = 12,
      title = "Regular Box, Row 2",
      "Nothing but Star Wars",
      status = "danger" #Se puede colocar un status ?validStatues
    ),
    #Row 3, column 1
    column(width = 6,
           infoBox(
             width = NULL,
             title = "Regular Box, Column 2",
             subtitle = "Don't let them end",
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
        "Click Box"
      )
    })
}

shinyApp(ui, server)





