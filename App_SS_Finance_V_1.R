########
#Global#
########

  #Instalacion y Validacion de Paqueterias

  #Nota el paquete sweetalertR aun no se encuentra en CRAN, se instala desde un repositorio github
  list.of.packages <- c("shiny", "shinydashboard","DT","tidyverse","dplyr","DBI","RSQLite","rhandsontable","sweetalertR")

  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

  if(length(new.packages) != 0){
    install.packages(new.packages)
  } else if (!is.logical(length(new.packages) != 0 & (new.packages %in% "sweetalertR"))){
    devtools::install_github("timelyportfolio/sweetalertR") 
  }

  #lectura de multiples paqueterias
  lapply(list.of.packages, require, character.only = TRUE)


################################################################################################
#El diseno del ShinyDashbord, se compone de 3 elementos;                                   
#
# header <- dashboardHeader()
# sidebar <- dashboardSidebar()
# body <- dashboardBody()
#
#Al terminar de definirlos se utilizan como parametros de la funcion dashboardPage()          
################################################################################################

######################
#Definicion de header#
######################

#Recordatorios en la parte superior derecha, se liga en la parte del server "output$task_menu"
header<- dashboardHeader()

########################
#Definicion de Siderbar#
########################
sidebar <- dashboardSidebar()

####################
#Definicion de Body#
####################
  body <- dashboardBody(
    helpText("Tabla Editable"),
    rHandsontableOutput(outputId = "Tabla"), #Muestra la Tabla
    br(),
    #Boton para Guardar los Resultados
    actionButton(inputId = "Btn_Guardar","Guardar Cambios")
  )
  
  
  

# Funcion dashboadPage; Crea una pagina de panel de control para usar una aplicacion Shiny
  ui <- dashboardPage(header = header,
                      sidebar = sidebar,
                      body = body)

########
#Server#
########

server <- function(input, output) {
  #Funcion render conectada con rHandsontableOutput
  output$Tabla <- renderRHandsontable({
    rhandsontable(head(iris))
  })
  #Guarda los cambios en la Tabla
  observeEvent(input$Btn_Guardar,
               write.csv(hot_to_r(input$Tabla),file = "iris.csv", row.names = FALSE))
}

shinyApp(ui, server)




