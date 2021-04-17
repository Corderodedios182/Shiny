library(shinydashboard)

header <- dashboardHeader(
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Lucy",
      message = "You can view the International Space Station!",
      href = "https://spotthestation.nasa.gov/sightings/"
    ),
    # Add a second messageItem() 
    messageItem(
      from = "Lucy",
      message = "Learn more about the International Space Station",
      href = "https://spotthestation.nasa.gov/faq.cfm"
    )
  ),   dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "The International Space Station is overhead!"
    )
  ),
  dropdownMenu(
    type = "tasks",
    taskItem(text = "Mission Learn Shiny Dashboard",
             value = 10)
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data",
             tabName = "data"),
    menuItem("Dashboard",
             tabName = "dashboard"
             )
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "data",
            tabBox(
              tabPanel("Fun Fact 1",title = "Panel 1"),
              tabPanel("Fun Fact 2", title = "Panel 2"))),
    tabItem(tabName = "dashboard",
            "graficas y analisis")
  )
  
)

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

server <- function(input, output) {}

shinyApp(ui, server)