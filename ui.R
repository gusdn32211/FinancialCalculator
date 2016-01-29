library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Loan Calculator", tabName = "loan", icon = icon("calculator")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "loan",
      
      fluidRow(
        box(
          title = "Amoritization Loan",
          width = 3,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
        
        textInput("Loan", "Amount", 1000),
        
        sliderInput("InterestRate", "Please Select the Interest Rate: ",
                    min=0, max=20, value=5, step=1),
        
        sliderInput("Years", "Please Select the Number of Years: ",
                    min=0, max=100, value=20, step=1),
        
        selectInput("TimeLength", "Please Select Timeframe",
                    choices = c("Annually", "Semi-Annual", "Quarterly", "Monthly"))
      ),
      box(
        width = 9,
        tabsetPanel(id = "tabset",
                    tabPanel("TableView", (tableOutput("view"))),
                    tabPanel("PlotView", (plotOutput("myPlot")))
      )
      )
      )),
    
    tabItem(tabName = "widgets",
            
            fluidRow(
              box(
                title = "Work In Progress",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                
                selectInput("Distribution", "Please Select Calculation Type",
                        choices = c("Present Value", "Future value"))
            )
            )
     )
))

shinyUI(fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Financial Calculator"),
    sidebar,
    body
  )
))