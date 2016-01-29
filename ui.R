shinyUI(fluidPage(
  titlePanel("Financial Calculator"),
  
  sidebarPanel(
    selectInput("Distribution", "Please Select Calculation Type",
                choices = c("Present Value", "Future value")),
    textInput("Loan", "Amount", 1000),
    sliderInput("InterestRate", "Please Select the Interest Rate: ",
                min=0, max=20, value=5, step=1),
    sliderInput("Years", "Please Select the Number of Years: ",
                min=0, max=100, value=20, step=1),
    selectInput("TimeLength", "Please Select Timeframe",
                choices = c("Annually", "Semi-Annual", "Quarterly", "Monthly"))
  ),
  mainPanel(
    tabsetPanel(id = "tabset",
                tabPanel("TableView", tableOutput("view")),
                tabPanel("PlotView", plotOutput("myPlot")))
  )
))