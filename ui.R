library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Loan Calculator", tabName = "loan", icon = icon("bar-chart")),
    menuItem("Basic Calculations ", tabName = "calculations", icon = icon("calculator")),
    menuItem("File Upload", tabName = "upload", icon = icon("sign-in"))

  )
)

dropdown <- dropdownMenu(
  
  type = "messages",
  messageItem(
    from = "System",
    message = "Welcome User.",
    icon = icon("smile-o"),
    time = Sys.Date(),
    singleton(
      tags$head(tags$script(src = "message-handler.js"))
    )

  )
)

header <- dashboardHeader(title = "Financial Calculator",dropdown)

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
                    min=0, max=20, value=5, step=.1),

        sliderInput("Years", "Please Select the Number of Years: ",
                    min=0, max=50, value=20, step=1),
        
        selectInput("TimeLength", "Please Select Timeframe",
                    choices = c("Annually", "Semi-Annual", "Quarterly", "Monthly")),
        fluidRow(
          column (width = 5),
          column (width = 7,
        downloadButton('downloadData', 'Download')
          )
        )
      ),
      
      box(
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        tabBox(
          title = tagList(shiny::icon("area-chart"), "Loan"),
          width = 12,
          
          tabPanel("TableView", (tableOutput("view"))),
          tabPanel("PlotView", (plotOutput("myPlot")))
        )
      )
      
      )), 
    
    tabItem(
      tabName = "calculations",
      fluidRow(
        box(
          width  = 12,
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            tabBox(
              title = tagList(shiny::icon("balance-scale"), "Calculations"),
              width = 12,
              tabPanel("PV/FV Calculator",
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             width = 13,
                             title = "Calculator",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                  
                             textInput("calc-input", "Amount", 1000),
                             
                             sliderInput("calc-interest", "Please Select the Interest Rate: ",
                                         min = 0, max = 20, value = 5, step = .1),
                             
                             sliderInput("calc-year", "Please Select the Number of Years: ",
                                         min = 0, max = 50, value = 5, step = 1)
                           )
                         ),
                        
                         column(
                           width = 4,
                           box(
                             width = 13,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "warning",
                             title = "Present Value",
                             verbatimTextOutput("present_text")
                           ),
                           box(
                             width = 13,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "danger",
                             title = "Future Value",
                             verbatimTextOutput("future_text")
                           )
                         ),
                         column(
                           width = 4,
                           infoBoxOutput(width= 13, "pvamount"),
                           infoBoxOutput(width= 13,"pvinterest"),
                           infoBoxOutput(width= 13,"pvyears")
                         )
                         
                       )
              ), 
              tabPanel("Return Calculator",
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             width = 13,
                             title = "Calculator",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             textInput("return-input-pv", "Present Value: ", 1000),
                             textInput("return-input-fv", "Future Value: ", 1200),
                             textInput("return-year", "Please Select the Number of Years: ", 5)
                           )
                         ), 
                         column(
                           width = 4,
                           box(
                             width = 13,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "info",
                             title = "Rate of Return",
                             verbatimTextOutput("return_text")
                           )
                         ), 
                         column(width = 4,
                                infoBoxOutput(width= 13, "returnPresentValue"),
                                infoBoxOutput(width= 13, "returnFutureValue"),
                                infoBoxOutput(width= 13, "returnYears")
                         )
                       )
              ),
              tabPanel("Period Calculator",
                       fluidRow(
                         column(
                           width = 4,
                           box(
                             width = 13,
                             title = "Calculator",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "primary",
                             
                             textInput("period-input-pv", "Present Value: ", 1000),
                             textInput("period-input-fv", "Future Value: ", 1200),
                             sliderInput("period-interest", "Please Select the Interest Rate: ",
                                         min = 0, max = 20, value = 5, step = .1)                           )
                         ),
                         column(
                           width = 4,
                           box(
                             width = 13,
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             status = "success",
                             title = "Time Length",
                             verbatimTextOutput("period_text")
                           )
                         ),
                         column(width = 4,
                                infoBoxOutput(width= 13, "periodPresentValue"),
                                infoBoxOutput(width= 13, "periodFutureValue"),
                                infoBoxOutput(width= 13, "periodInterest")
                         )
                       ) #fluidRow
                       
              ) #tabPanel
              
            ))
        )
      )
    ), 
    
    tabItem(tabName = "upload",
            
            fluidRow(
              box(
                title = "Document Upload",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                
                fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv')),
                tags$hr(),
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ',')
              ),
              box(
                title = "Data Statistics",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "success",
                width = 9,
                tableOutput('uploadView')
              )
    )
  )
  
))


shinyUI(fluidPage(
  
  dashboardPage(
    header,
    sidebar,
    body
  )
))