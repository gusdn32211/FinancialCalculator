library(shiny)
library(ggplot2)
library(shinydashboard)

shinyServer(function(input, output,session) {
  
  #Payment period
  period <- reactive({switch(input$TimeLength,
                   "Annually" = 1,
                   "Semi-Annual" = 2,
                   "Quarterly" = 4,
                   "Monthly" = 12)
  })
  #Interest Rate
  interestRate <- reactive({
    (as.numeric(input$InterestRate / 100) / period())
  })
  
  # Timeframe (years * period)
  timeframe <- reactive({input$Years * period()})

  # Table Data
  data <- reactive ({
    Year <- c(1:timeframe())
    Balance <- c(as.numeric(input$Loan), 2:timeframe())
    Interest <- c(interestRate()*Balance[1], 2:timeframe())
    payment <- c(((as.numeric(input$Loan) * interestRate()) / (1 - (1 + interestRate()) ^ (-period() * input$Years))), 2:timeframe())
    principal_repayment <- c(payment[1] - Interest[1], 2:timeframe())
    ending_balance <- c(Balance[1] - principal_repayment[1], 2:timeframe())
    cumulative_interest <- c(Interest[1], 2:timeframe())

    for (x in c(2:timeframe())) {
      Balance[x] <- ending_balance[x - 1]
      Interest[x] <- interestRate() * Balance[x]
      payment[x] <- payment [1]
      principal_repayment[x] <- payment[1] - Interest[x]
      ending_balance[x] <- Balance[x] - principal_repayment[x]
      cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
    }
    data.frame(Year = Year,
               Balance = Balance,
               Payment = payment,
               Interest = Interest,
               "Principal" = principal_repayment,
               "Ending Balance" = ending_balance,
               "Total Interest" = cumulative_interest)
  })
  
  # Present Value Calculation
  presentValue <-reactive({
     x <- as.numeric(input$`calc-input`) * (1 / ((1 + (input$`calc-interest`) / 100) ^ input$`calc-year`))
     print(paste("$", round(x, digits=2)))
  })
  
  # Future Value Calculation
  futureValue <-reactive({
    x <- as.numeric(input$`calc-input`) * ((1 + input$`calc-interest` / 100) ^ input$`calc-year`)
    print(paste("$", round(x, digits=2)))
  })
  
  # Return Rate Calculation
  returnRate <- reactive({
    x <- (((as.numeric(input$`return-input-fv`)/as.numeric(input$`return-input-pv`))^(1/as.numeric(input$`return-year`)))-1)*100
    print(paste(round(x, digits=2), "%"))
  })
  
  # Time Length Calculation
  timeLength <- reactive({
    x <- -(log(as.numeric(input$`period-input-pv`)/as.numeric(input$`period-input-fv`)))/log(1+(as.numeric(input$`period-interest`))/100)
    print(paste(round(x, digits=2), "Years"))
  })
  
  # Loan Calculator Table View
  output$view <- renderTable({
    data()
  })
  
  # Loan Calculator Graph View
  output$myPlot <- renderPlot({
    h <- ggplot(data(), aes(Year, Balance,
                             width = .9)) + geom_bar(aes(fill = Year), stat =
                                                       "identity", position = "identity")
    h + xlab("Time (Years)") + ylab("Balance ($)") + ggtitle("Debt Balance Relative to Time") + theme(plot.title = element_text(size = 20,
      lineheight = 1.6,
      face = "bold"
    )) + guides(fill = FALSE)
    
  })
  # Loan Calculator Download button
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste('financial.csv', sep='') 
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  # File Upload 
  output$uploadView <- renderTable({
    
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep=input$sep)
  })
  
  # Basic Calculation present value
  output$present_text <- renderText({
    presentValue()
  })
  
  # Basic Calculations future value
  output$future_text <- renderText({
    futureValue()
  })
  
  # Basic Calculations return rate
  output$return_text <- renderText({
    returnRate()
  })
  
  # PV/FV Calculator Amount
  output$pvamount <- renderInfoBox({
    infoBox(
      "Amount", print(paste0("$", input$`calc-input`)), icon = icon("credit-card")
      )
  })
  
  # PV/FV Calculator Interest
  output$pvinterest <- renderInfoBox({
    infoBox(
      "Interest", print(paste0(input$`calc-interest`, "%")), icon = icon("line-chart"),
      color = "purple"
    )
  })
  
  # PV/FV Calculator Years
  output$pvyears <- renderInfoBox({
    infoBox(
      "Years", input$`calc-year`, icon = icon("list", lib = "glyphicon"),
      color = "teal"
      #Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    )
  })
  
  # Return Present Value
  output$returnPresentValue <- renderInfoBox({
    infoBox(
      "Present Value", print(paste0("$", input$`return-input-pv`)), icon = icon("credit-card")
    )
  })
  
  # Return Future Value
  output$returnFutureValue <- renderInfoBox({
    infoBox(
      "Future Value", print(paste0("$", input$`return-input-fv`)), icon = icon("line-chart"),
      color = "purple"
    )
  })
  
  # Return Years
  output$returnYears <- renderInfoBox({
    infoBox(
      "Years", input$`return-year`, icon = icon("list", lib = "glyphicon"),
      color = "teal"
      )
  })
  
  # Period Time Length
  output$period_text <- renderText({
    timeLength()
  })  
  
  # Period Present Value
  output$periodPresentValue <- renderInfoBox({
    infoBox(
      "Present Value", print(paste0("$", input$`period-input-pv`)), icon = icon("credit-card")
    )
  })
  
  # Period Future Value
  output$periodFutureValue <- renderInfoBox({
    infoBox(
      "Future Value", print(paste0("$", input$`period-input-fv`)), icon = icon("line-chart"),
      color = "purple"
    )
  })
  
  # Period Interest Rate
  output$periodInterest <- renderInfoBox({
    infoBox(
      "Interest", print(paste0(input$`period-interest`, "%")), icon = icon("list", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  observe({
    session$sendCustomMessage(type = 'testmessage',
                              message = "hello user")
  })
})
