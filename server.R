library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  #Table View
  data<- eventReactive(input$table,{
    runif()
  })
  
  output$view <- renderTable({
    
    #Input values from users
    distType <- input$Distribution
    inputLoan <- as.numeric(input$Loan)
    inputInterest <- as.numeric(input$InterestRate)
    inputYears <- input$Years
    inputTime <- input$TimeLength
    
    #Payment period
    if (inputTime == "Annually")
    {period <- 1}
    if (inputTime == "Semi-Annual")
    {period <- 2}
    if (inputTime == "Quarterly")
    {period <- 4}
    if (inputTime == "Monthly")
    {period <- 12}
    
    #Initializing list sizes
    timeframe <- inputYears * period
    initList <- c(1:timeframe)
    Year <- initList
    Balance <- initList
    ending_balance <- initList
    Interest <- initList
    principal_repayment <- initList
    cumulative_interest <- initList
    
    # Interest Rate
    r <- (inputInterest / 100) / period
    # Formula for payment
    Payment <- (inputLoan * r) / (1 - (1 + r) ^ (-period * inputYears))
    
    # Initializing the first values in table
    Balance[1] <- inputLoan
    Interest[1] <- r * Balance[1]
    principal_repayment[1] <- Payment - Interest[1]
    ending_balance[1] <- Balance[1] - principal_repayment[1]
    cumulative_interest[1] <- Interest[1]
    
    # Replacing values rest of table
    for (x in c(2:timeframe)) {
      Balance[x] <- ending_balance[x - 1]
      Interest[x] <- r * Balance[x]
      principal_repayment[x] <- Payment - Interest[x]
      ending_balance[x] <- Balance[x] - principal_repayment[x]
      cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
    }
    
    dataSet <- data.frame(Year,
                          Balance, 
                          Payment, 
                          Interest,
                          "Principal" = principal_repayment,
                          "Ending Balance" = ending_balance,
                          "Total Interest" = cumulative_interest)
    
    
    head(dataSet, n = timeframe)
    
  })
  
  
  output$myPlot <- renderPlot({
    #Input values from users
    distType <- input$Distribution
    inputLoan <- as.numeric(input$Loan)
    inputInterest <- as.numeric(input$InterestRate)
    inputYears <- input$Years
    inputTime <- input$TimeLength
    
    #Payment period
    if (inputTime == "Annually")
    {period <- 1}
    if (inputTime == "Semi-Annual")
    {period <- 2}
    if (inputTime == "Quarterly")
    {period <- 4}
    if (inputTime == "Monthly")
    {period <- 12}
    
    #Initializing list sizes
    timeframe <- inputYears * period
    initList <- c(1:timeframe)
    Year <- initList
    Balance <- initList
    ending_balance <- initList
    Interest <- initList
    principal_repayment <- initList
    cumulative_interest <- initList
    
    # Interest Rate
    r <- (inputInterest / 100) / period
    # Formula for payment
    Payment <- (inputLoan * r) / (1 - (1 + r) ^ (-period * inputYears))
    
    Balance[1] <- inputLoan
    Interest[1] <- r * Balance[1]
    principal_repayment[1] <- Payment - Interest[1]
    ending_balance[1] <- Balance[1] - principal_repayment[1]
    cumulative_interest[1] <- Interest[1]
    
    for (x in c(2:timeframe)) {
      Balance[x] <- ending_balance[x - 1]
      Interest[x] <- r * Balance[x]
      principal_repayment[x] <- Payment - Interest[x]
      ending_balance[x] <- Balance[x] - principal_repayment[x]
      cumulative_interest[x] <-
        cumulative_interest[x - 1] + Interest[x]
    }
    
    dataSet<- eventReactive(input$plot,{
      runif(input$plot)
    })
    
    dataSet <- data.frame(Year,
                          Balance, 
                          Payment, 
                          Interest,
                          "Principal Repayment" = principal_repayment,
                          "Ending Balance" = ending_balance,
                          "Cumulative Interest" = cumulative_interest)
    
    # m <- ggplot(dataSet, aes(x = Year, y = cumulative_interest))
    # m + geom_line(arrow = arrow())
    # m + geom_line(arrow = arrow(angle = 30, ends = "last", type = "open"))

    # g <- ggplot(dataSet, aes(Year,Balance))
    # g + geom_bar(aes(fill = Year), stat = "identity")
    
    
    h <- ggplot(dataSet, aes(Year, Balance,
                             width = .9)) + geom_bar(aes(fill = Year), stat =
                                                       "identity", position = "identity")
    h + xlab("Time in Years") + theme(axis.title.y = element_text(angle = 0)) + ylab("Balance") + ggtitle("Debt Balance Relative to Time") + theme(plot.title = element_text(size = 20,
      lineheight = 1.6,
      face = "bold"
    )) + guides(fill = FALSE)
    
  })

})
