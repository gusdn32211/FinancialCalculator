library(shiny)
library(ggplot2)
library(shinydashboard)

shinyServer(function(input, output) {
  
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
  
  # Formula for payment
  payment <- reactive({(as.numeric(input$Loan) * interestRate()) / (1 - (1 + interestRate()) ^ (-period() * input$Years))
  })
  
  # Timeframe (years * period)
  timeframe <- reactive({input$Years * period()})
  
  # Initializing list 
  Year <- reactive({ c(1:timeframe())})

  # Initializing list sizes
  # Balance <- reactive({c(as.numeric(input$Loan), 2:timeframe())})
  # Interest <- reactive({c(interestRate()*Balance()[1], 2:timeframe())})
  # principal_repayment <- reactive({c(payment() - Interest()[1], 2:timeframe())})
  # ending_balance <- reactive({c(Balance()[1] - principal_repayment()[1], 2:timeframe())})
  # cumulative_interest <- reactive({c(Interest()[1], 2:timeframe())})
  # 
  
  # 
  # f <- reactive({
  #   Balance <- c(1:timeframe())
  #   ending_balance <- c(1:timeframe())
  #   Interest <- c(1:timeframe())
  #   principal_repayment <- c(1:timeframe())
  #   cumulative_interest <- c(1:timeframe())
  #   
  #   Balance[1] <- as.numeric(input$Loan)
  #   Interest[1] <- interestRate() * Balance[1]
  #   principal_repayment[1] <- payment() - Interest[1]
  #   ending_balance[1] <- Balance[1] - principal_repayment[1]
  #   cumulative_interest[1] <- Interest[1]
  #   for (x in c(2:timeframe())){
  #     
  #     Balance[x] <-  ending_balance[x - 1]
  #     Interest[x] <- interestRate() * Balance[x]
  #     principal_repayment[x] <- payment() - Interest[x]
  #     ending_balance[x] <- Balance[x] - principal_repayment[x]
  #     cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
  # }
  # })
  
  # # Replacing values rest of table
  #  reactive({ for (x in c(2:timeframe())) {
  #   Balance[[x]] <- ending_balance()[x - 1]
  #   Interest[[x]] <- interestRate() * Balance()[x]
  #   principal_repayment[[x]] <- payment() - Interest()[x]
  #   ending_balance[[x]] <- Balance()[x] - principal_repayment()[x]
  #   cumulative_interest[[x]] <- cumulative_interest()[x - 1] + Interest()[x]
  # }
  # })
  
  output$view <- renderTable({
    
    # Initializing list sizes
    Balance <- c(as.numeric(input$Loan), 2:timeframe())
    Interest <- c(interestRate()*Balance[1], 2:timeframe())
    principal_repayment <- c(payment() - Interest[1], 2:timeframe())
    ending_balance <- c(Balance[1] - principal_repayment[1], 2:timeframe())
    cumulative_interest <- c(Interest[1], 2:timeframe())


    # Replacing values rest of table
    for (x in c(2:timeframe())) {
      Balance[x] <- ending_balance[x - 1]
      Interest[x] <- interestRate() * Balance[x]
      principal_repayment[x] <- payment() - Interest[x]
      ending_balance[x] <- Balance[x] - principal_repayment[x]
      cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
    }

    dataSet <- data.frame(Year = Year(),
                          Balance,
                          Payment = payment(),
                          Interest,
                          "Principal" = principal_repayment,
                          "Ending Balance" = ending_balance,
                          "Total Interest" = cumulative_interest)
    
    # dataSet <- data.frame(Year = Year(),
    #                       Balance = f()$Balance, 
    #                       Payment = payment(), 
    #                       Interest = f()$Interest,
    #                       "Principal" = f()$principal_repayment,
    #                       "Ending Balance" = f()$ending_balance,
    #                       "Total Interest" = f()$cumulative_interest)
    
    
    head(dataSet, n = timeframe())
    
  })
  
  
  output$myPlot <- renderPlot({
    
    # Initializing list sizes
    Balance <- c(as.numeric(input$Loan), 2:timeframe())
    Interest <- c(interestRate()*Balance[1], 2:timeframe())
    principal_repayment <- c(payment() - Interest[1], 2:timeframe())
    ending_balance <- c(Balance[1] - principal_repayment[1], 2:timeframe())
    cumulative_interest <- c(Interest[1], 2:timeframe())
    
    
    # Replacing values rest of table
    for (x in c(2:timeframe())) {
      Balance[x] <- ending_balance[x - 1]
      Interest[x] <- interestRate() * Balance[x]
      principal_repayment[x] <- payment() - Interest[x]
      ending_balance[x] <- Balance[x] - principal_repayment[x]
      cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
    }
    
    dataSet <- data.frame(Year = Year(),
                          Balance, 
                          Payment = payment(), 
                          Interest,
                          "Principal" = principal_repayment,
                          "Ending Balance" = ending_balance,
                          "Total Interest" = cumulative_interest)

    
    h <- ggplot(dataSet, aes(Year, Balance,
                             width = .9)) + geom_bar(aes(fill = Year), stat =
                                                       "identity", position = "identity")
    h + xlab("Time (Years)") + ylab("Balance ($)") + ggtitle("Debt Balance Relative to Time") + theme(plot.title = element_text(size = 20,
      lineheight = 1.6,
      face = "bold"
    )) + guides(fill = FALSE)
    
  })
  
  output$downloadData <- downloadHandler(
    
    
    filename = function(){
      paste('financial.csv', sep='') 
    },
    content = function(file) {
      
      # Initializing list sizes
      Balance <- c(as.numeric(input$Loan), 2:timeframe())
      Interest <- c(interestRate()*Balance[1], 2:timeframe())
      principal_repayment <- c(payment() - Interest[1], 2:timeframe())
      ending_balance <- c(Balance[1] - principal_repayment[1], 2:timeframe())
      cumulative_interest <- c(Interest[1], 2:timeframe())


      # Replacing values rest of table
      for (x in c(2:timeframe())) {
        Balance[x] <- ending_balance[x - 1]
        Interest[x] <- interestRate() * Balance[x]
        principal_repayment[x] <- payment() - Interest[x]
        ending_balance[x] <- Balance[x] - principal_repayment[x]
        cumulative_interest[x] <- cumulative_interest[x - 1] + Interest[x]
      }

      dataSet <- data.frame(Year = Year(),
                            Balance,
                            Payment = payment(),
                            Interest,
                            "Principal" = principal_repayment,
                            "Ending Balance" = ending_balance,
                            "Total Interest" = cumulative_interest)
      
      write.csv(dataSet, file)
    }
  )
  
  output$uploadView <- renderTable({
    
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, sep=input$sep)
  })
  
  output$present_text <- renderText({
      x <- as.numeric(input$`calc-input`) * (1/ ((1+(input$`calc-interest`)/100)^input$`calc-year`))
      round(x, digits=2)
    
  })
  
  output$future_text <- renderText({
      x <- as.numeric(input$`calc-input`) * ((1+input$`calc-interest`/100)^input$`calc-year`)
      round(x, digits=2)
  })
  
  output$return_text <- renderText({
    x <- ((as.numeric(input$`return-input-fv`)/as.numeric(input$`return-input-pv`))^(1/as.numeric(input$`return-year`)))-1
    x <- x * 100
    print(paste(round(x, digits=2), "%"))
    })
  
})
