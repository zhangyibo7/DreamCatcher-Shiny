library(shiny)
library(quantmod)
library(lubridate)
library(TTR)

#/home/storage/sec/AAPL.O/2019.04.26.AAPL.O.RData
#rS8A9AP--NcFCcnCHmty



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NYSE Correlation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # File input, ticker list
      fileInput("ticker_file", "Please upload ticker list in csv",
                accept = c('text/csv','text/comma-separated-values,text/plain', '.csv')),
      
      # Input:  ----
      # In-sample-month for beta calculation
      numericInput(inputId="insampleMonth", 
                   label="Please specify in-sample length in months (Default: 60)", 
                   value=60),
      # out-of-sample month for testing
      numericInput(inputId="outsampleMonth", 
                   label="Please specify out-of-sample length in months (Default: 12)", 
                   value=12),
      # how many months for next test start
      numericInput(inputId="nextStartMonth", 
                   label="Please specify next trial start time in months (Default: 3)", 
                   value=3),
      helpText("AKA Overlapping time. E.g. 1st In-Sample Trial starts in Jan 2015, 2nd In-Sample Trial starts in Apr 2015, then this is 3"),
      # Moving Average window length
      numericInput(inputId="MA_length", 
                   label="Please specify simple MA length (Default: 200)", 
                   value=200),
      # bolinger band standard deviations
      numericInput(inputId="bband_dist", 
                   label="Please specify Bollinger Band Distance (Default: 4)", 
                   value=4),
      # average daily trading volume for screening
      numericInput(inputId="avgDailyVolume6", 
                   label="Please specify Average daily volume in (Default: 200000)", 
                   value=200000),
      # maximum notional stock price for screening
      numericInput(inputId="maxNotional", 
                   label="Please specify Max Notional Value (Default: 100)", 
                   value=100),
      # action button to start the process
      actionButton("update", "Start Screening")
      
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      verbatimTextOutput(outputId = "initial_screen"),
      verbatimTextOutput(outputId = "good_pairs")
      #tableOutput('corr')
      
    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output) {

  #-------------------------------------------------------------------------------------------------------------------------
  # initial screening

  good_tickers = NULL
  
  
  observeEvent(input$update, {
    req(input$ticker_file)
    volume_cri = input$avgDailyVolume6
    price_cri = input$maxNotional
    sample_len_cri = (input$insampleMonth + input$outsampleMonth + 2 * input$nextStartMonth) * 31
    
    ticker_list <- read.csv(input$ticker_file$datapath, header = TRUE)
    ticker_list = ticker_list[,1]

    # add progress bar to show currernt progress
    progress <-shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Initial Screening", value = 0)

 
    for (ticker in ticker_list){
      progress$inc(1/length(ticker_list))
      
      
      
      tryCatch({
        all_available <- getSymbols(as.character(ticker), src="yahoo", auto.assign = F)
        all_available = na.locf(all_available)
        all_available = na.locf(all_available, fromLast = TRUE)
        
        colnames(all_available) = c("Open","High","Low","Close","Volume","Adjusted")
        
        today_date = Sys.Date()
        
        # filter out volume
        avg_volume_6m = mean(all_available[paste0(today_date-183,"/",today_date-1)]$Volume)

        if (avg_volume_6m > volume_cri){
          

          
          # filter out price
          last_prc = all_available[nrow(all_available)]$Adjusted
          if (last_prc < price_cri){

            # filter out those without enough data
            # To be conservative, assume every month has 31 days
            
            if (index(all_available[1]) < (today_date-sample_len_cri)){
              good_tickers = c(good_tickers, ticker)
            }else{
              next
            }
            
          }else{
            next
          }
          
        }else{
          next
        }
        
      },error = function(e) {
        NULL
      },warning = function(w) {
        NULL
      }) # end of tryCatch
      
      

    } # end screening loop  

    
    # output good tickers that fit the criterias  
    output$initial_screen <- renderPrint({
    isolate(print(good_tickers))
      
    })
    
    
    
    
    
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------
    # regression filtering
    
    # add progress bar to show currernt progress
    progress <-shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Doing Regression", value = 0)
    
    good_pairs = data.frame()
    pidx = 1
    
    for (i in 1:length(good_tickers)){
      for (j in i:length(good_tickers)){
        progress$inc(1/length(good_tickers)^2/2)
        if(good_tickers[i]==good_tickers[j]){
          next
        }
        # leg2 = leg1*beta
        
        
        
        # test1
        leg1 = getSymbols(as.character(good_tickers[i]), src="yahoo", auto.assign = F)[,6]
        leg2 = getSymbols(as.character(good_tickers[j]), src="yahoo", auto.assign = F)[,6]
        leg1 = na.locf(leg1)
        leg2 = na.locf(leg2)
        leg1 = na.locf(leg1, fromLast = TRUE)
        leg2 = na.locf(leg2, fromLast = TRUE)
        colnames(leg1) = "Adjusted"
        colnames(leg2) = "Adjusted"
        
        # slice data based on input range
        insample_testDate = paste0(Sys.Date()- (input$insampleMonth + input$outsampleMonth)*31, "/", Sys.Date()-input$outsampleMonth*31)##
        insample_1 = leg1[insample_testDate]
        insample_2 = leg2[insample_testDate]
        pca = prcomp(cbind(insample_1, insample_2))$rotation
        beta1 = pca[2,1]/pca[1,1] # leg2 = beta*leg1
        
        spread1 = leg1$Adjusted*beta1 - leg2$Adjusted
        spread1 = merge.xts(spread1, BBands(spread1, n=input$MA_length, sd=input$bband_dist))[paste0(Sys.Date()-(input$outsampleMonth)*31, "/", Sys.Date()-1)]##
        
        # out of sample test 1
        if (all(spread1$Adjusted> spread1$dn & spread1$Adjusted < spread1$up)){
          
          # test2
          insample_testDate = paste0(Sys.Date()-(input$insampleMonth + input$outsampleMonth + input$nextStartMonth)*31, "/", Sys.Date()-(input$outsampleMonth+ input$nextStartMonth)*31)##
          insample_1 = leg1[insample_testDate]
          insample_2 = leg2[insample_testDate]
          pca = prcomp(cbind(insample_1, insample_2))$rotation
          beta2 = pca[2,1]/pca[1,1] # leg2 = beta*leg1
          
          spread1 = leg1$Adjusted*beta1 - leg2$Adjusted
          spread1 = merge.xts(spread1, BBands(spread1, n=input$MA_length, sd=input$bband_dist))[paste0(Sys.Date()-(input$outsampleMonth+input$nextStartMonth)*31, "/", Sys.Date()-input$nextStartMonth*31-1)]##
          
          if (all(spread1$Adjusted> spread1$dn & spread1$Adjusted < spread1$up)){
            
            # test3
            insample_testDate = paste0(Sys.Date()-(input$insampleMonth + input$outsampleMonth + input$nextStartMonth*2)*31, "/", Sys.Date()-(input$outsampleMonth+ input$nextStartMonth*2)*31) ##
            insample_1 = leg1[insample_testDate]
            insample_2 = leg2[insample_testDate]
            pca = prcomp(cbind(insample_1, insample_2))$rotation
            beta3 = pca[2,1]/pca[1,1] # leg2 = beta*leg1
            
            spread1 = leg1$Adjusted*beta1 - leg2$Adjusted
            spread1 = merge.xts(spread1, BBands(spread1, n=input$MA_length, sd=input$bband_dist))[paste0(Sys.Date()-(input$outsampleMonth+input$nextStartMonth*2)*31, "/", Sys.Date()-input$nextStartMonth*2*31-1)]##
            
            if (all(spread1$Adjusted> spread1$dn & spread1$Adjusted < spread1$up)){
              good_pairs[pidx, "leg1"] = good_tickers[i]
              good_pairs[pidx, "leg2"] = good_tickers[j]
              good_pairs[pidx, "beta1"] = beta1
              good_pairs[pidx, "beta2"] = beta2
              good_pairs[pidx, "beta3"] = beta3
              
              pidx=pidx+1
            }else{
              next
            }
            
            
          }else{
            next
          }
          
          
        }else{
          next
        }
        
      }
    }# end of outer for loop
    
    #-------------------------------------------------------------------------------------------------------------------------------------------------------
    # end of regression filtering
    
    
    output$good_pairs <- renderPrint({
      isolate(print(good_pairs))
      
    })
    

  }) # end of observeEvent

} # end of server function

# Create Shiny app ----
shinyApp(ui = ui, server = server)