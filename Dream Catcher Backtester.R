library(ggplot2)
library(lubridate)
library(plotly)
library(quantmod)
#library(Rblpapi)
library(xts)
options(scipen = 999)
#blpConnect()
rm(list=ls())

#setwd("C:/Users/Admin/Documents/Dream Catcher Backtester/")
#dataAll = as.xts(read.zoo("data.csv", header=T, sep=","))
#symbols = read.csv("EquitySymbols.csv")

symbols = data.frame(
  Inst1=c("AEP","LBRDK","LBTYA","UN","Z"),
  Inst2=c("XEL" ,"LBRDA","LBTYK","UL","ZG"),
  Beta =c(-1.26     ,-1     ,-1     ,-1  ,-1),
  stringsAsFactors=F)

#---------------------------------------------------
startDate = as.Date("2007-01-01")
inSampleWindow = years(5)
outSampleWindow = months(6)
yearsToTest = years(7)

localData = TRUE

sd1 = 2
sd2 = 6
ma = 100

percentileWidth = 0

risk = 10000
marginAPR = 0.015
transactionCost = 0.003

#---------------------------------------------------
ledgerPortfolio = xts()

for(x in 1:nrow(symbols)){

# Collect data
if(localData){
  # Pull data from local file
  data = merge(
          getSymbols(symbols[x,"Inst1"], auto.assign=F)[,4],
          getSymbols(symbols[x,"Inst2"], auto.assign=F)[,4]
  )
  #data = na.omit(dataAll[,c(test[x,1],test[x,2])])
}else{
  # Pull data form Bloomberg terminal
  if(inst2 == ""){
    # Run test on single instrument
    data = bdh(securities=inst1,fields="PX_LAST",start.date=as.Date("1970-01-01"))
    data = xts(data[,-1], order.by=data[,1])
    data = merge(data, 0)
  }else{
    # Run test on spread of two intruments
    data = bdh(securities=c(inst1,inst2),fields="PX_LAST",start.date=as.Date("1970-01-01"))
    data = lapply(data, function(d) xts(d[,-1], order.by=d[,1]))
    data = na.omit(do.call(merge, data))
  }  
}
names(data) = c("Inst1Px","Inst2Px")

# Modify logic to use rolling insample beta calculation if "beta" variable is set to 0
# for(j in 1:(yearsToTest/outSampleWindow)){
#   start = startDate + (j-1) * outSampleWindow
#   end = start + inSampleWindow
#   inSampleDates = paste0(start, "/", end)
#   outSampleDates = paste0(end, "/", end + outSampleWindow)
#   r = princomp(~ data[,1] + data[,2])
#   b = r$loadings[1,1] / r$loadings[2,1]  
# }

#**************************************************************************************************************
# Calculate Beta
# slice raw data into selected range
insample_testDate = paste0(Sys.Date()-(inSampleWindow + outSampleWindow), "/", Sys.Date()-inSampleWindow)##
tryCatch({
  pca = prcomp(cbind(data$Inst1Px[insample_testDate], data$Inst2Px[insample_testDate]))$rotation
  beta = pca[1,1]/pca[2,1]
  },error = function(e) {
    print("invalid data, failed to calculate Beta, assuming beta=1")
    beta = 1
  },warning = function(w) {
    print("invalid data, failed to calculate Beta, assuming beta=1")
    beta = 1
  }) # end of tryCatch
#**************************************************************************************************************



data$Beta = symbols[x,"Beta"]
data$SprdPx = data[,"Inst1Px"] + data[,"Inst2Px"] * data[,"Beta"]

# Compute MA and Bands
data$MA = rollmean(data[,"SprdPx"], k=ma, align="right")
data$Lower1 = BBands(data[,"SprdPx"],ma,sd=sd1)[,"dn"]
data$Upper1 = BBands(data[,"SprdPx"],ma,sd=sd1)[,"up"]
data$Lower2 = BBands(data[,"SprdPx"],ma,sd=sd2)[,"dn"]
data$Upper2 = BBands(data[,"SprdPx"],ma,sd=sd2)[,"up"]

# Use percentile band widths if UDV is > 0
if(percentileWidth > 0){
  # Compute moving average extremes
  crossMA = data
  crossMA$Signal = crossMA[,"SprdPx"] - crossMA[,"MA"]
  crossMA = crossMA[(crossMA[,"Signal"] * lag(crossMA[,"Signal"]) <= 0),]
  extremes = c()
  for(i in 1:(nrow(crossMA) - 1)){
    start = index(crossMA)[i]
    end = index(crossMA)[i + 1]
    mavg = crossMA[i, "MA"]
    if(crossMA[i,"Signal"] > 0){
      max = max(data[index(data) >= start & index(data) < end, "SprdPx"])
      extremes = rbind(extremes, abs(max - mavg))
    }else{
      min = min(data[index(data) >= start & index(data) < end, "SprdPx"])
      extremes = rbind(extremes, abs(min - mavg))
    }
  }

  # Compute bands
  data$Lower1 = NA
  data$Lower2 = NA
  data$Upper1 = NA
  data$Upper2 = NA
  for(j in 1:(yearsToTest/outSampleWindow)){
    start = startDate + (j-1) * outSampleWindow
    end = start + inSampleWindow
    inSampleDates = paste0(start, "/", end)
    outSampleDates = paste0(end, "/", end + outSampleWindow)
    width = round(quantile(extremes[inSampleDates,], widthPercentile), 0)
    data[outSampleDates,"Lower1"] = data[outSampleDates,"MA"] - width*sd1
    data[outSampleDates,"Lower2"] = data[outSampleDates,"MA"] - width*sd2
    data[outSampleDates,"Upper1"] = data[outSampleDates,"MA"] + width*sd1
    data[outSampleDates,"Upper2"] = data[outSampleDates,"MA"] + width*sd2
  }
}

data = na.omit(data[paste0(startDate,"/")])

# Backtest
pos = 0
pnl = 0
maReset = 0
ledgerTransaction = data.frame(
                      Entry.Date=as.Date(character(0)) ,Entry.Type=character(0), Entry.Price=numeric(0),
                      Quantity=numeric(0), Exit.Date=as.Date(character(0)), Exit.Type=character(0), Exit.Price=numeric(0), 
                      Duration=numeric(0), CarryCost=numeric(0), TransactionCost=numeric(0),
                      stringsAsFactors=F )
ledgerDaily = merge(
                data[,c("Inst1Px","Inst2Px","Beta","SprdPx")],
                xts(matrix(NA,nrow(data),6),order.by=index(data),dimnames=list(NULL,c("Inst1Qty","Inst2Qty","PnlGross","CarryCost","Fees","PnlNet"))) )

for(k in 2:(nrow(data)-1)){
  priceLast = as.numeric( data[k,"SprdPx"] )
  pricePrev = as.numeric( data[k-1,"SprdPx"] )
  # Check for entry
  if(pos == 0){
    if( maReset == 1 ){
      if( (( as.numeric(data[k-1,"SprdPx"]) - as.numeric(data[k-1,"MA"]) ) * ( as.numeric(data[k,"SprdPx"]) - as.numeric(data[k,"MA"]) )) <= 0 ){
        maReset = 0
      }
      ledgerDaily[k,c("Inst1Qty","Inst2Qty","PnlGross","CarryCost","Fees","PnlNet")] = 0
    }
    if( maReset == 0 ){
      if( priceLast <= as.numeric(data[k-1,"Lower1"]) ){
        entry = as.numeric( data[k-1,"Lower1"] )
        target = as.numeric( data[k-1,"MA"] )
        stop = as.numeric( data[k-1,"Lower2"] )
        pos = round( risk / (entry - stop) )
        beta = data[k,"Beta"]
        ledgerTransaction[nrow(ledgerTransaction)+1,] = data.frame( index(data[k,]), "Entry Buy", entry, pos, NA, NA, NA, NA, NA, NA, stringsAsFactors=F)
        ledgerDaily[k,"Inst1Qty"] = pos
        ledgerDaily[k,"Inst2Qty"] = pos * beta
        ledgerDaily[k,"PnlGross"] = (priceLast-entry)*pos
        ledgerDaily[k,"CarryCost"] = as.numeric(abs(ledgerDaily[k,"Inst1Px"] * ledgerDaily[k,"Inst1Qty"]) + abs(ledgerDaily[k,"Inst2Px"] * ledgerDaily[k,"Inst2Qty"])) *
                                     as.numeric(index(ledgerDaily[k+1,]) - index(ledgerDaily[k,])) / 360 * -marginAPR
        ledgerDaily[k,"Fees"] = (abs(pos) + abs(pos * beta)) * transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else if( priceLast >= as.numeric(data[k-1,"Upper1"]) ){
        entry = as.numeric( data[k-1,"Upper1"] )
        target = as.numeric( data[k-1,"MA"] )
        stop = as.numeric( data[k-1,"Upper2"] )
        pos = round( risk / (entry - stop) )
        beta = data[k,"Beta"]
        ledgerTransaction[nrow(ledgerTransaction)+1,] = data.frame( index(data[k,]), "Entry Sell", entry, pos, NA, NA, NA, NA, NA, NA, stringsAsFactors=F)
        ledgerDaily[k,"Inst1Qty"] = pos
        ledgerDaily[k,"Inst2Qty"] = pos * beta
        ledgerDaily[k,"PnlGross"] = (priceLast-entry)*pos
        ledgerDaily[k,"CarryCost"] = as.numeric(abs(ledgerDaily[k,"Inst1Px"] * ledgerDaily[k,"Inst1Qty"]) + abs(ledgerDaily[k,"Inst2Px"] * ledgerDaily[k,"Inst2Qty"])) *
                                     as.numeric(index(ledgerDaily[k+1,]) - index(ledgerDaily[k,])) / 360 * -marginAPR
        ledgerDaily[k,"Fees"] = (abs(pos) + abs(pos * beta)) * transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else{
        ledgerDaily[k,c("PnlGross","Inst1Qty","Inst2Qty","CarryCost","Fees","PnlNet")] = 0
      }
    }
  }else{
    # Manage open position
    if(pos > 0){
      if( priceLast >= target){
        pnl = pnl + sd1/(sd2-sd1)
        ledgerTransaction[nrow(ledgerTransaction), 5:8] = data.frame( index(data[k,]), "Target Sell", target, index(data[k,])-last(ledgerTransaction$Entry.Date), stringsAsFactors=F)
        ledgerDaily[k,"PnlGross"] = (target-pricePrev)*pos
        pos = 0
        ledgerDaily[k,c("Inst1Qty","Inst2Qty","CarryCost")] = 0
        ledgerDaily[k,"Fees"] = sum(abs(ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")])) * -transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else if( priceLast <= stop){
        pnl = pnl - 1
        maReset = 1
        ledgerTransaction[nrow(ledgerTransaction), 5:8] = data.frame( index(data[k,]), "Stop Sell", stop, index(data[k,])-last(ledgerTransaction$Entry.Date), stringsAsFactors=F)
        ledgerDaily[k,"PnlGross"] = (stop-pricePrev)*pos
        pos = 0
        ledgerDaily[k,c("Inst1Qty","Inst2Qty","CarryCost")] = 0
        ledgerDaily[k,"Fees"] = sum(abs(ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")])) * -transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else{
        ledgerDaily[k,c("Inst1Qty","Inst2Qty")] = ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")]
        ledgerDaily[k,"PnlGross"] = (priceLast-pricePrev)*pos
        ledgerDaily[k,"CarryCost"] = as.numeric((abs(ledgerDaily[k,"Inst1Px"] * ledgerDaily[k,"Inst1Qty"]) + abs(ledgerDaily[k,"Inst2Px"] * ledgerDaily[k,"Inst2Qty"]))) *
                                     as.numeric(index(ledgerDaily[k+1,]) - index(ledgerDaily[k,])) / 360 * -marginAPR
        ledgerDaily[k,"Fees"] = 0
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }
    }
    if(pos < 0){
      if( priceLast <= target){
        pnl = pnl + sd1/(sd2-sd1)
        ledgerTransaction[nrow(ledgerTransaction), 5:8] = data.frame( index(data[k,]), "Target Buy", target, index(data[k,])-last(ledgerTransaction[,"Entry.Date"]), stringsAsFactors=F)
        ledgerDaily[k,"PnlGross"] = (target-pricePrev)*pos
        pos = 0
        ledgerDaily[k,c("Inst1Qty","Inst2Qty","CarryCost")] = 0
        ledgerDaily[k,"Fees"] = sum(abs(ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")])) * -transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else if( priceLast >= stop){
        pnl = pnl - 1
        maReset = 1
        ledgerTransaction[nrow(ledgerTransaction), 5:8] = data.frame( index(data[k,]), "Stop Buy", stop, index(data[k,])-last(ledgerTransaction[,"Entry.Date"]), stringsAsFactors=F)
        ledgerDaily[k,"PnlGross"] = (stop-pricePrev)*pos
        pos = 0
        ledgerDaily[k,c("Inst1Qty","Inst2Qty","CarryCost")] = 0
        ledgerDaily[k,"Fees"] = sum(abs(ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")])) * -transactionCost
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }else{
        ledgerDaily[k,c("Inst1Qty","Inst2Qty")] = ledgerDaily[k-1,c("Inst1Qty","Inst2Qty")]
        ledgerDaily[k,"PnlGross"] = (priceLast-pricePrev)*pos
        ledgerDaily[k,"CarryCost"] = as.numeric(abs(ledgerDaily[k,"Inst1Px"] * ledgerDaily[k,"Inst1Qty"]) + abs(ledgerDaily[k,"Inst2Px"] * ledgerDaily[k,"Inst2Qty"])) * 
                                     as.numeric(index(ledgerDaily[k+1,]) - index(ledgerDaily[k,])) / 360 * -marginAPR
        ledgerDaily[k,"Fees"] = 0
        ledgerDaily[k,"PnlNet"] = sum(ledgerDaily[k,c("PnlGross","CarryCost","Fees")])
      }
    }
  }
}
ledgerDaily = na.omit(ledgerDaily)
ledgerDaily$RollingGross = cumsum(ledgerDaily[,"PnlGross"])
ledgerDaily$RollingNet = cumsum(ledgerDaily[,"PnlNet"])

for(m in 1:nrow(ledgerTransaction)){
  if(is.na(ledgerTransaction[m,"Exit.Date"])){
    tradeDates = paste0(ledgerTransaction[m,"Entry.Date"],"/",last(index(data)))
  }else{
    tradeDates = paste0(ledgerTransaction[m,"Entry.Date"],"/",ledgerTransaction[m,"Exit.Date"]-1)
  }
  tradeQuantity = abs(ledgerTransaction[m,"Quantity"])
  tradeBeta = as.numeric(data[ledgerTransaction[m,"Entry.Date"],"Beta"])
  ledgerTransaction[m,"CarryCost"] = (abs(mean(data[tradeDates,"Inst1Px"]) * tradeQuantity) + abs(mean(data[tradeDates,"Inst2Px"]) * tradeQuantity * tradeBeta)) * (ledgerTransaction[m,"Duration"]-1) / 360 *  -marginAPR
  ledgerTransaction[m,"TransactionCost"] = (abs(tradeQuantity * 2) + abs(tradeQuantity * tradeBeta * 2)) * -transactionCost
}

graphData = as.data.frame(data)
graphData$Date = as.Date(row.names(graphData))
p = plot_ly()
p = add_trace(p, data=graphData, x=~Date, y=~SprdPx, name="Price Line", type="scatter", mode="lines", line=list(color="#1f77b4"))
p = add_trace(p, x=~Date, y=~SprdPx, name="Price Point", type="scatter", mode="markers", marker=list(color="#1f77b4"), visible="legendonly")
p = add_trace(p, x=~Date, y=~MA, name="MA", type="scatter", mode="lines", line=list(color="#ff7f0e"))
p = add_trace(p, x=~Date, y=~Lower1, name="Lower1", type="scatter", mode="lines", line=list(color="#7f7f7f", width=1))
p = add_trace(p, x=~Date, y=~Lower2, name="Lower2", type="scatter", mode="lines", line=list(color="8c564b", width=1))
p = add_trace(p, x=~Date, y=~Upper1, name="Upper1", type="scatter", mode="lines", line=list(color="#7f7f7f", width=1))
p = add_trace(p, x=~Date, y=~Upper2, name="Upper2", type="scatter", mode="lines", line=list(color="8c564b", width=1))

for(l in 1:nrow(ledgerTransaction)){
  tradeData = data.frame( tradeDate=c(ledgerTransaction[l,"Entry.Date"], ledgerTransaction[l,"Exit.Date"]), 
                tradePrice=c(ledgerTransaction[l,"Entry.Price"], ledgerTransaction[l,"Exit.Price"]) )
  if(ledgerTransaction[l,"Entry.Type"] == "Entry Buy"){
    tradeColor = "#2ca02c" #cooked asparagus green
  }else{
    tradeColor = "#d62728" #brick red
  }
  p = add_trace(p, x=tradeData[,"tradeDate"], y=tradeData[,"tradePrice"], name=paste0("Trade ",l), type="scatter", mode="lines+markers", line=list(color=tradeColor,dash="dot"), marker=list(color=tradeColor,size=10))
}
print(p)

print(ledgerTransaction)
print(paste0("Realized Pnl: ", pnl * risk))
print(paste0("Carry Cost: ", round(100 * sum(ledgerTransaction[,"CarryCost"], na.rm=T ) / last(ledgerDaily[,"RollingGross"])), " %"))
g = ggplot(ledgerDaily, aes(x=index(ledgerDaily))) + geom_line(aes(y=RollingGross),color="blue") + geom_line(aes(y=RollingNet),color="black")
windows();print(g)

ledgerPortfolio = merge(ledgerPortfolio, ledgerDaily[,"PnlNet"])

}

ledgerPortfolio$Total = rowSums(ledgerPortfolio, na.rm=T)
ledgerPortfolio$Rolling = cumsum(ledgerPortfolio$Total)





ui <- fluidPage(
  
  # App title ----
  titlePanel("Dream Catcher Backtester"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # File input, ticker list
      fileInput("ticker_file", "Tickers to be tested, in .csv",
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
