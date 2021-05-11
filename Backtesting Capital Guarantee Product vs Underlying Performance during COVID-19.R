install.packages("xts")
library("quantmod")

#########################################################################################################
#Write a Backtesting Function 
#to compare the return of Capital Guarantee Product (Principal Guarantee Note) vs Underlying during COVID-19 (period: 2019-09-27 - 2020-03-27)
backtesting_function <- function (stock_data){
  notional <- 100
  r <- 0.0012
  TTM <- 0.5
  relative_price <- stock_data[[2,1]]/stock_data[[1,1]]*100

  underlying_price <- relative_price
  underlying_price_return <- underlying_price/notional - 1 #Return of underlying

  principal_guarantee_note_CF <- 100 + max((relative_price-100)*(100-100*exp(-r*TTM)),0) #Final Payoff of Principal Guarantee Note
  principal_guarantee_note_return <- principal_guarantee_note_CF/100 - 1 #Return of Principal Guarantee Note
  
  print(underlying_price_return) #Print return of Fixed-coupon note
  print(principal_guarantee_note_return) #Print return of Principal guarantee note
  
  summary <- setNames(data.frame(matrix(ncol = 2, nrow = 1)), c("Fixed-coupon note return", "Principal guarantee note return"))
  summary[1] <- underlying_price_return
  summary[2] <- principal_guarantee_note_return
  View(summary)
}

#########################################################################################################
#Backtest GSPC (i.e. market return)
getSymbols("^GSPC", from = "2019-09-27", to = "2020-03-28") #load historical data
GSPC_clean <- GSPC$GSPC.Adjusted[c("2019-09-27","2020-03-27")] #evaluation dates

backtesting_function(GSPC_clean) #Call the backtesting function

#########################################################################################################
#Backtest GOOG
getSymbols("GOOG", from = "2019-09-27", to = "2020-03-28") #load historical data
GOOG_clean <- GOOG$GOOG.Adjusted[c("2019-09-27","2020-03-27")] #evaluation dates

backtesting_function(GOOG_clean) #Call the backtesting function

#########################################################################################################
#Backtest FB
getSymbols("FB", from = "2019-09-27", to = "2020-03-28") #load historical data
FB_clean <- FB$FB.Adjusted[c("2019-09-27","2020-03-27")] #evaluation dates

backtesting_function(FB_clean) #Call the backtesting function

#########################################################################################################
#Backtest AAPL
getSymbols("AAPL", from = "2019-09-27", to = "2020-03-28") #load historical data
AAPL_clean <- AAPL$AAPL.Adjusted[c("2019-09-27","2020-03-27")] #evaluation dates

backtesting_function(AAPL_clean) #Call the backtesting function
