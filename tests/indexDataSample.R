require("quantmod")
require("xts")


index.data <- new.env()

symbols<-c('XLB','XLE','XLF','XLI','XLK','XLP','XLU','XLV','XLY','EFA', 'BND')

getSymbols.tiingo(symbols,
                  index.data,
                  api.key = "insert_key_here",
                  return.class = "xts", 
                  periodicity = "daily",
                  from = "2014-01-01",
                  to = Sys.Date())



