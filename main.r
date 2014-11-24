source(file="tools.r",encoding="UTF-8")
source(file="strategy1.r",encoding="UTF-8")

symbol<-"00001.HK"
stock<-read(symbol)['2012-01/2014-10']

tdata<-tradeSignal(stock)
result<-trade(tdata,100000)

