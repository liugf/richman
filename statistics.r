source(file="tools.r",encoding="UTF-8")
source(file="strategy1.r",encoding="UTF-8")

money<-300000
stocks<-stock_list()
stocks$assets<-0
stocks<-trade_all(stocks[1:30,], trade_signal,money,'2013-01/2013-12')

good_stocks<-stocks[which(stocks$asset>money),]

good_stocks<-trade_all(good_stocks, trade_signal,money,'2012-01/2013-12')
good_stocks
