source(file="tools.r",encoding="UTF-8")
source(file="strategy1.r",encoding="UTF-8")

symbol<-"00001.HK"
stock<-read(symbol)['2013-07/2014-11']

chartSeries(stock,name=symbol,theme=theme,TA = "addVo();addBBands();addSMA(); addMACD();")

tdata<-trade_signal(stock)
result<-trade(tdata,100000)
# 
# g<-draw_close(stock)
# g<-draw_volume(g, stock)
# g<-draw_macd(g, stock)
# g<-draw_bias(g, stock)
# g<-draw_cash(g, result)
# g<-draw_range(g, result, stock)
# g
