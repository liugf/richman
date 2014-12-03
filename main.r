source(file="tools.r",encoding="UTF-8")
source(file="strategy1.r",encoding="UTF-8")

symbol<-"01886.HK"
stock<-read(symbol)['2013-07/2014-11']

# chartSeries(stock,name=symbol,theme=theme,TA = "addVo();addBBands();addSMA(); addMACD();")

tdata<-trade_signal(stock)
ticks<-trade(tdata,100000,2000)

g<-draw_close(stock)
g<-draw_volume(g, stock)
g<-draw_macd(g, stock)
g<-draw_bias(g, stock)
g<-draw_cash(g, ticks)
g<-draw_range(g, ticks, stock)
g
