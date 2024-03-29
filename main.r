source(file="tools.r",encoding="UTF-8")
source(file="strategy1.r",encoding="UTF-8")

symbol<-"00024.HK"
stock<-read(symbol)['2013-01/2013-12']

tdata<-trade_signal(stock)
ticks<-trade(tdata,300000,2000)

# value<-market_value(stock,ticks)

# chartSeries(stock,name=symbol,theme=theme,TA = "addVo();addBBands();addSMA(); addMACD();")
# addTA(value)

g<-draw_close(stock)
g<-draw_volume(g, stock)
g<-draw_macd(g, stock)
g<-draw_bias(g, stock)
g<-draw_cash(g, ticks)
g<-draw_range(g, ticks, stock)
g
