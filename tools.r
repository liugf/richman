library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

# 定义作图主题
theme<-chartTheme("white")
theme$dn.col <- "green"
theme$up.col <- "red"

#本地历史读数据
read<-function(symbol){  
  s<-as.xts(read.zoo(file=paste("/data/stocks/history/" ,symbol,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
  factor<-s$Adjusted / s$Close
  s$Open<-s$Open*factor
  s$Close<-s$Close*factor
  s$High<-s$High*factor
  s$Low<-s$Low*factor
  s
}

stock_list<-function() {
  l<-read.csv(file="/data/stocks/stocks.csv",header = TRUE,sep=",")
}

#模拟交易
trade<-function(tdata, capital=100000,unit=100) {
  amount<-0       #持股数量
  cash<-capital   #现金
  
  ticks<-data.frame()
  for(i in 1:nrow(tdata)){
    row<-tdata[i,]
    if(row$op=='B'){
      amount<-floor(cash/(row$Value*unit))*unit
      cash<-cash-amount*row$Value
    }
    if(row$op=='S'){
      cash<-cash+amount*row$Value
      amount<-0
    }
    row$cash<-cash #现金
    row$amount<-amount #持股数量
    row$asset<-cash+amount*row$Value # 资产总值
    ticks<-rbind(ticks,row)
  }
  ticks$diff<-c(0,diff(ticks$asset)) # 资产总值差
  ticks
}

#移动平均
ma<-function(cdata,mas=c(5,20,60), name="stock"){ 
  ldata<-cdata
  for(m in mas){
    ldata<-merge(ldata,EMA(cdata,m))
  }
  ldata<-na.locf(ldata, fromLast=TRUE)
  names(ldata)<-c(paste(name, '_value', sep=''), paste(name, '_ma',mas,sep=''))
  return(ldata)
}

# MACD
macd<-function(cdata,conf=c(12,26,9)){ 
  data<-cdata
  data$dif<-EMA(cdata,conf[1])-EMA(cdata,conf[2])
  data<-na.locf(data, fromLast=TRUE)
  data$dea<-EMA(data$dif,conf[3])
  data<-na.locf(data, fromLast=TRUE)
  names(data)<-c('value', 'dif', 'dea')
  return(data)
}

# BIAS
bias<-function(cdata,spans=c(5,10), name="stock"){ 
  data<-ma(cdata, spans)
  bias<-cdata
  for(i in 1:length(spans)) {
    bias<-merge(bias, (data[,1]-data[,1+i])/data[,1+i] * 100)
  }
  names(bias)<-c(paste(name, '_value', sep=''), paste(name, '_bias',spans,sep=''))
  return(bias)
}

market_value<-function(stock,ticks) {
  amount<-0       #持股数量
  cash<-ticks[1,]$asset   #现金 
  stock$asset<-0
  tick_index <- 1
  for(i in 1:nrow(stock)){    
    if (tick_index <= nrow(ticks) && row.names(ticks[tick_index,]) == index(stock[i])){
      amount<-ticks[tick_index,]$amount
      cash<-ticks[tick_index,]$cash
      tick_index<-tick_index+1      
    }
    stock[i,]$asset<-cash+amount*stock[i]$Adjusted # 资产总值
  }
  stock$asset
}

draw_close<-function(stock, title='stock'){
  close_ma<-ma(stock$Adjusted, c(5,20), 'close')
  g<-ggplot()
  g<-g+geom_line(aes(x=Index, y=Value),data=data.frame(fortify(close_ma[,1],melt=TRUE), type="close"))
  g<-g+geom_line(aes(x=Index, y=Value,colour=Series),data=data.frame(fortify(close_ma[,-1],melt=TRUE), type="close"))
  g<-g+facet_grid(type ~ .,scales = "free_y")
  g
}

draw_bias<-function(g, stock, spans=c(5,10)){
  data<-bias(stock$Adjusted, spans)
  g<-g+geom_line(aes(x=Index, y=Value, colour=Series),data=data.frame(fortify(data[,-1],melt=TRUE), type="bias"))
  g
}

draw_volume<-function(g, stock){
  volume_ma<-ma(stock$Volume, c(5,10), 'volume')
  g<-g+geom_bar(aes(x=Index, y=Value), stat="identity", data=data.frame(fortify(volume_ma[,1],melt=TRUE), type="volume"))
  g<-g+geom_line(aes(x=Index, y=Value,colour=Series),data=data.frame(fortify(volume_ma[,-1],melt=TRUE), type="volume"))
  g
}

draw_macd<-function(g, stock){
  data<-macd(stock$Adjusted)  
  g<-g+geom_line(aes(x=Index, y=Value,colour=Series),data=data.frame(fortify(data$dif,melt=TRUE), type="macd"))
  g<-g+geom_line(aes(x=Index, y=Value,colour=Series),data=data.frame(fortify(data$dea,melt=TRUE), type="macd"))
  g
}

draw_cash<-function(g,result) {
  adata<-as.xts(result[which(result$op=='S'),]['cash'])
  g<-g+geom_line(aes(x=as.Date(Index), y=Value, colour=Series),data=data.frame(fortify(adata,melt=TRUE), type='cash'))
  g
}

draw_range<-function(g, ticks, stock){
  #赚钱的操作
  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
  rise<-rise[order(row.names(rise)),]
  
  #赔钱的操作
  fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
  fall<-fall[order(row.names(fall)),]
  
  yrng <-range(stock$Adjusted)
  plan<-as.xts(rise[c(1,2)])
  if(empty(plan)){
    rise_plan<-data.frame()
  } else {
    rise_plan<-data.frame(start=as.Date(index(plan)[which(plan$op=='B')]),end=as.Date(index(plan)[which(plan$op=='S')]),plan='rise_plan')
  }
  plan<-as.xts(fall[c(1,2)])
  if(empty(plan)){
    fall_plan<-data.frame()
  } else {
    fall_plan<-data.frame(start=as.Date(index(plan)[which(plan$op=='B')]),end=as.Date(index(plan)[which(plan$op=='S')]),plan='fall_plan')
  }
  plan<-rbind(rise_plan, fall_plan)
  plan$type<-'close'
  plan$low<-yrng[1]
  plan$high<-yrng[2]
  g<-g+geom_rect(aes(xmin=start,xmax=end,ymin=low,ymax=high,fill=plan),data=plan)  
  g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))
  g
}

trade_all<-function(stocks,strategy,capital=100000,period=''){
  stocks$asset<-capital
  for(i in 1:nrow(stocks)) {
    stock<-read(stocks[i,]$symbol)[period]
    if(nrow(stock)==0) next
    
    tdata<-strategy(stock)
    ticks<-trade(tdata,100000,stocks[i,]$unit)  
    value<-market_value(stock,ticks)
    stocks[i,]$asset<-last(value)$asset
  }
  stocks
}
