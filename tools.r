library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

#本地历史读数据
read<-function(symbol){  
  as.xts(read.zoo(file=paste("/data/stocks/history/" ,symbol,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
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

#模拟交易
trade<-function(tdata,capital=100000,position=1,fee=0.00003){#交易信号,本金,持仓比例,手续费比例
  amount<-0       #持股数量
  cash<-capital   #现金
  
  ticks<-data.frame()
  for(i in 1:nrow(tdata)){
    row<-tdata[i,]
    if(row$op=='B'){
      amount<-floor(cash/row$Value)
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
  
  #赚钱的操作
  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
  rise<-rise[order(row.names(rise)),]
  
  #赔钱的操作
  fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
  fall<-fall[order(row.names(fall)),]
  
  
  return(list(
    ticks=ticks,
    rise=rise,
    fall=fall
  ))
}

draw_close<-function(stock, title='stock'){
  close_ma<-ma(stock$Adjusted, c(5,20), 'close')
  g<-ggplot()
  g<-g+geom_line(aes(x=Index, y=Value),data=data.frame(fortify(close_ma[,1],melt=TRUE), type="close"))
  g<-g+geom_line(aes(x=Index, y=Value,colour=Series),data=data.frame(fortify(close_ma[,-1],melt=TRUE), type="close"))
  g<-g+facet_grid(type ~ .,scales = "free_y")
  g
}

draw_bias<-function(g, stock, span=c(12,26)){
  data<-ma(stock$Adjusted, span)
  data$bias<-(data[,1]-data[,2])/data[,2] * 100
  g<-g+geom_line(aes(x=Index, y=Value),data=data.frame(fortify(data$bias,melt=TRUE), type="bias"))
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
  adata<-as.xts(result$ticks[which(result$ticks$op=='S'),]['cash'])
  g<-g+geom_line(aes(x=as.Date(Index), y=Value, colour=Series),data=data.frame(fortify(adata,melt=TRUE), type='cash'))
  g
}

draw_range<-function(g, result, stock){
  yrng <-range(stock$Adjusted)
  plan<-as.xts(result$rise[c(1,2)])
  rise_plan<-data.frame(start=as.Date(index(plan)[which(plan$op=='B')]),end=as.Date(index(plan)[which(plan$op=='S')]),plan='rise_plan')
  plan<-as.xts(result$fall[c(1,2)])
  fall_plan<-data.frame(start=as.Date(index(plan)[which(plan$op=='B')]),end=as.Date(index(plan)[which(plan$op=='S')]),plan='fall_plan')
  plan<-rbind(rise_plan, fall_plan)
  plan$type<-'close'
  plan$low<-yrng[1]
  plan$high<-yrng[2]
  g<-g+geom_rect(aes(xmin=start,xmax=end,ymin=low,ymax=high,fill=plan),data=plan)  
  g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))
  g
}

