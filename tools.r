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