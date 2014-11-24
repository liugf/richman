tradeSignal<-function(stock) {
  close<-stock$Adjusted
  data<-close
  data$dif<-EMA(close,12)-EMA(close,26)
  data<-na.locf(data, fromLast=TRUE)
  names(data)<-c('Value', 'dif')
  
  data$dea<-EMA(data$dif,9)
  data<-na.locf(data, fromLast=TRUE)
  
  data$vo5<-EMA(stock$Volume,5)
  data$vo20<-EMA(stock$Volume,20)
  data<-na.locf(data, fromLast=TRUE)
  data<-data.frame(data)
  
  result<-data.frame()
  hold_stock=FALSE
  for(i in 28:nrow(data)){
    today<-data[i,]
    
    if(hold_stock && today$dif<0){
      today$op<-'S'
      result<-rbind(result,today[,c('Value', 'op')])
      hold_stock<-FALSE
    }
    
    if(!hold_stock && today$dif>0){      
      today$op<-'B'
      result<-rbind(result,today[,c('Value', 'op')])
      hold_stock<-TRUE
    }
  }
  result
}