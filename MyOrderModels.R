#arima
library(forecast)
library(smooth)
myarima = function(DDS,goriz,Test.len,real.graph,smooth.graph){
  source("PrepData.R")
  vvv<-vybros.values(DDS)
  ymaxim<-max(DDS$QNT)
  DDS<-vybros.corr(DDS)
  End.train<-length(DDS$QNT)-Test.len
  DDS.train <-DDS[1:End.train,]
  DDS.test<-DDS[(End.train+1):(End.train+Test.len),]
  DDS.testonly<-DDS$QNT;DDS.testonly[1:End.train]<-NA;
  DDS.train.ts <- ts(DDS.train$QNT)
  DDS.arima<- auto.arima(DDS.train.ts)
  fore <- forecast(DDS.arima, h=goriz)
  fore.values<-as.vector(round(fore$mean))
  DDS.foreonly<-DDS$QNT;DDS.foreonly[1:End.train]<-NA;DDS.foreonly[(End.train+1):(End.train+Test.len)]<-fore.values[1:min(goriz,Test.len)];

  if (real.graph=='yes') {
  plot(fore,ylim=c(0,ymaxim),xlab='date',ylab='Qnt')
  points(vvv$xv, vvv$yv, pch=16, col='red') 
  lines(fitted(fore),col='red')
  lines(DDS.testonly)
  }  
  if (smooth.graph=='yes') 
  {
  plot(TTR::SMA(DDS$QNT,7),ylim=c(0,ymaxim),type='l',main='smooth data',xlab='date',ylab='Qnt')
  points(vvv$xv, vvv$yv, pch=16, col='red')  
  lines(fitted(fore),col='red')
  lines(DDS.foreonly,col='blue')
  }
  #summary(DDS.arima)
  #Mean Absolute Percentage Error in Percentage
  MyMape<-MAPE(DDS$QNT[(End.train+1):(End.train+min(goriz,Test.len))],fore.values[1:min(goriz,Test.len)],digits=5)*100
  print(c('Mean Absolute Percentage Error = ', MyMape))
  result<-data.frame(fore.values, fore$upper[,1], MyMape)
  names(result) <- c('fore.values','upper80','MyMape')
result
}

#first.date, last.date - ?????? ? ????????? ??? ???????
#first.date<-order.day
#last.date<-sup.day

#q - days of forecast
myarima.train.sum = function(DDS,first.date,q, weatherCSV, real.graph,smooth.graph){ 
  source("PrepData.R")
  if (DDS$DATE[length(DDS$DATE)]<first.date) {End.train<-length(DDS$DATE)
  }else {End.train<-DDS$DailyInd[DDS$DATE==first.date]-1}
  vvv<-vybros.values(DDS)
  ymaxim<-max(DDS$QNT)
  #DDS<-vybros.corr(DDS)
  #Test.len<-DDS$DailyInd[DDS$DATE==last.date]-DDS$DailyInd[DDS$DATE==first.date]
  DDS.train <-DDS[1:End.train,]
  #DDS.test<-DDS[(End.train+1):(End.train+Test.len),]
  #DDS.testonly<-DDS$QNT;DDS.testonly[1:End.train]<-NA;
  DDS.train.ts <- ts(DDS.train$QNT)
  wait.days<-as.numeric(first.date-DDS$DATE[DDS$DailyInd==End.train])-1
  goriz<-wait.days+q
  last.date<-first.date+days(q-1)
  if(!missing(weatherCSV)) {
    weather<-Prep.weather.Data.spb(weatherCSV,DDS$DATE[1],last.date)
    DDS.arima<- auto.arima(DDS.train.ts,xreg=weather$T[1:End.train])
    fore <- forecast(DDS.arima,xreg=weather$T[(End.train+1):(End.train+q+1)], h=goriz)
  } 
  else{
    DDS.arima<- auto.arima(DDS.train.ts)
    fore <- forecast(DDS.arima, h=goriz)
  }
 
  fore.values<-as.vector(round(fore$mean))
  DDS.foreonly<-DDS$QNT;DDS.foreonly[1:End.train]<-NA;DDS.foreonly[(End.train+1):(End.train+Test.len)]<-fore.values[1:min(goriz,Test.len)];
  
  if (real.graph=='yes') {
    plot(fore,ylim=c(0,ymaxim))
    points(vvv$xv, vvv$yv, pch=16, col='red') 
    lines(fitted(fore),col='red')
    lines(DDS.testonly)
  }  
  if (smooth.graph=='yes') 
  {
    plot(TTR::SMA(DDS$QNT,7),ylim=c(0,ymaxim),type='l',main='smooth data')
    points(vvv$xv, vvv$yv, pch=16, col='red')  
    lines(fitted(fore),col='red')
    lines(DDS.foreonly,col='blue')
  }
  #summary(DDS.arima)
  #Mean Absolute Percentage Error in Percentage
  fore.values[(wait.days+1):goriz]
  fore.sum<-round(sum(fore$mean))
  #MyMape.values<-MAPE(DDS$QNT[(End.train+1):(End.train+min(goriz,Test.len))],fore.values[1:min(goriz,Test.len)],digits=5)*100
  #cat(c('Mean Absolute Percentage Error = ', MyMape))
  result<-data.frame(c((End.train+1):(End.train+q+1)), fore.values, fore$upper[,1])
  names(result) <- c('x','fore.values','upper80')
  result
}

#вычисление q прогнозных значений начиная с first.date используя модель mymodel = "arima" или "lr" (регрессия)
#можно использовать данные о погоде  "weatherCSV"
get.fore.values = function(DDS,first.date,q, mymodel,  weatherCSV){ 
  source("PrepData.R")
  if (DDS$DATE[length(DDS$DATE)]<first.date) {End.train<-length(DDS$DATE)
  }else {End.train<-DDS$DailyInd[DDS$DATE==first.date]-1}
  #vvv<-vybros.values(DDS); DDS<-vybros.corr(DDS)
  
  #Test.len<-DDS$DailyInd[DDS$DATE==last.date]-DDS$DailyInd[DDS$DATE==first.date]
  DDS.train <-DDS[1:End.train,]
  #DDS.test<-DDS[(End.train+1):(End.train+Test.len),]
  #DDS.testonly<-DDS$QNT;DDS.testonly[1:End.train]<-NA;
  wait.days<-as.numeric(first.date-DDS$DATE[DDS$DailyInd==End.train])-1
  goriz<-wait.days+q
  last.date<-first.date+days(q-1)
  
  #arima 
  if (mymodel=='arima') {
    print('arima')
    DDS.train.ts <- ts(DDS.train$QNT)
    if(!missing(weatherCSV)) {
      print('with weather')
      weather<-Prep.weather.Data(weatherCSV,DDS$DATE[1],last.date)
      weather.spb.train<-weather[1:End.train,]
      weather.spb.fore<-weather[(End.train+1):(End.train+goriz),]
      DDS.arima<- auto.arima(DDS.train.ts,xreg=weather.spb.train$T)
      fore <- forecast(DDS.arima,xreg=weather.spb.fore$T, h=goriz)
    } 
    else{
      print('without weather')
      DDS.arima<- auto.arima(DDS.train.ts)
      fore <- forecast(DDS.arima, h=goriz)
    }
    fore.values<-as.vector(fore$mean)
  }
  #lr 
  if (mymodel=='lr') {
    print('lr')
    DATE.fore<-DDS$DATE[DDS$DailyInd==End.train] + days(1:goriz)
    newdata<-DDS[1:goriz,]
    newdata$DATE<-DATE.fore
    newdata$year<-year(newdata$DATE)
    newdata$month<-month(newdata$DATE)
    newdata$DailyInd<-DDS$DailyInd[End.train]+c(1:goriz)
    DDS.new<-rbind(DDS,newdata)
    months<-data.frame(X1=1:(End.train+goriz),2,3,4,5,6,7,8,9,10,11,12)
    for (i in c(1:12)) {
      for (j in c(1:(End.train+goriz))) {
        if (i==DDS.new$month[j]) {months[j,i]<-1}
        else {months[j,i]<-0}
      }
    } 
    weeks<-data.frame(W1=1:(End.train+goriz),W2=2,W3=3,W4=4,W5=5,W6=6,W7=7)
    for (i in c(1:7)) {
      for (j in c(1:(End.train+goriz))) {
        if (i==wday(DDS.new$DATE[j])) {weeks[j,i]<-1}
        else {weeks[j,i]<-0}
      }
    } 
    months.train<-months[1:End.train,]
    months.fore<-months[(End.train+1):(End.train+goriz),]
    weeks.train<-weeks[1:End.train,]
    weeks.fore<-weeks[(End.train+1):(End.train+goriz),]
    
    if(!missing(weatherCSV)) {
      print('with weather')
      weather<-Prep.weather.Data(weatherCSV,DDS$DATE[1],last.date)
      weather.spb.train<-weather[1:End.train,]
      weather.spb.fore<-weather[(End.train+1):(End.train+goriz),]
      DS.train.lr <- data.frame(DATE=DDS.train$DATE, QNT=DDS.train$QNT, months.train,weeks.train,T=weather.spb.train$T)
      DS.test.lr <- data.frame(DATE=DATE.fore, months.fore,weeks.fore,T=weather.spb.fore$T)
      reg.train <- lm(QNT~DATE+X1+X2+X3+X4+X5+X6+X7+X9+X10+X11+W1+W2+W3+W4+W5+W6+T,DS.train.lr)
      fore.values=predict.lm(reg.train,DS.test.lr)
    } 
    else{
      print('without weather')
      DS.train.lr <- data.frame(DATE=DDS.train$DATE, QNT=DDS.train$QNT, months.train,weeks.train)
      DS.test.lr <- data.frame(DATE=DATE.fore, months.fore,weeks.fore)
      reg.train <- lm(QNT~DATE+X1+X2+X3+X4+X5+X6+X7+X9+X10+X11+W1+W2+W3+W4+W5+W6,DS.train.lr)
      fore.values=predict.lm(reg.train,DS.test.lr)
    }
    
  }
  
  last.fore.values<-fore.values[(wait.days+1):goriz]
  last.fore.date<-DDS$DATE[DDS$DailyInd==End.train]+days(c((wait.days+1):goriz))
  #fore.sum<-round(sum(fore$mean))
  #MyMape.values<-MAPE(DDS$QNT[(End.train+1):(End.train+min(goriz,Test.len))],fore.values[1:min(goriz,Test.len)],digits=5)*100
  #cat(c('Mean Absolute Percentage Error = ', MyMape))
  result<-data.frame(last.fore.date, last.fore.values)
  names(result) <- c('x','fore.values')
  result
}
  

