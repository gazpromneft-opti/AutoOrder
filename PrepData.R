library(dplyr)
library(lubridate)

#Чтение данных, правка типа полей
Prep.Sales.Data = function(SalesCSV){
  SalesP <- read.csv("SalesP.csv")
  salesP_df <- tbl_df(SalesP)
  salesP_df$SKU<-chr(salesP_df$SKU)
  salesP_df$DEPT<-chr(salesP_df$DEPT)
  salesP_df$DATE<-as.Date(salesP_df$DATE)
  salesP_df
}

#Исправление выбросов
vybros = function(data){
  x<-data$DATE
  y<-data$QNT
  # Заберём индексы точек выбросов
  ind <- which(y %in% boxplot.stats(y)$out)
  # Сохраним координаты точек выбросов в отдельном dataframe
  vybrosy <- data.frame(x=x[ind], y=y[ind])
  ymaxim<-max(y)
  for (i in ind)
  { y[i]<-mean(y[(i-5):(i+5)])
  }
  plot(x,y,col='blue', pch=16, ylim=c(0,ymaxim),type='l')
  points(vybrosy$x, vybrosy$y, pch=16, col='red')
  y->data$QNT
  data
}


#Фильтр по дням продаж товара 'ItemName'  на АЗС 'DPtName'
#names(SalesTable)     "DEPT" "SKU"  "DATE" "QNT" 
DailyDataSales = function(SalesTable,ItemName,DptName) {
  a<-SalesTable %>%
    filter(DEPT==DeptName,SKU==ItemName)
  a<-a %>%
    mutate(year=year(a$DATE),month=month(a$DATE)) %>%
    group_by(DEPT,SKU,year,month) %>%
    arrange(year, month, DATE) %>%
    mutate(DailyInd='1') 
  a$DailyInd<-1:length(a$DailyInd)
  a
}


#Для анализа по неделям придется убирать 53ю неделю, так как 1 января - всегда первая неделя
#Агрегация по неделям продаж товара 'ItemName'  на АЗС 'DPtName'
#names(SalesTable)     "DEPT" "SKU"  "DATE" "QNT" 
WeekDataSales = function(SalesTable,ItemName,DptName) {
  a<-SalesTable %>%
    mutate(wday=wday(SalesTable$DATE),month=month(SalesTable$DATE))
  a<-a %>%
    mutate(year=year(SalesTable$DATE),week=week(SalesTable$DATE)) %>%
    group_by(DEPT,SKU,year,week) %>%
    summarise(WeekQnt=sum(QNT)) %>%
    filter(DEPT==DptName,SKU==ItemName) %>%
    mutate(WeekInd='1') 
  a$WeekInd<-c(1:length(a$WeekInd))
  a<-a[a$WeekInd !=1,]
  a[a$week==52 & a$year==2015,"WeekQnt"]<-a[a$week==52 & a$year==2015,"WeekQnt"] + ceiling(a[a$week==53 & a$year==2015,"WeekQnt"]/2)
  a[a$week==1 & a$year==2016,"WeekQnt"]<-a[a$week==1 & a$year==2016,"WeekQnt"] + floor(a[a$week==53 & a$year==2015,"WeekQnt"]/2)
  a[a$week==52 & a$year==2016,"WeekQnt"]<-a[a$week==52 & a$year==2016,"WeekQnt"] + ceiling(a[a$week==53 & a$year==2016,"WeekQnt"]/2)
  a[a$week==1 & a$year==2017,"WeekQnt"]<-a[a$week==1 & a$year==2017,"WeekQnt"] + floor(a[a$week==53 & a$year==2016,"WeekQnt"]/2)
  a<-a[a$week !=53,] 
  a$WeekInd<-1:length(a$WeekInd)
  a
}

#Агрегация по месяцам продаж товара 'ItemName'  на АЗС 'DPtName'
#names(SalesTable)     "DEPT" "SKU"  "DATE" "QNT" 
MonthDataSales = function(SalesTable,ItemName,DptName) {
  a<-SalesTable %>%
    filter(DEPT==DeptName,SKU==ItemName)
  a<-a %>%
    mutate(year=year(a$DATE),month=month(a$DATE)) %>%
    group_by(DEPT,SKU,year,month) %>%
    summarise(MonthQnt=sum(QNT)) %>%
    mutate(MonthInd='1') 
  a$MonthInd<-1:length(a$MonthInd)
  a
}
