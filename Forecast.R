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

#Агрегация по неделям продаж товара 'ItemName'  на АЗС 'DPtName'
#names(SalesTable)     "DEPT" "SKU"  "DATE" "QNT" 
WeekDataSales = function(SalesTable,ItemName,DptName) {
  a<-SalesTable %>%
    mutate(year=year(SalesTable$DATE),week=week(SalesTable$DATE)) %>%
    group_by(DEPT,SKU,year,week) %>%
    summarise(WeekQnt=sum(QNT)) %>%
    filter(DEPT==DptName,SKU==ItemName) %>%
    mutate(WeekInd='1') 
  a$WeekInd<-c(1:length(a$WeekInd))
  a
}
