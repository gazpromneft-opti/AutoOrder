library(dplyr)
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
