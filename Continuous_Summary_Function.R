#functions to calculate summary statistics out of continuous data

#calculate daily summary stats

daily<-function(x) {
  require(tidyverse)
  
  dstats<-x%>%
    group_by(MLocID,Result_Date,Char_Name) %>%
    summarise(max=max(Result_Numeric),min=min(Result_Numeric),average=mean(Result_Numeric),observations=n())
    
  return(dstats)
}

##7 day rolling average of the daily maximum

sevenDADM<-function(x,days=NULL) {
  require(tidyverse)
  require(zoo)
  require(lubridate)
  
  dstats<-x %>%
    group_by(MLocID,Result_Date,Char_Name) %>%
    summarise(max=max(Result_Numeric),min=min(Result_Numeric),average=mean(Result_Numeric),observations=n())
  
  SevenD<-dstats%>%
    dplyr::group_by(MLocID,Char_Name)%>%
    dplyr::mutate(zoo::rollmeanr(max,k=7,fill=NA))
 
  
}

#60 day rolling average of the daily maximum
sixtyDADM<-function(x,days=NULL) {
  require(tidyverse)
  require(zoo)
  require(lubridate)
  
  dstats<-x%>%
    group_by(MLocID,Result_Date,Char_Name) %>%
    summarise(max=max(Result_Numeric),min=min(Result_Numeric),average=mean(Result_Numeric),observations=n())
  
  SixtyD<-dstats%>%
    dplyr::group_by(MLocID,Char_Name)%>%
    dplyr::mutate(zoo::rollmeanr(max,k=60,fill=NA))
}


library(AWQMSdata)
x<-AWQMS_Data_Cont(startdate='2020-01-01',char=c("pH","Temperature, water","Salinity","Conductivity","Dissolved oxygen (DO)"),enddate='2021-07-01')
try<-daily(x)
trysev<-sevenDADM(x)
trysixty<-sixtyDADM(x)