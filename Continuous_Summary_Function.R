#functions to calculate summary statistics out of continuous data

##calculate daily, 7 day rolling average of the daily maximum,and 60 day rolling average of the daily maximum

contsum<-function(x,days=NULL) {
  require(tidyverse)
  require(zoo)
  require(lubridate)
  
  #convert result date to date type
  x$Result_Date<-as.Date(x$Result_Date)
  
  #calculate daily 
  dstats<-x %>%
    group_by(MLocID,Result_Date,Char_Name) %>%
    summarise(max=max(Result_Numeric),min=min(Result_Numeric),average=mean(Result_Numeric),ninetyfifth=quantile(Result_Numeric,probs=.95, na.rm=TRUE),observations=n())
  
  #add column to help determine if dates are consecutive, each group of consecutive dates gets its own number
  dstats<-dstats %>%
    group_by(MLocID,Char_Name) %>%
    arrange(Result_Date) %>% 
    group_by(Consec_Group = cumsum(c(TRUE, diff(Result_Date) > 1))) %>% 
    ungroup()
  
  #rename column names so that they make more sense
  names(dstats)[names(dstats)=="ninetyfifth"]<-'95th%ile'
  #calculate 7 day rolling average of the maximum, use right adjustment (so it takes the 7 days prior)
  SevenD<-dstats%>%
    dplyr::group_by(MLocID,Char_Name,Consec_Group)%>%
    dplyr::mutate(zoo::rollmeanr(max,k=7,fill=NA))
 
  #rename columns so that they make more sense
  names(SevenD)[names(SevenD) == "zoo::rollmeanr(max, k = 7, fill = NA)"] <- '7DayAvgDailyMax'
  
  #calculate 60 day rolling average of the maximum, use right adjustment (so it takes 60 days prior) 
  SixtyD<-SevenD%>%
    dplyr::group_by(MLocID,Char_Name,Consec_Group)%>%
    dplyr::mutate(zoo::rollmeanr(max,k=60,fill=NA))
  
  #rename columns so that they make more sense
  names(SixtyD)[names(SixtyD) == "zoo::rollmeanr(max, k = 60, fill = NA)"] <- '60DayAvgDailyMax'
  
  return(SixtyD)
  
}



#library(AWQMSdata)
#x<-AWQMS_Data_Cont(startdate='2014-01-01',char=c("pH","Temperature, water","Salinity","Conductivity","Dissolved oxygen (DO)"),enddate='2021-01-01',station='WNF-081')
#try<-contsum(x)

#compare 90th percentile of continuous data from using daily average vs from raw continuous
#ninDavg<-try%>%
#  group_by(MLocID,Char_Name) %>%
#  summarise(ninetieth_avg=quantile(average,probs=.9,na.rm=TRUE))

#ninDmax<-try%>%
#  group_by(MLocID,Char_Name) %>%
#  summarise(ninetieth_max=quantile(max,probs=.9,na.rm=TRUE))

#ninraw<-x %>%
#  group_by(MLocID,Char_Name) %>%
#  summarise(ninetieth_raw=quantile(Result_Numeric,probs=0.9,na.rm=TRUE))

#new<-merge(x=ninDavg,y=ninraw,by.x=c("MLocID","Char_Name"),by.y=c("MLocID","Char_Name")) %>%
#  merge(ninDmax)

#new$diffavgraw<-(abs(new$ninetieth_avg-new$ninetieth_raw)/((new$ninetieth_avg+new$ninetieth_raw)/2))*100

#library(writexl)
#write_xlsx(new,"//deqhq1/WQSPfiles/Water Quality Permitting/PERMIT DEVELOPMENT/RPA and Permit Limit Development/Gap Analyses/CompareRawvsSummary.xlsx")
