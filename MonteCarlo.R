#monte carlo analysis using continuous temperature data as an example - how many samples do we need to get close to an actual 90th percentile value
#assuming the continuous dataset is the "true population"

library(AWQMSdata)
library(tidyverse)
library(ggplot2)

#get data - Dairy Creek at Hwy 8
dat<-AWQMS_Data_Cont(startdate='2019-01-01',char=c("Temperature, water"),enddate='2021-01-01',station= '10491-ORDEQ')

#calculate 90th percentile of the entire dataset - turns out to be 20.5
raw90<-quantile(dat$Result_Numeric,probs=0.9,na.rm=TRUE)

#get max and min date - data from 7/30/2019 to 10/28/2020 - a little over a year of data
max(dat$Result_Date)
min(dat$Result_Date)

#get max and min temp - 5.46 to 34.2 degrees (all in Deg C)
max(dat$Result_Numeric)
min(dat$Result_Numeric)

#number of samples to select from the dataset
samp<-seq(10,300,20)

#initialize dataframe for the loop
df_all<-data.frame()

#want to do a loop - have it randomly select a certain number of samples from the dataset and then calculate the 90th percentile
loop<-for(i in 1:500){
  #need to loop within a loop, first loop tells us to run the simulation 1000 times, second one tells selects random samples and gets us 90th percentile
  for (i in samp)
{
  #select random samples, do not allow replacement (a sample can only be selected once)
  sel<-sample(dat$Result_Numeric,size=i,replace=F)
  
  cal<-quantile(sel,probs=0.9,na.rm=TRUE)
  
  #calculate percent difference between random sample 90th percentile and "true" 90th percentile (20.5)
  percdiff<-(abs(cal-raw90)/((cal+raw90)/2))*100
  
  df<-data.frame(i,cal,percdiff)
  df_all<-rbind(df_all,df)
  
  }
}

#what is the summary stats on the percent difference?
sum<-df_all %>%
  group_by(i) %>%
  summarise(avg_percent_difference=mean(percdiff),median_percent_difference=median(percdiff),
            Ninetieth_percent_difference<-quantile(percdiff,probs=0.9,na.rm=TRUE),
            maximum_percent_difference=max(percdiff),count=n(),avg_90th=(mean(cal)))

#let's make a graph
ggplot(data=df_all,aes(x=i,y=percdiff))+
  geom_point()+
  scale_y_continuous(breaks=seq(0,35,1))+
  scale_x_continuous(breaks=seq(0,300,20))+
  stat_summary(fun=mean,geom="line",colour="green")+
  geom_line(y=5, colour="blue")+
  labs(x="# of random samples",y="percent difference",
       title="Effect of sample size on percent difference between 90th percentiles",
       subtitle="Temperature")+
  annotate(geom="text",x=150,y=17, label="Graph compares the caculated 90th percentile for a random subset of data")+
  annotate(geom="text",x=150, y=16,label="to the 'true' 90th percentile of the complete dataset")+
  annotate(geom="text",x=200, y=10, label="Green line is average percent difference. Blue line is 5% mark")+
  theme_bw()

