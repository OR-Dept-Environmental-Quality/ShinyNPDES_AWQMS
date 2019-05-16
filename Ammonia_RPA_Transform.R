#ammonia RPA function to put AWQMS data into format conducive to Ammonia RPA analysis

amRPA<-function(x){
  # x is data table

  #Need to check units
  x<-unit_conv(x,"Temperature, water","deg F","deg C")
  x<-unit_conv(x,"Ammonia","ug/l","mg/l")

  
  # only take the analytes we're interested in 
  char<-c("Alkalinity, total","pH","Temperature, water","Ammonia","Salinity","Ammonia-nitrogen",
          "Conductivity","Ammonia and ammonium")
  
  #remove any samples that are calculated from continuous data (eg. 7 day min)
  y<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base))
  
  #there were some special projects at one point that looked at "dissolved alkalinity"-according to Linda McRae (5/16/2019) 
  #what they did was take two samples, one was filtered (dissolved alkalinity) and the other one wasn't (total alkalinity)
  #usually alkalinity is taken on a non-filtered sample, so we shall remove the "dissolved alkalinity" samples
  y<-subset(y,!(y$Char_Name=="Alkalinity, total" & y$Sample_Fraction=="Dissolved"))
  
  #combine name and method speciation, otherwise we get a bunch of rows we don't need
  y$Char_Name<-paste0(y$Char_Name,(ifelse(is.na(y$Method_Speciation),paste(""),paste0(",",y$Method_Speciation))))
  
  #just want a subset of the columns, too many columns makes reshape very complicated
  x<-subset(y,select=c("Char_Name","Result","Result_Unit","SampleStartDate","SampleStartTime","OrganizationID","MLocID","Project1"))
  
  res<-reshape(x, timevar="Char_Name",
               idvar=c("MLocID","SampleStartDate","SampleStartTime","OrganizationID","Project1"),
               direction="wide") 
  
  #note, if you get warnings with res saying that multiple rows match, look for duplicates in data  
  
  return(res)
}

#library(AWQMSdata)
#test<-AWQMS_Data(startdate="2015-01-01",enddate="05-10-2019",station="10768-ORDEQ")
#try<-amRPA(test)