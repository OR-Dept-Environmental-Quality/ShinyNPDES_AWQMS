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
  y<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base)) #& 
  
  #combine name and method speciation, otherwise we get a bunch of rows we don't need
  #mostly interested in whether an analyte is Total Recoverable or Dissolved, and only for metals
  #can leave out some the other Sample Fractions
  y$Char_Name<-paste0(y$Char_Name,(ifelse(is.na(y$Method_Speciation),paste(""),paste0(",",y$Method_Speciation))))
  
  #just want a subset of the columns, too many columns makes reshape very complicated
  x<-subset(y,select=c("Char_Name","Result","Result_Unit","SampleStartDate","SampleStartTime","OrganizationID","MLocID","Project1"))
  
  res<-reshape(x, timevar="Char_Name",
               idvar=c("MLocID","SampleStartDate","SampleStartTime","OrganizationID","Project1"),
               direction="wide") 
  
  #note, if you get warnings with res saying that multiple rows match, look for duplicates in data  
  
  return(res)
}