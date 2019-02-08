#create a function that converts data from AWQMS long table format into the wide table format that 
#is used by the copper BLM model and the NPDES Copper BLM templates
#only return the correct columns, will have to search for correct parameters


#test dataset

testdata<-data.frame("Char_Name"= c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
                 "Temperature, water","Total Sulfate","Sulfide","Toluene"))
testdata$Result<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
testdata$Result_Unit<-c("mg/l","ug/l","ug/l","mg/l","ug/l","None","ug/l","ug/l","ug/l","ug/l","deg F","ug/l","ug/l","ng/l")
testdata$Statistical_Base<-c(NA,NA,NA,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

#need a unit conversaion function, it works, but must have units in same format AWQMS does (ie don't capitalize L)
BLMunit_conv<-function(x,char,unit,conv){
  #x is dataset,char is characteristics,unit is what the data currently is in, conv is what we want the units to be
  #convert Result
  x$Result<-ifelse(x$Char_Name %in% char &x$Result_Unit==unit,
                   (ifelse(unit=="deg F"& conv=="deg C",((x$Result-32)*0.5556),
                             (ifelse(unit=="ug/l"& conv=="mg/l",(x$Result*0.001),
                                  ifelse(unit=="mg/l"&conv=="ug/l",(x$Result*1000),x$Result))))),
            x$Result)
  #change unit to new unit
  x$Result_Unit<-ifelse(x$Char_Name %in% char&x$Result_Unit==unit,
                 (ifelse(unit=="deg F"& conv=="deg C","deg C",
                           (ifelse(unit=="ug/l"& conv=="mg/l","mg/l",
                             ifelse(unit=="mg/l"&conv=="ug/l","ug/l",x$Result_Unit))))),
                             x$Result_Unit)
  
  return(x)
}

#test<-BLMunit_conv(testdata,"Temperature, water","deg F","deg C")
#test<-BLMunit_conv(testdata,c("Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide"),"ug/l","mg/l")
#test<-BLMunit_conv(testdata,"Copper","mg/l","ug/l")
    
CuBLM<-function(x) {
  #x is table output from NPDES_AWQMSQuery.R
  
  #need to limit data to only CuBLM parameters and also exclude summary statistics 
  char<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
          "Temperature, water","Total Sulfate","Sulfide")
  x<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base))
  
  #do I need to do any unit conversions?-Yes need to check units
  x<-BLMunit_conv(x,"Temperature, water","deg F","deg C")
  x<-BLMunit_conv(x,c("Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide"),"ug/l","mg/l")
  x<-BLMunit_conv(x,"Copper","mg/l","ug/l")
  
  
  # only take the analytes we're interested in 
  char<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
          "Temperature, water","Total Sulfate","Sulfide")
  
  y<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base)) #& 
  
  #combine name and sample fraction, otherwise we get a bunch of rows we don't need
  #mostly interested in whether an analyte is Total Recoverable or Dissolved, can leave out some of the other Sample Fractions
  y$Char_Name<-ifelse(!(is.na(y$Sample_Fraction)|
                          y$Sample_Fraction %in% "Filtered, field"|
                          y$Sample_Fraction %in% "Filtered, lab"),
                      paste0(y$Char_Name,",",y$Sample_Fraction),
                      y$Char_Name)
  
  #just want a subset of the columns, too many columns makes reshape very complicated
  x<-subset(y,select=c("Char_Name","Result","SampleStartDate","SampleStartTime","OrganizationID","MLocID"))
  
  res<-reshape(x, timevar="Char_Name",
               idvar=c("MLocID","SampleStartDate","SampleStartTime","OrganizationID"),
               direction="wide") 
  
  #note, if you get warnings with res saying that multiple rows match, look for duplicates in data  
  
  return(res)
}


source("NPDES_AWQMSQuery.R")
data<-NPDES_AWQMS_Qry(startdate='2000-01-01',enddate='2018-12-27',org="PDX_BES(NOSTORETID)",montype="Facility Municipal Sewage (POTW)")

trans<-CuBLM(data)



