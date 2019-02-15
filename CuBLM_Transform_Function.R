#create a function that converts data from AWQMS long table format into the wide table format that 
#is used by the copper BLM model and the NPDES Copper BLM templates
    
CuBLM<-function(x) {
  #x is table output from NPDES_AWQMSQuery.R
  source("UnitConvert_Function.R")
  #need to limit data to only CuBLM parameters and also exclude summary statistics 
  char<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
          "Temperature, water","Total Sulfate","Sulfide")
  x<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base))
  
  #Need to check units
  x<-unit_conv(x,"Temperature, water","deg F","deg C")
  x<-unit_conv(x,c("Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide"),"ug/l","mg/l")
  x<-unit_conv(x,"Copper","mg/l","ug/l")
  
  
  # only take the analytes we're interested in 
  char<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
          "Temperature, water","Total Sulfate","Sulfide","Salinity","Conductivity")
  
  #remove any samples that are calculated from continuous data (eg. 7 day min)
  y<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base)) #& 
  
  #combine name and sample fraction, otherwise we get a bunch of rows we don't need
  #mostly interested in whether an analyte is Total Recoverable or Dissolved, and only for metals
  #can leave out some the other Sample Fractions
  y$Char_Name<-
    case_when(y$Char_Name %in% c("Calcium","Copper","Magnesium","Potassium","Sodium","Organic carbon")~paste0(y$Char_Name,",",y$Sample_Fraction),
              y$Char_Name %in% c("Alkalinity, total","Chloride","pH","Sulfate","Temperature, water","Total Sulfate","Sulfide","Salinity","Conductivity")~y$Char_Name)
    
  
  #just want a subset of the columns, too many columns makes reshape very complicated
  x<-subset(y,select=c("Char_Name","Result","SampleStartDate","SampleStartTime","OrganizationID","MLocID","Project1"))
  
  res<-reshape(x, timevar="Char_Name",
               idvar=c("MLocID","SampleStartDate","SampleStartTime","OrganizationID","Project1"),
               direction="wide") 
  
  #note, if you get warnings with res saying that multiple rows match, look for duplicates in data  
  
  return(res)
}


#source("NPDES_AWQMSQuery.R")
#data<-NPDES_AWQMS_Qry(startdate='2000-01-01',enddate='2018-12-27',org="PDX_BES(NOSTORETID)",montype="Facility Municipal Sewage (POTW)")

#trans<-CuBLM(data)



