#unit transform function
#designed for Copper BLM, so focuses on temperature, ang ug-mg conversions


unit_conv<-function(x,char,unit,conv){
  require(dplyr)
  #x is dataset,char is characteristics,
  #unit is what the data currently is in, 
  #conv is what we want the units to be
  
  #convert Result_Numeric
  x$Result_Numeric<-case_when(x$Char_Name %in% char & x$Result_Unit==unit & unit=="deg F" & conv=="deg C"~((x$Result_Numeric-32)*0.5556),
                              x$Char_Name %in% char & x$Result_Unit==unit & unit=="ug/l" & conv=="mg/l"~(x$Result_Numeric*0.001),
                              x$Char_Name %in% char & x$Result_Unit==unit & unit=="mg/l" & conv=="ug/l"~(x$Result_Numeric*1000),
                              x$Char_Name %in% char & x$Result_Unit==unit & unit=="ng/l" & conv=="ug/l"~(x$Result_Numeric/1000),
                              !(x$Char_Name %in% char & x$Result_Unit==unit)~x$Result_Numeric)
  
  #change unit to new unit
  x$Result_Unit<-case_when(x$Char_Name %in% char & x$Result_Unit==unit & unit=="deg F" & conv=="deg C"~"deg C",
                           x$Char_Name %in% char & x$Result_Unit==unit & unit=="ug/l" & conv=="mg/l"~"mg/l",
                           x$Char_Name %in% char & x$Result_Unit==unit & unit=="mg/l" & conv=="ug/l"~"ug/l",
                           x$Char_Name %in% char & x$Result_Unit==unit & unit=="ng/l" & conv=="ug/l"~"ug/l",
                           !(x$Char_Name %in% char & x$Result_Unit==unit)~x$Result_Unit)
 
   #change MDL and MRL values and units
  x$MDLValue<-case_when(x$Char_Name %in% char & x$MDLUnit==unit & unit=="deg F" & conv=="deg C"~((x$MDLValue-32)*0.5556),
                        x$Char_Name %in% char & x$MDLUnit==unit & unit=="ug/l" & conv=="mg/l"~(x$MDLValue*0.001),
                        x$Char_Name %in% char & x$MDLUnit==unit & unit=="mg/l" & conv=="ug/l"~(x$MDLValue*1000),
                        x$Char_Name %in% char & x$MDLUnit==unit & unit=="ng/l" & conv=="ug/l"~(x$MDLValue/1000),
                        !(x$Char_Name %in% char & x$MDLUnit==unit)~x$MDLValue)
  
  x$MDLUnit<-case_when(x$Char_Name %in% char& x$MDLUnit==unit & unit=="deg F"& conv=="deg C"~"deg C",
                       x$Char_Name %in% char& x$MDLUnit==unit & unit=="ug/l"& conv=="mg/l"~"mg/l",
                       x$Char_Name %in% char& x$MDLUnit==unit & unit=="mg/l"&conv=="ug/l"~"ug/l",
                       x$Char_Name %in% char & x$MDLUnit==unit & unit=="ng/l" & conv=="ug/l"~"ug/l",
                       !(x$Char_Name %in% char&x$MDLUnit==unit)~x$MDLUnit)
  
  x$MRLValue<-case_when(x$Char_Name %in% char & x$MRLUnit==unit & unit=="deg F"& conv=="deg C"~((x$MRLValue-32)*0.5556),
                        x$Char_Name %in% char & x$MRLUnit==unit & unit=="ug/l"& conv=="mg/l"~(x$MRLValue*0.001),
                        x$Char_Name %in% char & x$MRLUnit==unit & unit=="mg/l"&conv=="ug/l"~(x$MRLValue*1000),
                        x$Char_Name %in% char & x$MRLUnit==unit & unit=="ng/l" & conv=="ug/l"~(x$MRLValue/1000),
                        !(x$Char_Name %in% char & x$MRLUnit==unit)~x$MRLValue)
  
  x$MRLUnit<-case_when(x$Char_Name %in% char & x$MRLUnit==unit & unit=="deg F"& conv=="deg C"~"deg C",
                       x$Char_Name %in% char & x$MRLUnit==unit & unit=="ug/l"& conv=="mg/l"~"mg/l",
                       x$Char_Name %in% char & x$MRLUnit==unit & unit=="mg/l"& conv=="ug/l"~"ug/l",
                       x$Char_Name %in% char & x$MRLUnit==unit & unit=="ng/l" & conv=="ug/l"~"ug/l",
                       !(x$Char_Name %in% char & x$MRLUnit==unit)~x$MRLUnit)
                        
  
  #we changed Result_Numeric, now we want to make sure that Result shows the same value by replacing it 
  #with result numeric concatenated with < for NDs (or > when it exists)
  x$Result<-paste0(ifelse(!(x$Result_Operator %in% "="),x$Result_Operator,""),x$Result_Numeric)
  
  return(x)
  
}

#x<-NPDES_AWQMS_Qry(startdate = "2000-01-01", enddate = "2019-01-01", station = NULL, montype = NULL, 
#                 char = c("Temperature, water","Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide","Copper")
#                   , org = "GP-WM", HUC8 = NULL, HUC8_Name = NULL,reject=FALSE)

#test<-unit_conv(x,c("Calcium","Chloride","Magnesium","Potassium","Sodium","Sulfate","Organic carbon","Total Sulfate","Sulfide"),"ug/l","mg/l")

#test2<-unit_conv(x,"Copper","mg/l","ug/l")

#y<-AWQMS_Data(startdate = "2000-01-01", enddate = "2019-01-01", station = NULL, char = NULL, org = "GP-WM", 
 #                  HUC8 = NULL, HUC8_Name = NULL)