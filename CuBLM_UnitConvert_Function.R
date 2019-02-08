#unit transform function for Copper BLM

BLMunit_conv<-function(x,char,unit,conv){
  #x is dataset,char is characteristics,unit is what the data currently is in, conv is what we want the units to be
  #convert Result
  x$Result<-ifelse(x$Char_Name %in% char & x$Result_Unit==unit,
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