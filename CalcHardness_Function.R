#create function to transform calcium and magnesium data from AWQMS to hardness, also can use specific conductance

hardness<-function(x){

  require(dplyr)
  
  #make sure Ca and Mg is all in the same units
  x<-unit_conv(x,c("Calcium","Magnesium"),"ug/l","mg/l")
  
  #need to get unique identifiers
  new<-unique(subset(x,select=c("act_id","SampleStartDate","MLocID","MonLocType")))
  
  #select all total calcium
  tc<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Total") & x$Char_Name %in% "Calcium")
  #select all dissolved calcium
  dc<-subset(x,x$Sample_Fraction %in% "Dissolved" & x$Char_Name %in% "Calcium")
  #select all total magnesium
  tm<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Total") & x$Char_Name %in% "Magnesium")
  #select all dissolved magnesium
  dm<-subset(x,x$Sample_Fraction %in% "Dissolved" & x$Char_Name %in% "Magnesium")
  
  #select all specific conductance data, just get data in the right units for now - I don't have any translators
  cond<-subset(x,x$Char_Name %in% c("Conductance","Specific conductance","Specific conductivity","Conductivity") & x$Result_Unit %in% "umho/cm")
  
  #add them all together to the new dataset
  new$total.Calcium<-tc$Result_Numeric[match(new$act_id,tc$act_id,nomatch=NA)]
  new$diss.Calcium<-dc$Result_Numeric[match(new$act_id,dc$act_id,nomatch=NA)]
  new$total.Magnesium<-tm$Result_Numeric[match(new$act_id,tm$act_id,nomatch=NA)]
  new$diss.Magnesium<-dm$Result_Numeric[match(new$act_id,dm$act_id,nomatch=NA)]
  new$Conduct<-cond$Result_Numeric[match(new$act_id,cond$act_id,nomatch=NA)]
  
  #remove activity id's with no Ca, Mg, or conductuance data that made it through
  new<-subset(new,!(is.na(new$total.Calcium) & is.na(new$diss.Calcium) & is.na(new$total.Magnesium) & is.na(new$diss.Magnesium) & is.na(new$Conduct)))
  
  #calculate hardness. If total Ca and Mg are there, use those, if not then use dissolved Ca and Mg, if no Ca or Mg data, use conductance
  #note that in R, log=ln while log10 is used to denote the base 10 logarithm
  new$Calc.Hardness<-case_when(
    !(is.na(new$total.Calcium) & is.na(new$total.Magnesium))~2.497*new$total.Calcium+4.118*new$total.Magnesium,
    !(is.na(new$diss.Calcium) & is.na(new$diss.Magnesium))~2.497*new$diss.Calcium+4.118*new$diss.Magnesium,
    is.na(new$total.Calcium) & is.na(new$total.Magnesium) & is.na(new$diss.Calcium) & is.na(new$diss.Magnesium)~exp(1.02*log(new$Conduct)-1.16)
  )
  
  #specify which calc method was used to create hardness
  new$Hardness.Type<-case_when(
    !(is.na(new$total.Calcium) & is.na(new$total.Magnesium))~"Hardness from Total Ca+Mg",
    !(is.na(new$diss.Calcium) & is.na(new$diss.Magnesium))~"Hardness from Dissolved Ca+Mg",
    is.na(new$total.Calcium) & is.na(new$total.Magnesium) & is.na(new$diss.Calcium) & is.na(new$diss.Magnesium)~"Hardness from Specific Conductance"
  )
  
  return(new)

}



library(AWQMSdata)
#x<-AWQMS_Data(startdate='01-01-2010',char=c("Calcium","Magnesium","Conductance","Specific conductance","Specific conductivity","Conductivity"))

#try<-hardness(x)