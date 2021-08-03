library(AWQMSdata)
library(tidyverse)
library(plyr,include.only = "rbind.fill")


rpa<-AWQMS_Data(startdate='2007-01-01',org=c("CITY_ONTARIO(NOSTORETID)","CITY_THEDALLES(NOSTORETID)"))

dich<-rpa %>%
  subset(Char_Name %in% c("cis-1,3-Dichloropropene","trans-1,3-Dichloropropene")) %>%
  group_by(OrganizationID,SampleStartDate,SampleStartTime,MLocID,StationDes,MonLocType,SampleMedia,SampleSubmedia) %>%
  summarise(
            Result_Text= ifelse((substr(Result_Text,start=1,stop=1) %in% "<"),
                        Result_Numeric,
                        sum(Result_Numeric)),
            Result_Numeric=Result_Text,
            Char_Name = "1,3-Dichloropropene",
            CASNumber = "542756",
            MRLValue = mean(MRLValue),
            MDLValue = mean(MDLValue),
            Result_Type="Calculated",
            Result_Unit='ug/l',
            Method_Code="Calculated",
            Activity_Type="Calculated",
            Analytical_Lab="Calculated from isomer data"
            )

#need to add in < if applicable
dich$Result_Text<-ifelse(((!is.na(dich$MRLValue))|!(is.na(dich$MDLValue))) & 
                           (dich$Result_Numeric==dich$MRLValue | dich$Result_Numeric==dich$MDLValue),
                         paste0("<",dich$Result_Text),dich$Result_Text)
#get unique values
dich<-unique(dich)

#bind new rows to RPA dataframe
rpa<-rbind.fill(rpa,dich)

new1<-subset(new,
            select=c(CASNumber,Project1,act_id,act_id,StationDes,Activity_Type,Method_Code,Char_Name,
                     SampleMedia,SampleStartDate,Result_Text,MRLValue,MDLValue,Result_Unit,Analytical_Lab,
                     Result_status,Result_Type,MLocID,MonLocType,Result_Comment))

                           


