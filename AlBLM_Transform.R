#' Aluminum BLM
#'
#' A function that converts data from AWQMS long table format into the wide table format that
#' is used by the Aluminum BLM model and the NPDES Aluminum BLM templates. Ancillary data is
#' the first data found in the calendar day.
#'
#' Output includes the method detection and method reporting limit for Aluminum, as well as result
#' value and also the "Result_Type" for each characteristic so that
#' user can better judge the quality of the data. This function does not return data that has
#' a "Rejected" status in AWQMS.
#'
#' Written by Aliana Britson
#'
#' @param x table output from AWQMS query
#' @return Dataframe with input file for BLM
#' @importFrom magrittr "%>%"
#' @export
#'
#'


AlBLM <- function(x) {
  #x is table output from AWQMS query

  #Remove rejected data
  x <- x[x$Result_status != 'Rejected',]

  #Need to check units and make sure they are converted properly
  x <- AWQMSdata::unit_conv(x,c("Calcium","Magnesium","Organic carbon","Hardness, Ca, Mg","Alkalinity, total"),"ug/l","mg/l")
  x <- AWQMSdata::unit_conv(x,"Aluminum","mg/l","ug/l")
  x <- AWQMSdata::unit_conv(x,"Temperature, water", "deg F", "deg C")


  # Analytes of interest for Aluminum Model - note that the model uses hardness,
  # but it is possible to calculate hardness from Calcium and Magnesium or Conductivity, so they are included
  # temperature and alkalinity needed so that the pH can be adjusted depending on the mix dilutions
  char<-c("Hardness, Ca, Mg","Calcium","Aluminum","Magnesium","pH","Organic carbon","Conductivity","Alkalinity, total","Temperature, water")

  #Only want to keep analytes of interest, and remove any samples that are calculated from continuous data (eg. 7 day min)
  y<-subset(x,x$Char_Name %in% char & is.na(x$Statistical_Base))

  #combine name and sample fraction, otherwise we get a bunch of rows we don't need
  #interested in whether an analyte is Total Recoverable or Dissolved, and only for metals
  #can leave out the other Sample Fractions
  y$Char_Name<-
    dplyr::case_when(y$Char_Name %in% c("Calcium","Aluminum","Magnesium","Organic carbon")
                     ~paste0(y$Char_Name,",",y$Sample_Fraction),
                     y$Char_Name %in% c("Hardness, Ca, Mg","pH","Conductivity","Alkalinity, total","Temperature, water")
                     ~y$Char_Name)

  # Get only ancillary data
  ancillary <- y[!grepl("Aluminum", y$Char_Name), ]
  #Set date
  ancillary$date <- as.Date(ancillary$SampleStartDate)

  ancillary<-subset(ancillary,select=c("Char_Name","Result_Text","date","OrganizationID","MLocID", "Project1","Result_Type"))

  type<-subset(ancillary,select=c("Char_Name","date","OrganizationID","MLocID", "Project1","Result_Type"))

  #get ancillary data into wide table format
  ancillary <- ancillary %>%
    dplyr::group_by(Char_Name, date, OrganizationID, MLocID,Project1) %>%
    dplyr::summarise(Result_Text = dplyr::first(Result_Text)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = Char_Name, value =Result_Text)

  #get result type data into wide table format
  type<- type %>%
    dplyr::group_by(Char_Name, date, OrganizationID, MLocID,Project1) %>%
    dplyr::summarise(Result_Type = dplyr::first(Result_Type)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = Char_Name, value =Result_Type )

  Aluminum <- y[grepl("Aluminum", y$Char_Name), ]



  Aluminum<-subset(Aluminum,select=c("OrganizationID","Project1", "MLocID","SampleStartDate","SampleStartTime", "Char_Name","Result_Text","Result_Type","MDLValue",
                                 "MRLValue"))

  Aluminum_joined <- Aluminum %>%
    dplyr::mutate(date = as.Date(SampleStartDate)) %>%
    dplyr::left_join(ancillary, by = c('date', 'OrganizationID', 'MLocID',  "Project1")) %>%
    dplyr::left_join(type, by = c('date', 'OrganizationID', 'MLocID',  "Project1"))

  #column names are a bit weird now, rename a bit
  names(Aluminum_joined)<-stringr::str_replace(names(Aluminum_joined),"\\.x","")
  names(Aluminum_joined)<-stringr::str_replace(names(Aluminum_joined),"\\.y"," Result_Type")

  return(Aluminum_joined)
}

#library(AWQMSdata)
#library(tidyverse)
#x<-AWQMS_Data(startdate='2019-12-02',enddate='2020-03-02')
#try<-AlBLM(x)

