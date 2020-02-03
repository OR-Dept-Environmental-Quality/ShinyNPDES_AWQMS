#query designed specifically for NPDES permit writers for RPA analysis. 
#It calls necessary data from VW_AWQMS_Results using an ODBC connection
#this query is based off of Travis Pritchard's AWQMS_Data query
#This query will only return Water data from the following station types:
#'BEACH Program Site-Ocean','BEACH Program Site-River/Stream',
#'Canal Drainage','Canal Irrigation','Canal Transport','Estuary','Facility Industrial',
#'Facility Municipal Sewage (POTW)','Facility Other','Lake','Ocean','Reservoir','River/Stream',
#'River/Stream Perennial', 'BEACH Program Site-Estuary'
# in additon, the query is set up so that no rejected data is returned

NPDES_AWQMS_Qry<-function 
(startdate = "2000-01-01", enddate = NULL, station = NULL, project=NULL, montype = NULL, char = NULL, org = NULL, 
 HUC8 = NULL, HUC8_Name = NULL, AU_ID= NULL) 
{
  if (missing(startdate)) {
    stop("Need to input startdate")
  }
  
  query <- "SELECT OrganizationID,Org_Name,Project1,StationDes, MLocID,Lat_DD,Long_DD,MonLocType,
  HUC8_Name,AU_ID,act_id,SampleStartDate,SampleStartTime,SampleMedia,
  SampleSubmedia,SamplingMethod,Activity_Type,Statistical_Base,Time_Basis,Char_Name,Char_Speciation,
  Sample_Fraction,CASNumber,Result,Result_Numeric,Result_Operator,Result_Unit,Analytical_method,Method_Code,Method_Context,Analytical_Lab,
  MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit,Activity_Comment,Result_Comment,Result_status,Result_Type,Result_UID\n  
  FROM [awqms].[dbo].[VW_AWQMS_Results]\n  
  WHERE SampleStartDate >= Convert(datetime, {startdate})"
  if (length(enddate) > 0) {
    query = paste0(query, "\n AND SampleStartDate <= Convert(datetime, {enddate})")
  }
  if (length(station) > 0) {
    query = paste0(query, "\n AND MLocID IN ({station*})")
  }
  if (length(char) > 0) {
    query = paste0(query, "\n AND Char_Name in ({char*}) ")
  }
  
  if (length(project) > 0) {
    query = paste0(query, "\n AND Project1 in ({project*}) ")
    
  }
  if (length(org) > 0) {
    query = paste0(query, "\n AND OrganizationID in ({org*}) ")
  }
  if (length(HUC8) > 0) {
    query = paste0(query, "\n AND HUC8 in ({HUC8*}) ")
  }
  if (length(HUC8_Name) > 0) {
    query = paste0(query, "\n AND HUC8_Name in ({HUC8_Name*}) ")
  }
  if (length(AU_ID) > 0) {
    query = paste0(query, "\n AND AU_ID in ({AU_ID*}) ")
  }
  if (length(montype)>0){
    query=paste0(query,"\n AND MonLocType in ({montype*})")
  } 
  else {
    query=paste0(query,"\n AND MonLocType in ('BEACH Program Site-Ocean','BEACH Program Site-River/Stream', 'BEACH Program Site-Estuary',
               'Canal Drainage','Canal Irrigation','Canal Transport','Estuary','Facility Industrial',
               'Facility Municipal Sewage (POTW)','Facility Other','Lake','Ocean','Reservoir','River/Stream',
               'River/Stream Perennial','Facility Public Water Supply (PWS)')")
    }
  query=paste0(query,"\n AND SampleMedia in ('Water') AND MLocID <> '10000-ORDEQ' AND activity_type NOT LIKE 'Quality Control%' AND Result_status NOT LIKE 'Rejected'")
  
  con <- DBI::dbConnect(odbc::odbc(), "AWQMS")
  qry <- glue::glue_sql(query, .con = con)
  data_fetch <- DBI::dbGetQuery(con, qry)
  DBI::dbDisconnect(con)
  return(data_fetch)
}


#data<-NPDES_AWQMS_Qry(startdate='2000-01-01',enddate='2018-12-27')


#####create function like AWQMS_Stations but only pulls desired station types 
#(hoping to cut down on time for Shiny app by cutting out extra stuff)
#removed 'Canal Irrigation' as monlocType, unlikely to ever be used by NPDES

NPDES_AWQMS_Stations<-function (char = NULL, HUC8 = NULL, HUC8_Name = NULL, 
          org = NULL) 
{
  con <- DBI::dbConnect(odbc::odbc(), "AWQMS")
  query = "SELECT distinct  [MLocID], [StationDes], [MonLocType], [EcoRegion3], [EcoRegion4], [HUC8], [HUC8_Name], [HUC10], [HUC12], [HUC12_Name], [Lat_DD], [Long_DD], [Reachcode], [Measure], [AU_ID]\n  FROM [awqms].[dbo].[VW_AWQMS_Results]"
  
  query<-paste0(query,"\n WHERE MonLocType in ('BEACH Program Site-Ocean','BEACH Program Site-River/Stream',
               'Canal Drainage','Canal Transport','Estuary','Facility Industrial',
                  'Facility Municipal Sewage (POTW)','Facility Other','Lake','Ocean','Reservoir','River/Stream',
                  'River/Stream Perennial')")

  if (length(char) > 0) {
      query <- paste0(query, "\n AND Char_Name IN ({char*})")
    }
  
  if (length(HUC8) > 0) {
    if (length(char > 0)) {
      query = paste0(query, "\n AND HUC8 IN ({HUC8*})")
    }
    else {
      query <- paste0(query, "\n AND HUC8 IN ({HUC8*})")
    }
  }
  if (length(HUC8_Name) > 0) {
    if (length(char > 0) | length(HUC8) > 
        0) {
      query = paste0(query, "\n AND HUC8_Name in ({HUC8_Name*}) ")
    }
    else {
      query <- paste0(query, "\n AND HUC8_Name IN ({HUC8_Name*})")
    }
  }
  if (length(org) > 0) {
    if (length(char > 0) | length(HUC8) > 
        0 | length(HUC8_Name) > 0) {
      query = paste0(query, "\n AND OrganizationID in ({org*}) ")
    }
    else {
      query <- paste0(query, "\n AND OrganizationID in ({org*}) ")
    }
    
  }
  qry <- glue::glue_sql(query, .con = con)
  data_fetch <- DBI::dbGetQuery(con, qry)
  DBI::dbDisconnect(con)
  return(data_fetch)
}

#stat<-NPDES_AWQMS_Stations()
