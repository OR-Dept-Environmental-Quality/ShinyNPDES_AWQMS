#Trying to create versions of the AWQMSData functions that don't automatically call the .Renviron - I think this is causing issues in RConnect

noenvAWQMS_Chars<-
function (project = NULL, MLocID = NULL) 
{
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver   =  Sys.getenv('DRIVER'),
                        Server   =  Sys.getenv('AWQMS_SERVER'),
                        Database =  Sys.getenv('AWQMS_Database'),
                        UID      =  Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  AWQMS_data <- dplyr::tbl(con, "results_deq_vw")
  if (length(project) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Project1 %in% 
                                  project)
  }
  if (length(MLocID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% {
      {
        MLocID
      }
    })
  }
  AWQMS_data <- dplyr::collect(dplyr::distinct(dplyr::select(AWQMS_data, 
                                                             Char_Name)))
  DBI::dbDisconnect(con)
  return(AWQMS_data)
}


noenvAWQMS_Orgs<-
function (project = NULL, MLocID = NULL) 
{
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver   =  Sys.getenv('DRIVER'),
                        Server   =  Sys.getenv('AWQMS_SERVER'),
                        Database =  Sys.getenv('AWQMS_Database'),
                        UID      =  Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  AWQMS_data <- dplyr::tbl(con, "results_deq_vw")
  if (length(project) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Project1 %in% 
                                  project)
  }
  if (length(MLocID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% {
      {
        MLocID
      }
    })
  }
  AWQMS_data <- dplyr::collect(dplyr::distinct(dplyr::select(AWQMS_data, 
                                                             OrganizationID, org_name)))
  DBI::dbDisconnect(con)
  return(AWQMS_data)
}

noenvAWQMS_Stations<-
function (project = NULL, MonLocType = NULL, Char_Name = NULL, 
          HUC8 = NULL, HUC8_Name = NULL, OrganizationID = NULL, crit_codes = FALSE) 
{
  if (!is.null(c(HUC8, HUC8_Name, MonLocType))) {
    print("Query stations database...")
    tictoc::tic("Station Database Query")
    station_con <- DBI::dbConnect(odbc::odbc(),
                                  Driver = Sys.getenv('DRIVER'),
                                  Server = Sys.getenv('STATIONS_SERVER'),
                                  Database = Sys.getenv('STATIONS_Database'),
                                  UID    = Sys.getenv('STATIONS_usr'),
                                  PWD    = Sys.getenv('STATIONS_pass'))
    
    stations_filter <- dplyr::select(dplyr::tbl(station_con, 
                                                "VWStationsFinal"), MLocID, StationDes, GNIS_Name, 
                                     AU_ID, Lat_DD, Long_DD, MonLocType, EcoRegion3, EcoRegion4, 
                                     HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, Reachcode, 
                                     Measure, COMID, OWRD_Basin, FishCode, SpawnCode, 
                                     WaterTypeCode, WaterBodyCode, BacteriaCode, DO_code, 
                                     DO_SpawnCode, pH_code, ben_use_code)
    if (!is.null(HUC8)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8 %in% {
                                         {
                                           HUC8
                                         }
                                       })
    }
    if (!is.null(HUC8_Name)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8_Name %in% {
                                         {
                                           HUC8_Name
                                         }
                                       })
    }
    if (!is.null(MonLocType)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       MonLocType %in% {
                                         {
                                           MonLocType
                                         }
                                       })
    }
    stations_filter <- dplyr::collect(stations_filter)
    mlocs_filtered <- stations_filter$MLocID
    DBI::dbDisconnect(station_con)
    print("Query stations database- Complete")
    tictoc::toc()
  }

  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver   =  Sys.getenv('DRIVER'),
                        Server   =  Sys.getenv('AWQMS_SERVER'),
                        Database =  Sys.getenv('AWQMS_Database'),
                        UID      =  Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  AWQMS_data <- dplyr::tbl(con, "results_deq_vw")
  if (exists("mlocs_filtered")) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% mlocs_filtered)
  }
  if (length(project) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Project1 %in% 
                                  project)
  }
  if (length(Char_Name) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Char_Name %in% 
                                  {
                                    {
                                      Char_Name
                                    }
                                  })
  }
  if (length(OrganizationID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, OrganizationID %in% 
                                  {
                                    {
                                      OrganizationID
                                    }
                                  })
  }
  print("Query AWQMS database...")
  AWQMS_data <- dplyr::distinct(dplyr::select(AWQMS_data, MLocID, 
                                              OrganizationID))
  tictoc::tic("AWQMS database query")
  AWQMS_data <- dplyr::collect(AWQMS_data)
  print("Query AWQMS database- Complete")
  tictoc::toc()
  if (exists("stations_filter")) {
    AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                   by = "MLocID")
  }
  else {
    stations <- AWQMS_data$MLocID
    if (length(stations) == 0) {
      AWQMS_data <- dplyr::mutate(AWQMS_data, StationDes = NA_character_, 
                                  MonLocType = NA_character_, EcoRegion3 = NA_character_, 
                                  EcoRegion4 = NA_character_, HUC8 = NA_character_, 
                                  HUC8_Name = NA_character_, HUC10 = NA_character_, 
                                  HUC12 = NA_character_, HUC12_Name = NA_character_, 
                                  Reachcode = NA_character_, Measure = NA_character_, 
                                  AU_ID = NA_character_, WaterTypeCode = NA_character_, 
                                  WaterBodyCode = NA_character_, ben_use_code = NA_character_, 
                                  FishCode = NA_character_, SpawnCode = NA_character_, 
                                  DO_code = NA_character_, DO_SpawnCode = NA_character_, 
                                  BacteriaCode = NA_character_, pH_code = NA_character_, 
      )
    }
    else {
      tictoc::tic("Station Database Query")
      print("Query stations database...")
      station_con <- DBI::dbConnect(odbc::odbc(),
                                    Driver = Sys.getenv('DRIVER'),
                                    Server = Sys.getenv('STATIONS_SERVER'),
                                    Database =Sys.getenv('STATIONS_Database'),
                                    UID    = Sys.getenv('STATIONS_usr'),
                                    PWD    = Sys.getenv('STATIONS_pass'))
      
      stations_filter <- dplyr::collect(dplyr::filter(dplyr::select(dplyr::tbl(station_con, 
                                                                               "VWStationsFinal"), MLocID, StationDes, GNIS_Name, 
                                                                    AU_ID, Lat_DD, Long_DD, MonLocType, EcoRegion3, 
                                                                    EcoRegion4, HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, 
                                                                    Reachcode, Measure, COMID, OWRD_Basin, FishCode, 
                                                                    SpawnCode, WaterTypeCode, WaterBodyCode, BacteriaCode, 
                                                                    DO_code, DO_SpawnCode, pH_code, ben_use_code), 
                                                      MLocID %in% stations))
      print("Query stations database- Complete")
      tictoc::toc()
      AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                     by = "MLocID")
    }
    if (crit_codes == FALSE) {
      AWQMS_data <- dplyr::select(AWQMS_data, -WaterTypeCode, 
                                  -WaterBodyCode, -ben_use_code, -FishCode, -SpawnCode, 
                                  -DO_code, -DO_SpawnCode, -BacteriaCode, -pH_code)
    }
    DBI::dbDisconnect(con)
  }
  return(AWQMS_data)
}

noenvAWQMS_Data<-
function (startdate = "1949-09-15", enddate = NULL, MLocID = NULL, 
          MonLocType = NULL, AU_ID = NULL, project = NULL, Char_Name = NULL, 
          CASNumber = NULL, Statistical_Base = NULL, SampleMedia = NULL, 
          SampleSubmedia = NULL, OrganizationID = NULL, HUC8 = NULL, 
          HUC8_Name = NULL, HUC10 = NULL, HUC12 = NULL, HUC12_Name = NULL, 
          EcoRegion3 = NULL, last_change_start = NULL, last_change_end = NULL, 
          crit_codes = FALSE, filterQC = TRUE, return_query = FALSE) 
{
  if (!(is.character(HUC8) | is.null(HUC8))) {
    stop("HUC8 value must be a character")
  }
  if (!(is.character(HUC10) | is.null(HUC10))) {
    stop("HUC10 value must be a character")
  }
  if (!(is.character(HUC12) | is.null(HUC12))) {
    stop("HUC12 value must be a character")
  }
  if (!is.null(c(HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, 
                 AU_ID, EcoRegion3))) {
    print("Query stations database...")
    tictoc::tic("Station Database Query")
    
    station_con <- DBI::dbConnect(odbc::odbc(),
                                  Driver = Sys.getenv('DRIVER'),
                                  Server = Sys.getenv('STATIONS_SERVER'),
                                  Database =Sys.getenv('STATIONS_Database'),
                                  UID    = Sys.getenv('STATIONS_usr'),
                                  PWD    = Sys.getenv('STATIONS_pass'))
    
    stations_filter <- dplyr::select(dplyr::tbl(station_con, 
                                                "VWStationsFinal"), MLocID, EcoRegion3, EcoRegion4, 
                                     HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, Reachcode, 
                                     Measure, AU_ID, WaterTypeCode, WaterBodyCode, ben_use_code, 
                                     FishCode, SpawnCode, DO_code, DO_SpawnCode, BacteriaCode, 
                                     pH_code)
    if (!is.null(HUC8)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8 %in% {
                                         {
                                           HUC8
                                         }
                                       })
    }
    if (!is.null(HUC8_Name)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8_Name %in% {
                                         {
                                           HUC8_Name
                                         }
                                       })
    }
    if (!is.null(HUC10)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC10 %in% {
                                         {
                                           HUC10
                                         }
                                       })
    }
    if (!is.null(HUC12)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC12 %in% {
                                         {
                                           HUC12
                                         }
                                       })
    }
    if (!is.null(HUC12_Name)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC12_Name %in% {
                                         {
                                           HUC12_Name
                                         }
                                       })
    }
    if (!is.null(AU_ID)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       AU_ID %in% {
                                         {
                                           AU_ID
                                         }
                                       })
    }
    if (!is.null(EcoRegion3)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       EcoRegion3 %in% {
                                         {
                                           EcoRegion3
                                         }
                                       })
    }
    stations_filter <- dplyr::collect(stations_filter)
    mlocs_filtered <- stations_filter$MLocID
    DBI::dbDisconnect(station_con)
    print("Query stations database- Complete")
    tictoc::toc()
  }
  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver   =  Sys.getenv('DRIVER'),
                        Server   =  Sys.getenv('AWQMS_SERVER'),
                        Database =  Sys.getenv('AWQMS_Database'),
                        UID      =  Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  AWQMS_data <- dplyr::tbl(con, "results_deq_vw")
  if (exists("mlocs_filtered")) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% mlocs_filtered)
  }
  if (length(startdate) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, SampleStartDate >= 
                                  startdate)
  }
  if (length(enddate) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, SampleStartDate <= 
                                  enddate)
  }
  if (length(last_change_start) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, res_last_change_date >= 
                                  last_change_start)
  }
  if (length(last_change_end) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, res_last_change_date <= 
                                  last_change_end)
  }
  if (length(MLocID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% {
      {
        MLocID
      }
    })
  }
  if (length(MonLocType) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MonLocType %in% 
                                  {
                                    {
                                      MonLocType
                                    }
                                  })
  }
  if (length(project) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Project1 %in% 
                                  project)
  }
  if (length(Char_Name) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Char_Name %in% 
                                  {
                                    {
                                      Char_Name
                                    }
                                  })
  }
  if (length(CASNumber) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, CASNumber %in% 
                                  {
                                    {
                                      CASNumber
                                    }
                                  })
  }
  if (length(Statistical_Base) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Statistical_Base %in% 
                                  {
                                    {
                                      Statistical_Base
                                    }
                                  })
  }
  if (length(SampleMedia) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, SampleMedia %in% 
                                  {
                                    {
                                      SampleMedia
                                    }
                                  })
  }
  if (length(SampleSubmedia) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, SampleSubmedia %in% 
                                  {
                                    {
                                      SampleSubmedia
                                    }
                                  })
  }
  if (length(OrganizationID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, OrganizationID %in% 
                                  {
                                    {
                                      OrganizationID
                                    }
                                  })
  }
  if (filterQC == TRUE) {
    AWQMS_data <- dplyr::filter(AWQMS_data, !Activity_Type %like% 
                                  "Quality Control%")
  }
  if (return_query) {
    AWQMS_data <- dplyr::show_query(AWQMS_data)
  }
  else {
    print("Query AWQMS database...")
    tictoc::tic("AWQMS database query")
    AWQMS_data <- dplyr::collect(AWQMS_data)
    print("Query AWQMS database- Complete")
    tictoc::toc()
    if (exists("stations_filter")) {
      AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                     by = "MLocID")
    }
    else {
      stations <- AWQMS_data$MLocID
      if (length(stations) == 0) {
        AWQMS_data <- dplyr::mutate(AWQMS_data, StationDes = NA_character_, 
                                    MonLocType = NA_character_, EcoRegion3 = NA_character_, 
                                    EcoRegion4 = NA_character_, HUC8 = NA_character_, 
                                    HUC8_Name = NA_character_, HUC10 = NA_character_, 
                                    HUC12 = NA_character_, HUC12_Name = NA_character_, 
                                    Reachcode = NA_character_, Measure = NA_character_, 
                                    AU_ID = NA_character_, WaterTypeCode = NA_character_, 
                                    WaterBodyCode = NA_character_, ben_use_code = NA_character_, 
                                    FishCode = NA_character_, SpawnCode = NA_character_, 
                                    DO_code = NA_character_, DO_SpawnCode = NA_character_, 
                                    BacteriaCode = NA_character_, pH_code = NA_character_, 
        )
      }
      else {
        tictoc::tic("Station Database Query")
        print("Query stations database...")
        station_con <- DBI::dbConnect(odbc::odbc(),
                                      Driver = Sys.getenv('DRIVER'),
                                      Server = Sys.getenv('STATIONS_SERVER'),
                                      Database =Sys.getenv('STATIONS_Database'),
                                      UID    = Sys.getenv('STATIONS_usr'),
                                      PWD    = Sys.getenv('STATIONS_pass'))
        
        stations_filter <- dplyr::collect(dplyr::filter(dplyr::select(dplyr::tbl(station_con, 
                                                                                 "VWStationsFinal"), MLocID, EcoRegion3, EcoRegion4, 
                                                                      HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, 
                                                                      Reachcode, Measure, AU_ID, WaterTypeCode, WaterBodyCode, 
                                                                      ben_use_code, FishCode, SpawnCode, DO_code, 
                                                                      DO_SpawnCode, BacteriaCode, pH_code), MLocID %in% 
                                                          stations))
        print("Query stations database- Complete")
        tictoc::toc()
        AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                       by = "MLocID")
      }
      if (crit_codes == FALSE) {
        AWQMS_data <- dplyr::select(AWQMS_data, -WaterTypeCode, 
                                    -WaterBodyCode, -ben_use_code, -FishCode, -SpawnCode, 
                                    -DO_code, -DO_SpawnCode, -BacteriaCode, -pH_code)
      }
      DBI::dbDisconnect(con)
    }
    return(AWQMS_data)
  }
}

noenvAWQMS_Data_Cont<-
function (startdate = NULL, enddate = NULL, MLocID = NULL, AU_ID = NULL, 
          Char_Name = NULL, SampleMedia = NULL, OrganizationID = NULL, 
          HUC8 = NULL, HUC8_Name = NULL, HUC10 = NULL, HUC12 = NULL, 
          HUC12_Name = NULL, Result_Status = NULL, crit_codes = FALSE) 
{
  if (!is.null(c(HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, 
                 AU_ID))) {
    print("Query stations database...")
    tictoc::tic("Station Database Query")
    
    station_con <- DBI::dbConnect(odbc::odbc(),
                                  Driver = Sys.getenv('DRIVER'),
                                  Server = Sys.getenv('STATIONS_SERVER'),
                                  Database =Sys.getenv('STATIONS_Database'),
                                  UID    = Sys.getenv('STATIONS_usr'),
                                  PWD    = Sys.getenv('STATIONS_pass'))
    
    stations_filter <- dplyr::select(dplyr::tbl(station_con, 
                                                "VWStationsFinal"), MLocID, StationDes, Lat_DD, Long_DD, 
                                     MonLocType, EcoRegion3, EcoRegion4, HUC8, HUC8_Name, 
                                     HUC10, HUC12, HUC12_Name, Reachcode, Measure, AU_ID, 
                                     WaterTypeCode, WaterBodyCode, ben_use_code, FishCode, 
                                     SpawnCode, DO_code, DO_SpawnCode, BacteriaCode, pH_code)
    if (!is.null(HUC8)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8 %in% {
                                         {
                                           HUC8
                                         }
                                       })
    }
    if (!is.null(HUC8_Name)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC8_Name %in% {
                                         {
                                           HUC8_Name
                                         }
                                       })
    }
    if (!is.null(HUC10)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC10 %in% {
                                         {
                                           HUC10
                                         }
                                       })
    }
    if (!is.null(HUC12)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC12 %in% {
                                         {
                                           HUC12
                                         }
                                       })
    }
    if (!is.null(HUC12_Name)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       HUC12_Name %in% {
                                         {
                                           HUC12_Name
                                         }
                                       })
    }
    if (!is.null(AU_ID)) {
      stations_filter <- dplyr::filter(stations_filter, 
                                       AU_ID %in% {
                                         {
                                           AU_ID
                                         }
                                       })
    }
    stations_filter <- dplyr::collect(stations_filter)
    mlocs_filtered <- stations_filter$MLocID
    DBI::dbDisconnect(station_con)
    print("Query stations database- Complete")
    tictoc::toc()
  }

  con <- DBI::dbConnect(odbc::odbc(), 
                        Driver   =  Sys.getenv('DRIVER'),
                        Server   =  Sys.getenv('AWQMS_SERVER'),
                        Database =  Sys.getenv('AWQMS_Database'),
                        UID      =  Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  AWQMS_data <- dplyr::tbl(con, "continuous_results_deq_vw")
  if (exists("mlocs_filtered")) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% mlocs_filtered)
  }
  if (length(startdate) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Result_Date >= 
                                  startdate)
  }
  if (length(enddate) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Result_Date <= 
                                  enddate)
  }
  if (length(MLocID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, MLocID %in% {
      {
        MLocID
      }
    })
  }
  if (length(Char_Name) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Char_Name %in% 
                                  {
                                    {
                                      Char_Name
                                    }
                                  })
  }
  if (length(SampleMedia) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, SampleMedia %in% 
                                  {
                                    {
                                      SampleMedia
                                    }
                                  })
  }
  if (length(OrganizationID) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, OrganizationID %in% 
                                  {
                                    {
                                      OrganizationID
                                    }
                                  })
  }
  if (length(Result_Status) > 0) {
    AWQMS_data <- dplyr::filter(AWQMS_data, Result_Status %in% 
                                  {
                                    {
                                      Result_Status
                                    }
                                  })
  }
  print("Query AWQMS database...")
  tictoc::tic("AWQMS database query")
  AWQMS_data <- dplyr::collect(AWQMS_data)
  print("Query AWQMS database- Complete")
  tictoc::toc()
  if (exists("stations_filter")) {
    AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                   by = "MLocID")
  }
  else {
    stations <- AWQMS_data$MLocID
    if (length(stations) == 0) {
      AWQMS_data <- dplyr::mutate(AWQMS_data, StationDes = NA_character_, 
                                  MonLocType = NA_character_, EcoRegion3 = NA_character_, 
                                  EcoRegion4 = NA_character_, HUC8 = NA_character_, 
                                  HUC8_Name = NA_character_, HUC10 = NA_character_, 
                                  HUC12 = NA_character_, Lat_DD = NA_character_, 
                                  Long_DD = NA_character_, HUC12_Name = NA_character_, 
                                  Reachcode = NA_character_, Measure = NA_character_, 
                                  AU_ID = NA_character_, WaterTypeCode = NA_character_, 
                                  WaterBodyCode = NA_character_, ben_use_code = NA_character_, 
                                  FishCode = NA_character_, SpawnCode = NA_character_, 
                                  DO_code = NA_character_, DO_SpawnCode = NA_character_, 
                                  BacteriaCode = NA_character_, pH_code = NA_character_, 
      )
    }
    else {
      tictoc::tic("Station Database Query")
      print("Query stations database...")
      station_con <- DBI::dbConnect(odbc::odbc(),
                                    Driver = Sys.getenv('DRIVER'),
                                    Server = Sys.getenv('STATIONS_SERVER'),
                                    Database =Sys.getenv('STATIONS_Database'),
                                    UID    = Sys.getenv('STATIONS_usr'),
                                    PWD    = Sys.getenv('STATIONS_pass'))
      
      stations_filter <- dplyr::collect(dplyr::filter(dplyr::select(dplyr::tbl(station_con, 
                                                                               "VWStationsFinal"), MLocID, StationDes, Lat_DD, 
                                                                    Long_DD, MonLocType, EcoRegion3, EcoRegion4, 
                                                                    HUC8, HUC8_Name, HUC10, HUC12, HUC12_Name, Reachcode, 
                                                                    Measure, AU_ID, WaterTypeCode, WaterBodyCode, 
                                                                    ben_use_code, FishCode, SpawnCode, DO_code, DO_SpawnCode, 
                                                                    BacteriaCode, pH_code), MLocID %in% stations))
      print("Query stations database- Complete")
      tictoc::toc()
      AWQMS_data <- dplyr::left_join(AWQMS_data, stations_filter, 
                                     by = "MLocID")
    }
    if (crit_codes == FALSE) {
      AWQMS_data <- dplyr::select(AWQMS_data, -WaterTypeCode, 
                                  -WaterBodyCode, -ben_use_code, -FishCode, -SpawnCode, 
                                  -DO_code, -DO_SpawnCode, -BacteriaCode, -pH_code)
    }
  }
  DBI::dbDisconnect(con)
  return(AWQMS_data)
}