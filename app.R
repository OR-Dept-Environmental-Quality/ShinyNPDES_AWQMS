#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This app, based on a modified verison of the AWQMSdata_ShinyHelp app built by Travis Pritchard, 
######is designed to pull out RPA related data from AWQMS and put it into a table for use in RPA analysis

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(leaflet)
library(dplyr)
library(xlsxjars)
library(mapview)
library(leaflet.extras)
#library(mapedit)
#library(sf)
library(shinybusy)
library(openxlsx)
library(tidyr)
library(tidyverse)

#attempt to turn off scientific notation
options(scipen=999)

#in case the shinybusy package needs to be installed/reinstalled
#remotes::install_github("dreamRs/shinybusy")

#Need to remake query, cannot use AWQMS_Data as it pulls out too much data for the app to work,
#plus, for NPDES only need a subset of data- 
#the function NPDES_AWQMS_Qry will only pull water data from a select set of monloc types
source("NPDES_AWQMSQuery.R")
#contains function to transform data into proper units and format to run through Ammonia RPA
source("Ammonia_RPA_Transform.R")
#function to combine characteristic name and sample fraction for metals
source("NameandFraction.R")


# Query out the valid values ---------------------------------------------

#NPDES only needs a limited # of Chars, this should help speed up the program

#make it so only the RPA groupings are shown in the drop down

chars <- c("All RPA","All Toxics","Copper BLM","ph RPA","Ammonia RPA","DO RPA","Pesticides and PCB RPA","Base Neutral RPA", "Acid Extractable RPA",
           "VOC RPA","Metals RPA","Industrial General","None")

#create variables with specific characteristics for the different groups

#Industrial general parameters
ingen<-c("Chemical oxygen demand","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen", "Organic carbon",
         "Biochemical oxygen demand, non-standard conditions","Biochemical oxygen demand, standard conditions","pH","Temperature, water")
#pH RPA
phrpa<-c("Alkalinity, total","pH","Temperature, water","Salinity","Conductivity")

#Ammonia RPA
ammrpa<-c("Alkalinity, total","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen","Conductivity","pH","Temperature, water","Salinity")

#Copper BLM
cuB<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
       "Temperature, water","Total Sulfate","Sulfide")

#Dissolved Oxygen RPA
dorpa<-c("Dissolved oxygen (DO)","Dissolved oxygen saturation","Biochemical oxygen demand, non-standard conditions",
         "Biochemical oxygen demand, standard conditions","Kjeldahl nitrogen","Total Kjeldahl nitrogen","Temperature, water",
         "Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")  

#Pesticides and PCBs
pestrpa<-c("p,p'-DDT","Parathion","Chlordane","Lindane","Dieldrin","Endrin","Methoxychlor","p,p'-DDD","p,p'-DDE","Heptachlor",
           "Azinphos-methyl","Malathion","Aldrin",".alpha.-Hexachlorocyclohexane",".beta.-Hexachlorocyclohexane",
           "Benzene Hexachloride, Beta (BHC)","1,2,3,4,5,6-Hexachlorocyclohexane",".alpha.-Endosulfan","Heptachlor epoxide",
           "Endosulfan sulfate","Mirex","Chlorpyrifos","Endrin aldehyde","Toxaphene","Demeton","Aroclor 1260","Aroclor 1254",
           "Aroclor 1221","Aroclor 1232","Aroclor 1248","Aroclor 1016",".beta.-Endosulfan","Aroclor 1242","Total PCBs")

#Base Neutral
bneut<-c("Benzo[a]pyrene","Dibenz[a,h]anthracene","Benz[a]anthracene","N-Nitrosodimethylamine","Hexachloroethane",
         "Hexachlorocyclopentadiene","Isophorone","Acenaphthene","Diethyl phthalate","Dibutyl phthalate","Phenanthrene",
         "Butyl benzyl phthalate","N-Nitrosodiphenylamine","Fluorene","Hexachlorobutadiene","Naphthalene","2-Chloronaphthalene",
         "3,3'-Dichlorobenzidine","Benzidine","1,2,4,5-Tetrachlorobenzene","Nitrobenzene","BDE-003",
         "Bis(2-chloro-1-methylethyl) ether","Bis(2-chloroethyl) ether","Bis(2-chloroethoxy)methane","Di(2-ethylhexyl) phthalate",
         "Di-n-octyl phthalate","Hexachlorobenzene","Anthracene","1,2,4-Trichlorobenzene","2,4-Dinitrotoluene","1,2-Diphenylhydrazine",
         "Pyrene","Dimethyl phthalate","Benzo[ghi]perylene","Indeno[1,2,3-cd]pyrene","Benzo(b)fluoranthene","Fluoranthene",
         "Benzo[k]fluoranthene","Acenaphthylene","Chrysene","2,6-Dinitrotoluene","Pentachlorobenzene","N-Nitrosodi-n-propylamine",
         "p-Chlorophenyl phenyl ether","Azobenzene")

#Acid Extractable
aext<-c("2,4-Dinitrophenol","p-Chloro-m-cresol","Pentachlorophenol","2,4,6-Trichlorophenol","o-Nitrophenol","o-Chlorophenol",
        "2,4,5-Trichlorophenol","p-Nitrophenol","2,4-Dimethylphenol","Phenol","Phenols","2,4-Dichlorophenol","4,6-Dinitro-o-cresol")

#Volatile Organic Carbons 
#Trichloroethene (TCE) has been retired in AWQMS, replaced with Trichloroethylene
vocrpa<-c("Carbon tetrachloride","Chloroform","Benzene","1,1,1-Trichloroethane","Methyl bromide","Chloromethane","Chloroethane",
          "Vinyl chloride","Methylene chloride","Tribromomethane","Dichlorobromomethane","1,1-Dichloroethane","1,1-Dichloroethylene",
          "1,2-Dichloropropane","1,1,2-Trichloroethane","Trichloroethene (TCE)","Trichloroethylene","1,1,2,2-Tetrachloroethane","o-Dichlorobenzene",
          "Ethylbenzene","p-Dichlorobenzene","Acrolein","Allyl chloride","1,2-Dichloroethane","Toluene","Chlorobenzene",
          "2-Chloroethyl vinyl ether","Chlorodibromomethane","Tetrachloroethene","Tetrachloroethylene","trans-1,2-Dichloroethylene",
          "m-Dichlorobenzene","1,3-Dichloropropene","Acrylonitrile")

#Metals and Hardness
metalsrpa<-c("Cyanide","Cyanides amenable to chlorination (HCN & CN)","Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
             "Beryllium","Cadmium","Chromium","Copper","Zinc","Selenium","Nitrate","Inorganic nitrogen (nitrate and nitrite)",
             "Nitrate + Nitrite","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Total hardness","Hardness, Ca, Mg",
             "Hardness, carbonate","Hardness, non-carbonate","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")

#all toxics (metals, voc, acid extractable, base neutral,pesticides and PCBs,metals)
tox<-c(metalsrpa,vocrpa,aext,bneut,pestrpa)

#one-off characteristics of interest
oneoff<-unique(c("Chlorine",tox,phrpa,ammrpa,dorpa,cuB,"Chemical oxygen demand","Turbidity Field", "Orthophosphate","Escherichia coli",
                 "Fecal Coliform","Phosphate-phosphorus","Total solids","Total suspended solids","Manganese","Flow","Total dissolved solids",
                 "Chlorine, Total Residual","Nitrite","Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)","Organic Nitrogen"))


# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations and organizations and save the cache
if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 7){
  

#NPDES_AWQMS_Stations functions only pulls stations of interest to NPDES program
station <- NPDES_AWQMS_Stations()
Mtype<-station$MonLocType
auid<-station$AU_ID
auid<-sort(station$AU_ID)
station <- station$MLocID
station <- sort(station)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)



#save query information in a file. Don't have to redo pulls each time. Saves a lot of time. 
save(station, Mtype, auid, organization, file = 'query_cache.RData')
} else {
  load("query_cache.RData")
}


HUC8_Names <- c('Alsea', 'Alvord Lake', 'Applegate', 'Beaver-South Fork',
                'Brownlee Reservoir', 'Bully', 'Burnt', 'Chetco', 'Chief Joseph',
                'Clackamas', 'Coast Fork Willamette', 'Coos','Coquille',
                'Crooked-Rattlesnake',  'Donner und Blitzen',' Goose Lake',
                'Guano', 'Harney-Malheur Lakes', 'Illinois', 'Imnaha', 'Jordan',
                'Lake Abert', 'Little Deschutes','Lost', 'Lower Columbia', 'Lower Columbia-Clatskanie',
                'Lower Columbia-Sandy','Lower Crooked','Lower Deschutes', 'Lower Grande Ronde', 'Lower John Day',
                'Lower Malheur', 'Lower Owyhee', 'Lower Rogue', 'Lower Willamette', 'Mckenzie', 'Middle Columbia-Hood',
                'Middle Columbia-Lake Wallula', 'Middle Fork John Day', 'Middle Fork Willamette', 'Middle Owyhee',
                'Middle Rogue', 'Middle Snake-Payette', 'Middle Snake-Succor', 'Middle Willamette', 'Molalla-Pudding',
                'Necanicum', 'Nehalem', 'North Fork John Day', 'North Santiam', 'North Umpqua', 'Not Loaded', 'Powder',
                'Siletz-Yaquina', 'Siltcoos', 'Silver', 'Silvies', 'Siuslaw', 'Sixes', 'Smith', 'South Fork Owyhee', 'South Santiam',
                'South Umpqua', 'Sprague', 'Summer Lake', 'Trout', 'Tualatin', 'Umatilla', 'Umpqua', 'Upper Columbia-Entiat',
                'Upper Columbia-Priest Rapids', 'Upper Crooked', 'Upper Deschutes', 'Upper Grande Ronde,Upper John Day',
                'Upper Klamath', 'Upper Klamath Lake', 'Upper Malheur', 'Upper Quinn', 'Upper Rogue', 'Upper Willamette',
                'Walla Walla', 'Wallowa', 'Warner Lakes', 'Williamson', 'Willow', 'Wilson-Trusk-Nestuccu', 'Yamhill')


# Define UI 
ui <- fluidPage(

   # Sidebar with parameter inputs

  sidebarLayout(
      sidebarPanel(
        #permittee name
        textInput("permittee",
                  label="Permittee Name"),
        #permit #
        textInput("permit_num",
                  label="Permit Number"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        
        # Start Date (make start date six months ago)
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = Sys.Date()-182),
        # End date
        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),
       
        #characteristics
         selectizeInput("characteristics",
                     "Select RPA Group (pick one only)",
                     choices = chars,
                     multiple = FALSE,
                     selected="All RPA"),
       #specific characteristics outside of groups
         selectizeInput("oneoff",
                        "Specific Characteristics not part of groupings",
                        choices=oneoff,
                        multiple=TRUE),

       # Monitoring locations 
        selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = station,
                        multiple = TRUE),
       
       # Monitoring location types
        selectizeInput("montype",
                       "Select Monitoring Location Types",
                       choices=Mtype,
                       multiple=TRUE),
       
       #add warning
       tags$em("Warning: HUC8 may not include all stations on coast"),
       
       # huc8 names 
       selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = HUC8_Names,
                       multiple = TRUE),
       
       #AU_IDs
       selectizeInput("AUID",
                      "Select Assessment Unit",
                      choices = auid,
                      multiple = TRUE),
    
       #Orgs
       selectizeInput("orgs",
                       "Select organization",
                       choices = organization,
                       multiple = TRUE),
       #Reject button
       checkboxInput("Reject",
                     label = "Keep Rejected data",
                     value = FALSE),
       #keep summary stats button
       checkboxInput("Summary",
                     label="Keep Continuous Data Summary Statistics (other than 7 day avg)?",
                     value=FALSE),
       
       #add action button, so query doesn't run until button is clicked
       actionButton("goButton","Run Query"),
       
       #add a download button so we can download query results
       downloadButton('downloadData', 'Download Data'),
       
       #add button to make download of map optional
       checkboxInput("NoMap",
                     label="Add map to data download?",
                     value= FALSE)
        ),


     # Setup main panel
       mainPanel(
        h1("RPA Data Builder"),
        verbatimTextOutput("contwar"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        
        #three tabs: directions,plot and map
        tabsetPanel(
        #directions tab
        tabPanel("Directions", 
                 mainPanel(h5("Select parameters on left to build table and map"),
                           h5("Click 'Run Query' Button to perform search after selecting desired parameters."),
                           h5("Peruse the 'Table' and 'Map' tabs to view results and locations"),
                           h5("Click 'Download Data' to download results"),
                           h5("Note: All RPA also includes Chlorine data"),
                           h5("Warning: after running the query, if you change your mind about whether to include the map, 
                              you must select the box to add the map and then re run the query to ensure that the map will be 
                              part of the download"),
                           tags$br(),
                           h5("Warning: running query with all characteristics and a large timeframe (1+ year) can overload the server.
                              If you want to search for all characteristics, it is recommended that you run the query with a short timeframe
                              first (~6 months), then use the map to locate several stations of interest and refine query accordingly.
                              Alternatively, you may also refine by HUC8. However, refining search by HUC8 may not include all stations near 
                              the coast.")
                           )
                 ),
        # Aliana added a data table
        tabPanel("Table",
                 dataTableOutput("table")),
        #add leaflet map
        tabPanel("Map",leafletOutput("locs")),
        #check diagnositcs
        tabPanel("RPA Summary",
                 dataTableOutput("RPAsum"))
        )
   )
),

#add icon to show when program is running query or download
add_busy_spinner(spin = "fading-circle"))

# Define server logic required to display query
server <- function(input, output) {
  
   
   #have to make dates into strings, otherwise they come out as funny numbers
   #all other variables are reactive 'as is' except for reject button
   #isolate data so that you have to click a button so that it runs the query using eventReactive.

   orig<-eventReactive(input$goButton,{
     
   rstdt<-toString(sprintf("%s",input$startd))
   rendd<-toString(sprintf("%s",input$endd))
   rrej<-if(input$Reject) {TRUE} else {FALSE} 
   
   #build characteristic list
   gch<-switch(input$characteristics,"All RPA"=unique(c(phrpa,ammrpa,cuB,dorpa,ingen,pestrpa,bneut,aext,vocrpa,metalsrpa,"Chlorine")),
                 "Copper BLM"=cuB,   
                 "ph RPA"=phrpa,
                 "Ammonia RPA"=ammrpa,
                 "DO RPA"=dorpa,
                 "Pesticides and PCB RPA"=pestrpa,
                 "Base Neutral RPA"=bneut,
                 "Acid Extractable RPA"=aext,
                 "VOC RPA"=vocrpa,
                 "Metals RPA"=metalsrpa,
                 "All Toxics"=tox,
                 "Industrial General"=ingen,
                 "None"=character(0)) #none is an empty character string so we can just pull one-off parameters
   one<-c(input$oneoff)
   rchar<-c(gch,one)
   
   #actual query for data
   dat<-NPDES_AWQMS_Qry(startdate=rstdt,enddate=rendd,station=c(input$monlocs),montype=c(input$montype),
                  char=c(rchar),org=c(input$orgs),HUC8_Name=c(input$huc8_nms), AU_ID=c(input$AUID),reject=rrej)
   
   
   #want to add list of characteristics for each monitoring location to the leaflet popup, to do that we're going to have to pull 
   #in data() and add a column that has all characteristic names for each monitoring location....
   #if I just add data$char_Names I only get the first char (usually temperature, water)
   #able fix this by grouping via MLocID, then getting the unique chars via summarize
   #then merge the two dataframes together using MLocID, creates column called "type" that has chars
   grp<-dat %>% group_by(MLocID) %>% 
     summarize(type = paste(sort(unique(Char_Name)),collapse=", "))
   
   #merge 
   
   mer<-merge(dat,grp, by="MLocID")
   
   mer
   })
   #if summary statistics are included, create flag showing that continous data is available and remove all data that isn't 7 day avg
   output$contwar<-renderText({
     warn<-unique(if(any(!is.na(orig()$Time_Basis))) {paste("Continous data may be available upon request")})
     warn
   })
   
   #remove summary stats that are not 7 day avg
   data<-eventReactive(input$goButton,{
     dat<-if(input$Summary){orig()} else {subset(orig(),is.na(orig()$Time_Basis)|orig()$Time_Basis %in% "7DADMean")}
     dat
   })
   
   #take data, make a subtable for VIEWING in the shiny app so it only shows desired columns from the AWQMS pull in desired order
   tsub<-eventReactive(input$goButton,{
     tsub<-select(data(),OrganizationID,Project1,StationDes,MLocID,MonLocType,SampleStartDate,SampleMedia,
               SampleSubmedia,Activity_Type,Statistical_Base,Char_Name,Char_Speciation,
               Sample_Fraction,CASNumber,Result,Result_Unit,Method_Code,Method_Context,
               Activity_Comment,Result_Comment,Result_status,Result_Type)
   tsub
   })
   
   #take data, make a subtable for DOWNLOAD so that we only show the desired columns from the AWQMS data pull and in the desired order
   dsub<-eventReactive(input$goButton,{
     dsub<-select(data(),OrganizationID,Org_Name,Project1,act_id,StationDes,MLocID,MonLocType,SampleStartDate,SampleStartTime,SampleMedia,
                 SampleSubmedia,Activity_Type,Statistical_Base,Time_Basis,Char_Name,Char_Speciation,
                 Sample_Fraction,CASNumber,Result,Result_Unit,Analytical_method,Method_Code,Method_Context,Analytical_Lab,
                 MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit,
                 Activity_Comment,Result_Comment,Result_status,Result_Type)
     dsub
   })
   
   #table of queried data for Shiny app view  
   output$table<-renderDataTable({
     
     tsub()
   })
   
   #leaflet map
   mymap<- eventReactive(input$goButton,{   
     leaflet(data()) %>%
       addTiles()%>%
       addMarkers(lng=~Long_DD,
                  lat=~Lat_DD,
                  popup=paste("Station ID: ",data()$MLocID,"<br>",
                              "Description: ",data()$StationDes,"<br>",
                              "Characteristics: ",data()$type,"<br>"),
                  popupOptions= popupOptions(maxHeight = 75)) %>%
       #want to be able to select points on map via polygon.
       #first step is to be able to draw polygon on map
       addDrawToolbar(editOptions = editToolbarOptions())
   })
   
   #show map in shiny viewer
   output$locs<-renderLeaflet({ mymap()})
   
   #step two to be able to select points on map via polygon: get the bounds of the polygon
   # Show summary information for debuging only
   #coords<-eventReactive(input$mymap_draw_new_feature,
   #                    { print(str(input$mymap_draw_new_feature))})
   #output$summary <- renderPrint({coords()})
   
   
   #doesn't seem to work, wants to call a shiny within a shiny- no go
   #convert points to sf object so we can select them
   #pts<-st_as_sf(data(),coords=c("Lat_DD","Long_DD"),remove=FALSE)
   #selectFeatures(pts,map=map,mode="draw",viewer=NULL)
   
   
   #transform data for Copper BLM
   copper<-eventReactive(input$goButton,{
     cu<-CuBLM(data())
     
     cu
   })
   
   
   #take data, make subtable just for toxics RPA data
   rpa<-eventReactive(input$goButton,{
     
     #only keep characteristics that are in the tox character list

     rpa<-subset(data(),(Char_Name %in% tox))
    
     if (nrow(rpa)!=0){
       #combine method_code and method_Context columns
     rpa$Method_Code<-paste0(rpa$Method_Code," (",rpa$Method_Context,")")

     #need to do unit conversions, al in ug/l, except for Alkalinity, which should be in mg/L
     #checked AWQMS database, all alkalinity is reported in mg/l or mg/l CaCO3, no need for conversion
     #get list of char names in RPA
     names<-unique(rpa$Char_Name)
     #remove alkalinity, that needs to stay as mg/l
     names<-names[names !="Alkalinity, total"]
    
     rpa<-unit_conv(rpa,names,"mg/l","ug/l")
     rpa<-unit_conv(rpa,names,"ng/l","ug/l")
     
     #add T, D, or I to CAS# for certain parameters (mostly metals, used RPA workbook to identify parameters) 
     #so that the RPA workbooks will recognize them
     
     #dissolved
     rpa$CASNumber<-
       ifelse(rpa$Char_Name %in% c("Copper","Magnesium","Potassium","Sodium","Cyanide","Aluminum","Iron","Lead",
                                   "Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Beryllium","Cadmium","Chromium",
                                   "Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Methylmercury(1+)") &
              rpa$Sample_Fraction %in% "Dissolved",
              paste0(rpa$CASNumber,"D"),
              rpa$CASNumber)
     #total
     rpa$CASNumber<-
       ifelse(rpa$Char_Name %in% c("Copper","Magnesium","Potassium","Sodium","Cyanide","Aluminum","Iron","Lead",
                                   "Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Beryllium","Cadmium","Chromium",
                                   "Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Methylmercury(1+)") &
              (rpa$Sample_Fraction %in% "Total Recoverable"|
              rpa$Sample_Fraction %in% "Total"),
              paste0(rpa$CASNumber,"T"),
              rpa$CASNumber)
     
     #free cyanide, have to add CAS # to match with RPA spreadsheet since AWQMS free cyanide parameter doesn't have CAS
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Cyanides amenable to chlorination (HCN & CN)"),
                           paste0("57125F"),
                           rpa$CASNumber)
     
     #inorganic arsenic, have to add CAS # to match with RPA spreadsheet since AWQMS inorganic arsenic parameter doesn't have CAS
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Arsenic, Inorganic"),
                           paste0("7440382I"),
                           rpa$CASNumber)
     
     #harndess, add 'Hardness' to CAS so RPA tool has something to work with
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Total hardness","Hardness, Ca, Mg",
                                                "Hardness, carbonate","Hardness, non-carbonate"),
                           paste0("HARDNESS"),
                           rpa$CASNumber)
     
     #combine Char_Name and Sample_Fraction for just metals
     rpa<-namefrac(rpa)
    
    #need to change data so that anything less than MDL is reported as ND 
    #(right now anything less than MRL is reported as <MRL, or <MDL if reported to detection limit)
     rpa$Result<-ifelse((!is.na(rpa$MDLValue)) & rpa$Result_Numeric<=rpa$MDLValue,"ND",rpa$Result)
     
     #change data that is between MDL and MRL to have < infront of result
     rpa$Result<-ifelse((!is.na(rpa$MDLValue)) & (!is.na(rpa$MRLValue)) 
                        & rpa$Result_Numeric<rpa$MRLValue & rpa$Result_Numeric>rpa$MDLValue,
                        paste0("<",rpa$Result),
                        rpa$Result)
     
     #remove estimated data if result is above MRL value (want to keep data between MRL and MDL, even though it is estimated)
     #however, don't want data that is biased low due to matrix issues, so change to where we keep "<" data (between MDL and MRL)
     #(need to do >MDLValue because if we do >= it will pull in all NDs since we put those into AWQMS as "<MDL")
     #only take certain rows, change order so that it is more in line with RPA
     rpa<-subset(rpa,rpa$Result_Type!="Estimated" | (rpa$Result_Numeric<=rpa$MRLValue & rpa$Result_Numeric>rpa$MDLValue),
                 select=c(CASNumber,Project1,act_id,act_id,StationDes,Activity_Type,Method_Code,Char_Name,
                              SampleMedia,SampleStartDate,Result,MRLValue,MDLValue,Result_Unit,Analytical_Lab,
                              Result_status,Result_Type,MLocID,MonLocType,Result_Comment))

     
     #need to remove dashes from CASNumber row
     rpa$CASNumber<-gsub("-","",rpa$CASNumber)
     }
     return(rpa)
     
   })
   
   #RPA summary, this is to help replace the summary stats page in the RPA spreadsheet
   RPAsum<-eventReactive(input$goButton, {
      if (nrow(rpa())!=0){
         RPAsum<-#rpa()%>%
            #1,2,4-Trichlorobenzene gets counted twice since it is run as both as part of the volatile and semivolatile suite...
            #for NPDES purposes, only the semi-volatile analyses count, so remove anything done by 624 or 624.1
            subset(rpa(),
                   subset= (!(Method_Code==c("624 (U.S. Environmental Protection Agency)") & Char_Name==c("1,2,4-Trichlorobenzene"))))%>%
            
            #directions on how to deal with NDs and < taken from RPA IMD Appendix C.
            #for calculating mean, ND=0, between DL and QL=DL
            mutate(Result_mean = case_when(Result=="ND" ~ 0,
                                           as.numeric(Result)>=MRLValue ~ as.numeric(Result),
                                           "<" %in% substr(Result,start=1,stop=1) & !is.na(MDLValue) ~ as.numeric(MDLValue),
                                           "<" %in% substr(Result,start=1,stop=1) & is.na(MDLValue) ~ as.numeric(MRLValue)
                                           )) %>%
            #for calculating geo mean, ND=1/2DL, but need to be careful for carcinogens- sometimes 1/2 DL is near the  WQ criterion, which can lead to RP when we don't actually know
            #create column that uses result for caclulating the geo mean
            mutate(Result_geomean=case_when(Result=="ND" & !is.na(MDLValue) ~ as.numeric(MDLValue)/2,
                                            Result=="ND" & is.na(MDLValue) ~ as.numeric(MRLValue)/2,
                                            as.numeric(Result)>=MRLValue ~ as.numeric(Result),
                                            "<" %in% substr(Result,start=1,stop=1) & !is.na(MDLValue) ~ as.numeric(MDLValue),
                                            "<" %in% substr(Result,start=1,stop=1) & is.na(MDLValue) ~ as.numeric(MRLValue)
            )) %>%
            group_by(Char_Name)%>%
   
            #do summary stats
            #note that geomean actually will have more logic associated with it for carcinogens...will need to incorporate that
            summarise(count_all = n(), count_result = sum(Result!="ND"), average = round(mean(Result_mean, na.rm = TRUE),2),
                      mn = mean(Result_mean, na.rm = TRUE),
                      stdev = sd(Result_mean, na.rm = T), max = as.character(max(Result_mean, na.rm = TRUE)), 
                      geomean = round(exp(mean(log(Result_geomean))),2)) %>% 
            #for CV, if number of observations is less than 10 then CV=0.6, else calculate the CV
            mutate(CV = ifelse(count_all>=10,round(stdev/mn,2),0.6))
         
         
         #add CAS#
         cas<-subset(rpa(),select=c(unique(Char_Name),unique(CASNumber)))
         RPAsum<-unique(left_join(RPAsum,cas, by="Char_Name"))
         
         #get into an order that can go right into the RPA spreadsheet
         RPAsum<-subset(RPAsum,select=c(Char_Name,count_result,count_all,max,geomean,average,CV,CASNumber))
   
        
      } 
      #need to add else statement or the RPAsum dataframe is not created at all and then the data download breaks
      else {RPAsum<-data.frame()}
      RPAsum
   })
   
   #table of queried data for Shiny app view  
   output$RPAsum<-renderDataTable({
      
      RPAsum()
   })
   #ammonia RPA output, similar to Copper BLM output
   amm<-eventReactive(input$goButton,{
     
     am<-amRPA(data())
     
     am
   })
   
   #pH RPA output, creates a list of data frames, one for each site
   pHrpa<-eventReactive(input$goButton,{
   
     #subset data so we only get the parameters we need 
     dat2<-subset(data(),Char_Name %in% c('Temperature, water','Alkalinity, total','pH','Salinity'))
     
     #remove any samples that are calculated from continuous data (eg. 7 day min)
     dat2<-subset(dat2, is.na(dat2$Statistical_Base))
   
     #remove "dissolved alkalinity" samples- were only done in a few projects and are not the way we usually do alkalinity
     dat2<-subset(dat2,!(dat2$Char_Name=="Alkalinity, total" & dat2$Sample_Fraction=="Dissolved" & dat2$OrganizationID=='OREGONDEQ'))
     
     #have occasional issues with 2 pH values (one field, one lab) for ORDEQ data
     #remove Sample-Routine pH if ORDEQ data
     dat2<-subset(dat2,!(dat2$Char_Name=='pH' & dat2$OrganizationID=='OREGONDEQ' & dat2$Activity_Type=='Sample-Routine'))
     
   #temp, alk, pH, salinity, conversions
   #salinity in ppt and ppth--all is parts per thousand 
   #alkalinity is all in mg/l
   #has no units, and all data in weird units in AWQMS doesn't need any conversion
   dat2<-unit_conv(dat2,'Temperature, water','deg F', 'deg C')
   
   #only need select set of columns
   dat2<-subset(dat2,select=c(MLocID,Char_Name,Result,act_id))
   
   #remove "-FM" from end of activity id, so alkalinity doesn't end up in its own row with no field parameters from the same activity
   #applies to some ORDEQ data
   dat2$act_id<-gsub("-FM$","",dat2$act_id)
   
   #split data by monitoring location (creates list of dataframes) and convert data from long to wide format
   dat3<-split(dat2,dat2$MLocID)
   dat3<-lapply(dat3,function(x) spread(x,key=Char_Name,value=Result))
   
   dat3
   
   })

   #create list of the parameters in query, get it into a formatted excel to export so we have record of query
   #add sheet for search criteria,map data, and conditionally RPA data, Copper BLM, and Ammonia RPA if data is available
   param<-eventReactive(input$goButton, {
     
     #create strings for the input parameters
     startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
     enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
     rejected<-paste0("Is rejected data included?  ",if(input$Reject) {TRUE} else {FALSE})
     stations<- paste0("Stations = ",toString(sprintf("'%s'", input$monlocs)))
     monty<- paste0("Monitoring Location Types = ",toString(sprintf("'%s'", input$montype)))
     charc<- paste0("RPA Group = ",toString(sprintf("'%s'", input$characteristics)))
     onof<- paste0("Characteristics = ",toString(sprintf("'%s'", input$oneoff)))
     huc8s<-paste0("HUC8 = ",toString(sprintf("'%s'", input$huc8_nms)))
     auids<-paste0("Assessment Unit = ",toString(sprintf("'%s'",input$AUID)))
     organiz<- paste0("Organization = ",toString(sprintf("'%s'", input$orgs)))
     allchar<- paste0("List of all potential RPA characteristics (All Toxics includes Pesticides/PCB RPA, Base Neutral RPA, Acid Extractable RPA, VOC RPA, and Metals RPA) \n",
                      "pH RPA: ",toString(phrpa), "\n\n",
                      "Ammonia RPA: ",toString(ammrpa),"\n\n",
                      "Copper BLM: ",toString(cuB),"\n\n",
                      "Do RPA: ",toString(dorpa),"\n\n",
                      "Pesticide and PCBs RPA: ",toString(pestrpa),"\n\n",
                      "Base Neutral RPA: ",toString(bneut),"\n\n",
                      "Acid Exractable RPA: ",toString(aext),"\n\n",
                      "Volatile Organic Carbon RPA: ",toString(vocrpa), "\n\n",
                      "Metals and Hardness RPA: ",toString(metalsrpa), "\n\n",
                      "Industrial General Parameters: ",toString(ingen))
     
     #add continuous data availability warning
     warn<-unique(if(any(!is.na(orig()$Time_Basis))) {paste("Continous data may be available upon request")})
     
     #create workbook and sheet
     wb<-createWorkbook()
       
     #Search Criteria
     addWorksheet(wb,"Search Criteria")
       
       #Create title styles
       mainTitle<-createStyle(fontSize=16,fontColour="blue",textDecoration=c("bold","underline"))
       subTitle<-createStyle(fontSize=14,textDecoration="italic")
       wrap<-createStyle(wrapText=TRUE)
       #create bold style
       bold<-createStyle(textDecoration="bold")
       #create rotated text styel
       rotate<-createStyle(textRotation = 45)
       
       # Add title
       title<-"RPA Data Search Criteria"
       #add title to sheet
       addStyle(wb,sheet="Search Criteria",style=mainTitle,rows=1,cols=1)
       writeData(wb,sheet="Search Criteria",x=title,startRow=1,startCol=1)
       
       #add subtitles
       addStyle(wb,sheet="Search Criteria",style=subTitle,rows=2:4,cols=1)
       writeData(wb,sheet="Search Criteria",x=paste0(input$permittee),startRow=2,startCol=1)
       
       permit<-paste0("Permit # ",input$permit_num)
       writeData(wb,sheet="Search Criteria",x=permit,startRow=3,startCol=1)
       
       querydate<-paste0("Date of query, ",Sys.Date())
       writeData(wb,sheet="Search Criteria",x=querydate,startRow=4,startCol=1)
       
       #add sub title for continuous data warning
       if(length(warn)>0) {writeData(wb,sheet="Search Criteria",x=warn,startRow=5,startCol=1)}
       
       #populate rows with parameters
       writeData(wb,sheet="Search Criteria",x=startdt,startCol=1,startRow=7)
       writeData(wb,sheet="Search Criteria",x=enddt,startCol=1,startRow=8)
       writeData(wb,sheet="Search Criteria",x=stations,startCol=1,startRow=9)
       writeData(wb,sheet="Search Criteria",x=monty,startCol=1,startRow=10)
       writeData(wb,sheet="Search Criteria",x=charc,startCol=1,startRow=11)
       writeData(wb,sheet="Search Criteria",x=onof,startCol=1,startRow=12)
       writeData(wb,sheet="Search Criteria",x=huc8s,startCol=1,startRow=13)
       writeData(wb,sheet="Search Criteria",x=auids,startCol=1,startRow=14)
       writeData(wb,sheet="Search Criteria",x=organiz,startCol=1,startRow=15)
       writeData(wb,sheet="Search Criteria",x=rejected,startCol=1,startRow=16)
       writeData(wb,sheet="Search Criteria",x=allchar,startCol=1,startRow=18)
       
       addStyle(wb,sheet="Search Criteria",style=wrap,rows=18,cols=1)
       setColWidths(wb,sheet="Search Criteria", cols=1, widths=220)
       
       #Map
       #conditional on whether map button is checked
       if (input$NoMap==TRUE) 
       
       {addWorksheet(wb,"Map") 
        
       #create map with limited labels
       map<-leaflet(data()) %>%
         addTiles()%>%
         addMarkers(lng=~Long_DD,
                    lat=~Lat_DD,
                    label=~MLocID,
                    labelOptions=labelOptions(noHide=T))
       
       #save as jpeg file (pdf isn't able to be displayed, png has much larger file size)
       mapshot(map,file="map.jpeg")
       
       insertImage(wb,"Map","map.jpeg",width=10,height=7)
       }
       
       #Diagnostics sheet, for totals and other stats we want to calculate
       addWorksheet(wb,"Diagnostics")
       #create counts of each parameter (combine metals name and fraction first)
            cnt<-namefrac(dsub())
            counter<-count(cnt,Char_Name,name="Totals")
            #add counts of actual and estimated and calculated
            actestcnt<-count(cnt,Char_Name,Result_Type,name="Totals")
            #take actual and estimated and calculated and join to counter, based on Char_Name
            
            counter<-left_join(counter,subset(actestcnt,Result_Type=='Estimated'),by="Char_Name")
            counter<-left_join(counter,subset(actestcnt,Result_Type=='Actual'),by="Char_Name")
            counter<-left_join(counter,subset(actestcnt,Result_Type=='Calculated'),by="Char_Name")
            
            #take columns that we need
            tots<-subset(counter,select=c(Char_Name,Totals.x,Totals.y,Totals.x.x,Totals.y.y))
            
            #rename columns to make them more understandable
            names(tots)<-c("Pollutant","Total_Count","Estimated_Result_Count","Actual_Result_Count","Calculated_Result_Count")
            #replace all NAs with 0
            tots[is.na(tots)]<-0
            
            
            
            writeDataTable(wb,"Diagnostics",x=tots,tableStyle="none",startRow=3,startCol=1)
            writeData(wb,"Diagnostics",startRow=1,startCol=1,x="Estimated Result Count column contains ALL estimated results, including those that are between MDL and MRL (QL and DL)")
            setColWidths(wb,"Diagnostics",cols=1:5,widths=20)

  #Data worksheets
       # All data      
       addWorksheet(wb,"Data")
            writeDataTable(wb,"Data",x=dsub(),tableStyle="none")
        
       #RPA          
       if (nrow(rpa())!=0) {addWorksheet(wb,"Toxics_Data_Format")
                           writeDataTable(wb,"Toxics_Data_Format",startRow=4,x=rpa(),tableStyle="none")
                           writeData(wb,"Toxics_Data_Format",startRow=1,x="Only copy columns with highlighted column headers into RPA workbook")
                        
                           writeData(wb,"Toxics_Data_Format",startRow=2,x="Examine 'Result_Comment' and 'Result_Type' columns to determine data usability")
                           addStyle(wb,"Toxics_Data_Format",style=bold,rows=2,cols=1:15)
                           
                           #create shading style
                           shaderot<-createStyle(fgFill="yellow2",textRotation = 45)
                           addStyle(wb,"Toxics_Data_Format",style=shaderot,cols=1:15,rows=4)
                                                          }
       
       #RPA summary stats
       if (nrow(RPAsum())!=0) {addWorksheet(wb,"Toxics_SummaryStats")
               writeDataTable(wb,"Toxics_SummaryStats",startRow=2,x=RPAsum(),tableStyle="none")
            }
       #Copper BLM                    
       if (nrow(copper())!=0) {addWorksheet(wb,"CuBLM_Data_Format")
                              writeData(wb,"CuBLM_Data_Format",startRow=1,x="Copper BLM data. Examine MLocID for sample location. Examine Result Type columns for data quality")
                              writeData(wb,"CuBLM_Data_Format",startRow=2,x="data has already been converted into proper units for Cu BLM analysis (ug/L for Copper, mg/L for all other concentrations, and degrees C for temperature). No unit conversion necessary")
                              writeData(wb,"CuBLM_Data_Format",startRow=3,x="Note that if both dissolved and total recoverable copper were collected, there will be two rows for each sampling event.")
                              writeData(wb,"CuBLM_Data_Format",startRow=4,x="For Calcium, Magnesium, Potassium, and Sodium: The worksheet selected the dissolved fraction of the analyte if available. If the dissolved fraction was not available, then the total recoverable fraciton is used below. Check 'Data' worksheet for analyte fraction.")
                                        
                              #remove date column, overkill
                              copper<-within(copper(),rm("date"))
                              
                              #fix names to remove spaces and commas
                              names(copper)<-str_replace_all(names(copper), c(" " = "." , "," = "" ))
                              
                              
                              #make sure all columns are there for each parameter (add as NA if there is no data)
                              copper<-if(!("Temperature.water" %in% colnames(copper))){add_column(copper, "Temperature.water"=NA)} else {copper}
                              copper<-if(!("pH" %in% colnames(copper))){add_column(copper, "pH"=NA)} else {copper}
                              copper<-if(!("Alkalinity.total" %in% colnames(copper))){add_column(copper, "Alkalinity.total"=NA)} else {copper}
                              copper<-if(!("CalciumDissolved" %in% colnames(copper))){add_column(copper, "CalciumDissolved"=NA)} else {copper}
                              copper<-if(!("Chloride" %in% colnames(copper))){add_column(copper, "Chloride"=NA)} else {copper}
                              copper<-if(!("MagnesiumDissolved" %in% colnames(copper))){add_column(copper, "MagnesiumDissolved"=NA)} else {copper}
                              copper<-if(!("Organic.carbonDissolved" %in% colnames(copper))){add_column(copper, "Organic.carbonDissolved"=NA)} else {copper}
                              copper<-if(!("PotassiumDissolved" %in% colnames(copper))){add_column(copper, "PotassiumDissolved"=NA)} else {copper}
                              copper<-if(!("SodiumDissolved" %in% colnames(copper))){add_column(copper, "SodiumDissolved"=NA)} else {copper}
                              copper<-if(!("Sulfate" %in% colnames(copper))){add_column(copper, "Sulfate"=NA)} else {copper}
                              copper<-if(!("Sulfide" %in% colnames(copper))){add_column(copper, "Sulfide"=NA)} else {copper}
                              #note that there is no humic acid paramter in AWQMS, nor is it asked for from the permittees, adding the column
                              #as a placeholder since the HA column is in the permitting Copper BLM calculation tool
                              copper<-if(!("Humic Acid" %in% colnames(copper))){add_column(copper, "Humic Acid"=NA)} else {copper}
                              
                              #Same for result type
                              copper<-if(!("Temperature.water.Result_Type" %in% colnames(copper))){add_column(copper, "Temperature.water.Result_Type"=NA)} else {copper}
                              copper<-if(!("pH.Result_Type" %in% colnames(copper))){add_column(copper, "pH.Result_Type"=NA)} else {copper}
                              copper<-if(!("Alkalinity.total.Result_Type" %in% colnames(copper))){add_column(copper, "Alkalinity.total.Result_Type"=NA)} else {copper}
                              copper<-if(!("CalciumDissolved.Result_Type" %in% colnames(copper))){add_column(copper, "CalciumDissolved.Result_Type"=NA)} else {copper}
                              copper<-if(!("Chloride.Result_Type" %in% colnames(copper))){add_column(copper, "Chloride.Result_Type"=NA)} else {copper}
                              copper<-if(!("MagnesiumDissolved.Result_Type" %in% colnames(copper))){add_column(copper, "MagnesiumDissolved.Result_Type"=NA)} else {copper}
                              copper<-if(!("Organic.carbonDissolved.Result_Type" %in% colnames(copper))){add_column(copper, "Organic.carbonDissolved.Result_Type"=NA)} else {copper}
                              copper<-if(!("PotassiumDissolved.Result_Type" %in% colnames(copper))){add_column(copper, "PotassiumDissolved.Result_Type"=NA)} else {copper}
                              copper<-if(!("SodiumDissolved.Result_Type" %in% colnames(copper))){add_column(copper, "SodiumDissolved.Result_Type"=NA)} else {copper}
                              copper<-if(!("Sulfate.Result_Type" %in% colnames(copper))){add_column(copper, "Sulfate.Result_Type"=NA)} else {copper}
                              copper<-if(!("Sulfide.Result_Type" %in% colnames(copper))){add_column(copper, "Sulfide.Result_Type"=NA)} else {copper}
                              #note that there is no humic acid paramter in AWQMS, nor is it asked for from the permittees, adding the column
                              #as a placeholder since the HA column is in the permitting Copper BLM calculation tool
                              copper<-if(!("Humic Acid Result_Type" %in% colnames(copper))){add_column(copper, "Humic Acid Result_Type"=NA)} else {copper}
                              
                              #want to select dissolved metal if available, substitute total recoverable if that was all that was analyzed
                              copper$Magnesium<-ifelse(!(is.na(copper$MagnesiumDissolved)),
                                                       copper$MagnesiumDissolved,
                                                       ifelse(!(is.na(copper$MagnesiumTotal.Recoverable)),
                                                          copper$MagnesiumTotal.Recoverable,
                                                          NA))
                              copper$Magnesium.Result_Type<-ifelse(!(is.na(copper$MagnesiumDissolved.Result_Type)),
                                                                   copper$MagnesiumDissolved.Result_Type,
                                                                   ifelse(!(is.na(copper$MagnesiumTotal.Recoverable.Result_Type)),
                                                                          copper$MagnesiumTotal.Recoverable.Result_Type,
                                                                          NA))
                              
                              copper$Calcium<-ifelse(!(is.na(copper$CalciumDissolved)),
                                                       copper$CalciumDissolved,
                                                       ifelse(!(is.na(copper$CalciumTotal.Recoverable)),
                                                              copper$CalciumTotal.Recoverable,
                                                              NA))
                              copper$Calcium.Result_Type<-ifelse(!(is.na(copper$CalciumDissolved.Result_Type)),
                                                                   copper$CalciumDissolved.Result_Type,
                                                                   ifelse(!(is.na(copper$CalciumTotal.Recoverable.Result_Type)),
                                                                          copper$CalciumTotal.Recoverable.Result_Type,
                                                                          NA))
                              copper$Sodium<-ifelse(!(is.na(copper$SodiumDissolved)),
                                                       copper$SodiumDissolved,
                                                       ifelse(!(is.na(copper$SodiumTotal.Recoverable)),
                                                              copper$SodiumTotal.Recoverable,
                                                              NA))
                              copper$Sodium.Result_Type<-ifelse(!(is.na(copper$MagnesiumDissolved.Result_Type)),
                                                                   copper$MagnesiumDissolved.Result_Type,
                                                                   ifelse(!(is.na(copper$MagnesiumTotal.Recoverable.Result_Type)),
                                                                          copper$MagnesiumTotal.Recoverable.Result_Type,
                                                                          NA))
                              copper$Potassium<-ifelse(!(is.na(copper$PotassiumDissolved)),
                                                       copper$PotassiumDissolved,
                                                       ifelse(!(is.na(copper$PotassiumTotal.Recoverable)),
                                                              copper$PotassiumTotal.Recoverable,
                                                              NA))
                              copper$Potassium.Result_Type<-ifelse(!(is.na(copper$PotassiumDissolved.Result_Type)),
                                                                   copper$PotassiumDissolved.Result_Type,
                                                                   ifelse(!(is.na(copper$PotassiumTotal.Recoverable.Result_Type)),
                                                                          copper$PotassiumTotal.Recoverable.Result_Type,
                                                                          NA))
                                                          
                              
                              #reorder columns for easy copy/paste into Cu BLM tool
                              copper<-subset(copper,select=c("OrganizationID","Project1","MLocID","SampleStartDate","SampleStartTime","Char_Name","Result",
                                                "MDLValue","MRLValue","Result_Type","Temperature.water","pH","Organic.carbonDissolved","Humic Acid",
                                                "Calcium","Magnesium","Sodium","Potassium","Sulfate","Chloride",
                                                "Alkalinity.total","Sulfide","Temperature.water.Result_Type","pH.Result_Type","Organic.carbonDissolved.Result_Type",
                                                "Humic Acid Result_Type","Calcium.Result_Type","Magnesium.Result_Type",
                                                "Sodium.Result_Type","Potassium.Result_Type","Sulfate.Result_Type","Chloride.Result_Type",
                                                "Alkalinity.total.Result_Type","Sulfide.Result_Type"))
                              
                              #need to reformat the columns so that they come through as numbers
                              
                              
                              writeDataTable(wb,"CuBLM_Data_Format",startRow=6,x=copper,tableStyle="none")
                              
                              }
       #Ammonia RPA                     
       if (nrow(amm())!=0) {addWorksheet(wb,"Ammonia_RPA_Format")
                           writeDataTable(wb,"Ammonia_RPA_Format",x=amm(),tableStyle="none")}
       
        #pH RPA
        if (length(pHrpa())!=0) {addWorksheet(wb,"pH_RPA")
            #add counter to increment
            num<-1
            
            #remove MLocID column, overkill, keep act_id column, want something to tie back to original data
            pHrpa<-lapply(pHrpa(),function(x) x[,-c(1)])
            
            #for loop to write data all on one spreadsheet
            for (i in 1:length(pHrpa())) {
              
              #add header that is monlocID
              writeData(wb,"pH_RPA",x=names(pHrpa[i]),startRow=1,startCol=num+2)
              addStyle(wb,"pH_RPA",style=bold,rows=1,cols=1:1000)
              
              #make sure all columns are there for each parameter (add as NA if there is no data)
              pHrpa[[i]]<-if(!("Temperature, water" %in% colnames(pHrpa[[i]]))){add_column(pHrpa[[i]], "Temperature, water"=NA)} else {pHrpa[[i]]}
              pHrpa[[i]]<-if(!("pH" %in% colnames(pHrpa[[i]]))){add_column(pHrpa[[i]], "pH"=NA)} else {pHrpa[[i]]}
              pHrpa[[i]]<-if(!("Alkalinity, total" %in% colnames(pHrpa[[i]]))){add_column(pHrpa[[i]], "Alkalinity, total"=NA)} else {pHrpa[[i]]}
              pHrpa[[i]]<-if(!("Salinity" %in% colnames(pHrpa[[i]]))){add_column(pHrpa[[i]], "Salinity"=NA)} else {pHrpa[[i]]}
              
              #reorder columns
              pHrpa[[i]]<-pHrpa[[i]][,c("act_id","pH","Temperature, water","Salinity","Alkalinity, total")]
              
              #add data
              writeData(wb,"pH_RPA",x=pHrpa[[i]],startRow=3,startCol=num,colNames=TRUE)
              #add 7 to counter so that next station doesn't overwrite the previous
              num<-num+7
            }
        }   
     wb
   })
   
   

# Download button- only works in Chrome
# gives an excel workbook with multiple sheets
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste(input$permit_num,"_AWQMS_Download-", Sys.Date(),".xlsx", sep="")},
  content = function(file) {
    #sheet with query parameters
    saveWorkbook(param(),file)
    })

}
# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
