#
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
# This app, based on a modified verison of the AWQMSdata_ShinyHelp app built by Travis Pritchard, 
##is designed to pull out RPA related data from AWQMS and put it into a table for use in RPA analysis

library(shiny)
library(AWQMSdata)
library(leaflet)
library(plyr,include.only = "rbind.fill")
library(mapview)
library(leaflet.extras)
#library(mapedit)
#library(sf)
library(shinybusy)
library(openxlsx)
library(tidyverse)
library(DT)
library(stringi)
library(odbc)
library(DBI)
library(webshot)
webshot::install_phantomjs()


#Run this if you need to update the AWQMSdata package
#devtools::install_github("TravisPritchardODEQ/AWQMSdata")

#attempt to turn off scientific notation
options(scipen=999)

#function to combine characteristic name and sample fraction for metals
source("NameandFraction.R")
#function to calculate hardness from Ca and Mg, or conductivity
source("CalcHardness_Function.R")
#function to transform Aluminum BLM data (will likely be part of AWQMSdata in future, but not yet)
source("AlBLM_Transform.R")
#function to calculate summary stats from continuous data
source("Continuous_Summary_Function.R")
#function that changed all of the AWQMSdata functions into ones that might work with RConnect
source("noenvAWQMSFunctions.R")

#### Define Inputs for UI#####

print("Now loading query cache. Note: may take 15 min or more if cache needs to refresh")

#have separate caches for variables so that they don't have to all refresh at the same rate

# Check to see if saved cache of data exists. If it does not, or is greater than
# 14 days old, query out stations and organizations and save the cache
if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 14){
  

#get station info
station <- noenvAWQMS_Stations(HUC8_Name=c('Alsea', 'Alvord Lake', 'Applegate', 'Beaver-South Fork', 'Brownlee Reservoir', 'Bully', 'Burnt', 'Chetco',
                          'Chief Joseph','Clackamas', 'Coast Fork Willamette', 'Coos','Coquille', 'Crooked-Rattlesnake',  
                          'Donner und Blitzen',' Goose Lake', 'Guano', 'Harney-Malheur Lakes', 'Illinois', 'Imnaha', 'Jordan',
                          'Lake Abert', 'Little Deschutes','Lost', 'Lower Columbia', 'Lower Columbia-Clatskanie', 'Lower Columbia-Sandy',
                          'Lower Crooked','Lower Deschutes', 'Lower Grande Ronde', 'Lower John Day', 'Lower Malheur', 'Lower Owyhee',
                          'Lower Rogue', 'Lower Willamette', 'Mckenzie', 'Middle Columbia-Hood', 'Middle Columbia-Lake Wallula', 
                          'Middle Fork John Day', 'Middle Fork Willamette', 'Middle Owyhee','Middle Rogue', 'Middle Snake-Payette', 
                          'Middle Snake-Succor', 'Middle Willamette', 'Molalla-Pudding','Necanicum', 'Nehalem', 'North Fork John Day', 
                          'North Santiam', 'North Umpqua', 'Not Loaded', 'Powder','Siletz-Yaquina', 'Siltcoos', 'Silver', 'Silvies', 
                          'Siuslaw', 'Sixes', 'Smith', 'South Fork Owyhee', 'South Santiam','South Umpqua', 'Sprague', 'Summer Lake', 
                          'Trout', 'Tualatin', 'Umatilla', 'Umpqua', 'Upper Columbia-Entiat','Upper Columbia-Priest Rapids', 
                          'Upper Crooked', 'Upper Deschutes', 'Upper Grande Ronde,Upper John Day','Upper Klamath', 'Upper Klamath Lake', 
                          'Upper Malheur', 'Upper Quinn', 'Upper Rogue', 'Upper Willamette','Walla Walla', 'Wallowa', 'Warner Lakes', 
                          'Williamson', 'Willow', 'Wilson-Trusk-Nestuccu', 'Yamhill'))
Mtype<-station$MonLocType
auid<-station$AU_ID
auid<-base::sort(station$AU_ID)
station <- station$MLocID
station <- base::sort(station)

organization <- noenvAWQMS_Orgs()
organization <- organization$OrganizationID
organization <- base::sort(organization)

#save query information in a file. Don't have to redo pulls each time. Saves a lot of time. 
save(station, Mtype, auid, organization, file = 'query_cache.RData')
} else {
  load("query_cache.RData")
}

# Check to see if saved cache of data exists. If it does not, or is greater than
# 6 months old, query out all characteristics and save the cache
if(!file.exists("query_cache_allchar.RData") | 
  difftime(Sys.Date() ,file.mtime("query_cache_allchar.RData") , units = c("days")) > 182){
  
 allchar<-noenvAWQMS_Chars()
  
  #save query information in a file. Don't have to redo pulls each time. Saves a lot of time. 
  save(allchar, file = 'query_cache_allchar.RData')
} else {
  load("query_cache_allchar.RData")
}

#get characteristics into format that is easier to use by app
list<-split(allchar,seq(nrow(allchar)))
oneoff<-unlist(list,use.names=FALSE)


########################################### Define UI ########################################################
ui <- fluidPage(

   # Sidebar with parameter inputs

  sidebarLayout(
      sidebarPanel(
        
        tags$em("Permittee Identifying Information"),
        tags$br(),
        
        #permittee name
        textInput("permittee",
                  label="Permittee Name"),
        #permit #
        textInput("permit_num",
                  label="Permit Number"),
        # Add line
        tags$hr(style="border-color: black;"),
        #Add break
        tags$br(),
        
        tags$em("Data Pull Parameters"),
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
                     choices = c("All RPA","Toxics","Copper and Aluminum BLM","pH and Ammonia RPA","DO RPA","All AWQMS Characteristics","None"),
                     multiple = FALSE,
                     selected="All RPA"),
      
         #specific parameters outside of groups, choices is NULL so that server-side selectize can be used to improve performance 
         selectizeInput("oneoff",
                        "Specific Characteristics not part of groupings",
                        choices=NULL,
                        multiple=TRUE),

       # Monitoring locations, choices is NULL so that server-side selectize can be used to improve performance 
        selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = NULL,
                        multiple = TRUE),
       
       # Monitoring location types, choices is NULL so that server-side selectize can be used to improve performance 
        selectizeInput("montype",
                       "Select Monitoring Location Types",
                       choices=NULL,
                       multiple=TRUE),
       
       #add warning
       tags$em("Warning: HUC8 may not include all stations on coast or ocean"),
       
       # huc8 names 
       selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = c('Alsea', 'Alvord Lake', 'Applegate', 'Beaver-South Fork', 'Brownlee Reservoir', 'Bully', 'Burnt', 'Chetco',
                                   'Chief Joseph','Clackamas', 'Coast Fork Willamette', 'Coos','Coquille', 'Crooked-Rattlesnake',  
                                   'Donner und Blitzen',' Goose Lake', 'Guano', 'Harney-Malheur Lakes', 'Illinois', 'Imnaha', 'Jordan',
                                   'Lake Abert', 'Little Deschutes','Lost', 'Lower Columbia', 'Lower Columbia-Clatskanie', 'Lower Columbia-Sandy',
                                   'Lower Crooked','Lower Deschutes', 'Lower Grande Ronde', 'Lower John Day', 'Lower Malheur', 'Lower Owyhee',
                                   'Lower Rogue', 'Lower Willamette', 'Mckenzie', 'Middle Columbia-Hood', 'Middle Columbia-Lake Wallula', 
                                   'Middle Fork John Day', 'Middle Fork Willamette', 'Middle Owyhee','Middle Rogue', 'Middle Snake-Payette', 
                                   'Middle Snake-Succor', 'Middle Willamette', 'Molalla-Pudding','Necanicum', 'Nehalem', 'North Fork John Day', 
                                   'North Santiam', 'North Umpqua', 'Not Loaded', 'Powder','Siletz-Yaquina', 'Siltcoos', 'Silver', 'Silvies', 
                                   'Siuslaw', 'Sixes', 'Smith', 'South Fork Owyhee', 'South Santiam','South Umpqua', 'Sprague', 'Summer Lake', 
                                   'Trout', 'Tualatin', 'Umatilla', 'Umpqua', 'Upper Columbia-Entiat','Upper Columbia-Priest Rapids', 
                                   'Upper Crooked', 'Upper Deschutes', 'Upper Grande Ronde,Upper John Day','Upper Klamath', 'Upper Klamath Lake', 
                                   'Upper Malheur', 'Upper Quinn', 'Upper Rogue', 'Upper Willamette','Walla Walla', 'Wallowa', 'Warner Lakes', 
                                   'Williamson', 'Willow', 'Wilson-Trusk-Nestuccu', 'Yamhill'),
                       multiple = TRUE),
       
       #AU_IDs, choices is NULL so that server-side selectize can be used to improve performance 
       selectizeInput("AUID",
                      "Select Assessment Unit",
                      choices = NULL,
                      multiple = TRUE),
    
       #Orgs, choices is NULL so that server-side selectize can be used to improve performance
       selectizeInput("orgs",
                       "Select organization",
                       choices = NULL,
                       multiple = TRUE),
       
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
                 mainPanel(h4("This application helps NPDES permit writers view and download data from AWQMS for NPDES permitting purposes in a format
                               that can be used directly with current RPA workbook tools"),
                           tags$br(),
                           h4("How to use:"),
                           h5("Select parameters on left to build table and map for desired geographic area/organization/timeframe"),
                           h5("Note that the user should fill in 'Permittee Name' and 'Permit Number', but that these items do not refine the search.
                              These boxes carry over into the data download and file naming only."),
                           h5("Click 'Run Query' Button to perform search after selecting desired parameters."),
                           h5("Peruse the 'Grab Data', 'Continuous Data', and 'Map' tabs to view results and locations"),
                           h5("Click 'Download Data' to download results"),
                           h5("Note: 'All RPA' also includes Chlorine data when available"),
                           tags$br(),
                           h4("Helpful Tips:"),
                           h5("Effluent Priority Pollutant Scan and Copper/Aluminum BLM Data is entered into AWQMS under the name of the organization that collected the data. 
                              To find this data select the appropriate time range for 'Select Start Date' and 'Select End Date' and the correct organization under 
                              'Select Organization'. Note that this data pull should include both the effluent and ambient Copper/Aluminum BLM data"),
                           h5("To find relevant ambient data for a permittee, use the Integrated Report Map to find the Assessment Unit a permittee discharges into and the next 
                              Assessment Unit upstream. Include both of these in the 'Select Assessment Unit' box. Set the 'Select Start Date' to 10 years ago. 
                              Note that data older than 10 years is generally not used for NPDES permit renewal or issuance purposes."),
                           h5("The data pull will not include any quality control data (e.g. duplicates, blanks)"),
                           tags$br(),
                           h5("Warning: after running the query, if you change your mind about whether to include the map, 
                              you must select the box to add the map (underneath the 'run query' button) and then re run the query to ensure that the map will be 
                              part of the download"),
                           tags$br(),
                           h5("Warning: running query with all characteristics and a large timeframe (1+ year) can overload the server. Be sure to constrain
                              your search either with a shorter timeframe (~6 months), by specific organization, or by geographic region (HUC8 or Assessment Unit)")
                           )
                 ),
        
        #Data table
        tabPanel("Grab Data",
                 DT::dataTableOutput("table")),
        #Continuous Data table
        tabPanel("Continuous Data",
                 DT::dataTableOutput("continuous")),
        #add leaflet map
        tabPanel("Map",leafletOutput("locs")),
        #check diagnositcs
        tabPanel("RPA Summary",
                 DT::dataTableOutput("RPAsum"))
        )
   )
),

#add icon to show when program is running query or download
add_busy_spinner(spin = "fading-circle"))

###############################################  SERVER    ###########################################################

# Define server logic required to display query
server <- function(input, output, session) {
  
   #to help increase app performance, need to use server-side selectize for monlocs, montype, and AUID
   
   updateSelectizeInput(session, 'monlocs', choices = station, server = TRUE)
   updateSelectizeInput(session, 'montype', choices = Mtype, server = TRUE)
   updateSelectizeInput(session, 'AUID', choices = auid, server = TRUE)
   updateSelectizeInput(session, 'oneoff', choices= oneoff, server=TRUE)
   updateSelectizeInput(session, 'orgs', choices= organization, server=TRUE)
   
   #NPDES only needs a limited # of Chars, this should help speed up the program.
   #create variables with specific characteristics for the different analyte/RPA groups 
   #(note, keeping various groupings for metals, VOCs, base neutrals because they can be easier to update, but will all be 
   #pulled under "Toxics")
   
   #pH and Ammonia RPA (almost the same, ammonia just has the ammonia chars)
   phammrpa<-c("Alkalinity, total","pH","Temperature, water","Salinity","Conductivity","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")
   
   #Copper and Aluminum BLM
   cuB<-c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
          "Temperature, water","Total Sulfate","Sulfide","Conductivity","Specific conductance", "Aluminum","Total hardness","Hardness, Ca, Mg")
   
   #Dissolved Oxygen RPA
   dorpa<-c("Dissolved oxygen (DO)","Dissolved oxygen saturation","Biochemical oxygen demand, non-standard conditions",
            "Biochemical oxygen demand, standard conditions","Kjeldahl nitrogen","Total Kjeldahl nitrogen","Temperature, water",
            "Ammonia ","Ammonia and ammonium","Ammonia-nitrogen")  
   
   #Pesticides and PCBs
   pestrpa<-c("p,p'-DDT","Parathion","Chlordane","Lindane","Dieldrin","Endrin","Methoxychlor","p,p'-DDD","p,p'-DDE","Heptachlor",
              "Azinphos-methyl","Malathion","Aldrin",".alpha.-Hexachlorocyclohexane",".beta.-Hexachlorocyclohexane",
              "Benzene Hexachloride, Beta (BHC)","1,2,3,4,5,6-Hexachlorocyclohexane",".alpha.-Endosulfan","Heptachlor epoxide",
              "Endosulfan sulfate","Mirex","Chlorpyrifos","Endrin aldehyde","Toxaphene","Demeton","Aroclor 1260","Aroclor 1254",
              "Aroclor 1221","Aroclor 1232","Aroclor 1248","Aroclor 1016",".beta.-Endosulfan","Aroclor 1242","Total PCBs",
              "2,3,7,8-Tetrachlorodibenzo-p-dioxin")
   
   #Base Neutral
   bneut<-c("Benzo[a]pyrene","Dibenz[a,h]anthracene","Benz[a]anthracene","N-Nitrosodimethylamine","Hexachloroethane",
            "Hexachlorocyclopentadiene","Isophorone","Acenaphthene","Diethyl phthalate","Dibutyl phthalate","Phenanthrene",
            "Butyl benzyl phthalate","N-Nitrosodiphenylamine","Fluorene","Hexachlorobutadiene","Naphthalene","2-Chloronaphthalene",
            "3,3'-Dichlorobenzidine","Benzidine","1,2,4,5-Tetrachlorobenzene","Nitrobenzene","BDE-003",
            "Bis(2-chloro-1-methylethyl) ether","Bis(2-chloroethyl) ether","Bis(2-chloroethoxy)methane","Di(2-ethylhexyl) phthalate",
            "Di-n-octyl phthalate","Hexachlorobenzene","Anthracene","1,2,4-Trichlorobenzene","2,4-Dinitrotoluene","1,2-Diphenylhydrazine",
            "Pyrene","Dimethyl phthalate","Benzo[ghi]perylene","Indeno[1,2,3-cd]pyrene","Benzo(b)fluoranthene","Fluoranthene",
            "Benzo[k]fluoranthene","Acenaphthylene","Chrysene","2,6-Dinitrotoluene","Pentachlorobenzene","N-Nitrosodi-n-propylamine",
            "p-Chlorophenyl phenyl ether","Azobenzene","Bis(2-chloroisopropyl) ether")
   
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
             "m-Dichlorobenzene","1,3-Dichloropropene","Acrylonitrile","trans-1,3-Dichloropropene","cis-1,3-Dichloropropene")
   
   #Metals and Hardness
   metalsrpa<-c("Cyanide","Cyanides amenable to chlorination (HCN & CN)","Cyanide, free","Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
                "Beryllium","Cadmium","Chromium","Copper","Zinc","Selenium","Nitrate","Inorganic nitrogen (nitrate and nitrite)",
                "Nitrate + Nitrite","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Total hardness","Hardness, Ca, Mg",
                "Hardness, carbonate","Hardness, non-carbonate","Methylmercury(1+)")
   
   #all toxics (metals, voc, acid extractable, base neutral, pesticides and PCBs) - and "other parameters with state WQ crit" 
   tox<-c(metalsrpa,vocrpa,aext,bneut,pestrpa, "N-Nitrosodiethylamine","Phosphorus","Asbestos","Barium","Manganese","Hydrogen sulfide","Silvex",
          "2,4-D","2,3,7,8-Tetrachlorodibenzo-p-dioxin","N-Nitrosodi-n-butylamine","N-Nitrosopyrrolidine","Chloride")
   
   #isolate data so that you have to click a button so that it runs the query using eventReactive.
   
   #get grab data
   data<-eventReactive(input$goButton,{
     
   
   #fix some of the inputs
      gch<-switch(input$characteristics,"All RPA"=base::unique(c(phammrpa,cuB,dorpa,tox,"Chlorine","Flow")),
                  "Copper and Aluminum BLM"=cuB,   
                  "pH and Ammonia RPA"=phammrpa,
                  "DO RPA"=dorpa,
                  "Toxics"=tox,
                  "All AWQMS Characteristics"=oneoff,
                  "None"= character(0)) #none is an empty character string so we can just pull one-off parameters
      one<-c(input$oneoff)
      rchar<-c(gch,one)
      
      #if no monitoring locations types are selected, then need to subset types so we don't get a bunch of stuff that is irrelevant
      mon<-ifelse(stri_isempty(input$montype),c('BEACH Program Site-Ocean','BEACH Program Site-River/Stream', 'BEACH Program Site-Estuary',
            'Canal Drainage','Canal Irrigation','Canal Transport','Estuary','Facility Industrial',
            'Facility Municipal Sewage (POTW)','Facility Other','Lake','Ocean','Reservoir','River/Stream',
            'River/Stream Perennial','Facility Public Water Supply (PWS)'),input$montype)
      
      #query the data, doesn't pull data that is blank, or quality control data 
      dat<-noenvAWQMS_Data(startdate=toString(sprintf("%s",input$startd)),
                      enddate=toString(sprintf("%s",input$endd)),
                      MLocID=c(input$monlocs),
                      MonLocType=mon,
                      Char_Name=rchar,
                      OrganizationID=c(input$orgs),
                      HUC8_Name=c(input$huc8_nms), 
                      AU_ID=c(input$AUID),
                      filterQC=TRUE)
   
   
   #remove summary stats that are not 7 day avg, also check for and remove non-UTF8 characteristics from result_comment column
   #(which can prevent final excel download from opening)
   
   dat<-subset(dat,is.na(dat$Time_Basis)|dat$Time_Basis %in% "7DADMean")
   
   dat$Result_Comment<-iconv(dat$Result_Comment,"UTF-8","UTF-8",sub='')
   
   #want to add list of characteristics for each monitoring location to the leaflet popup, to do that we're going to have to pull 
   #in data() and add a column that has all characteristic names for each monitoring location....
   #if I just add data$char_Names I only get the first char (usually temperature, water)
   #able fix this by grouping via MLocID, then getting the unique chars via summarize
   #then merge the two dataframes together using MLocID, creates column called "type" that has chars
   grp<-dat %>% group_by(MLocID) %>% 
     summarize(type = paste(base::sort(base::unique(Char_Name)),collapse=", "))
   
   #merge 
   
   mer<-merge(dat,grp, by="MLocID")
   
   mer
   })
   
   #query for continuous data
   cont<-eventReactive(input$goButton, {
      
   #query for continuous data - note that we are not including rejected or unreviewed data,
   #also, we only want temperature, pH, conductivity, salinity, and DO data, 
      #fix some of the inputs
      gch<-switch(input$characteristics,"All RPA"=base::unique(c(phammrpa,cuB,dorpa,tox,"Chlorine","Flow")),
                  "Copper and Aluminum BLM"=cuB,   
                  "pH and Ammonia RPA"=phammrpa,
                  "DO RPA"=dorpa,
                  "Toxics"=tox,
                  "All AWQMS Characteristics"=oneoff,
                  "None"=character(0)) #none is an empty character string so we can just pull one-off parameters
      one<-c(input$oneoff)
      rchar<-c(gch,one)
      
        
    dat<-noenvAWQMS_Data_Cont(startdate=toString(sprintf("%s",input$startd)),enddate=toString(sprintf("%s",input$endd)),
                        MLocID=c(input$monlocs),
                        Char_Name=rchar,
                        OrganizationID=c(input$orgs), 
                        HUC8_Name=c(input$huc8_nms), 
                        AU_ID=c(input$AUID), 
                        Result_Status=c("Accepted","Final","Validated","Preliminary","Provisional"))
   
   #remove non-UTF8 characteristics from comments column
   #(which can prevent final excel download from opening)
   
   dat$Comments<-iconv(dat$Comments,"UTF-8","UTF-8",sub='')
   
   #want to add list of characteristics for each monitoring location to the leaflet popup, to do that we're going to have to pull 
   #in data() and add a column that has all characteristic names for each monitoring location....
   #if I just add data$char_Names I only get the first char (usually temperature, water)
   #able fix this by grouping via MLocID, then getting the unique chars via summarize
   #then merge the two dataframes together using MLocID, creates column called "type" that has chars
   grp<-dat %>% group_by(MLocID) %>% 
      summarize(type = paste(base::sort(base::unique(Char_Name)),collapse=", "))
   
   #merge 
   
   mer<-merge(dat,grp, by="MLocID")
   
   mer
   })
   
   #take data, make a subtable for VIEWING in the shiny app so it only shows desired columns from the AWQMS pull in desired order
   tsub<-eventReactive(input$goButton,{
     tsub<-select(data(),OrganizationID,Project1,StationDes,MLocID,MonLocType,SampleStartDate,SampleMedia,
               SampleSubmedia,Activity_Type,Statistical_Base,Char_Name,Char_Speciation,
               Sample_Fraction,CASNumber,Result_Text,Result_Unit,Method_Code,Method_Context,
               Activity_Comment,Result_Comment,Result_status,Result_Type)
   tsub
   })
   
   #table to view continuous data
   tcont<-eventReactive(input$goButton,{
      tcont<-select(cont(),OrganizationID,StationDes,MLocID,MonLocType,Char_Name, Depth, Depth_Unit,
                     Result_Date,Result_Time,Result_Numeric,Result_Unit,Result_Status,Comments)
      tcont
   })
   
   #take data, make a subtable for DOWNLOAD so that we only show the desired columns from the AWQMS data pull and in the desired order
   dsub<-eventReactive(input$goButton,{
     dsub<-select(data(),OrganizationID,org_name,Project1,act_id,StationDes,MLocID,MonLocType,Lat_DD,Long_DD,AU_ID,
                  SampleStartDate,SampleStartTime,SampleMedia,SampleSubmedia,Activity_Type,Statistical_Base,Time_Basis,Char_Name,Char_Speciation,
                 Sample_Fraction,CASNumber,Result_Text,Result_Unit,Analytical_method,Method_Code,Method_Context,Analytical_Lab,
                 MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit,
                 Activity_Comment,Result_Comment,Result_status,Result_Type)
     dsub
   })
   
   #download for continuous data
   dcont<-eventReactive(input$goButton,{
      dcont<-select(cont(), OrganizationID,org_name,StationDes,MLocID,MonLocType,HUC8_Name,HUC12_Name,Lat_DD,Long_DD,AU_ID,Equipment_ID,
                    Media,Sub_Media,Result_Date,Result_Time,Time_Zone,Char_Name,Result_Numeric,Operator,Result_Unit,Result_Status,
                    DQL,Depth,Depth_Unit,Comments)
      
      dcont
   })
   
   #table of queried data for Shiny app view  
   output$table<-renderDataTable({
     
     tsub()
   })
   
   #table of queried continuous data for Shiny app view
   output$continuous<-renderDataTable({
      tcont()
   })
   
   #leaflet map
   mymap<- eventReactive(input$goButton,{   
      #need to combine information from grab and continuous data if we're going to get the map to work
      #take both datasets and subset so they have the same basic columns
      subdat<-select(data(),MLocID,StationDes,type,Long_DD,Lat_DD)
      subcont<-select(cont(),MLocID,StationDes,type,Long_DD,Lat_DD)
      
      #combine dataframes and get unique values
      comb<-base::unique(rbind(subdat,subcont))
      
      #create map
     leaflet(comb) %>%
       addTiles()%>%
       addMarkers(lng=~Long_DD,
                  lat=~Lat_DD,
                  popup=paste("Station ID: ",comb$MLocID,"<br>",
                              "Description: ",comb$StationDes,"<br>",
                              "Characteristics: ",comb$type,"<br>"),
                  popupOptions= popupOptions(maxHeight = 75)) 
     #%>%
       #want to be able to select points on map via polygon.
       #first step is to be able to draw polygon on map
       #addDrawToolbar(editOptions = editToolbarOptions())
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
   
   #transform data for Aluminum BLM data
   aluminum<-eventReactive(input$goButton,{
      al<-AlBLM(data())
      
      #have hardness be calculated when Ca, Mg, or conductivity data are available
      #need to use original data since AlBLM function messes with characteristic naming conventions
      hard<-hardness(data())
      
      al<-merge(al,hard,by.x=c("MLocID","SampleStartDate"), all.x=TRUE)
      
      al
   })
   
   #calculate summary stats from continuous data
   contsumstat<-eventReactive(input$goButton, {
      stats<-contsum(cont())
      
      stats
   })
   
   #take data, make sub-table just for toxics RPA data
   rpa<-eventReactive(input$goButton,{
     
     #only keep characteristics that are in the tox character list
     rpa<-subset(data(),(Char_Name %in% tox))
    
     if (nrow(rpa)!=0){
       #combine method_code and method_Context columns
     rpa$Method_Code<-paste0(rpa$Method_Code," (",rpa$Method_Context,")")
     
     #remove estimated data if result is above MRL value (want to keep data between MRL and MDL, even though it is estimated)
     #however, don't want data that is biased low due to matrix issues, so change to where we keep "<" data (between MDL and MRL)
     #(need to do >MDLValue because if we do >= it will pull in all NDs since we put those into AWQMS as "<MDL")
     rpa<-subset(rpa,rpa$Result_Type!="Estimated" | (rpa$Result_Numeric<=rpa$MRLValue & rpa$Result_Numeric>rpa$MDLValue))

     #need to do unit conversions, all in ug/l, except for Alkalinity, which should be in mg/L
     #checked AWQMS database, all alkalinity is reported in mg/l or mg/l CaCO3, no need for conversion
     #get list of char names in RPA
     names<-base::unique(rpa$Char_Name)
     #remove alkalinity and hardness, those needs to stay as mg/l
     names<-names[!(names %in% c("Alkalinity, total","Hardness, Ca, Mg","Total hardness"))]
     
     #make sure hardness and alkalinity are in mg/l (occasionally is in ug/l)
     rpa<-unit_conv(rpa,c("Alkalinity, total","Hardness, Ca, Mg", "Total hardness"),"ug/l",'mg/l')
     
     #convert everything else to ug/l
     rpa<-unit_conv(rpa,names,"mg/l","ug/l")
     rpa<-unit_conv(rpa,names,"ng/l","ug/l")
     
     
     #1,3-dichloropropene is almost always reported as cis and trans isomers. Add them together and fix CAS
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
           Analytical_Lab="Calculated from isomer data") %>%
        base::unique()
     
     #need to add in < if applicable
     dich$Result_Text<-ifelse(((!is.na(dich$MRLValue))|!(is.na(dich$MDLValue))) & 
                                 (dich$Result_Numeric==dich$MRLValue | dich$Result_Numeric==dich$MDLValue),
                              paste0("<",dich$Result_Text),dich$Result_Text)

     #bind new rows to RPA dataframe
     rpa<-rbind.fill(rpa,dich)
     
     
     #issues with permittees submitting discrete grabs instead of composites, need to combine samples that should have been composited or else the 
     #rpa tool counts each discrete grab as it's own sample, thus inflating the sample count.
     
     #add yearmonth column here
     #note that this strips out the day in order to work since some 24 hr samples are across 2 days. 
     #However, I haven't seen a case yet where a permittee samples on the last day of the month and goes into the next month, so I think this should be fine
     rpa<-rpa%>%
        mutate(YearMonth=format(as.Date(SampleStartDate),"%Y-%m"))
        
        ##two part grouping - first get a count of all samples taken on a particular day for a particular analyte, label these as "Yes" in a column called multiple
        count<-rpa %>%
        group_by(OrganizationID,MLocID,SampleSubmedia,SampleStartDate,Char_Name,Sample_Fraction) %>%
        summarise(
           count_samples = n())
     
     #then merge this back into the main dataset
     rpa2<-merge(rpa,count,by=c('OrganizationID','MLocID','SampleSubmedia','SampleStartDate','Char_Name','Sample_Fraction')) %>%
        mutate(Multiple=ifelse(count_samples>1,"Yes","No"))
     
     #then regroup the data and include the count as a grouping
     #note that the code assumes that any NDs are equal to the MRL/MDL that was reported for the purpose of averaging the results
     all<-rpa2 %>%
        group_by(OrganizationID,MLocID,StationDes,MonLocType,SampleMedia,SampleSubmedia,Char_Name,Sample_Fraction,CASNumber,YearMonth,Multiple) %>%
        summarise(
           Result_Text= mean(Result_Numeric),
           Result_Numeric=Result_Text,
           MRLValue = mean(MRLValue),
           MDLValue = mean(MDLValue),
           Result_Type="Calculated",
           Result_Unit='ug/l',
           Method_Code="Calculated",
           Activity_Type="Calculated",
           Analytical_Lab="Calculated from multiple samples (usually multiple discrete grabs)"
        ) %>%
        base::unique()
     
     
     #need to add in < if applicable
     all$Result_Text<-ifelse((!is.na(all$MRLValue) & all$Result_Numeric==all$MRLValue)|
                                (!is.na(all$MDLValue) &all$Result_Numeric==all$MDLValue),
                             paste0("<",all$Result_Text),all$Result_Text)  
     
     #Remove the multiple samples from the rpa dataset and add in the calculated data
     #get char names for multiple counts
     multname<-all%>%
        subset(Multiple=="Yes", select=c(OrganizationID,YearMonth,MLocID,StationDes,MonLocType,
                                         SampleMedia,SampleSubmedia,Char_Name,Sample_Fraction,CASNumber))
     
     #only take rows that aren't in multname
     rpa<-anti_join(rpa,multname,by=c('OrganizationID','YearMonth','MLocID','Char_Name','Sample_Fraction','SampleSubmedia'))
     
     #bind in calculated data where count>1
     rpa<-rbind.fill(rpa,subset(all,Multiple=="Yes"))
     
     #add in YearMonth as SampleStartDate for calculated data so that we have a way to track when samples were taken,
     #transform the non "YearMonth" dates into a format excel can recognize
     rpa<-rpa %>%
        mutate(SampleStartDate = ifelse(!(is.na(Multiple)),YearMonth,format(as.Date(SampleStartDate),"%m/%d/%Y")))
     
     #add T, D, or I to CAS# for certain parameters (mostly metals, used RPA workbook to identify parameters) 
     #so that the RPA workbooks will recognize them
     
     #inorganic arsenic, have to add CAS # to match with RPA spreadsheet since AWQMS inorganic arsenic parameter doesn't have CAS
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Arsenic, Inorganic"),
                           paste0("7440382I"),
                           rpa$CASNumber)
     #dissolved
     rpa$CASNumber<-
       ifelse(rpa$Char_Name %in% c("Copper","Magnesium","Potassium","Sodium","Cyanide","Aluminum","Iron","Lead",
                                   "Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Beryllium","Cadmium","Chromium",
                                   "Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Methylmercury(1+)","Arsenic, Inorganic") &
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
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Cyanides amenable to chlorination (HCN & CN)","Cyanide, free"),
                           paste0("57125F"),
                           rpa$CASNumber)
     

     
     #hardness, add 'Hardness' to CAS so RPA tool has something to work with
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Total hardness","Hardness, Ca, Mg",
                                                "Hardness, carbonate","Hardness, non-carbonate"),
                           paste0("HARDNESS"),
                           rpa$CASNumber)
     
     #same for alkalinity
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Alkalinity, total"),
                           paste0("ALKALINITY"),
                           rpa$CASNumber)
     
     #same for nitrate-nitrite
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Nitrate + Nitrite"),
                           paste0("NITR"),
                           rpa$CASNumber)
     
     #same for total phenolic compounds
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Phenols"),
                           paste0("PHENOLICS"),
                           rpa$CASNumber)
     
     #there has been confusion in the CAS #s for Bis(2-chloroisopropyl) ether and Bis(2-chloro-1-methylethyl) ether. 
     #they are distinct but structurally similar chemicals. The name Bis(2-chloroisopropyl) ether has been used to refer to 
     #chemicals under both CAS 108-60-1 and 39638-32-9. 108-60-1 is the correct CAS according to EPA. 
     #AWQMS has Bis(2-chloroisopropyl) ether listed with CAS # 39638-32-9
     #changing the CAS # back for RPA purposes 
     #(source: Clarification of the relationship between bis(2-chloro-1-methylethyl) ether (CASRM 108-60-1) and bis(2-chloroisopropyl) ether, EPA September 2016)
     rpa$CASNumber<-ifelse(rpa$Char_Name %in% c("Bis(2-chloroisopropyl) ether"),
                           paste0("108601"),
                           rpa$CASNumber)
     
     #combine Char_Name and Sample_Fraction for just metals
     rpa<-namefrac(rpa)
     
     #change data that is between MDL and MRL to have e in front of result
     rpa$Result_Text<-ifelse((!is.na(rpa$MDLValue)) & (!is.na(rpa$MRLValue)) 
                        & rpa$Result_Numeric<rpa$MRLValue & rpa$Result_Numeric>rpa$MDLValue,
                        paste0("e",rpa$Result_Text),
                        rpa$Result_Text)
     
     #only take certain rows, change order so that it is more in line with RPA
     rpa<-subset(rpa,select=c(CASNumber,Project1,act_id,act_id,StationDes,Activity_Type,Method_Code,Char_Name,
                              SampleMedia,SampleStartDate,Result_Text,MRLValue,MDLValue,Result_Unit,Analytical_Lab,
                              Result_status,Result_Type,MLocID,MonLocType,Result_Comment))
     
     #need to remove dashes from CASNumber row
     rpa$CASNumber<-gsub("-","",rpa$CASNumber)
     
     
     }
     return(rpa)
     
   })
   
   #RPA summary, this is to help replace the summary stats page in the RPA spreadsheet
   RPAsum<-eventReactive(input$goButton, {
      if (nrow(rpa())!=0){
         RPAsum<-
            #1,2,4-Trichlorobenzene gets counted twice since it is run as both as part of the volatile and semivolatile suite...
            #for NPDES purposes, only the semi-volatile analyses count, so remove anything done by 624 or 624.1
            subset(rpa(),
                   subset= (!((Method_Code==c("624 (U.S. Environmental Protection Agency)")|
                                  Method_Code==c("624.1 (U.S. Environmental Protection Agency)"))
                                 & Char_Name==c("1,2,4-Trichlorobenzene"))))%>%
            
            #directions on how to deal with NDs and < taken from RPA IMD Appendix C.
            #for calculating mean, ND=0, between DL and QL=DL
            mutate(Result_mean = case_when(as.numeric(Result_Text)>=MRLValue ~ as.numeric(Result_Text),
                                           substr(Result_Text,start=1,stop=1) %in% "e" & !is.na(MDLValue) ~ as.numeric(MDLValue),
                                           substr(Result_Text,start=1,stop=1) %in% "e" & is.na(MDLValue) ~ as.numeric(MRLValue),
                                           substr(Result_Text,start=1,stop=1) %in% "<" ~ 0
            )) %>%
            #for calculating geo mean, ND=1/2DL, but need to be careful for carcinogens- sometimes 1/2 DL is near the  WQ criterion, which can lead to RP when we don't actually know
            #create column that uses result for caculating the geo mean
            mutate(Result_geomean=case_when(as.numeric(Result_Text)>=MRLValue ~ as.numeric(Result_Text),
                                            substr(Result_Text,start=1,stop=1) %in% "e" & !is.na(MDLValue) ~ as.numeric(MDLValue),
                                            substr(Result_Text,start=1,stop=1) %in% "e" & is.na(MDLValue) ~ as.numeric(MRLValue),
                                            substr(Result_Text,start=1,stop=1) %in% "<" & !is.na(MDLValue) ~ as.numeric(MDLValue)/2,
                                            substr(Result_Text,start=1,stop=1) %in% "<" & is.na(MDLValue) ~ as.numeric(MRLValue)/2
                                            
            )) %>%
            group_by(MonLocType,Char_Name)%>%
   
            #do summary stats
            #note that geomean actually will have more logic associated with it for carcinogens...will need to incorporate that
            summarise(count_all = n(), count_result = sum(!(substr(Result_Text,start=1,stop=1) %in% "<")), average = round(mean(Result_mean, na.rm = TRUE),2),
                      mn = mean(Result_mean, na.rm = TRUE),
                      stdev = sd(Result_mean, na.rm = T), max = as.character(max(Result_mean, na.rm = TRUE)), 
                      geomean = round(exp(mean(log(Result_geomean))),2)) %>% 
            #for CV, if number of observations is less than 10 then CV=0.6, else calculate the CV
            mutate(CV = ifelse(count_all>=10,round(stdev/mn,2),0.6))
         
         
         #add CAS#
         cas<-subset(rpa(),select=c(base::unique(Char_Name),base::unique(CASNumber)))
         RPAsum<-base::unique(left_join(RPAsum,cas, by="Char_Name"))
         
         #get into an order that can go right into the RPA spreadsheet
         RPAsum<-subset(RPAsum,select=c(MonLocType,Char_Name,count_result,count_all,max,geomean,average,CV,CASNumber))
   
        
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
     
     amdata<-data()
     amdata<-unit_conv(amdata,"Temperature, water","deg F","deg C")
     amdata<-unit_conv(amdata,"Ammonia","ug/l","mg/l")
     
     
     # only take the analytes we're interested in 
     char<-c("Alkalinity, total","pH","Temperature, water","Ammonia","Salinity","Ammonia-nitrogen",
             "Conductivity","Ammonia and ammonium")
     
     #remove any samples that are calculated from continuous data (eg. 7 day min)
     y<-subset(amdata,amdata$Char_Name %in% char & is.na(amdata$Statistical_Base))
     
     #there were some special projects at one point that looked at "dissolved alkalinity"-according to Linda McRae (5/16/2019) 
     #what they did was take two samples, one was filtered (dissolved alkalinity) and the other one wasn't (total alkalinity)
     #usually alkalinity is taken on a non-filtered sample, so we shall remove the "dissolved alkalinity" samples
     y<-subset(y,!(y$Char_Name=="Alkalinity, total" & y$Sample_Fraction=="Dissolved"))
     
     #combine name and method speciation, otherwise we get a bunch of rows we don't need
     y$Char_Name<-paste0(y$Char_Name,(ifelse(is.na(y$Method_Speciation),paste(""),paste0(",",y$Method_Speciation))))
     
     #remove "-FM" from end of activity id, so alkalinity doesn't end up in its own row with no field parameters from the same activity
     #applies to some ORDEQ data
     amdata$act_id<-gsub("-FM$","",amdata$act_id)
     
     
     amdata
   })
   
   #need to be able to calculate hardness from Ca and Mg for Aluminum Criteria and Toxics RPA
   hard<-eventReactive(input$goButton,{
      harddat<-hardness(data())
      
      harddat
   })

   ###############################          EXCEL OUTPUT       ##########################################################
   
   ### Style library-put all different styles used in workbooks here
   
   #Create title styles
   mainTitle<-createStyle(fontSize=16,fontColour="blue",textDecoration=c("bold","underline"))
   subTitle<-createStyle(fontSize=14,textDecoration="italic")
   wrap<-createStyle(wrapText=TRUE)
   
   #create bold style
   bold<-createStyle(textDecoration="bold")
   
   #create rotated text style
   rotate<-createStyle(textRotation = 45)
   
   #create shading style
   shaderot<-createStyle(fgFill="yellow2",textRotation = 45)
    
   
#create list of the parameters in query, get it into a formatted excel to export so we have record of query
#add sheet for search criteria,map data, and conditionally RPA data, Copper BLM, and Ammonia RPA if data is available  
   param<-eventReactive(input$goButton, {
      
      #create workbook and sheet
      wb<-createWorkbook()
      
     #create strings for the input parameters
      
     startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
     enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
     stations<- paste0("Stations = ",toString(sprintf("'%s'", input$monlocs)))
     monty<- paste0("Monitoring Location Types = ",toString(sprintf("'%s'", input$montype)))
     charc<- paste0("RPA Group = ",toString(sprintf("'%s'", input$characteristics)))
     onof<- paste0("Characteristics = ",toString(sprintf("'%s'", input$oneoff)))
     huc8s<-paste0("HUC8 = ",toString(sprintf("'%s'", input$huc8_nms)))
     auids<-paste0("Assessment Unit = ",toString(sprintf("'%s'",input$AUID)))
     organiz<- paste0("Organization = ",toString(sprintf("'%s'", input$orgs)))
     allchar<- paste0("List of all RPA characteristics (Toxics includes Pesticides/PCBs, Base Neutral, Acid Extractable, VOC, and Metals groupings) \n\n",
                      "pH and Ammonia RPA: ",toString(phammrpa), "\n\n",
                      "Copper BLM: ",toString(cuB),"\n\n",
                      "DO RPA: ",toString(dorpa),"\n\n",
                      "Pesticide and PCBs: ",toString(pestrpa),"\n\n",
                      "Base Neutral: ",toString(bneut),"\n\n",
                      "Acid Exractable: ",toString(aext),"\n\n",
                      "Volatile Organic Carbon: ",toString(vocrpa), "\n\n",
                      "Metals and Hardness: ",toString(metalsrpa))
     
     ###Search Criteria
     addWorksheet(wb,"Search Criteria")
       
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
       #if(length(warn)>0) {writeData(wb,sheet="Search Criteria",x=warn,startRow=5,startCol=1)}
       
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

       writeData(wb,sheet="Search Criteria",x=allchar,startCol=1,startRow=18)
       
       addStyle(wb,sheet="Search Criteria",style=wrap,rows=18,cols=1)
       setColWidths(wb,sheet="Search Criteria", cols=1, widths=220)
       
   ###Map
       #conditional on whether map button is checked
       if (input$NoMap==TRUE) 
       
       {addWorksheet(wb,"Map") 
        
       #create map with limited labels
          
          #need to combine information from grab and continuous data if we're going to get the map to work
          #take both datasets and subset so they have the same basic columns
          subdat<-select(data(),MLocID,StationDes,type,Long_DD,Lat_DD)
          subcont<-select(cont(),MLocID,StationDes,type,Long_DD,Lat_DD)
          
          #combine dataframes and get unique values
          comb<-base::unique(rbind(subdat,subcont))
          
       map<-leaflet(comb) %>%
         addTiles()%>%
         addMarkers(lng=~Long_DD,
                    lat=~Lat_DD,
                    label=~MLocID,
                    labelOptions=labelOptions(noHide=T))
       
       #save as jpeg file (pdf isn't able to be displayed, png has much larger file size)
       mapshot(map,file="map.jpeg")
       
       insertImage(wb,"Map","map.jpeg",width=10,height=7)
       }
       
      ###Diagnostics sheet, for totals and other stats we want to calculate
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

  #####Data worksheets
       #All grab data      
       addWorksheet(wb,"Data")
            writeDataTable(wb,"Data",x=dsub(),tableStyle="none")
      
       #continuous data   
       if (nrow(cont())!=0) {addWorksheet(wb,"Continuous Data")
                             writeDataTable(wb,"Continuous Data",x=dcont(),tableStyle="none")
       }
      
       #summary of continuous data
       if(nrow(contsumstat())!=0) {addWorksheet(wb,"Continuous Summary Stats")
                                   writeData (wb,"Continuous Summary Stats",startRow=1,x="Summary Statistics Calculated from Continuous data")
                                   writeData (wb,"Continuous Summary Stats",startRow=2,x="Summary Statistics include the daily maximum, daily minimum, daily average, the number of observations per day, the 7 day average of the daily maximum, and the 60 day average of the daily maximum")
                                   writeData (wb,"Continuous Summary Stats",startRow=3,x="The 7 and 60 day average are calculated with using the 7 day or 60 days preceeding (aka 'right adjusted'). This is consistent with the Integrated Report.")
                                   writeData (wb,"Continuous Summary Stats",startRow=4,x="ninetyninth column is the 99th percentile of the daily temperature data, it is included as a check to ensure that the daily maximum is not an outlier")
                                   writeDataTable(wb,"Continuous Summary Stats", startRow=6, x=contsumstat(),tableStyle="none")}
              
       #RPA          
       if (nrow(rpa())!=0) {addWorksheet(wb,"Toxics_Data_Format")
                           writeDataTable(wb,"Toxics_Data_Format",startRow=4,x=rpa(),tableStyle="none")
                           writeData(wb,"Toxics_Data_Format",startRow=1,x="Only copy columns with highlighted column headers into RPA workbook")
                        
                           writeData(wb,"Toxics_Data_Format",startRow=2,x="Examine 'Result_Comment' and 'Result_Type' columns to determine data usability")
                           addStyle(wb,"Toxics_Data_Format",style=bold,rows=2,cols=1:15)
                           addStyle(wb,"Toxics_Data_Format",style=shaderot,cols=1:15,rows=4)
                                                          }
       
       #RPA summary stats
       if (nrow(RPAsum())!=0) {addWorksheet(wb,"Toxics_SummaryStats")
               writeDataTable(wb,"Toxics_SummaryStats",startRow=3,x=RPAsum(),tableStyle="none")
               writeData(wb,"Toxics_SummaryStats",startRow=1,startCol=1,x="Compare with summary statistic results in RPA tool, contact Aliana Britson and Erich Brandstetter in case of discrepancy")
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
                              copper<-subset(copper,select=c("OrganizationID","Project1","MLocID","SampleStartDate","SampleStartTime","Char_Name","Result_Text","Result_Type",
                                                "MDLValue","MRLValue","Temperature.water","pH","Organic.carbonDissolved","Humic Acid",
                                                "Calcium","Magnesium","Sodium","Potassium","Sulfate","Chloride",
                                                "Alkalinity.total","Sulfide","Temperature.water.Result_Type","pH.Result_Type","Organic.carbonDissolved.Result_Type",
                                                "Humic Acid Result_Type","Calcium.Result_Type","Magnesium.Result_Type",
                                                "Sodium.Result_Type","Potassium.Result_Type","Sulfate.Result_Type","Chloride.Result_Type",
                                                "Alkalinity.total.Result_Type","Sulfide.Result_Type"))
                              
                              #need to reformat the columns so that they come through as numbers
                              
                              
                              writeDataTable(wb,"CuBLM_Data_Format",startRow=6,x=copper,tableStyle="none")
                              
       }
            
       #Aluminum BLM (copied from CuBLM...need to tweak)
            if (nrow(aluminum())!=0) {addWorksheet(wb,"AlBLM_Data_Format")
               writeData(wb,"AlBLM_Data_Format",startRow=1,x="Aluminum BLM data. Examine MLocID for sample location. Examine Result Type columns for data quality")
               writeData(wb,"AlBLM_Data_Format",startRow=2,x="data has already been converted into proper units for Al BLM analysis (ug/L for Aluminum, mg/L for Hardness). No unit conversion necessary")
               writeData(wb,"AlBLM_Data_Format",startRow=3,x="Note that if both dissolved and total recoverable aluminum were collected, there will be two rows for each sampling event.")
               writeData(wb,"AlBLM_Data_Format",startRow=4,x="Calc.Hardness is hardness calculated from calcium and magnesium, or conductivity data and Hardness.Type is how hardness was calculated")
               
               #remove date column, overkill
               aluminum<-within(aluminum(),rm("date"))
               
               #fix names to remove spaces and commas
               names(aluminum)<-str_replace_all(names(aluminum), c(" " = "." , "," = "" ))
               
               
               #make sure all columns are there for each parameter (add as NA if there is no data)
               aluminum<-if(!("Tempearture.water" %in% colnames(aluminum))){add_column(aluminum, "Temperature.water"=NA)} else {aluminum}
               aluminum<-if(!("Alkalinity.total" %in% colnames(aluminum))){add_column(aluminum, "Alkalinity.total"=NA)} else {aluminum}
               aluminum<-if(!("pH" %in% colnames(aluminum))){add_column(aluminum, "pH"=NA)} else {aluminum}
               aluminum<-if(!("Hardness.Ca.Mg" %in% colnames(aluminum))){add_column(aluminum, "Hardness.Ca.Mg"=NA)} else {aluminum}
               aluminum<-if(!("Organic.carbonDissolved" %in% colnames(aluminum))){add_column(aluminum, "Organic.carbonDissolved"=NA)} else {aluminum}
          
               
               #Same for result type
               
               aluminum<-if(!("Temperature.water.Result_Type" %in% colnames(aluminum))){add_column(aluminum, "Temperature.water.Result_Type"=NA)} else {aluminum}
               aluminum<-if(!("Alkalinity.total.Result_Type" %in% colnames(aluminum))){add_column(aluminum, "Alkalinity.total.Result_Type"=NA)} else {aluminum}
               aluminum<-if(!("pH.Result_Type" %in% colnames(aluminum))){add_column(aluminum, "pH.Result_Type"=NA)} else {aluminum}
               aluminum<-if(!("Hardness.Ca.Mg.Result_Type" %in% colnames(aluminum))){add_column(aluminum, "Hardness.Ca.Mg.Result_Type"=NA)} else {aluminum}
               aluminum<-if(!("Organic.carbonDissolved.Result_Type" %in% colnames(aluminum))){add_column(aluminum, "Organic.carbonDissolved.Result_Type"=NA)} else {aluminum}

               #reorder columns for easy copy/paste into Cu BLM tool
               aluminum<-subset(aluminum,select=c("OrganizationID","Project1","MLocID","SampleStartDate","SampleStartTime","Char_Name","Result_Text","Result_Type",
                                              "MDLValue","MRLValue","Temperature.water","Alkalinity.total","Organic.carbonDissolved", "Hardness.Ca.Mg","pH","Calc.Hardness",
                                              "Organic.carbonDissolved.Result_Type","Hardness.Ca.Mg.Result_Type","pH.Result_Type","Hardness.Type"
                                              ))
               
               #need to reformat the columns so that they come through as numbers
               
               
               writeDataTable(wb,"AlBLM_Data_Format",startRow=6,x=aluminum,tableStyle="none")
               
            }
            
       #Ammonia RPA                     
       if (nrow(amm())!=0) {addWorksheet(wb,"Ammonia_RPA_Format")
           
           #get ammonia data
           amm<-subset(amm(),Char_Name %in% c("Ammonia","Ammonia-nitrogen","Ammonia and ammonium"), 
                       select=c("act_id","MLocID","SampleStartDate","Result_Text","Result_Unit","Result_Type"))
           
           #Temperature data
           temp<-subset(amm(),Char_Name=="Temperature, water",
                        select=c("act_id","MLocID","SampleStartDate","Result_Text","Result_Unit","Result_Type"))
           
           #pH
           ph<-subset(amm(),Char_Name=="pH",
                      select=c("act_id","MLocID","SampleStartDate","Result_Text","Result_Unit","Result_Type"))  
           
           #Alkalinity
           alk<-subset(amm(),Char_Name=="Alkalinity, total",
                       select=c("act_id","MLocID","SampleStartDate","Result_Text","Result_Unit","Result_Type"))
           
           #get salinity data for avg salinity
           sal<-subset(amm(),Char_Name=="Salinity")
           saltype<-sal%>%
             group_by(MonLocType,Result_Unit)%>%
             summarise(average = mean(Result_Numeric))
           
           
           #format 
           writeData(wb,"Ammonia_RPA_Format",startRow=1,startCol=1,x="Ammonia RPA. Copy and paste MLocID, SampleStartDate, and Result into RPA workbook.")
           
           writeData(wb,"Ammonia_RPA_Format",startRow=3,startCol=1,x="Ammonia")
           writeData(wb,"Ammonia_RPA_Format",startRow=3,startCol=8,x="Temperature")
           writeData(wb,"Ammonia_RPA_Format",startRow=3,startCol=15,x="pH")
           writeData(wb,"Ammonia_RPA_Format",startRow=3,startCol=22,x="Alkalinity")
           writeData(wb,"Ammonia_RPA_Format",startRow=3,startCol=29,x="Average Salinity by Monitoring Location Type")
           writeDataTable(wb,"Ammonia_RPA_Format",x=amm,startRow=4,startCol=1,tableStyle="none")
           writeDataTable(wb,"Ammonia_RPA_Format",x=temp,startRow=4,startCol=8,tableStyle="none")
           writeDataTable(wb,"Ammonia_RPA_Format",x=ph,startRow=4,startCol=15,tableStyle="none")
           writeDataTable(wb,"Ammonia_RPA_Format",x=alk,startRow=4,startCol=22,tableStyle="none")
           writeDataTable(wb,"Ammonia_RPA_Format",x=saltype,startRow=4,startCol=29,tableStyle="none")
                           
       }
            
      #Hardness calculations
      if (nrow(hard())!=0) {addWorksheet(wb,"Calculated Hardness")
         writeData(wb,"Calculated Hardness",startRow=1,startCol=1,x="Hardness calculated from Calcium and Magnesium or from Specific Conductance")
         writeData(wb,"Calculated Hardness",startRow=2,startCol=1,x="Hardness is in mg/l")
         writeData(wb,"Calculated Hardness",startRow=4,startCol=1,x=hard())
      }
   
     wb
   })
   
   

# Download button- only works in Chrome
# gives an excel workbook with multiple sheets
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste(input$permit_num,"-DATA-AWQMS-", format(Sys.Date(),"%Y%m%d"),".xlsx", sep="")},
  content = function(file) {
    #sheet with query parameters
    saveWorkbook(param(),file)
    })

}

# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
