#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This app, based on a modified verison of the AWQMSdata_ShinyHelp app built by Travis Pritchard, 
######is designed to pull out RPA related data from AWQMS and put it into a table for use in RPA analysis

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(leaflet)
library(xlsx)
library(dplyr)
library(xlsxjars)
library(mapview)


#Need to remake query, cannot use AWQMS_Data as it pulls out too much data for the app to work,
#plus, for NPDES only need a subset of data- 
#the function NPDES_AWQMS_Qry will only pull water data since the year 2000 from a select set of monloc types

#right now this is sourced, but will likely change it so that it is part of it's own library that gets pulled in at the beginning
source("NPDES_AWQMSQuery.R")
#funciton not ready yet, but sourcing it for when it is
#source("CopperBLM_Function.R")

# Query out the valid values ---------------------------------------------

#NPDES only needs a limited # of Chars, this should help speed up the program

chars <- c("All RPA Characteristics","Copper BLM Characteristics",".alpha.-Endosulfan ",".alpha.-Hexachlorocyclohexane ",
           ".beta.-Endosulfan ",".beta.-Hexachlorocyclohexane ","1,1,1-Trichloroethane ","1,1,2,2-Tetrachloroethane ",
           "1,1,2-Trichloroethane ","1,1-Dichloroethane ","1,1-Dichloroethylene ","1,2,3,4,5,6-Hexachlorocyclohexane",
           "1,2,4,5-Tetrachlorobenzene ","1,2,4-Trichlorobenzene ","1,2-Dichloroethane ","1,2-Dichloropropane ","1,2-Diphenylhydrazine ",
           "1,3-Dichloropropene","2,4,5-Trichlorophenol ","2,4,6-Trichlorophenol ","2,4-Dichlorophenol ","2,4-Dimethylphenol ",
           "2,4-Dinitrophenol ","2,4-Dinitrotoluene ","2,6-Dinitrotoluene ","2-Chloroethyl vinyl ether ","2-Chloronaphthalene ",
           "3,3'-Dichlorobenzidine ","4,6-Dinitro-o-cresol ","Acenaphthene ","Acenaphthylene ","Acrolein ","Aldrin ",
           "Alkalinity, total","Allyl chloride ","Aluminum ","Ammonia ","Ammonia and ammonium","Ammonia-nitrogen","Anthracene ",
           "Antimony ","Aroclor 1016 ","Aroclor 1221 ","Aroclor 1232 ","Aroclor 1242 ","Aroclor 1248 ","Aroclor 1254 ","Aroclor 1260 ",
           "Arsenic","Arsenic ion (3+) ","Arsenic, Inorganic","Azinphos-methyl ","Benz[a]anthracene ","Benzene ",
           "Benzene Hexachloride, Beta (BHC)","Benzidine ","Benzo(b)fluoranthene ","Benzo[a]pyrene ","Benzo[ghi]perylene ",
           "Benzo[k]fluoranthene ","Beryllium ","Biochemical oxygen demand, non-standard conditions",
           "Biochemical oxygen demand, standard conditions","Bis(2-chloro-1-methylethyl) ether ","Bis(2-chloroethoxy)methane ",
           "Bis(2-chloroethyl) ether ","Butyl benzyl phthalate ","Cadmium ","Calcium","Carbon tetrachloride ","Chlordane ","Chloride",
           "Chlorobenzene ","Chlorodibromomethane ","Chloroethane ","Chloroform ","Chloromethane","Chlorpyrifos ","Chromium ",
           "Chromium(III)","Chromium(VI) ","Chrysene ","Conductivity","Copper","Copper ","Cyanide ","Demeton ",
           "Di(2-ethylhexyl) phthalate ","Dibenz[a,h]anthracene ","Dibutyl phthalate ","Dichlorobromomethane ","Dieldrin ",
           "Diethyl phthalate ","Dimethyl phthalate ","Di-n-octyl phthalate ","Dissolved oxygen (DO)","Dissolved oxygen saturation",
           "Endosulfan sulfate ","Endrin ","Endrin aldehyde ","Ethylbenzene ","Fluoranthene ","Fluorene ","Hardness, Ca, Mg",
           "Hardness, carbonate","Hardness, non-carbonate","Heptachlor ","Heptachlor epoxide ","Hexachlorobenzene ","Hexachlorobutadiene ",
           "Hexachlorocyclopentadiene ","Hexachloroethane ","Indeno[1,2,3-cd]pyrene ","Inorganic nitrogen (nitrate and nitrite)","Iron ",
           "Isophorone ","Kjeldahl nitrogen","Lead ","Lindane ","Magnesium","Malathion ","m-Dichlorobenzene ","Mercury ","Methoxychlor ",
           "Methyl bromide ","Methylene chloride ","Mirex ","Naphthalene ","Nickel ","Nitrate ","Nitrate + Nitrite","Nitrobenzene ",
           "N-Nitrosodimethylamine ","N-Nitrosodi-n-propylamine ","N-Nitrosodiphenylamine ","o-Chlorophenol ","o-Dichlorobenzene ",
           "o-Nitrophenol ","Organic carbon","p,p'-DDD ","p,p'-DDE ","p,p'-DDT ","Parathion ","p-Bromophenyl phenyl ether ",
           "p-Chloro-m-cresol ","p-Chlorophenyl phenyl ether ","p-Dichlorobenzene ","Pentachlorobenzene ","Pentachlorophenol ","pH",
           "Phenanthrene ","Phenol ","p-Nitrophenol ","Potassium","Pyrene ","Salinity","Selenium ","Silver ","Sodium","Sulfate","Sulfide",
           "Temperature, water","Tetrachloroethene ","Tetrachloroethylene ","Thallium ","Toluene ","Total hardness","Total Kjeldahl nitrogen",
           "Total Sulfate","Toxaphene ","trans-1,2-Dichloroethylene ","Tribromomethane ","Trichloroethene (TCE) ","Vinyl chloride ","Zinc ")


# Check to see if saved cache of data exists. If it does not, or is greater than
# 7 days old, query out stations and organizations and save the cache
# 

if(!file.exists("query_cache.RData") | 
   difftime(Sys.Date() ,file.mtime("query_cache.RData") , units = c("days")) > 7){
  

#NPDES_AWQMS_Stations functions only pulls stations 
station <- NPDES_AWQMS_Stations()
Mtype<-station$MonLocType
station <- station$MLocID
station <- sort(station)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)

save(station, Mtype, organization, file = 'query_cache.RData')
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
                     "Select characteristics",
                     choices = chars,
                     multiple = TRUE,
                     selected="All RPA Characteristics"),

       # Monitoring locations 
        selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = station,
                        multiple = TRUE),
       
       # Monitoring location types--this won't work unless I add monitoring type to NPDES query
        selectizeInput("montype",
                       "Select Monitoring Location Types",
                       choices=Mtype,
                       multiple=TRUE),
       
       #add warning
       tags$em("Warning: HUC8 may not include all stations"),
       
       # huc8 names 
       selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = HUC8_Names,
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
       #add action button, idea is to not run query until the button is clicked)
       actionButton("goButton","Run Query"),
       #add a download button
       downloadButton('downloadData', 'Download Data')
        ),


     # Setup main panel
       mainPanel(
        h1("RPA Data Builder"),
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
                           tags$br(),
                           h5("Warning: running query with all characteristics and a large timeframe (1+ year) can overload the server.
                              If you want to search for all characteristics, it is recommended that you run the query with a short timeframe
                              first (~6 months), then use the map to locate several stations of interest and refine query accordingly.
                              Alternatively, you may also refine by HUC8. However, refining search by HUC8 may not include all stations.")
                           )
                 ),
        # Aliana added a data table
        tabPanel("Table",dataTableOutput("table")),
        #add leaflet map
        tabPanel("Map",leafletOutput("locs"))
        )
   )
))

# Define server logic required to display query
server <- function(input, output) {
  
   
   #have to make dates into strings, otherwise they come out as funny numbers
   #all other variables are reactive 'as is' except for reject button
   #isolate data so that you have to click a button so that it runs the query using eventReactive.

   data<-eventReactive(input$goButton,{
     
   rstdt<-toString(sprintf("%s",input$startd))
   rendd<-toString(sprintf("%s",input$endd))
   rrej<-if(input$Reject) {TRUE} else {FALSE} 
   rchar<-if(input$characteristics=="All RPA Characteristics") {chars} else {input$characteristics}
   rchar<-if(input$characteristics=="Copper BLM Characteristics") 
     {c("Alkalinity, total","Calcium","Chloride","Copper","Magnesium","pH","Potassium","Sodium","Sulfate","Organic carbon",
     "Temperature, water","Total Sulfate","Sulfide")} else {input$characteristics}
   

   dat<-NPDES_AWQMS_Qry(startdate=rstdt,enddate=rendd,station=c(input$monlocs),montype=c(input$montype),
                  char=c(rchar),org=c(input$orgs),HUC8_Name=c(input$huc8_nms),reject=rrej)
   
   #want to add list of characteristics for each monitoring location to the popup, I think to do that we're going to have to pull 
   #in data() and add a column that has all characteristic names for each monitoring location....
   #if I just add data$char_Names I only get the first char (usually temperature, water)
   #able to do this by grouping via MLocID, then getting the unique chars via summarize
   #then merge the two dataframes together using MLocID, creates column called "type" that has chars
   grp<-dat %>% group_by(MLocID) %>% 
     summarize(type = paste(sort(unique(Char_Name)),collapse=", "))
   
   #merge 
   
   mer<-merge(dat,grp, by="MLocID")
   
   mer
   })
   
   #take data, make a subtable for VIEWING so that we only show the desired columns from the AWQMS data pull and in the desired order
   tsub<-eventReactive(input$goButton,{
     tsub<-select(data(),Org_Name,Project1,StationDes,MLocID,MonLocType,SampleStartDate,SampleMedia,
               SampleSubmedia,Activity_Type,Statistical_Base,Char_Name,Char_Speciation,
               Sample_Fraction,CASNumber,Result,Result_Unit,Analytical_method,
               Activity_Comment,Result_Comment,Result_status,Result_Type)
   tsub
   })
   
   #take data, make a subtable for DOWNLOAD so that we only show the desired columns from the AWQMS data pull and in the desired order
   dsub<-eventReactive(input$goButton,{
     dsub<-select(data(),OrganizationID,Org_Name,Project1,StationDes,MLocID,MonLocType,SampleStartDate,SampleStartTime,SampleMedia,
                 SampleSubmedia,Activity_Type,Statistical_Base,Time_Basis,Char_Name,Char_Speciation,
                 Sample_Fraction,CASNumber,Result,Result_Unit,Analytical_method,Method_Code,Method_Context,Analytical_Lab,
                 MDLType,MDLValue,MDLUnit,MRLType,MRLValue,MRLUnit,
                 Activity_Comment,Result_Comment,Result_status,Result_Type)
     dsub
   })
   
   #take data, make subtable just for RPA data
  # rpa<-eventReactive(input$goButton,{
   #  rpa<-select(data(),)
  # })
   
   #table of queried data      
   output$table<-renderDataTable({
     
    tsub()
   })
   
   #leaflet map
   output$locs<-renderLeaflet({
     
     leaflet(data()) %>%
       addTiles()%>%
       addMarkers(lng=~Long_DD,
                  lat=~Lat_DD,
                  popup=paste("Station ID: ",data()$MLocID,"<br>",
                              "Description: ",data()$StationDes,"<br>",
                              "Characteristics: ",data()$type,"<br>"),
                  popupOptions= popupOptions(maxHeight = 75))
   })

   #create list of the parameters in query, try to get it into a formatted excel to export
   param<-eventReactive(input$goButton, {
     
     #create strings for the input parameters
     startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
     enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
     rejected<-paste0("Is rejected data included?  ",if(input$Reject) {TRUE} else {FALSE})
     stations<- paste0("Stations = ",toString(sprintf("'%s'", input$monlocs)))
     monty<- paste0("Monitoring Location Types = ",toString(sprintf("'%s'", input$montype)))
     charc<- paste0("Characteristics = ",toString(sprintf("'%s'", input$characteristics)))
     huc8s<-paste0("HUC8 = ",toString(sprintf("'%s'", input$huc8_nms)))
     organiz<- paste0("Organization = ",toString(sprintf("'%s'", input$orgs)))
     allchar<- paste0("List of all potential RPA characteristics: ",toString(chars))
     
     #create workbook and sheet
     wb<-createWorkbook()
     sheet<-createSheet(wb,sheetName="Search Criteria")
     
     #add title function 
     ##code borrowed from "http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r"
     #add title function
     #++++++++++++++++++++++++
     # Helper function to add titles
     #++++++++++++++++++++++++
     # - sheet : sheet object to contain the title
     # - rowIndex : numeric value indicating the row to 
     #contain the title
     # - title : the text to use as title
     # - titleStyle : style object to use for title
     xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
       rows <-createRow(sheet,rowIndex=rowIndex)
       sheetTitle <-createCell(rows, colIndex=1)
       setCellValue(sheetTitle[[1,1]], title)
       setCellStyle(sheetTitle[[1,1]], titleStyle)
     }
     TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, 
                                        color="blue", isBold=TRUE, underline=1)
     SUB_TITLE_STYLE <- CellStyle(wb) + 
       Font(wb,  heightInPoints=14, 
            isItalic=TRUE, isBold=FALSE)
     # Add title
     xlsx.addTitle(sheet, rowIndex=1, title="RPA Data Search Criteria",
                   titleStyle = TITLE_STYLE)
     # Add sub title
     xlsx.addTitle(sheet, rowIndex=2, 
                   title=paste0(input$permittee),
                   titleStyle = SUB_TITLE_STYLE)
     # Add sub title
     xlsx.addTitle(sheet, rowIndex=3, 
                   title=paste0("Permit # ",input$permit_num),
                   titleStyle = SUB_TITLE_STYLE)
     # Add sub title
     xlsx.addTitle(sheet, rowIndex=4, 
                   title=paste0("Date of query, ",Sys.Date()),
                   titleStyle = SUB_TITLE_STYLE)
     
     #Create Cell Block and populate the rows with the parameters
     cells<-CellBlock(sheet,6,1,10,1)
     CB.setRowData(cells,startdt,1)
     CB.setRowData(cells,enddt,2)
     CB.setRowData(cells,stations,3)
     CB.setRowData(cells,monty,4)
     CB.setRowData(cells,charc,5)
     CB.setRowData(cells,huc8s,6)
     CB.setRowData(cells,organiz,7)
     CB.setRowData(cells,rejected,8)
     if(input$characteristics=="All RPA Characteristics") {
       CB.setRowData(cells,allchar,10,rowStyle = CellStyle(wb,alignment=Alignment(wrapText=TRUE)))
       }
     setColumnWidth(sheet,1,120)
     
     #add the leaflet map as a sheet in the download excel
     map<-leaflet(data()) %>%
                  addTiles()%>%
                  addMarkers(lng=~Long_DD,
                             lat=~Lat_DD,
                             label=~MLocID,
                             labelOptions=labelOptions(noHide=T))
     
     mapshot(map,file="map.png")
     
     sheet2<-createSheet(wb,sheetName = "Map")
     addPicture("map.png",sheet2)
       
     wb
   })
   
   

# Download button- only works in Chrome
#gives an excel with two sheets, the first is the serach parameters (needs some work), the second is the data
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste("AWQMS_Download-", Sys.Date(),"_",input$permit_num,".xlsx", sep="")},
  content = function(file) {
    #sheet with query parameters
    saveWorkbook(param(),file)
    #sheet with data
    write.xlsx(dsub(), file,sheetName="Data",row.names = FALSE,showNA=FALSE,append=TRUE)
    #sheet with just RPA format
    #write.xlsx(rpa(),file,sheetName="RPA_Data",row.names=FALSE,showNA=FALSE,append=TRUE)
    })

}
# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
