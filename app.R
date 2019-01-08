#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This is Aliana's attempt at using the Shiny App built by Travis Pritchard 
######to create a specific downloadable query from AWQMS designed for use by the NPDES Permit Writers
######ideally the data would be able to go straight into the RPA analysis


##Note- removed ability to show QC data- will not be needed for RPA analysis

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)
library(leaflet)
library(xlsx)

#Need to remake query, cannot use AWQMS_Data as it pulls out too much data for the app to work,
#plus, for NPDES only need a subset of data- 
#the function NPDES_AWQMS_Qry will only pull water data since the year 2000 from a select set of monloc types

#right now this is sourced, but will likely change it so that it is part of it's own library that gets pulled in at the beginning
source("NPDES_AWQMSQuery.R")

# Query out the valid values ---------------------------------------------

#NPDES only needs a limited # of Chars, this should help speed up the program
#once I get a 'NPDES Parameter group' in AWQMS I should try to figure out if I can have it just query the parameter group
#either that or I can remove the ability to search by parameter and just have it return all of them

##copper BLM data pull as well? Currently don't have DOC-perhaps talk with Rob Burkhart to see which parameters they're using for that and double check with the model

chars <- c(".alpha.-Endosulfan ",".alpha.-Hexachlorocyclohexane ",".beta.-Endosulfan ",".beta.-Hexachlorocyclohexane ",
          ".delta.-Hexachlorocyclohexane ","1,1,1-Trichloroethane ","1,1,2,2-Tetrachloroethane ","1,1,2-Trichloroethane ",
          "1,1-Dichloroethane ","1,1-Dichloroethylene ","1,2,4,5-Tetrachlorobenzene ","1,2,4-Trichlorobenzene ","1,2-Dichloroethane ",
          "1,2-Dichloropropane ","1,2-Diphenylhydrazine ","1,3-Dichloropropene","2,3,7,8-Tetrachlorodibenzo-p-dioxin ",
          "2,4,5-Trichlorophenol ","2,4,6-Trichlorophenol ","2,4-D ","2,4-Dichlorophenol ","2,4-Dimethylphenol ","2,4-Dinitrophenol ",
          "2,4-Dinitrotoluene ","2,6-Dinitrotoluene ","2-Chloroethyl vinyl ether ","2-Chloronaphthalene ","3,3'-Dichlorobenzidine ",
          "4,6-Dinitro-o-cresol ","Acenaphthene ","Acenaphthylene ","Acrolein ","Aldrin ","Allyl chloride ","Ammonia","Ammonia and ammonium",
          "Ammonia-nitrogen","Anthracene ","Antimony ","Aroclor 1016 ","Aroclor 1221 ","Aroclor 1232 ","Aroclor 1242 ",
          "Aroclor 1248 ","Aroclor 1254 ","Aroclor 1260 ","Arsenic ","Arsenic, Inorganic","Azinphos-methyl ","Azobenzene ",
          "Barium ","Benz[a]anthracene ","Benzene ","Benzene Hexachloride, Beta (BHC)","Benzene Hexachloride, Delta (BHC)",
          "Benzidine ","Benzo(b)fluoranthene ","Benzo[a]pyrene ","Benzo[ghi]perylene ","Benzo[k]fluoranthene ","Beryllium ",
          "Bis(2-chloro-1-methylethyl) ether ","Bis(2-chloroethoxy)methane ","Bis(2-chloroethyl) ether ","Bis(chloromethyl) ether",
          "Butyl benzyl phthalate ","Cadmium ","Carbon tetrachloride ","Chlordane ","Chlordane, technical, and/or chlordane metabolites",
          "Chlorobenzene ","Chlorodibromomethane ","Chloroethane ","Chloroform ","Chloromethane","Chlorpyrifos ","Chromium ",
          "Chromium(III)","Chromium(VI) ","Chrysene ","Copper ","Cyanide ","Demeton ","Di(2-ethylhexyl) phthalate ",
          "Dibenz[a,h]anthracene ","Dibutyl phthalate ","Dichlorobromomethane ","Dieldrin ","Diethyl phthalate ","Dimethyl phthalate ",
          "Di-n-octyl phthalate ","Dioxins and furans as 2,3,7,8-TCDD TEQs","Endosulfan ","Endosulfan sulfate ","Endrin ",
          "Endrin aldehyde ","Ethylbenzene ","Fluoranthene ","Fluorene ","Hardness, Ca, Mg","Hardness, carbonate",
          "Hardness, non-carbonate","Heptachlor ","Heptachlor epoxide ","Hexachlorobenzene ","Hexachlorobutadiene ",
          "Hexachlorocyclopentadiene ","Hexachloroethane ","Hydrogen sulfide","Indeno[1,2,3-cd]pyrene ",
          "Inorganic nitrogen (nitrate and nitrite)","Inorganic phosphorus","Iron ","Isophorone ","Lead ","Lindane ","Malathion ",
          "Manganese ","m-Dichlorobenzene ","Mercury ","Methoxychlor ","Methyl bromide ","Methylene chloride ","Methylmercury(1+) ",
          "Mirex ","Naphthalene ","Nickel ","Nitrate ","Nitrate + Nitrite","Nitrobenzene ","Nitrosamine","N-Nitrosodiethylamine",
          "N-Nitrosodiethylamine ","N-Nitrosodimethylamine ","N-Nitrosodi-n-butylamine ","N-Nitrosodi-n-propylamine ",
          "N-Nitrosodiphenylamine ","N-Nitrosopyrrolidine ","o-Chlorophenol ","o-Dichlorobenzene ","o-Nitrophenol ","p,p'-DDD ",
          "p,p'-DDE ","p,p'-DDT ","Parathion ","p-Bromophenyl phenyl ether ","p-Chloro-m-cresol ","p-Chlorophenyl phenyl ether ",
          "p-Dichlorobenzene ","Pentachlorobenzene ","Pentachlorophenol ","pH","Phenanthrene ","Phenol ","Phenols","Phosphorus ",
          "p-Nitrophenol ","Polychlorinated biphenyls ","Pyrene ","Selenium ","Silver ","Silvex ","Temperature, water",
          "Tetrachloroethene ","Tetrachloroethylene ","Thallium ","Toluene ","Total hardness","Total PCBs","Toxaphene ",
          "trans-1,2-Dichloroethylene ","Tribromomethane ","Tributlytin ","Tributyltin ","Trichloroethene (TCE) ",
          "Turbidity","Turbidity Field","Vinyl chloride ","Zinc ")

station <- NPDES_AWQMS_Stations()
station <- station$MLocID
station <- sort(station)

organization <- AWQMS_Orgs()
organization <- organization$OrganizationID
organization <- sort(organization)

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
        
        # Start Date
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = '2000-01-01'),
        # End date
        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),
       #characteristics
         selectizeInput("characteristics",
                     "Select characteristics",
                     choices = chars,
                     multiple = TRUE),

       # Monitoring locations 
        selectizeInput("monlocs",
                        "Select Monitoring Locations",
                        choices = station,
                        multiple = TRUE),
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
        h5("Select parameters on left to build table and map"),
        #tags$br(),
        h5("Click 'Run Query' Button to perform search after selecting desired parameters."),
        h5("Click 'Download Data' to download results"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        
        #two tabs, one for plot and one for map
        tabsetPanel(
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
   

   dat<-NPDES_AWQMS_Qry(startdate=rstdt,enddate=rendd,station=c(input$monlocs),
                  char=c(input$characteristics),org=c(input$orgs),HUC8_Name=c(input$huc8_nms),reject=rrej)
   dat
   #long<-reactive({dat()$Long_DD})
   #lat<-reactive({dat()$Lat_D})
   #mon<-reactive({dat()$MLocID})
   })

   #create list of the parameters in query, try to get it into a formatted excel to export
   param<-eventReactive(input$goButton, {
     
     #create strings for the input parameters
     startdt<-paste0("Startdate = ",toString(sprintf("%s",input$startd)))
     enddt<-paste0("Enddate = ",toString(sprintf("%s",input$endd)))
     rejected<-paste0("Is rejected data included?  ",if(input$Reject) {TRUE} else {FALSE})
     stations<- paste0("Stations = ",toString(sprintf("'%s'", input$monlocs)))
     charc<- paste0("Characteristics = ",toString(sprintf("'%s'", input$characteristics)))
     huc8s<-paste0("HUC8 = ",toString(sprintf("'%s'", input$huc8_nms)))
     organiz<- paste0("Organization = ",toString(sprintf("'%s'", input$orgs)))
     
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
     cells<-CellBlock(sheet,6,1,7,1)
     CB.setRowData(cells,startdt,1)
     CB.setRowData(cells,enddt,2)
     CB.setRowData(cells,stations,3)
     CB.setRowData(cells,charc,4)
     CB.setRowData(cells,huc8s,5)
     CB.setRowData(cells,organiz,6)
     CB.setRowData(cells,rejected,7)
     
     wb
     #param<-list(startdt,enddt,stations,charc,huc8s,organiz,rejected)
   })
   
#table of queried data      
output$table<-renderDataTable({
  
  data()
  })

#leaflet map
output$locs<-renderLeaflet({
  
  leaflet(data()) %>%
    addTiles()%>%
    addMarkers(lng=~Long_DD,
               lat=~Lat_DD,
               popup=paste("Station ID: ",data()$MLocID,"<br>",
                           "Description: ",data()$StationDes,"<br>"))
})

# Download button- only works in Chrome
#gives an excel with two sheets, the first is the serach parameters (needs some work), the second is the data
#set to give NAs as blank cells
output$downloadData <- downloadHandler(
  
  filename = function() {paste("AWQMS_Download-", Sys.Date(),"_",input$permit_num,".xlsx", sep="")},
  content = function(file) {
    saveWorkbook(param(),file)
    #write.xlsx(param(),file,sheetName="Search Criteria",col.names=FALSE,row.names=FALSE)
    write.xlsx(data(), file,sheetName="Data",row.names = FALSE,showNA=FALSE,append=TRUE)
    #include another csv file here that will contain the query parameters (need to create reactive function that will contain these)
    })

}
# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
