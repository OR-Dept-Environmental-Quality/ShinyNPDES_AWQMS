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

#Need to remake query, cannot use AWQMS_Data as it pulls out too much data for the app to work,
#plus, for NPDES only need a subset of data- 
#the function NPDES_AWQMS_Qry will only pull water data since the year 2000 from a select set of monloc types

#right now this is sourced, but will likely change it so that it is part of it's own library that gets pulled in at the beginning
source("NPDES_AWQMSQuery.R")

# Query out the valid values ---------------------------------------------

#NPDES only needs a limited # of Chars, this should help speed up the program
#once I get a 'NPDES Parameter group' in AWQMS I should try to figure out if I can have it just query the parameter group
#either that or I can remove the ability to search by parameter and just have it return all of them

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

   # Application title
   titlePanel("ORDEQ AWQMS data retrieval function for NPDES"),

   # Sidebar with parameter inputs
   sidebarLayout(
      sidebarPanel(
        # Start Date
        dateInput("startd",
                  label = "Select Start Date",
                  min = '1949-09-15',
                  value = '2000-01-01'
                  ),

        # End date
        dateInput("endd",
                  label = "Select End Date",
                  min = '1900-1-1'),

       #characteristics
         selectizeInput("characteristics",
                     "Select parameters",
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
        h1("AWQMS Data Builder"),
        h5("Select parameters on left to build data retrieval function"),
        #tags$br(),
        h5("Copy and paste function below into a different R session"),
        h5("Click 'Run Query' Button to perform search after selecting desired parameters."),
        h5("Click 'Download Data' to download results"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        textOutput("selected_chars"),
        
        #create two panels within main panel, one for table and the other for a leaflet map
        splitLayout(
        # Aliana added a data table
        dataTableOutput("table"),
        #add leaflet map
        leafletOutput("locs")
        )
   )
))

# Define server logic required to display query
server <- function(input, output) {

  
   output$selected_chars <- renderText({

     # Convert all field entries to strings of vectors - This allows their use in the query
     stats <- toString(sprintf("'%s'", input$monlocs))
     vals <- toString(sprintf("'%s'", input$characteristics))
     huc8s <-toString(sprintf("'%s'", input$huc8_nms))
     organiz <- toString(sprintf("'%s'", input$orgs))

     # Begin the query 
     qry <- "NPDES_AWQMS_Qry("

  
  # Add parameters to query - 
     #General format - If field is not blank, and fields above are not blank add a comma and the paramter
        # If above fields are all blank, do not add the comma
        # If field is empty, do nothing
     
  #Start date
      if(length(input$startd) > 0){
       qry <- paste0(qry, "startdate = '", input$startd,"'" )
     }

  #enddate
     if(length(input$endd) > 0){

       if(length(input$startd) > 0){
         qry <- paste0(qry, ", ")}

       qry <- paste0(qry, "enddate = '", input$endd,"'" )
     }

  #monlocs
       if(length(input$monlocs) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0){
         qry <- paste0(qry, ", ")
         }

        qry <- paste0(qry,"station = c(",stats,")"  )


       }

  #chars
     if(length(input$characteristics) > 0){


       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"char = c(",vals,") "  )

     }

 #HUC8s
     if(length(input$huc8_nms) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$samp_med) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"HUC8_Name = c(",huc8s,") "  )

     }
     
 #orgs
     if(length(input$orgs) > 0){
       
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$samp_med) > 0|
          length(input$huc8_nms) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"org = c(",organiz,") "  )
       
     }
     
     #reject filter
     
     if(input$Reject) {
       
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$huc8_nms) > 0|
          length(input$orgs) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"reject = TRUE")  
       
     }
     
     qry <- paste0(qry, ")")


})
   
   #have to make dates into strings, otherwise they come out as funny numbers
   #all other variables are reactive 'as is' except for reject button
   #isolate data so that you have to click a button so that it runs the query the first time.
   #However, it just runs after I click it the first time, will need to tinker to get it to run only after clicking button
   isolate({

   rstdt<-reactive({toString(sprintf("%s",input$startd))})
   rendd<-reactive({toString(sprintf("%s",input$endd))})
   rrej<-reactive({if(input$Reject) {TRUE} else {FALSE} })
   

   dat<-reactive({NPDES_AWQMS_Qry(startdate=rstdt(),enddate=rendd(),station=c(input$monlocs),
                  char=c(input$characteristics),org=c(input$orgs),HUC8_Name=c(input$huc8_nms),reject=rrej())})
   
   long<-reactive({dat()$Long_DD})
   lat<-reactive({dat()$Lat_D})
   mon<-reactive({dat()$MLocID})
   })

#table of queried data      
output$table<-renderDataTable({
  if (input$goButton==0)
    return()
  
  dat()
  })

#leaflet map
output$locs<-renderLeaflet({
  leaflet() %>%
    addTiles()%>%
    addMarkers(lng=long(),
               lat=lat(),
               popup=mon())
})

# Download button- only works in Chrome
output$downloadData <- downloadHandler(
  
  filename = function() {paste("dataset-", Sys.Date(), ".csv", sep="")},
  content = function(file) {write.csv(dat(), file,row.names = FALSE)})

}
# Run the application
shinyApp(ui = ui, server = server)

#make sure you do runApp(launch.browser=TRUE) or in the Run App tab, click "Run External" if you want to download-
#only works in Chrome
