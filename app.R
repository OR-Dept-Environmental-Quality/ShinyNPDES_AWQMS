#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
######This is Aliana's attempt at using the Shiny App built by Travis Pritchard 
######to create a specific downloadable query from AWQMS designed for use by the NPDES Permit Writers
######ideally the data would be able to go straight into the RPA analysis


##Note- removed ability to show QC data- will not be needed for RPA analysis
## Also removed stat basis- these are for Integrated Report, NPDES folks should calc their own stats when querying continuous data

print("Initial data queries may take a few minutes.")

library(shiny)
library(AWQMSdata)


# Query out the valid values ---------------------------------------------

chars <- AWQMS_Chars()
chars <- chars$Char_Name
chars <- sort(chars)

station <- AWQMS_Stations()
station <- station$MLocID
station <- sort(station)

projects <- AWQMS_Projects()
projects <- projects$Project
projects <- sort(projects)

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
                  value = '2010-01-01'
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


       # Projects 
       selectizeInput("projs",
                       "Select Projects",
                       choices = projects,
                       multiple = TRUE),

       # huc8 names 
       selectizeInput("huc8_nms",
                       "Select HUC 8",
                       choices = HUC8_Names,
                       multiple = TRUE),
        
        
       #Orgs
       selectizeInput("orgs",
                       "Select organization",
                       choices = organization,
                       multiple = TRUE)

        ),


     # Setup main panel
       mainPanel(
        h1("AWQMS_Data() Builder"),
        h5("Select parameters on left to build data retrieval function"),
        #tags$br(),
        h5("Copy and paste function below into a different R session"),
        h5("addition of output table and download button forthcoming- UNDER CONSTRUCTION"),
        # Add line
        tags$hr(),
        #Add break
        tags$br(),
        textOutput("selected_chars"),
        textOutput("sc1"),
        textOutput("sc2"),
        textOutput("sc3"),
        textOutput("sc4"),
        textOutput("sc5"),
        # Aliana attempting to add table of data
        tableOutput("table"),
        #add a download button, see if I can't get it to work
        downloadButton('downloadData', 'Download data')
   )
))

# Define server logic required to display query
server <- function(input, output) {
output$sc1<- renderText({if(length(input$monlocs) > 0)
  {toString(sprintf("'%s'", input$monlocs))} else {NULL}})
  
output$sc2<- renderText({if(length(input$characteristics) > 0)  
                  {toString(sprintf("'%s'", input$characteristics))} else {NULL}})

output$sc3<- renderText({if(length(input$projs) > 0) 
  {toString(sprintf("'%s'", input$projs))} else {NULL}})

output$sc4<- renderText({if(length(input$huc8_nms) > 0)
  {toString(sprintf("'%s'", input$huc8_nms))} else {NULL}})

output$sc5<- renderText({if(length(input$orgs) > 0) 
  {toString(sprintf("'%s'", input$orgs))} else {NULL}})

  
   output$selected_chars <- renderText({

     # Convert all field entries to strings of vectors - This allows their use in the query
     stats <- toString(sprintf("'%s'", input$monlocs))
     vals <- toString(sprintf("'%s'", input$characteristics))
     proj_select <-toString(sprintf("'%s'", input$projs))
     huc8s <-toString(sprintf("'%s'", input$huc8_nms))
     organiz <- toString(sprintf("'%s'", input$orgs))

     # Begin the query 
     qry <- "AWQMS_Data("

  
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

  #projects
     if(length(input$projs) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0){
         qry <- paste0(qry, ", ")
       }

       qry <- paste0(qry,"project = c(",proj_select,") "  )

     }

  #sample media
     #sample media
     {
       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$stat_basis) > 0|
          length(input$projs) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"media = c('Water') "  )  
       
     }
       


 #HUC8s
     if(length(input$huc8_nms) > 0){

       if(length(input$startd) > 0 |
          length(input$endd) > 0|
          length(input$monlocs) > 0|
          length(input$characteristics) > 0|
          length(input$projs) > 0|
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
          length(input$projs) > 0|
          length(input$samp_med) > 0|
          length(input$huc8_nms) > 0){
         qry <- paste0(qry, ", ")
       }
       
       qry <- paste0(qry,"org = c(",organiz,") "  )
       
     }
     

     qry <- paste0(qry, ")")


})
   #table of queried data
   #likely can't just put qry in expectin it to run, will likely have to make it more reactive than that
   #qry is text, need to actually evaluate the statement and then render the Table
   #need to use Travis' code above, but instead of outputting to a text query, have it evaluate 
   #within the AWQMS_Data function
   
   #memory issue...can't allocate that much, which makes sense, need to find a way to get it to not 
   #run query- or run it in SQL and not R......
   #what if I put starting default values in that won't return anything or will return a small amount of data?
   
   #still having error: cannot coerce type 'closure' to vector of type 'character'
   
#newdata<-reactive({data<-AWQMS_Data(rstdt(),rendd(),rstats(),rproj_select(),rvals(),media='Water',rorganiz(),rhuc8s(),filterQC=TRUE)})

output$table<-renderTable({

  rstats<-if(length(input$monlocs) > 0) {toString(sprintf("'%s'", input$monlocs))} else {NULL}
  rvals<- if(length(input$characteristics) > 0) {toString(sprintf("'%s'", input$characteristics))} else {NULL}
  rproj_select<-if(length(input$projs) > 0) {toString(sprintf("'%s'", input$projs))} else {NULL}
  rhuc8s<-if(length(input$huc8_nms) > 0) {toString(sprintf("'%s'", input$huc8_nms))} else {NULL}
  rorganiz<-if(length(input$orgs) > 0) {toString(sprintf("'%s'", input$orgs))} else {NULL}
  
  dat<-AWQMS_Data(startdate=input$startd,enddate=input$endd,station=c(rstats),project=c(rproj_select),
                   char=c(rvals),media=c('Water'),org=c(rorganiz),HUC8=c(rhuc8s),filterQC=TRUE)
  dat

  })

#have to make the data itself a reactive expression for this to work
output$downloadData <- downloadHandler(
  
  filename = function() { 
    paste("dataset-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(dat, file)
  })

}
# Run the application
shinyApp(ui = ui, server = server)

