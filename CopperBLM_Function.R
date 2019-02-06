#create a function that converts data from AWQMS long table format into the wide table format that 
#is used by the copper BLM model and the NPDES Copper BLM templates
#only return the correct columns, will have to search for correct parameters

CuBLM<-function(x) {
  #x is table output from NPDES_AWQMSQuery.R
  
  #do I need to do any unit conversions?
  
  #change from long to wide (how do I keep site information?)
  xwide<-spread(x,Char_Name,Result)
}
   