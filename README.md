# ShinyNPDES_AWQMS
This is a tool designed to pull data directly out of AWQMS into a format that is conducive to RPA and copper BLM analysis, 
as well as provide supporting information about the data pull

The Shiny app currently allows for one to search for data in AWQMS by pollutant (either by groupings or by specific pollutants), monitoring location, organization, dates,
HUC8, and matrix type. If the user wishes to view rejected data a button can be clicked to include rejected data in the query. 
A similar option is also available for continuous data summary statistics

Some words of warning: if data pulls are done for large areas and timeframes it can easily overload the application 
(R has limited memory compared to SQL). It is recommended that you refine the search by HUC, media type, and limit the initial date 
range before running the query. However, if you are searching for data on the coast, beach stations or other stations associated with
the Pacific Ocean may not appear if you refine by HUC8. 

