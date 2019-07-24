#function to add sample fraction to metals for NPDES RPA and Split sample purposes

namefrac<-function(x)
{x$Char_Name<-
  ifelse(x$Char_Name %in% c("Calcium","Copper","Magnesium","Potassium","Sodium","Cyanide","Aluminum","Iron","Lead",
                            "Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Beryllium","Cadmium","Chromium",
                            "Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Methylmercury(1+)","Organic carbon",
                            "Barium","Cobalt","Manganese","Vanadium","Molybdenum","Boron","Lithium","Uranium","Hardness, Ca, Mg","Silica", 
                            "Ammonia"),
         paste0(x$Char_Name,", ",x$Sample_Fraction),
         x$Char_Name)
  return(x)
}
