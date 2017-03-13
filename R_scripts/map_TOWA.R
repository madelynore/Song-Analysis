library(devtools)
#install_github("maRce10/warbleR")
library(warbleR)

#library(plyr)
library(rebird)
library(ggmap)
library(tidyverse)
library(foreign)
#library(XML)


#checks if region exists in ebird
ebirdregioncheck("states", "CA-BC")

#gets a map from API and shows it
California <- get_map(location="california",
                      zoom= 5,
                      maptype="roadmap",
                      source= "google")
ggmap(California,
       extent="device")

#gets recent observations from ebird of TOWA in the US
TOWA_US <- ebirdregion(region = 'US', species= 'Setophaga townsendi')

#creates map around BC
British_Columbia <- get_map(location="british columbia",
                      zoom= 4,
                      maptype="roadmap",
                      source= "google")

#shows map 
# ggmap(British_Columbia,
#       extent="device")

#gets Xeno-canto data to map 
XC_data <- read.csv("data/XC_metadata.csvy", comment.char = "#") %>%
  #confirms that Longitude is a numeric value
  verify(is.numeric(Longitude),error_fun="Not numeric") %>% 
  
  #confirms Latitude is a numeric value
  verify(is.numeric(Latitude),error_fun="Not numeric")

#plots the Xen-canto data points on the map
ggmap(British_Columbia) + geom_point(aes(x=Longitude, y= Latitude),
                                     data=XC_data)

# #reads the XML file
# TOWA_range_map <- xmlParse("data/Den_tow_TOWAx_Range.xml")
# 
# #confirms that it is an XML
# class(TOWA_range_map)
# 
# #exploring xml
# TOWA_top <- xmlRoot(TOWA_range_map)
# class(TOWA_top)
# #gives name of the node
# xmlName(TOWA_top)
# #shows first entry
# TOWA_top[[1]]
# 
# #turn XML into dataframe
# #TOWA_range <- ldply(xmlTo)
# TOWA_range <- ldply(xmlToList("data/Den_tow_TOWAx_Range.xml"), data.frame)
# View(TOWA_range)

#reads dbf
TOWA_range <- read.dbf("data/Den_tow_TOWAx_Range.dbf")
View(TOWA_range)
