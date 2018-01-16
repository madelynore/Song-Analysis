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


# Map song types  ---------------------------------------------------------
fieldrec <- read.csv(file = "data/song_data_2017.csv", strip.white = TRUE)

##### match song type to song data info - a test... 
##working only on getting the field recordings from 2017 and with the type a songs
#basic plan: get file names, strip them down so they are only the song IDs, then link them to the 
#spreadsheet with all the song information, then plot on a map using GPS corrodinates

#generate list of files that belong to that song type
type_a <- list.files(path = "sound/Clear songs/type A/")

#remove the image file name from the string
pattern <- '-.\\.+tiff'
#select only unique ones
type_a <- unique(sub(pattern, type_a, replacement = ""))
#create a list that only has the field recordings
A_fieldrec <- type_a[grep('+SR', type_a)]
#remove all unneccesary elements
A_fieldrec <- uniquify(A_fieldrec)

#create column for song type
fieldrec$song.category <- character(length = nrow(fieldrec))

#match list to IDs in song types and name song category
for (i in 1:length(A_fieldrec)){
  num <- which(fieldrec$Song.ID == A_fieldrec[i])
  fieldrec$song.category[num] = 'A'
}

categorize <- fuction(x, category){
  for (i in 1:length(x)){
  num <- which(fieldrec$Song.ID == x[i])
  fieldrec$song.category[num] = category
  }
}


#creates map around BC
British_Columbia <- get_map(location="british columbia",
                            zoom= 4,
                            maptype="roadmap",
                            source= "google")

#plots the field recording data points on the map
ggmap(British_Columbia) + geom_point(aes(x=GPS.coordinate.W, y= GPS.coordinate.N, 
                                         col = factor(fieldrec$song.category)), data=fieldrec)

## THIS WORKS! Halle-fucking-lujah