##Madelyn Ore
##14 Feb 2017

library(readxl)
library(tidyverse)
library(stringr)

# Macauley Library --------------------------------------------------------
ML_data <- read_excel("data_raw/ML_SongRecordings.xlsx", sheet=1, col_names=TRUE)
names(ML_data) <- make.names(names(ML_data))
#View(ML_data)


#Remove unnecessary columns
NVM <- c(2:4, 6:7, 19, 22:35, 37:38, 40:45,47) #specify columns to eliminate
ML_col_cleaned <- ML_data %>% 
  select(-NVM)


#rearrange columns & change names
ML_arrange <- ML_col_cleaned[,c("ML.Catalog..","Year","Month","Day","Time","Country","County","State","Locality","Latitude","Longitude","Elevation..m.", "Behavior","Stimulus","Habitat","Background.Species","Processed.","X..of.songs.in.recording", "Public.Note")] 
ML_name <- rename(ML_arrange, Remarks = Public.Note, Songtype = Behavior, Processed.comments=Processed., Songs.In.Recording = X..of.songs.in.recording) # new name = old name
#View(ML_name)


#separate Database type from ID number
ML_ID_fix <- ML_name %>% 
  separate(ML.Catalog.., into=c("Database.type" ,"ID"), sep=2) #separates the first two letters
View(ML_ID_fix)


# #edit times
# ML_time_fix <- ML_ID_fix$Time %>% 
#   for (x in Time) {
#     if (x==0){
#      x <- is.na(x)
#     } else{
#       hm(x)
#     }
#   }
# View(ML_time_fix)

#write ML spreadsheet
write_csv(ML_ID_fix, path= "data/ML_song_recordings.csv")

# Xeno Canto --------------------------------------------------------------


XC_data <- read_excel("data_raw/xeno-canto v1.xls", sheet=1, col_names=TRUE)
names(XC_data) <- make.names(names(XC_data))
#View(XC_data)


#Separate columns of date
XC_date_sep <- XC_data %>% 
  separate(Date, into = c("Year","Month","Day"),sep="-")


#clean up Time 
XC_time_fix <- XC_date_sep %>% 
  separate(Time, into=c("wtf","Time"), sep=" ") %>%
  #remove the date thing
  select(-wtf)        

#View(XC_time_fix)

#separate state from locality
XC_locfix <- XC_time_fix %>% 
  mutate(split_location = str_split(Location, ","),
         State = map_chr(split_location, ~.x[length(.x)]),
         Locality = map_chr(split_location, ~.))
XC_locfix2 <- XC_locfix %>% 
  separate(Locality, into=c("Locality","strip"), sep=",")
# XC_loc1 <- X
#   separate(State, into=c("County","State"),sep=',',extra="merge", fill='right'C_loc %>% )
View(XC_locfix)

#Separate playback stimulus
#make string of interest an object
no_playback <- "playback-used:no"
yes_playback <- "playback-used:yes"
  
XC_playback <- XC_loc_fix %>% 
  mutate(Stimulus = str_extract(no_playback))
  #this shit doesn't work

#remove unnecessary columns 
bye <- c(1:4,18:21,24)
XC_data_colRemoved <- XC_loc_fix %>% 
  select(-bye)
View(XC_data_colRemoved)


#rearrange columns & change names
XC_renamed <-XC_data_colRemoved %>% 
  rename(Background.Species = Background, ID = Catalogue.number, Locality = Location, Songs.In.Recording = X..of.songs.in.recording)  # new name = old name
XC_addcol <- XC_renamed %>% 
  mutate(Database.type="XC")
XC_arrange <- XC_addcol[,c("Database.type","ID","Year","Month","Day","Time","Country","State","Locality","Latitude","Longitude","Elevation","Songtype","Background.Species","Process.comments","Songs.In.Recording","Remarks")]


# #write XC spreadsheet
write_csv(XC_arrange, path= "data/XC_song_recordings.csv")


  
  
  
  
  