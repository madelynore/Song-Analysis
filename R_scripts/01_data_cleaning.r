##Madelyn Ore
##14 Feb 2017

require(readxl)
require(stringr)
require(tidyverse)




# Macauley Library --------------------------------------------------------
ML_data <- read_excel("data_raw/ML_SongRecordings.xlsx", sheet=1, col_names=TRUE)
names(ML_data) <- make.names(names(ML_data))
#View(ML_data)


####Remove unnecessary columns###
NVM <- c(2:4, 6:7, 19, 22:35, 37:38, 40:45,47) #specify columns to eliminate
ML_col_cleaned <- ML_data %>% 
  select(-NVM)


###rearrange columns & change names###
ML_arrange <- ML_col_cleaned[,c("ML.Catalog..","Year","Month","Day","Time","Country","County","State","Locality","Latitude","Longitude","Elevation..m.", "Behavior","Stimulus","Habitat","Background.Species","Processed.","X..of.songs.in.recording", "Public.Note")] 
ML_name <- rename(ML_arrange, Remarks = Public.Note, Songtype = Behavior, Processed.comments=Processed., Songs.In.Recording = X..of.songs.in.recording) # new name = old name
#View(ML_name)


###separate Database type from ID number###
ML_ID_fix <- ML_name %>% 
  separate(ML.Catalog.., into=c("Database.type" ,"ID"), sep=2) #separates the first two letters
View(ML_ID_fix)


####edit times###

#changes zeros to NA
ML_ID_fix$Time[ML_ID_fix$Time == 0] <-NA

#pads times >10 with zeros
ML_ID_fix$Time <- sprintf("%04d", ML_ID_fix$Time)

#inserts colon
ML_ID_fix$Time <- sub("([[:digit:]]{2,2})$", ':\\1', ML_ID_fix$Time)



###write ML spreadsheet##
write.csv(ML_ID_fix, file = "data/ML_song_recordings.csv")

# Xeno Canto --------------------------------------------------------------

#reads raw data file
XC_data <- read_excel("data_raw/xeno-canto v1.xls", sheet=1, col_names=TRUE)
#makes the column headers into objects
names(XC_data) <- make.names(names(XC_data))
#View(XC_data)


####Separate columns of date####
XC_date_sep <- XC_data %>% 
  separate(Date, into = c("Year","Month","Day"),sep="-")


####clean up Time ###
XC_time_fix <- XC_date_sep %>% 
  separate(Time, into=c("wtf","Time"), sep=" ") %>%
  #remove the date thing
  select(-wtf)        

#View(XC_time_fix)

####separate state from locality###
XC_locfix <- XC_time_fix %>% 
  mutate(split_location = str_split(Location, ","),
         State = map_chr(split_location, ~.x[length(.x)]))

state <- unique(XC_locfix$State)
loc_list <- XC_locfix$split_location

head(state)


loc_list[[-" WA"]]
head(loc_list)
 for (i in loc_list){
  for (x in loc_list[i]){
    if (x == state){
      delete.response()
    }
  }
}

loc_list %>% 
  if (!(state)){
    reduce(c)
  }
loc_list %>% 
  if(!(state)){
    Reduce(sapply(c, loc_list))
  }

XC_locfix$split_location <- loc_list[-state]

XC_locfix$Location <- XC_locfix$Location[-state]

if (!(map_chr(split_location, ~.x[length(.x)])))
# XC_locfix2 <- XC_locfix %>% 
#   separate(Locality, into=c("Locality","strip"), sep=",")
# XC_loc1 <- X
#   separate(State, into=c("County","State"),sep=',',extra="merge", fill='right'C_loc %>% )
View(XC_locfix)

####Separate playback stimulus###

#separates outs playback info
XC_stim <- XC_locfix %>% 
  separate(Remarks, into=c("Remarks","Stimulus"), sep= 'playback-used:')
#changes yes or no to playback or natural
XC_stim$Stimulus[XC_stim$Stimulus == "no"] <- "Natural"
XC_stim$Stimulus[XC_stim$Stimulus == "yes"] <- "Playback"

###remove unnecessary columns ###
bye <- c(1:4,18:21,24)
XC_data_colRemoved <- XC_stim %>% 
  select(-bye)
View(XC_data_colRemoved)


###rearrange columns & change names##
XC_renamed <-XC_data_colRemoved %>% 
  rename(Background.Species = Background, Recording_ID = Catalogue.number,
         Locality = Location, Songs.In.Recording = X..of.songs.in.recording)  # new name = old name
XC_addcol <- XC_renamed %>% 
  mutate(Database.type="XC")
XC_arrange <- XC_addcol[,c("Database.type","ID","Year","Month",
                           "Day","Time","Country","State","Locality","Latitude","Longitude",
                           "Elevation","Songtype","Background.Species","Process.comments",
                           "Songs.In.Recording","Remarks")]


###write XC spreadsheet##
write_csv(XC_arrange, path= "data/XC_song_recordings.csv")



# Summer 2017 Data --------------------------------------------------------

### Google sheet####

#loads data from google doc sheet
GS_data <- read.csv("data_raw/Song Data - Form Responses 1.csv", strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("NA","",'na') )

#adds columns for species name and stimulus type and populates the values in the column
GS_add <- GS_data %>% 
  add_column("Species"=c(rep("Setophaga townsendi", 83)),"Stimulus"=c(rep("Natural",83))) 


#remove timestamp column
GS_remove_timestamp <- GS_add %>% 
  select(-Timestamp) 

#rearrange columns to match exel
GS_col_arrange <- GS_remove_timestamp[,c("Species","Song.ID","Date","Time","Locality","GPS.coordinate.North",
                                         "GPS.coordinate.West",
                                         "Elevation..m.","Habitat.Type","X..Canopy.Cover",
                                         "Dominant.Species","Tree.Size.Class","Understory",
                                         "Estimated.distance.from.record..m.",
                                         "Background.species","Disturbance.in.recording.",
                                         "Stimulus","Banded","Initials.of.Recorder","Notes")]

#renames the columns on the google sheet so they match the names on the excel sheet 
#new name first
GS_rename <- GS_col_arrange %>% 
  rename(Est.dist.from.recorder = Estimated.distance.from.record..m.,
         GPS.coordinate.N = GPS.coordinate.North , GPS.coordinate.W = GPS.coordinate.West, 
         Understory.code = Understory, Recorder.Initials = Initials.of.Recorder, 
         Date..MDY. = Date)


#changes the data type to match the Summer 2017 dataset 
GS_rename$Understory.code <- as.factor(GS_rename$Understory.code)
GS_rename$Stimulus <- as.factor(GS_rename$Stimulus)

View(GS_rename)


# Excel summer 2017 -------------------------------------------------------

Summer_TOWA <- read.csv("data_raw/song_data_2017.csv",  strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("NA","",'na') )

#Removes observations of other species
Summer_TOWA <- Summer_2017_data %>% 
  filter(ï..Species=="TOWA")

#changes from bird code to scientific name
Summer_TOWA$ï..Species <- gsub('TOWA' , 'Setophaga townsendi', Summer_TOWA$ï..Species)

#renames the columns on the excel sheet so they match the names on the google sheet 
#new name first
Summer_rename <- Summer_TOWA %>% 
  rename(Tree.Size.Class = Tree.class.Size, Elevation..m. = Elevationv..m.,
         Background.species = Background.spp, Stimulus = Natural.or.Playback., Banded = Banded., 
         X..Canopy.Cover= X..Canopy.cover, Species = ï..Species)

View(Summer_rename)

Song_data_2017 <- full_join(GS_rename, Summer_rename)

#Removes * character from the GPS coordinates and replaces with a space
Song_data_2017$GPS.coordinate.N <- chartr('*',' ', Song_data_2017$GPS.coordinate.N)
Song_data_2017$GPS.coordinate.W <- chartr('*',' ', Song_data_2017$GPS.coordinate.W)

View(Song_data_2017)

#writes CSV
write_csv(Song_data_2017, path = "data/song_data_backup.csv")

#alternate way with write.csv()
# write.csv(Song_data_2017, file = "c:\\Users\\Madelyn Ore\\Documents\\UBC Irwin\\Song Analysis\\TOWA\\Song-Analysis\\data\\song_data.csv")

# #outputs what type of data each column is being read as, and summary information about it
# summary(GS_rename)
# summary(Summer_rename)


song_data <- read.csv("data/song_data_2017.csv", strip.white = TRUE)
View(song_data)

#Removes recordings that are not usable
song_data <- song_data %>% 
  filter(Usable != "no")



write.csv(song_data, "data/song_data_2017_v2.csv")



# Blood Data-- edit for banding database ----------------------------------

bl <- read.csv(file = "data/old versions/banding_data.csv", strip.white = TRUE, na.strings = "")

#remove unneccesary columns--> song ID and habitat
bl <- bl[,-c(3,10)]

#replace * and ' ' with .
bl$GPS.coordinate.N <- sub("[.]", "", bl$GPS.coordinate.N)
bl$GPS.coordinate.W <- sub("[.]", "", bl$GPS.coordinate.W)

bl$GPS.coordinate.N <- chartr("*", ".", bl$GPS.coordinate.N)
bl$GPS.coordinate.N <- chartr(" ", ".", bl$GPS.coordinate.N)
bl$GPS.coordinate.W <- chartr("*", ".", bl$GPS.coordinate.W)
bl$GPS.coordinate.W <- chartr(" ", ".", bl$GPS.coordinate.W)

#fill in column
bl$Feather <- rep("N", length= nrow(bl))

for (i in 1:length(bl$Sex)){
  if(bl$Sex[i] == 'Male'){
    bl$Sex[i] = 'M'
  }
  if(bl$Sex[i] == "Female"){
    bl$Sex[i] = 'F'
  }
  if(bl$Sex[i] == "UNKNOWN"){
    bl$Sex[i] = 'U'
  }
}

#change date format
bl <- bl %>% 
  separate(col = date..MDY., into = c('Month', 'Day', 'Year'), sep = '/')

#organize columns
bl <- bl[,c("Record..", "Day", "Month", "Year", 'Locality', 'GPS.coordinate.N', 'GPS.coordinate.W', 'Accuracy',
            'Elevation..m.', 'Time', 'Species', 'Sex', 'Age', "Breeding.", "CP", "BP", "Bander.Initials",
            "Band..","Color.band.code", "Blood.Sample.Taken", "Feather", "Photos.Taken", "Fat", "Wing.Chord..mm.",
            "Tail.Length..mm.", "Tarsus.length..mm.", "Weight", "Plong.Slong", "Exp.Culmen",
            "Bill.length..mm.", "Beak.depth", "beak.width", "Notes")]

write.csv(bl, file = "data/banding_data_2017.csv")









