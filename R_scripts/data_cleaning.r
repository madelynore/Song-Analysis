##Madelyn Ore
##14 Feb 2017

library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(tibble)


# Macauley Library --------------------------------------------------------


ML_data <- read_excel("data_raw/ML_SongRecordings.xlsx", sheet=1, col_names=TRUE)
names(ML_data) <- make.names(names(ML_data))
#View(ML_data)

#Remove unnecessary columns
NVM <- c(2:4, 6:7, 19, 22:35, 37:38, 40:45,47) #specify columns to eliminate
ML_col_cleaned <- ML_data %>% 
  select(-NVM)

#rearrange columns & change names
ML_arrange <- ML_col_cleaned[,c(1,3,4,5,6,7,9,8,10,11,12,13,14,15,16,2,18,19,17)] 
ML_name <- rename(ML_arrange, Remarks = Public.Note, Songtype = Behavior, Processed.comments=Processed.) # new name = old name
#View(ML_name)


#separate Database type from ID number
ML_ID_fix <- ML_name %>% 
  separate(ML.Catalog.., into=c("Database type","ID"), sep=2) #separates the first two letters
#View(ML_ID_fix)

#edit times

#write ML spreadsheet
write_csv(ML_ID_fix, path= "data/ML_song_recordings.csv")

# Xeno Canto --------------------------------------------------------------


XC_data <- read_excel("data_raw/xeno-canto v1.xls", sheet=1, col_names=TRUE)
names(XC_data) <- make.names(names(XC_data))
#View(XC_data)


#remove unnecessary columns 
bye <- c(1:4,16:19)
XC_data_colRemoved <- XC_data %>% 
  select(-bye)
View(XC_data_colRemoved)

#rearrange columns & change names
XC_renamed <-XC_data_colRemoved %>% 
  rename(Background.Species = Background, ID = Catalogue.number)  # new name = old name
XC_addcol <- XC_renamed %>%
  mutate(Database.type="XC")
XC_arrange <- XC_addcol[,c(14,11,1,2,4,3,5,6,7,8,10,12,13,9)]

  # add_column(Database.type= 1:106, .before=1)
  
View(XC_arrange)

#Separate columns of date
XC_date_sep <- XC_renamed %>% 
  separate(Date, into = c("Year","Month","Day"),sep="-")

#clean up Time 
XC_time_fix <- XC_date_sep %>% 
  separate(Time, into=c("wtf","Time"), sep=" ") %>%
  #remove the date thing
  select(-wtf)        

View(XC_time_fix)

#separate state from locality
# XC_loc<- XC_time_fix %>%
#   separate(Location, into=c("Locality","State"), extra = 'merge', fill="left")
# # XC_loc1 <- X
# #   separate(State, into=c("County","State"),sep=',',extra="merge", fill='right'C_loc %>% )

library(purrr)
library(stringr)
XC_time_fix %>% 
  mutate(split_location = str_split(Location, ","),
         state_location = map_chr(split_location, ~ .x[length(.x)])) %>% 
  View

# View(XC_loc)
# 
# #write XC spreadsheet
# write_csv(XC_time_fix, path= "data/XC_song_recordings.csv")


  
  
  
  
  