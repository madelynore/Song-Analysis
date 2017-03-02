##Madelyn Ore
##Feb 2017

#necessary libraries to conduct this effort
library(assertr)

#reading in the proper files
ML_data <- read.csv("data/ML_song_recordings.csv")
XC_data <- read.csv("data/XC_song_recordings.csv")


# Macauley Library assertions ---------------------------------------------
#all states where breeding Townsend's Warblers would be
states <- c("Alaska", "California", "Montana", "Oregon", "Washington", "Alberta", "British Columbia")
#nickname of database
MLname_of_database <- c("ML")

ML_data %>% 
  #verifys there are the correct number of columns
  verify(ncol(.)==20) %>% 
  
  #checks that it is the correct database being tested
  assert(in_set(MLname_of_database), Database.type)

  #checks that the location is a factor
  verify(is.factor(Locality)) %>% 
    
  #checks there is a reasonable number of songs in the recording, no misplaced zeros
  assert(within_bounds(0,100), Songs.In.Recording) %>% 
    
  #checks that the location of the recordings makes sense
  assert(in_set(states),  State)

# Xeno Canto assertions ---------------------------------------------------
#nickname of database
XCname_of_database <- c("XC")
  
XC_data %>% 
  #verifys there are the correct number of columns
    verify(ncol(.)==17) %>% 
  
  #checks that it is the correct database being tested
    assert(in_set(name_of_database), Database.type)

  #checks that the location is a factor
  verify(is.factor(Locality)) %>% 
    
    #checks there is a reasonable number of songs in the recording, no misplaced zeros
    assert(within_bounds(0,100), Songs.In.Recording) %>% 
    
    #checks that the location of the recordings makes sense
    assert(in_set(states),  State)
