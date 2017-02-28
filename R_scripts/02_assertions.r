##Madelyn Ore
##Feb 2017

#necessary libraries to conduct this effort
library(assertr)

#reading in the proper files
ML_data <- read.csv("data/ML_song_recordings.csv")
XC_data <- read.csv("data/XC_song_recordings.csv")


# Macauley Library assertions ---------------------------------------------
states <- c("Alaska", "California", "Montana", "Oregon", "Washington", "Alberta", "British Columbia")
MLname_of_database <- c("ML")

ML_data %>% 
  verify(ncol(.)==20) %>% 
  assert(in_set(MLname_of_database), Database.type)
  verify(is.factor(Locality)) %>% 
  assert(within_bounds(0,100), Songs.In.Recording) %>% 
  assert(in_set(states),  State)

# Xeno Canto assertions ---------------------------------------------------
XCname_of_database <- c("XC")
  
XC_data %>% 
    verify(ncol(.)==17) %>% 
    assert(in_set(name_of_database), Database.type)
  verify(is.factor(Locality)) %>% 
    assert(within_bounds(0,100), Songs.In.Recording) %>% 
    assert(in_set(states),  State)
