library(warbleR)
library(tidyverse)

#try to get warbleR to install
library(devtools)
install_github('seewave')

setwd("~/Documents/TOWA song analysis ORE/All sound files")
getwd()
##get selection table with all start and end points for each song
selec <- read.csv(file = 'TOWA_selec_clear.csv', strip.white = TRUE)
#set class of end string
selec$end. <- as.character(selec$end.)

#create new column, removing backslash... that format is the way to escape backslash as 
#special character
selec$end <- sub("([\\])", "", selec$end.)

#remove the 'end.' column
selec <- selec[,-4]

##get list of all song files
song <- list.files(pattern = 'wav$')

checkwavs()

##run a snr test on all songs
sig2noise(songs, mar = 0.1)


##cut songs into individual song files
cut_sels(selec, mar = 0.05, path = 'corr_songs/'                                                                                                                                                                                                                                                                                                                                                   )