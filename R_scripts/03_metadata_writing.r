##Madelyn Ore
##Feb 2017
 
library(csvy)
library(dplyr)
library(readr)

write_csvy(ML_data, file= "data/ML_metadata.csvy") #writes out the levels of the data
read.csv("data/ML_metadata.csvy", comment.char = "#")  #ignore all the comments and just shows the csv

write_csvy(XC_data, file= "data/XC_metadata.csvy")
read.csv("data/MLmetadata.csvy", comment.char = "#")


