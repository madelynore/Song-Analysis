##Madelyn Ore
##14 Feb 2017

library(readxl)
library(tidyr)
library(dplyr)


# Macauley Library --------------------------------------------------------


ML_data <- read_excel("data_raw/ML_SongRecordings.xlsx", sheet=1, col_names=TRUE)
names(ML_data) <- make.names(names(ML_data))
head(ML_data)
typeof(ML_data)


#Remove unnecessary columns
NVM <- c(2:4, 6:7, 23:35, 37:38, 41:46) #specify columns to eliminate
subset(ML_data, select= -NVM)



# Xeno Canto --------------------------------------------------------------


XC_data <- read_excel("data_raw/xeno-canto v1.xls", sheet=1, col_names=TRUE)
names(XC_data) <- make.names(names(XC_data))
head(XC_data)
View(XC_data)

#remove unnecessary columns
bye <- c(1:4,16:18)
XC_data_colRemoved <- subset(XC_data, select= -bye)
head(XC_data_colRemoved)

#Separate columns of date
?separate

XC_data_colRemoved %>% 
  separate(Date, into = c("Year","Month","Day"),sep="-")
