###### Cross Correlation analysis #######

### for Use with PuTTY
#for installing a package with the CRAN repo included, 
# install.packages("warbleR", dependencies = TRUE, repos = "https://cran.r-project.org/package=warbleR")

##installing tidyverse to PuTTY
devtools::install_github('tidyverse/tidyverse')

##trying to get warbleR to install
# library(devtools)
#install_github('seewave')
# ##install package forom github, for installing warbler on the cluster
devtools::install_github('maRce10/warbleR')
#putty error:
#ERROR: dependencies ‘seewave’, ‘dtw’, ‘jpeg’, ‘proxy’ are not available for package ‘warbleR’
install.packages("seewave", repos="http://cran.at.r-project.org/", dependencies = TRUE)
#^didn't work >.< 

### beginning of code to run x correlation --written to run in PuTTY ####
library(warbleR)
library(tidyverse)

setwd("~/songs")
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
s2n <- sig2noise(selec, mar = 0.1)


##cut songs into individual song files
cut_sels(selec, mar = 0.05, path = '~/xcorr')





