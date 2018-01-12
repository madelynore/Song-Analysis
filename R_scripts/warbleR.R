#20 Sept 2017
#test workflow warbleR
#Madelyn Ore

library(tidyverse)
library(warbleR)


# one song ----------------------------------------------------------------


#set working directory to folder with song in it
setwd(choose.dir())
#check that you did that
getwd()

#check that files in wd are readable
checkwavs()

#get file names for songs you want to analyze
song <- list.files(pattern = "wav$")

#create a spectrogram image of your sound file, flist = subset of songs you want to analyze,
#       ovlp = overlap of sound windows, it = file type to save image as, flim = c(lower freq bound, upper freq bound)
#       sxrow = seconds per row, rows = #rows
lspec(flist = song, ovlp = 50, it = "tiff", flim = c(1.5,8), sxrow = 5, rows = 2)

#autodetect signals, save as object- these settings capture all signals, but doesn't distinguish final notes separately
SRF08M03 <- autodetec(flist = song, threshold = 12, envt = "abs", ssmooth = 1500, wl = 512, bp = c(2,10),
          res = 300, flim = c(2 , 10), ls = TRUE, sxrow = 5, rows = 2, mindur = 0.03, maxdur = 0.5,
          redo = TRUE, it = "tiff", set = TRUE, smadj = "both")

str(SRF08M03)
View(SRF08M03)

#detects margins to measure noise around signals
snrspecs(SRF08M03, flim = c(2,10), snrmar = 0.1, mar = 0.1, it = "tiff" )

#calculate SNR
sig2noise(SRF08M03, mar = 0.1)

write.csv(SRF08M03, "SRF08M03_selecs.csv", row.names = FALSE)

#manually locate signals
manualoc(wl=512, flim = c(4,6), reccomm = TRUE, selcomm = TRUE, title = TRUE, osci = TRUE)

#test how well frequencies will be detected. isn't working for one song :(
trackfreqs(SRF08M03, flim = c(2, 10), bp = c(3, 9), it = "tiff")

#batch process acoustic measurements
specan(SRF08M03, bp = c(2,10), threshold = 15)



# whole sound file --------------------------------------------------------

sounds <- list.files(pattern = "wav$")

XC_test <- "XC160215 - Townsend's Warbler - Setophaga townsendi.wav"

lspec(flist = XC_test, ovlp = 50, it = "tiff", flim = c(2,8), sxrow = 10, rows = 3)

#autodetect signals, save as object- these settings capture all signals, but doesn't distinguish final notes separately
XC_detec <- autodetec(flist = XC_test, threshold = 10, envt = "abs", ssmooth = 800, wl = 512, bp = c(2,10),
                      res = 100, flim = c(2 , 10), ls = TRUE, sxrow = 10, rows = 3, mindur = 0.01, maxdur = 0.9,
                      redo = TRUE, it = "tiff", set = TRUE, smadj = "both")


#manually creating df from the outputs from Raven
XC <- data.frame(sound.files = c(rep("XC160215 - Townsend's Warbler - Setophaga townsendi.wav", 3)), 
                 selec = c(seq(1,3, by = 1)), start = c(0.715218733, 13.159822065, 23.491903604), 
                 end = c(2.665038445, 14.870656092, 25.244465290))


#detects margins to measure noise around signals
snrspecs(XC, flim = c(2,10), snrmar = 0.5, mar = 1, it = "tiff" )

#calculate SNR
sig2noise(XC, mar = 0.1)

#test how well frequencies will be detected. isn't working for one song :(
trackfreqs(XC, flim = c(2, 10), bp = c(3, 9), it = "tiff")

#batch process acoustic measurements
XC_measure <- specan(XC, bp = c(2,10), threshold = 10)

XC_measure <- XC_measure[, grep("fun|peakf", colnames(XC_measure), invert = TRUE)]


View(XC_measure)



#####recording from 2017 field season#########

SRF08M03_full <- "SRF08M03_1.WAV.wav"


#create a spectrogram image of your sound file, flist = subset of songs you want to analyze,
#       ovlp = overlap of sound windows, it = file type to save image as, flim = c(lower freq bound, upper freq bound)
#       sxrow = seconds per row, rows = #rows
lspec(flist = SRF08M03_full, ovlp = 50, it = "tiff", flim = c(2,8), sxrow = 10, rows = 15)


#autodetect signals, save as object- these settings capture all signals, but doesn't distinguish final notes separately
autodetec(flist = SRF08M03_full, threshold = 8, envt = "abs", ssmooth = 900, wl = 512, bp = c(3,8),
                      res = 100, flim = c(2 , 10), ls = TRUE, sxrow = 10, rows = 15, mindur = 0.01,
                      maxdur = 1,redo = TRUE, it = "tiff", set = TRUE, smadj= "both")


read



# Trials ------------------------------------------------------------------

SRF27M05 <- read.table("data_raw/SRF27M05_2.WAV.Table.1.selections.txt", sep = "\t", header = TRUE)

#removes wave-form data, and unneccasry columns
SRF27M05 <- SRF27M05[SRF27M05$View != "Waveform 1", c("Selection", "Begin.Time..s.","End.Time..s.")]

#adds sound files name
SRF27M05 <- SRF27M05 %>% 
  add_column(sound.files = rep(c("SRF27M05_wav"), nrow(SRF27M05)), .before = 1)

#organizes files in proper warbler format
SRF27M05 <- SRF27M05 %>% 
  rename(selec = Selection, start = Begin.Time..s., end = End.Time..s.)



#set working directory to folder with song in it
setwd(dir = "sound/SRF27M02 test/")
#check that you did that
getwd()

#check that files in wd are readable
checkwavs()

#get file names for songs you want to analyze
SRF27M05_wav <- list.files(pattern = "wav$")

#create a spectrogram image of your sound file, flist = subset of songs you want to analyze,
#       ovlp = overlap of sound windows, it = file type to save image as, flim = c(lower freq bound, upper freq bound)
#       sxrow = seconds per row, rows = #rows
lspec(flist = SRF27M05_wav, ovlp = 50, it = "tiff", flim = c(2,8), sxrow = 10, rows = 15)

#change sound.file name to reflect actual file name, 
######### REARRANGE THIS SO IT GETS ACTUAL FILE NAME WHEN CREATING COLUMN#####
SRF27M05$sound.files <- rep(c(SRF27M05_wav), nrow(SRF27M05))

#detects margins to measure noise around signals
snrspecs(SRF27M05, flim = c(2,10), snrmar = 1, mar = 0.1, it = "tiff" )

#calculate SNR
SNR <- sig2noise(SRF27M05, mar = 0.1)

#test how well frequencies will be detected
trackfreqs(SRF27M05, flim = c(2, 10), bp = c(3.2, 7), it = "tiff", threshold = 50, wl = 700)

#batch process acoustic measurements
SRF27M05_measure <- specan(SRF27M05, bp = c(2,10), threshold = 15)

#pick three songs to analyze
Analysis_potential <- integer()
for (i in 1:nrow(SNR)){
  if (SNR$SNR[i] >= 2){
    Analysis_potential <- c(Analysis_potential, SNR$selec[i])
  }
}

analyze_this <- round(runif(3, min = min(Analysis_potential), max = max(Analysis_potential)),0)
print(analyze_this)


# SRE20M04 ----------------------------------------------------------------

setwd(choose.dir())

SRE20M04 <-read.table("data_raw/SRE20M04.WAV.Table.1.selections.txt", sep = "\t", header = TRUE)

#removes wave-form data, and unneccasry columns
SRE20M04 <- SRE20M04[SRE20M04$View != "Waveform 1", c("Selection", "Begin.Time..s.","End.Time..s.")]

#adds sound files name
SRE20M04 <- SRE20M04 %>% 
  add_column(sound.files = rep(c("SRE20M04.wav"), nrow(SRE20M04)), .before = 1)

#organizes files in proper warbler format
SRE20M04 <- SRE20M04 %>% 
  rename(selec = Selection, start = Begin.Time..s., end = End.Time..s.)

#check that files in wd are readable
checkwavs(path = 'sound/SRE20M05/')

#get file names for songs you want to analyze
SRE20M04_wav <- list.files(pattern = "wav$", path = (choose.dir()))

SRE20M04$sound.files <- rep(c(SRE20M04_wav), nrow(SRE20M04))


#detects margins to measure noise around signals
snrspecs(SRE20M04, flim = c(2,10), snrmar = 0.1, mar = 1, it = "tiff", inner.mar = c(6,5,5,3) )

#calculate SNR
SNR <- sig2noise(SRE20M04, mar = 0.1)

#test how well frequencies will be detected
trackfreqs(SRE20M04, flim = c(2, 10), bp = c(3, 8), it = "tiff", threshold = 20, wl = 700)



# Create spec printouts of songs ------------------------------------------

#set working directory to folder with song in it
setwd(choose.dir())
#check that you did that
parent_folder <- "C:/Users/Madelyn Ore/Documents/UBC Irwin/Song Analysis/TOWA/Field Summer 2017 Data/Filtered_sound"
getwd()

file_names <- list.dirs()

file_names <- file_names[17:27]

for (i in 1:length(file_names)){
  setwd(file_names[i])
  #check that files in wd are readable
  checkwavs()

  #get file names for songs you want to analyze
  songs <- list.files(pattern = "wav$")

  #create a spectrogram image of your sound file, flist = subset of songs you want to analyze,
  #       ovlp = overlap of sound windows, it = file type to save image as, flim = c(lower freq bound, upper freq bound)
  #       sxrow = seconds per row, rows = #rows
  lspec(flist = songs, ovlp = 50, it = "tiff", flim = c(2,8), sxrow = 15, rows = 6, redo = FALSE)
  
  setwd(parent_folder)
}

## for ML and XC
#set working directory to folder with song in it
setwd(choose.dir())
#check that you did that
getwd()

#check that files in wd are readable
checkwavs()

#get file names for songs you want to analyze
songs <- list.files(pattern = "wav$")

lspec(flist = songs, ovlp = 50, it = "tiff", flim = c(2,8), sxrow = 15, rows = 6, redo = FALSE)



# Create pdfs for each song by selection ----------------------------------

#set working directory to folder with song in it
setwd(choose.dir())
#check that you did that
getwd()

#check that files in wd are readable
checkwavs()

#get file names for songs you want to analyze
song <- list.files(pattern = "wav$", path = "sound/")

SRsel <- read.csv(file = "SRF08M03_selecs.csv", header = TRUE)

#create individual spectrograms from selection table
specreator(SRsel, wl = 512, flim= c(3,9), it = "tiff", res =150, osci = TRUE, ovlp = 90)

#work flow:
# compile all selection tables into one spreadsheet --> done in data cleaning script
# create specialized data sheet to go through all song files
TOWAsel <- read.csv(file = "TOWA_warbleR_selec.csv")
TOWAsel <- TOWAsel[,-1]
# get spec printouts of all songs

specreator(TOWAsel, wl = 512, flim= c(3,9), it = "tiff", res =150, osci = TRUE, ovlp = 90)
# create a catalog of all songs

clearsel <- read.csv('data/TOWA_selec_clear.csv')
clearsel <- clearsel[,-1]

catalog(clearsel, wl = 512, flim= c(3,9), it = "jpeg", res = 150, ovlp = 90, nrow= 3, ncol = 3)


