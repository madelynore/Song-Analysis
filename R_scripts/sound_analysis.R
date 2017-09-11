#Madelyn Ore
#March 2017

library(warbleR)
library(readxl)

XC_db <- read_excel("data_raw/xeno-canto v1.xls", col_names = TRUE) 
names(XC_db) <- make.names(names(XC_db))

setwd(tempdir())

TOWA <- querxc(qword="Setophaga townsendi", download = FALSE, X= XC_db,
               path = "sound")
View(TOWA)

sound_info <- read_excel("data/sound_file_data.xlsx", sheet=1, col_names = TRUE)
names(sound_info) <- make.names(names(sound_info))
View(sound_info)



sound_info %>% 
  lspec(flim=c(1.5,11))

data(list=c("XC137649"))
XC137649 <- readWave("sound_files/XC137649 - Townsend's Warbler - Setophaga townsendi.wav")


ad <- autodetec(sound_info, threshold = 20)

setwd(tempdir())
data(list = c("XC13749"))
writeWave(XC137649,"XC137649.wav")
ad1 <- autodetec(threshold = 5, env = "hil", ssmooth = 300, power=1,
                bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE,
                wl = 300, ls = FALSE, sxrow = 2, rows = 4, mindur = 0.1, maxdur = 1, set = TRUE)
#run it with different settings
ad2 <- autodetec(threshold = 90, env = "abs", ssmooth = 300, power = 1, redo = TRUE,
                bp=c(2,9), xl = 2, picsize = 2, res = 200, flim= c(1,11), osci = TRUE,
                wl = 300, ls = FALSE, sxrow = 2, rows = 4, mindur=0.1, maxdur=1, set = TRUE)
#check this folder!!
getwd()
View(tempdir())
