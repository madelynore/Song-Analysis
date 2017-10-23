# song analysis 

###Randomize Location Order ###

#set working directory to folder
setwd(parent_folder)
#list folders with location names
locations <- list.dirs()
locations <- locations[2:27]

#make reproducible
seed(001)
#random order locations
order_locations <- sample(locations)

#creates master order spreadsheet
Master_order <- data.frame(locations = c(order_locations), song_order = vector(mode = "character", 
                                                                        length = length(locations)),
                           stringsAsFactors = FALSE)

#strips away unncessary elements to get song ID
uniquify <- function(x){
  x <- unlist(strsplit(x, ".wav"))
  x <- unlist(strsplit(x, "_[0-9]"))
  x <- unlist(strsplit(x, ".WAV"))
  x <- unique(x[x != ""])
  return(x)
}

### random order of song IDs ###
setwd(parent_folder)
for (i in 1:length(Master_order$locations)){
  setwd(Master_order$locations[i])
  songs <- list.files(pattern = "wav$")
  songs <- uniquify(songs)
  samp <- sample(songs)
  Master_order$song_order[i] <- toString(samp)
  setwd(parent_folder)
}

setwd(choose.dir())

write.csv(x = Master_order, file = "data/order_of_analysis.csv")


TOWA <-read.csv(file = "data/song_data_2017.csv", strip.white = TRUE)

#sets songs as integers
TOWA$Num.clear.songs <- as.integer(TOWA$Num.clear.songs)

#creates column for songs to analyze
TOWA$clear.songs.analyze <- character(length = nrow(TOWA))

#loop to randomly select 3 songs to analyze based on how many songs in the recording
for (i in 1:length(TOWA$Num.clear.songs)){
  if (TOWA$Num.clear.songs[i] <= 3 | is.na(TOWA$Num.clear.songs[i])){
    next
  }else{
    TOWA$clear.songs.analyze[i] <- paste(sample(TOWA$Num.clear.songs[i], size= 3, replace = FALSE ), collapse = ",")
  }
}

#makes the above a function
rand_songs <- function(n, output){
  for (i in 1:length(n)){
    if (n[i] <= 3 | is.na(n[i])){
      next
    }else{
      output[i] <- paste(sample(n[i], size = 3, replace = FALSE), collapse = ",")
    }
  }
}

write.csv(TOWA, file = "data/song_data_2017_v5.csv")


sample(14, size=3)

