### Principal Component Analysis


# PCA prelim --------------------------------------------------------------


#find the averages of clear and buzzy songs across the three measurements, link to sample site and plot
#a principal component analysis


library(tidyverse)
library(ggplot2)
library(ggfortify)


#read the song measurements
sng <- read.csv(file = "data/TOWA_song_measurements.csv", header = TRUE,
                 strip.white = TRUE)
#remove unneccessary row names col
sng <- sng[,-1]

#subset df into only the spectrogram output, and separate into buzzy and clear songs
#running only on field season, intially to test
sng_c <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "clear",
                select = 1:ncol(sng))
sng_b <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "buzzy",
                select = 1:ncol(sng))

#find means for all the measured values
m_sng_C <- sng_c %>% 
  group_by(Song.ID.1) %>% 
  summarize(low_freq = mean(Low.Freq..Hz.), high_freq = mean(High.Freq..Hz.),
            agg_entropy = mean(Agg.Entropy..bits.), avg_entropy = mean(Avg.Entropy..bits.),
            bw_90 = mean(BW.90...Hz.), center_freq = mean(Center.Freq..Hz.), 
            delta_time = mean(Delta.Time..s.),freq_5 = mean(Freq.5...Hz.),
            freq_95 = mean(Freq.95...Hz.), max_freq = mean(Max.Freq..Hz.),
            total_num_notes = mean(num.notes), num_unique_notes = mean(num.unique.notes))


m_sng_B <- sng_b %>% 
  group_by(Song.ID.1) %>% 
  summarize(low_freq = mean(Low.Freq..Hz.), high_freq = mean(High.Freq..Hz.),
            agg_entropy = mean(Agg.Entropy..bits.), avg_entropy = mean(Avg.Entropy..bits.),
            bw_90 = mean(BW.90...Hz.), center_freq = mean(Center.Freq..Hz.), 
            delta_time = mean(Delta.Time..s.),freq_5 = mean(Freq.5...Hz.),
            freq_95 = mean(Freq.95...Hz.), max_freq = mean(Max.Freq..Hz.),
            total_num_notes = mean(num.notes), num_unique_notes = mean(num.unique.notes))

#add song type to the dataframes-- to be used to index later
m_sng_C$buzzy.or.clear <- rep("clear", nrow(m_sng_C))
m_sng_B$buzzy.or.clear <- rep("buzzy", nrow(m_sng_B))


# Prelim PCA for clear songs ----------------------------------------------


#removes na values from analysis
m_sng_C <- na.omit(m_sng_C)

# run variance check -- variances are vvery different
c <- var(m_sng_C)
diag(c)

#run PCA, scale = TRUE means that it is run using a correlation matrix (not covar matrix)
#good to standarize variances
pc_clear <- prcomp(m_sng_C[,2:13], scale. = TRUE)
#plots the PCA and generates arrows in the direction that each variable pulls the points
biplot(pc_clear, cex = 0.7)

#summarize eigenvalues
summary(pc_clear)
screeplot(pc_clear, type = "lines")

#obtain PC scores
predict(pc_clear)

#eigenvalues
pc_clear$sdev

#running prelim PCA for buzzy songs
#removes na values from analysis
m_sng_B <- na.omit(m_sng_B)

# run variance check -- variances are vvery different
b <- var(m_sng_B)
diag(b)

#run PCA, scale = TRUE means that it is run using a correlation matrix (not covar matrix)
#good to standarize variances
pc_buzzy <- prcomp(m_sng_B[,2:13], scale. = TRUE)
#plots the PCA and generates arrows in the direction that each variable pulls the points
biplot(pc_buzzy, cex = 0.7)

#summarize eigenvalues
summary(pc_buzzy)
screeplot(pc_buzzy, type = "lines")

#obtain PC scores
predict(pc_buzzy)

#eigenvalues
pc_buzzy$sdev


# PCA for buzzy vs clear --------------------------------------------------


#combining buzzy and clear
sngs <- full_join(m_sng_B, m_sng_C)

# run variance check -- variances are vvery different
v <- var(sngs)
diag(v)

#run PCA, scale = TRUE means that it is run using a correlation matrix (not covar matrix)
#good to standarize variances
pc_all <- prcomp(sngs[,2:13], scale. = TRUE)
#plots the PCA and generates arrows in the direction that each variable pulls the points
biplot(pc_all, cex = 0.7)

#summarize eigenvalues
summary(pc_all)
screeplot(pc_all, type = "lines")

#obtain PC scores
scores <- predict(pc_all)
#obtain PC values
pc_all$rotation

#plots PCA, coloring based on buzzy or clear
autoplot(pc_all, data = sngs, colour = 'buzzy.or.clear', loadings = TRUE, loadings.label = TRUE)



# PCA for field season ** by location **----------------------------------------------------
#subset data, so its only clear songs from the field season,
#only using spectrogram because has the most accurate/complete measurements, the others are duplicates 
cl_fs <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "clear" & Source == "Field Season",
                select = 1:ncol(sng))

# bu_fs <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "buzzy" & Source == "Field Season",
       # select = 1:ncol(sng))
#create avg output -- tidyverse 
m_clear <- cl_fs %>% 
  group_by(Song.ID.1) %>% 
  summarize(low_freq = mean(Low.Freq..Hz.), high_freq = mean(High.Freq..Hz.),
            agg_entropy = mean(Agg.Entropy..bits.), avg_entropy = mean(Avg.Entropy..bits.),
            bw_90 = mean(BW.90...Hz.), center_freq = mean(Center.Freq..Hz.), 
            delta_time = mean(Delta.Time..s.),freq_5 = mean(Freq.5...Hz.),
            freq_95 = mean(Freq.95...Hz.), max_freq = mean(Max.Freq..Hz.),
            total_num_notes = mean(num.notes), num_unique_notes = mean(num.unique.notes))


#read song data to get sample site
fs <- read.csv(file = "data/song_data_2017.csv", stringsAsFactors = FALSE) 
#create a space for location in the mean measurements df 
m_clear$location <- vector(length = nrow(m_clear))

#fill location column with location information, indexing from song info table
for (i in 1:nrow(m_clear)){
  num <- which(fs$Song.ID == m_clear$Song.ID.1[i])
  m_clear$location[i] = fs$Sample.Site[num]
}

#create a space for the coast vs interior songs
m_clear$c_vs_i <- vector(length = nrow(m_clear))
#fill with coast or interior information
for (i in 1:nrow(m_clear)){
  num <- which(fs$Song.ID == m_clear$Song.ID.1[i])
  m_clear$c_vs_i[i] = fs$Coast.or.Interior[num]
}

#create a space for the regional classification
m_clear$region <- vector(length = nrow(m_clear))
#fill with coast or interior information
for (i in 1:nrow(m_clear)){
  num <- which(fs$Song.ID == m_clear$Song.ID.1[i])
  m_clear$region[i] = fs$region[num]
}

##run PCA
m_clear <- na.omit(m_clear)
# run variance check -- variances are vvery different
cl <- var(m_clear)
diag(cl)

#run PCA, scale = TRUE means that it is run using a correlation matrix (not covar matrix)
#good to standarize variances
fs_pc <- prcomp(m_clear[,2:13], scale. = TRUE)
#plots the PCA and generates arrows in the direction that each variable pulls the points
biplot(fs_pc, cex = 0.7)

#summarize eigenvalues
summary(fs_pc)
screeplot(fs_pc, type = "lines")

#obtain PC scores
predict(fs_pc)
#obtain PC values
fs_pc$rotation

#make a factor and order factor as they appear in the data
m_clear$location <- factor(m_clear$location, levels= unique(m_clear$location))

#plots PCA based on the locations of the individuals
autoplot(fs_pc, data = m_clear, colour = 'location', loadings = TRUE, loadings.label = TRUE)

#plots PCA based on coast vs interior
autoplot(fs_pc, data = m_clear, colour = 'c_vs_i', loadings = TRUE, loadings.label = TRUE)

#plot PCA based on region
autoplot(fs_pc, data = m_clear, colour = 'region')


# Correlation matrix ------------------------------------------------------


###correlation matrix ###
cor_clear <- round(cor(m_clear[,2:13]), 2)
heatmap(cor_clear)




# DFA ---------------------------------------------------------------------


#method 1. using MASS library linear discriminant function analysis -- 501
library(MASS)


# DFA by buzzy.clear ------------------------------------------------------


#run dfa, buzzy vs clear as predicted by other variables
a_dfa <- lda(buzzy.or.clear ~. , data = sngs)
#trait loadings
print(a_dfa)
a_dfa$scaling

#at LDs 1 and 2 to dataframe
sngs <- cbind(sngs, predict(a_dfa)$x[,1:2])
#plot the two LDs and label 
plot(LD2 ~ LD1, data = sngs, col = as.numeric(sngs$buzzy.or.clear))
legend( locator(1), legend = as.character(levels(sngs$buzzy.or.clear)), 
        col = 1:length(levels(sngs$buzzy.or.clear)) )

#ggplot map of same thing 
ggplot(sngs) + geom_point(aes(x = sngs$LD1, y = sngs$LD2, colour = buzzy.or.clear))

# DFA by location ---------------------------------------------------------


###running DFA based on locations, for factors most important in PCA
#remove the id and coast/interior columns
cdata <- m_clear[,2:14]

#run dfa, location as predicted by other variables
c_dfa <- lda(location ~. , data = cdata)
#trait loadings
print(c_dfa)
c_dfa$scaling

#at LDs 1 and 2 to dataframe
m_clear <- cbind(m_clear, predict(c_dfa)$x[,1:2])
#plot the two LDs and label 
plot(LD2 ~ LD1, data = m_clear, col = as.numeric(m_clear$location))
legend( locator(1), legend = as.character(levels(m_clear$location)), 
        col = 1:length(levels(m_clear$location)) )

#ggplot map of same thing 
ggplot(m_clear) + geom_point(aes(x = m_clear$LD1, y = m_clear$LD2, colour = location))


# DFA by region -----------------------------------------------------------


##DFA based on region
reg <- m_clear[,c(2:13,18)]

#run dfa, location as predicted by other variables
reg_dfa <- lda(region ~. , data = reg)
#at LDs 1 and 2 to dataframe
reg <- cbind(reg, predict(reg_dfa)$x[,1:2])

#ggplot map of same thing 
ggplot(reg) + geom_point(aes(x = LD1, y = LD2, colour = region))


# #method 2. using lfda
# library(lfda)
# autoplot(c_fs_dfa, m_clear, colour = 'c_vs_i')


