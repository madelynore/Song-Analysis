### Principal Component Analysis
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
sng_c <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "clear" & Source = "Field Season",
                select = 1:ncol(sng))
sng_b <- subset(sng, View == 'Spectrogram 1' & buzzy.or.clear == "buzzy" & Source = "Field Season",
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

##Prelim PCA for clear songs
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
predict(pc_all)

autoplot(pc_all, data = sngs, colors = as.factor('buzzy.or.clear'))

