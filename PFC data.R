#install.packages("R.matlab")
library(R.matlab)
library(ggplot2)
library(tidyverse)





rawData.ee <- readMat("EE049_Behavior_PFC.mat")
spikes1.ms <- data.frame(spikeTimes = rawData.ee$spiket * 0.05)
rat1.times <-data.frame(trialTimes = rawData.ee$SessionNP)

rawData.ff <- readMat("FF103_Behavior_PFC.mat")
spikes2.ms <- data.frame(spikeTimes = rawData.ff$spiket * 0.05)
rat2.times <-data.frame(trialTimes = rawData.ff$SessionNP)

rawData.gg <- readMat("GG069_Behavior_PFC.mat")
spikes3.ms <- data.frame(spikeTimes = rawData.gg$spiket * 0.05)
rat3.times <-data.frame(trialTimes = rawData.gg$SessionNP)



spikes.df <- data.frame(spikeTimes = NULL, rat = NULL)


data1 <- spikes1.ms %>%
  mutate(rat = "ee")
spikes.df <- rbind(spikes.df, data1)

data2 <- spikes2.ms %>%
  mutate(rat = "ff")
spikes.df <- rbind(spikes.df, data2)

data3 <- spikes3.ms %>%
  mutate(rat = "gg")
spikes.df <- rbind(spikes.df, data3)

data4 <- rat1.times %>%
  mutate(trialID = rownames(rat1.times))
spikes.df <- rbind(spikes.df, data4)


for (i in 1:19)
{
  data <- rat1.times %>%
    mutate(trialID = factor(i))
  spikes.df <- rbind(data, rat1.times)
}



#write_csv(rat1.times, "PFC_allData.csv")
#write_csv(rat2.times, "PFC_allData2.csv")
#write_csv(rat3.times, "PFC_allData3.csv")
