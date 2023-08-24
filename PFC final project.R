library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)



spikeTimes <- read_csv("spikeDataPFC.csv")


spikeTimes %>%
  ggplot() +
  geom_tile(aes(x = spikeTimes, y = rat, width = 10, fill = rat))


rat.ee <- spikeTimes %>%
  filter(rat == "ee") %>%
  ggplot() +
  geom_histogram(aes(x = spikeTimes, fill = "red"), color = "black", bins = 50)
  
rat.ff <- spikeTimes %>%
  filter(rat == "ff") %>%
  ggplot() +
  geom_histogram(aes(x = spikeTimes), fill = "blue", color = "black", bins = 50)


rat.gg <- spikeTimes %>%
  filter(rat == "gg") %>%
  ggplot() +
  geom_histogram(aes(x = spikeTimes), fill = "darkgreen", color = "black", bins = 50)


grid.arrange(rat.ee, rat.ff, rat.gg)








#app




