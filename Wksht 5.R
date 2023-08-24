setwd('/Users/suj/Desktop/DataScience')
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)

spikes.df <- data.frame(spikeTimes = NULL, cellID = NULL)


for (i in 1:10)
{
  data <- read_csv(paste0 ("spikeTimes_neuron", toString(i), "_M1Cortex.csv")) %>%
    mutate(cellID = factor(i))
  spikes.df <- rbind(spikes.df, data)
}

spikes.df <- spikes.df[c("spikes", "cellID")]

#graph
plot <- ggplot(spikes.df) + 
  geom_tile(mapping = aes(x = spikes, y = cellID, width = .05, fill = cellID)) + 
  xlab("Time (ms)") +
  ylab("Cell Number") +
  ggtitle("Network Spike Times") +
  theme_bw()


plot + annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 11,
                 alpha = .3, color = "black", fill = "gray")



new <- spikes.df %>% group_by(cellID) %>% 
  mutate(a = cut(spikes, breaks = seq(0,10,.1), dig.lab = 5)) %>%
  summarize(count = as.vector(table(a)), bins = as.vector(names(table(a))))

fire.rate <- ggplot(new) +
  geom_col(mapping = aes(x = bins, y = count, fill = cellID)) +
  ylab("Count") +
  xlab("Time (s)") +
  ggtitle("Firing Rate") +
  scale_x_discrete(labels = NULL) +
  theme_classic()

fire.rate + annotate("rect", xmin = 35, xmax = 45, ymin = 0, ymax = 15,
           alpha = .3, color = "black", fill = "gray")


#FF
ff.spike1 <- spikes.df %>%
  filter(spikes < 3.5)

ff.spike2 <- spikes.df %>%
  filter(spikes > 3.5, spikes < 4.5)

ff.spike3 <- spikes.df %>%
  filter(spikes > 4.5)

FF1 <- var(ff.spike1$spikes)/mean(ff.spike1$spikes)
FF1

FF2 <- var(ff.spike2$spikes)/mean(ff.spike2$spikes)
FF2

FF3 <- var(ff.spike3$spikes)/mean(ff.spike3$spikes)
FF3



#Firing rate

nrow(filter(filter(spikes.df, spikes < 3.5)))/3.5 #46.6 spikes/sec
nrow(filter(filter(spikes.df, spikes > 3.5, spikes < 4.5))) #70 spikes/sec
nrow(filter(filter(spikes.df, spikes > 4.5)))/5.5 #46.4 spikes/sec

FR <- ggplot(spikes.df) + 
  geom_histogram(mapping = aes(x = spikes, fill = cellID), bins = 100) + 
  xlab("Time (s)") +
  ylab("Firing Rate") +
  ggtitle("Network Firing Rate") +
  theme_bw()
FR

FR + annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 12.5,
           alpha = .3, color = "black", fill = "gray")


#min diff
diff.spikes <- spikes.df %>%
  group_by(cellID) %>%
  summarize(diff = diff(spikes))
min(diff.spikes$diff)


#synchrony 
synchrony <- ggplot(spikes.df) +
  geom_histogram(aes(x = spikes,y = ..count../10, fill = cellID), bins = 488) +
  ylab("Probability of Firing") +
  xlab("Time (s)") +
  ggtitle("Probability of Firing for Neurons") +
  theme_classic()

synchrony + annotate("rect", xmin = 3.5, xmax = 4.5, ymin = 0, ymax = 1,
           alpha = .3, color = "black", fill = "gray")


#ISI

diff.spikes1 <- spikes.df %>%
  group_by(cellID) %>%
  filter(spikes < 3.5) %>%
  summarize(diff = diff(spikes))

CV1 <- sd(diff.spikes1$diff)/mean(diff.spikes1$diff)
CV1

diff.spikes2 <- spikes.df %>%
  group_by(cellID) %>%
  filter(spikes > 3.5, spikes < 4.5) %>%
  summarize(diff = diff(spikes))

CV2 <- sd(diff.spikes2$diff)/mean(diff.spikes2$diff)
CV2

diff.spikes3 <- spikes.df %>%
  group_by(cellID) %>%
  filter(spikes > 4.5) %>%
  summarize(diff = diff(spikes))

CV3 <- sd(diff.spikes3$diff)/mean(diff.spikes3$diff)
CV3



ISI.pre <- ggplot(diff.spikes1) +
  geom_histogram(aes(x = diff, fill = cellID), bins = 100) +
  ylab("Count") +
  xlab("Time (s)") +
  ggtitle("Interspike Interval for Neurons Before Stimulus") +
  theme_classic()
ISI.pre

ISI.during <- ggplot(diff.spikes2) +
  geom_histogram(aes(x = diff, fill = cellID), bins = 100) +
  ylab("Count") +
  xlab("Time (s)") +
  ggtitle("Interspike Interval for Neurons During Stimulus") +
  theme_classic()
ISI.during

ISI.post <- ggplot(diff.spikes3) +
  geom_histogram(aes(x = diff, fill = cellID), bins = 100) +
  ylab("Count") +
  xlab("Time (s)") +
  ggtitle("Interspike Interval for Neurons After Stimulus") +
  theme_classic()
ISI.post

grid.arrange(ISI.pre, ISI.during, ISI.post)




  
