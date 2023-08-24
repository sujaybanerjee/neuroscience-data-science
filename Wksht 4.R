library(ggplot2)
library(tidyverse)



#create a data frame with 2 columns
#1st holds spike times, 2nd hold neuron number
allSpikes.df <- data.frame(spikeTimes = NULL, cellID = NULL)

spike.df <- read_csv("spikeTime_sim1.csv") %>%
  mutate(cellID = factor(1))
#bind this data frame to the allSpikes one
allSpikes.df <- rbind(allSpikes.df, spike.df)
  
  
spike.df <- read_csv("spikeTime_sim2.csv") %>%
  mutate(cellID = factor(2))
  #bind this data frame to the allSpikes one
allSpikes.df <- rbind(allSpikes.df, spike.df)
  
  
spike.df <- read_csv("spikeTime_sim3.csv") %>%
  mutate(cellID = factor(3))
  #bind this data frame to the allSpikes one
allSpikes.df <- rbind(allSpikes.df, spike.df)
  
  
spike.df <- read_csv("spikeTime_sim4.csv") %>%
  mutate(cellID = factor(4))
  #bind this data frame to the allSpikes one
allSpikes.df <- rbind(allSpikes.df, spike.df)
  
  
spike.df <- read_csv("spikeTime_sim5.csv") %>%
  mutate(cellID = factor(5))
  #bind this data frame to the allSpikes one
allSpikes.df <- rbind(allSpikes.df, spike.df)



#spike times for all neurons
ggplot(data = allSpikes.df) + 
  geom_tile(mapping = aes(x = spikes, y = cellID), width = 10, height = .5) + 
  xlab("Time (ms)") +
  ylab("Cell Number") +
  ggtitle("Spike Times") +
  theme_bw()


#ISI

time <- diff(allSpikes.df$spikes) 
time <- time[time>0]
df.ISI <- data.frame(time)

#synchrony 
min(df.ISI$time)
#20.34ms bin size


allSpikes.df %>% 
  group_by(cellID) %>%
  summarize(sd(Freq)/mean(Freq))


ggplot(data = df.ISI) + 
  geom_histogram(mapping = aes(x = time), bins = 100, fill = "royalblue", color = "black") +
  xlab("Time Differences (ms)") +
  ylab("Count") +
  ggtitle("Interspike Interval For All Neurons") +
  theme_classic()

#CV
CV <- df.ISI %>%
  summarize(sd(time)/mean(time))
CV

#Firing Rate

bins <- cut(allSpikes.df$spikes, breaks = seq(0,10000, by = 20), dig.lab = 5)
df.fire <- data.frame(table(bins)) %>%
  mutate(count = Freq)


#overall firing rate
ggplot(data = df.fire) +
  geom_col(mapping = aes(x = bins, y = fire.rate.per.neuron), fill = "darkblue") +
  ylab("Firing Rate (Hz)") +
  xlab("Time (ms)") +
  ggtitle("Avg. Firing Rate vs. Time Interval Per Neuron") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#synchrony firing rate
ggplot(data = df.fire) +
  geom_col(mapping = aes(x = bins, y = Freq), fill = "darkblue") +
  ylab("Spike Count") +
  xlab("Time (ms)") +
  ggtitle("Avg. Firing Rate vs. Time Interval Per Neuron for Synchronicity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#probability of firing
ggplot(data = df.fire) +
  geom_col(mapping = aes(x = bins, y = Freq/5),fill = "darkgreen") +
  ylab("Probability") +
  xlab("Time (ms)") +
  ggtitle("Probability of Firing for Neurons") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#line graph
ggplot(data = df.fire) +
  geom_line(mapping = aes(x = seq(20,10000, by = 20), y = Freq)) +
  xlab("Time (ms)") +
  ggtitle("Firing Rate vs. Time") +
  theme_classic()
  

newDF <- allSpikes.df %>%
  group_by(cellID) %>%
  mutate(bins = cut(spikes, seq(0,10000,500), dig.lab = 5)) %>%
  count(cellID, bins) %>%
  mutate(FR = n*2)


allSpikes.df %>%
  group_by(cellID) %>%
  mutate(bins = cut(spikes, seq(0,10000,500), dig.lab = 5)) %>%
  count(cellID, bins) %>%
  mutate(FR = n*2) %>% 
  ggplot() +
  geom_col(mapping = aes(x = bins, y = FR, fill = cellID)) +
  facet_grid(cols = vars(cellID)) +
  ylab("Firing Rate (Hz)") +
  xlab("Time (ms)") +
  ggtitle("Firing Rate vs. Time Interval Per Neuron") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




#FF
FF <- df.fire %>%
  summarize((var(Freq))/mean(Freq))
FF

FF.for.each <- newDF %>% 
  group_by(cellID) %>%
  summarize(var(FR)/mean(FR))
FF.for.each

#spike count per neuron
allSpikes.df %>% 
  group_by(cellID) %>%
  summarize(numSpikes = length(cellID)) %>%
  ggplot() + 
  geom_col(aes(x = cellID, y = numSpikes, fill = cellID)) + 
  ylab("Spike Count") +
  xlab("Cell Number") +
  ggtitle("Spike Count for Each Neuron")








