library(ggplot2)
library(tidyverse)
#spike.df <- read_csv("spikeTime_sim2.csv")
spike.df <- read_csv("spikeTimes_example2_FR.csv")

ggplot(data = spike.df) + 
  geom_tile(mapping = aes(x = sp1, y = '')) + 
              xlab("Time (ms)") +
              ylab("Cell") +
  ggtitle("Spike Times") +
  theme_bw()

#There is not a whole lot of variability. There are some reasons to 
#suspect that the firing rate won't vary drastically 
#No large white spaces, so expect FF to be lower than 1


#The second data shows a much less dense spike train. There are lots of faint spikes,
#but not as many concentrated ones. It looks more variable than the previous data and 
#expect FF to be greater than 1

spike.df %>%
  summarize((num = (length(sp1)/10000)*1000))

bins <- cut(spike.df$sp1, breaks = seq(0,10000, by = 500), dig.lab = 5)
df <- data.frame(table(bins)) %>%
  mutate(fire.rate = Freq*2)


ggplot(data = df) +
  geom_col(mapping = aes(x = bins, y = fire.rate), fill = "darkblue") +
  ylab("Firing Rate (Hz)") +
  xlab("Time (ms)") +
  ggtitle("Firing Rate vs. Time Interval") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


FF <- df %>%
  summarize((var(Freq))/mean(Freq))
FF

df %>%
  summarize(mean(Freq))

#Since the FF is less than 1 (0.27), it is more regular than Poisson
#This means that there is much less variability in the spike count



#The FF is 1.28, which is less regular than Poisson. There is a lot of variability 
#in the spike count







         



