library(ggplot2)
library(tidyverse)
spike.df <- read_csv("spikeTimes_example2_FR.csv")
#spike.df <- read_csv("spikeTime_sim2.csv")

sp <- diff(spike.df$sp1)
df <- data.frame(sp)

ggplot(data = df) + 
  geom_histogram(mapping = aes(x = sp), bins = 30, fill = "royalblue", color = "black") +
  xlab("Time Differences (ms)") +
  ylab("Count") +
  ggtitle("Interspike Interval") +
  theme_classic()



#The ISI graph is skewed right which indicates that the interspike interval is 
#concentrated around 15-40 ms. There are very few intervals above 200 and there are some outliers
#at much higher time differences


#In the second data, the ISI graph is skewed right again and it is concentrated around 25-60 ms time 
#differences. Around 140 ms there is a large gap before the outliers around 200 ms. There is variability
#than the previous data



df %>%
  summarize(sd(sp)/mean(sp))

#The CV is 1.14 which indicates that our ISI distribution is less regular than Poisson
#This means that the time intervals are more variable and lead to the bigger the gaps that
#we can see in our distribution



#The CV of the second data is 0.55, which indicates that our ISI distribution is more regular
#than Poisson. The time intervals are less variable and lead to smaller gaps and spread of data.

