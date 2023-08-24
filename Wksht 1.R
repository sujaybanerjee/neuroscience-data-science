library("ggplot2")
library("tidyverse")

ggplot(data = spike.df) + 
  geom_tile(mapping = aes(x = sp1, y = 1, height = 1, width = 10)) 

ggplot(data = spike.df) + 
  geom_histogram(mapping = aes(x = sp1), 
                 binwidth = 1000,
                 color = "blue",
                 fill = "gray") +
                 theme_bw()

ggplot(data = spike.df) + 
  geom_histogram(mapping = aes(x = sp1), 
                 binwidth = 500,
                 color = "red",
                 fill = "gray") +
  theme_bw()

ggplot(data = spike.df) + 
  geom_histogram(mapping = aes(x = sp1), 
                 binwidth = 100,
                 color = "green",
                 fill = "gray") +
  theme_bw()




  
