#Class Notes 1/10/22

1+1

#R works byy calling "functions"
#the first function is the "c" function - - concatenate

#1, 4, and 8 together in a list
#technically a vector with length 3
c(1,4,8)

#Let's store above vector for future use
vector1 = c(1,4,8)

#if i want  to use/call vector 1
vector1

#Let's store above vector for future use
vector1 <- c(1,4,8)

#mean of vectors
mean(x = vector1)

#How does mean function work?
?mean

#we dont need to label arguments/inputs as long as
#we provide them in order
mean(vector1)
mean(x = vector1)

#we can store this mean the same way
mean1 <- mean(vector1)

#optional arguments
student.sleep <- c(1, 4, 8, NA) 

#take mean again
mean(x = student.sleep)


#optional argument to "deal" with missing values
mean(x = student.sleep, na.rm = TRUE)
mean(na.rm = TRUE, x = student.sleep)
mean(na.rm = TRUE, student.sleep)
mean(TRUE, student.sleep) #Error


#Install a new package
install.packages("ggplot2")

#every time we open RStudio and want to use functions/data
#from a package we need to load its library
library("ggplot2")


#ggplot2 package lets us make cool graphs
diamonds

#View diamonds set
view(diamonds)

#whats going on in this data set
?diamonds

#can we visualize the relationship between weight (carat)
#of diamonds and their price?

#creates our blank (grey) canvas
ggplot(data = diamonds)

#what type(s) of graphs should I make?

#two variables: quantitative/continuous and discrete/categorical
#quantitative: values are numbers, and the numbers have mathematical significance

#Here is what the code for EVERY graph in ggplot2 will look like
ggplot(data = xxxx) + 
  geom_xxxxx(mapping = aes(x = xxxxx, y = xxxxx))

#Lets actually make our graph 
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price)) #binded in aesthetics vector

#Lets actually make another graph
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = depth, y = table))
  
#spice up
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price),
             color = "blue",
             size = 4) +
  theme_bw()


### Class notes 1/11

#Here is what the code for EVERY graph in ggplot2 will look like
ggplot(data = xxxx) + 
  geom_xxxxx(mapping = aes(x = xxxxx, y = xxxxx))



#Lets make a graph visualizing the relationship between color of diamond and price
ggplot(data = diamonds) + 
  geom_boxplot(mapping = aes(x = color, y = price))


#Now lets check clarity to see what is causing the weird trends
ggplot(data = diamonds) + 
  geom_boxplot(mapping = aes(x = clarity, y = price))

#Wb carat (and color)?
ggplot(data = diamonds) + 
  geom_boxplot(mapping = aes(x = color, y = carat))


#what if I want to make a graph with 3 or more variables
#carat, price, and color
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price, color = color))

#can we add 4th variable
#can we visualize carat, price, color, and clarity
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price, color = color, shape = clarity))
#this works but not so good because hard to distinguish between color and clarity points


#Faceting
#allows us to pull apart a graph based on variables

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price, color = color)) +
  facet_grid(clarity~color)



#what if we are interested in describing the price of diamonds
ggplot(data = diamonds) + 
  geom_density(mapping = aes(x = price))

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), bins = 100)


#Focus on some of the cheaper diamonds
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = price), bins = 100) + 
  xlim(0, 5000)


#install tidyverse
install.packages("tidyverse")
library("tidyverse")



#the filter() function

#suppose I want to buy only the "best"
#very expensive - >$10,000
filter(.data = diamonds, price > 10000)

#expensive diamonds only of the best color (D)

filter(.data = diamonds, price > 10000, color == "D")


#cool uses of filter
#I want heavy diamonds(carat >1.5) with 3 worst clarities
filter(.data = diamonds, carat > 1.5 & 
       (clarity == "I1" |
       clarity == "SI1"|
       clarity == "SI2"))


desirable.clarities <- c("I1", "SI1", "SI2")

good.diamonds <- filter(.data = diamonds, carat > 1.5, 
       clarity %in% desirable.clarities)


#the summarize() function
summarize(.data = diamonds)


#calculate the mean price of diamonds
summarize(.data = diamonds, 
          meanPrice = mean(price),
          medianCarat = median(carat))




# 1/12/22 Class Notes


#new function group_by()

grouped.diamonds <- group_by(.data = diamonds, color)

#calculate the mean price of diamonds
summary.stats <- summarize(.data = grouped.diamonds, 
          meanPrice = mean(price),
          medianCarat = median(carat))

#what is the average carat of expensive diamonds <$10000 with colors E and F?
x <- filter(.data = diamonds, price > 10000, color %in% c("E", "F"))
grouped.x <- group_by(.data = x, color)
summarized.x <- summarize(grouped.x, mean = mean(carat))
#cumbersome


#Introducing the pipe operator %>%
#take thing on left and make it first argument of thing on right
vector1 <- c(1,2,3)

mean(vector1)
vector1 %>% mean()



#rewrite cumbersome code with pipe

diamonds %>% 
  filter(price > 10000, color %in% c("E", "F")) %>% 
  group_by(color) %>% 
  summarize(mean = mean(carat))


#lets look at "flights" data set from nycflights13 package
#What is the average delay of flights into Burlington Airport?
#what is the average delay of flights into Logan Airport
#bonus: what proportion of flights are delayed in these airports


install.packages("nycflights13")
library("nycflights13")


flights %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  summarize(avg.delay = mean(dep_delay, na.rm = TRUE))


flights %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  #filter(arr_delay > 0) %>%
  group_by(dest) %>%
  summarize(avg.delay = mean(arr_delay > 0, na.rm = TRUE))
#checks if true and then takes proportion



flights %>%
  filter(dest %in% c("BOS", "BTV")) %>%
  filter(dep_delay > 0) %>%
  group_by(dest) %>%
  summarize(avg.delay = mean(dep_delay, na.rm = TRUE))


flights %>%
  filter(origin %in% c("JFK", "LGA")) %>% 
  ggplot() + 
  geom_violin(mapping = aes(x = origin, y = dep_time, fill = origin))


flights %>%
  group_by(carrier) %>% 
  summarize(mean = mean(distance)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = carrier, y = mean, fill = mean))


flights %>%
  group_by(carrier) %>% 
  summarize(mean = mean(distance)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = carrier, y = mean, fill = mean))




#class notes 1/13/22

#read profiles data into R
library(tidyverse)
profiles <- read_csv(file.choose())


#instead we can use file paths
#click on file in finder and then option command c
profiles <- read_csv("/Users/suj/Desktop/DataScience/profiles.csv")

#also we can set working directory to a folder
setwd("/Users/suj/Desktop/DataScience/")
profiles <- read_csv("profiles.csv")






