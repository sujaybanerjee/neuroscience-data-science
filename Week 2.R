library(tidyverse)
library(ggplot2)

#Class Notes 1/18/22

#read profiles data into R
profiles <- read_csv(file.choose())


#instead we can use file paths
#click on file in finder and then option command c
profiles <- read_csv("/Users/suj/Desktop/DataScience/profiles.csv")

#also we can set working directory to a folder
setwd("/Users/suj/Desktop/DataScience/")
profiles <- read_csv("profiles.csv")


#Distrubution of heights of respondents
profiles %>%
  ggplot() +
  geom_boxplot(aes(x = height))


#lets narrow our graph a bit
profiles %>%
  ggplot() +
  geom_boxplot(aes(x = height, y = sex)) +
  xlim(48, 96)


#lets investigate further
profiles %>%
  ggplot() +
  geom_bar(aes(x = height, fill = sex)) +
  xlim(48, 96) +
  facet_wrap(~sex)


#count m vs f
profiles %>%
  count(sex)


#whats going on with the body type
profiles %>%
  count(body_type) %>%
  arrange(-n)

#lets graph this
profiles %>%
  count(body_type) %>%
  arrange(-n) %>%
  ggplot() + 
  geom_col(mapping = aes(x = reorder(body_type, n), y = n))


#lets dig into essays

#Do people who like to play soccer have different body types
#than those who dont mention soccer at all?


#########
#Toy example aside:
toy.vector <- c("Alex", "Becky", "Charlie")

#introducing str_detect()
#detects if given string is present (case sensitive)
str_detect(toy.vector, "A")

#Lets replace "A" with :)
str_replace(toy.vector, "A", ":)")
str_replace_all(toy.vector, "A", ":)")
##########


#Lets make a new variable for people who like soccer

soccer.data <- profiles %>%
  mutate(soccer = str_detect(essay0, "soccer"))
  
#Lets check that this worked
yes.soccer <- soccer.data %>%
  filter(soccer == TRUE)


#lets make our graph
#Count up each combination of body_type and soccer
soccer.data %>%
  count(body_type, soccer) %>%
  na.omit()


#bar graph
soccer.data %>%
  count(body_type, soccer) %>%
  na.omit() %>%
  ggplot() +
  geom_col(aes(x = body_type, y = n, fill = soccer))


#sanity check (think ab flights activity)
#what proportion of people like soccer

soccer.data %>%
  group_by(body_type) %>%
  summarize(prop = mean(soccer, na.rm = TRUE))


soccer.data %>%
  group_by(soccer) 


#above code is tricker than expected so we can use geom_bar()

soccer.data %>%
  filter(!is.na(soccer)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = soccer, fill = body_type),
           color = "black",
           position = "fill") + #shows proportions instead of counts
  theme_bw()


#what about the relationship between being with friends on Friday nights
#and doing drugs?


friends.data <- profiles %>%
  mutate(social = str_detect(essay7, "friends"))

yes.friends <- friends.data %>%
  filter(social = TRUE)


friends.data %>%
  count(drugs, social) %>%
  na.omit()


friends.data %>% 
  group_by(drugs) %>%
  summarize(prop = mean(social, na.rm = TRUE))


friends.data %>%
  filter(!is.na(social)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = social, fill = drugs),
           position = "fill") + #shows proportions instead of counts
  theme_bw()



#what about the relationship between being with friends on Friday nights
#and drinking?


friends.data <- profiles %>%
  mutate(social = str_detect(essay7, "friends"))

yes.friends <- friends.data %>%
  filter(social = TRUE)


friends.data %>%
  count(drinks, social) %>%
  na.omit()


friends.data %>% 
  group_by(drinks) %>%
  summarize(prop = mean(social, na.rm = TRUE))


friends.data %>%
  filter(!is.na(social)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = social, fill = drinks),
           position = "fill") + #shows proportions instead of counts
  theme_bw()




#Class notes 1/19/22
install.packages("tidytext")
library(tidytext)

#introducing tokenizing text


#Lets find the most common words in essay2
essay2.tokens <- profiles %>% 
  unnest_tokens(input = "essay2", output = "word")


top.words <- essay2.tokens %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words)
  #filter(!(word %in% c("a", "the", "to", "at")))


###### messin around
essay7.tokens <- profiles %>% 
  unnest_tokens(input = "essay7", output = "word")


read.write <- essay7.tokens %>%
  count(word) %>%
  arrange(-n) %>%
  filter(word %in% c("reading", "writing"))

essay7.tokens <- profiles %>% 
  unnest_tokens(input = "essay7", output = "word")


top.words <- essay7.tokens %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words)



intellect.data <- profiles %>%
  mutate(nerd = str_detect(essay7, "reading", "writing"))
############
  


#intro to web scraping
#something exists on the internet, and I want it!

install.packages("rvest")
library(rvest)

url <- "https://en.wikipedia.org/wiki/Joe_Biden"

biden.text <- url %>%
  read_html() %>%  #reads the HTML code of the url/web
  html_elements("p") %>% # only look at elements i care about
  html_text() #convert those elements form gross HTML to meaningful R object
  
  
#lets look at 10th paragraph  of biden text
biden.text[10]
  
#lets look at the most common words

#turn biden.text vector into data set instead
biden.data <- tibble(text = biden.text)

biden.data %>%
  unnest_tokens(input = "text", output = "word") %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(-n)



#lets scrape in a data set
brady.url <- "https://en.wikipedia.org/wiki/Tom_Brady"

brady.data <- brady.url %>%
  read_html() %>%
  html_element(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table()

#replace column names with values in first row of data
colnames(brady.data) <- paste(colnames(brady.data), brady.data[1, ]) #blank means all cols


#Get rid of first and last row
brady.data2 <- brady.data[-c(1,24), ]
#bad way to do this because it will overwrite and recursively get rid
#of too many lines if running multiple times
#only way to go back is to rescrape
#instead make it a new data set



#lets look at bradys passing yards over time
brady.data2 %>%
  mutate(passing.yards.numeric = as.numeric(str_remove_all(`Passing Yds`, "[:punct:]"))) %>%
  ggplot() +
  geom_col(aes(x = `Year Year`, y = passing.yards.numeric))


#lets look at bradys passing yards over time
brady.data2 %>%
  mutate(passing.yards.numeric = as.numeric(str_remove_all(`Passing Yds`, "[:punct:]"))) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(`Year Year`), y = passing.yards.numeric))







#Class Notes 1/20/22

#Lets get some life expectancy data

life <- read_csv("life_expectancy_years.csv")


#lets start by making basic graph of life expectancy for two different countries
#China, Brazil

#first let's reshape data from wide format to long format
#using pivot_longer()

life.long <- life %>%
  pivot_longer(-country, names_to = "Year", values_to = "LifeExpectancy") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)

life.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = LifeExpectancy, color = country), size = 1.5) 



#Lets repeat above process with income
income <- read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")


income.long <- income %>%
  mutate(across(.cols = -country, .fns = ~as.numeric(str_replace(.x,"k", "e3")))) %>%#replaces k with e3
  pivot_longer(-country, names_to = "Year", values_to = "income") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)

income.long %>%
  filter(country %in% c("China", "Brazil")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = income, color = country), size = 1.5) 


#Lets join our income data and our life expectancy data
joined.data <- income.long %>%
  inner_join(life.long, by = c("country", "Year"))

#Lets make one frame of our animation
#year 2000


#only start using scientific notation above this number
options(scipen = 100000)



#scrape in region data
library(rvest)

url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

region.data <- url %>%
  read_html() %>%
  html_element("table") %>%
  html_table()


#How do I join my "joined.data" and "region.data"

joined.region.data <- joined.data %>%
  inner_join(region.data, by = c("country" = "Country"))

#make our graph colored by region
joined.region.data %>%
  filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = income, y = LifeExpectancy, color = Region)) +
  scale_x_log10() 

#How can I figure out which countries are being "deleted"
#anti_join
joined.data %>%
  anti_join(region.data, by = c("country" = "Country")) %>%
  filter( Year == 2000)

