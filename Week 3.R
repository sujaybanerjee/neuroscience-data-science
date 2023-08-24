library(tidyverse)
library(ggplot2)
library(tidytext)
library(rvest)



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


## class notes 1/24/22

pop <- read_csv("population_total.csv")



pop.long <- pop %>%
  mutate(across(.cols = -country, .fns = ~str_replace(.x,"k", "e3"))) %>% #replaces k with e3
  mutate(across(.cols = -country, .fns = ~str_replace(.x,"M", "e6"))) %>%
  mutate(across(.cols = -country, .fns = ~as.numeric(str_replace(.x,"B", "e9")))) %>%
  pivot_longer(-country, names_to = "Year", values_to = "pop") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 2022)


#join my joined data with pop.long

final.joined.data <- joined.region.data %>%
  inner_join(pop.long)


#let's animate
#install.packages("gganimate")
library(gganimate)
#install.packages("gifski")
#install.packages("av")
#install.packages("png")
library(gifski)
library(av)
library(png)
#step 1 build single frame that looks perfect

graph1 <- final.joined.data %>%
  #filter(Year == 2000) %>%
  ggplot() +
  geom_point(aes(x = income, y = LifeExpectancy, color = Region, size = pop)) +
  scale_x_log10() + 
  theme_bw() + 
  transition_time(Year) +
  labs(title = "Year: {frame_time}")

#check range of years
range(final.joined.data$Year)

animation1 <- animate(graph1, nframes = 223)




#instead, lets pick 3 countries
#canada, china, angola


final.joined.data %>%
  filter(country %in% c("Canada", "China", "Angola")) %>%
  ggplot() +
  geom_line(aes(x = Year, y = income, color = country), size = 2) +
  theme_bw() +
  transition_reveal(Year)


diamonds %>%
  ggplot() +
  geom_point(aes(x = carat, y= price, color = color)) +
  transition_states(color, state_length = 1, transition_length = 1) +
  enter_fade() +
  exit_fade()



#make ggplot  in 3d 
install.packages("rayshader")
library(rayshader)


diamonds.3d <- diamonds %>%
  ggplot() +
  geom_hex(aes(x = carat, y = price)) + 
  scale_fill_viridis_c() +
  theme_bw()
plot_gg(diamonds.3d)
=install.packages("fivethirtyeight",repos =
                   'https://fivethirtyeightdata.github.io/drat/', type
                 = 'source')
library(fivethirtyeight)


#Bechdel data set

#Can we visualize the change in representation of women
#in movies over time?


bechdel %>%
  count(year) %>%
  ggplot() +
  geom_line(aes(year, y = n))

#can I visualize clean test in a given year?
bechdel %>%
  filter(year == 2000) %>%
  ggplot() +
  geom_bar(aes(x = binary))

#all years
bechdel %>%
  #filter(year == 2000) %>%
  ggplot() +
  geom_bar(aes(x = year, fill = clean_test), position = "fill")


bechdel %>%
  ggplot() +
  geom_bar(aes(x = year, fill = clean_test), position = "fill")

bechdel %>%
  mutate(decate = str_sub(year, start = 1, end = 3)) %>%
  ggplot() +
  geom_bar(aes(x = decade, fill = clean_test), position = "fill")



#class notes 1/25/22

install.packages("rnaturalearth")
library(rnaturalearth)
library(ggplot2)
library(tidyverse)


#mapping using simple features (sf)
world <- ne_countries(returnclass = "sf")

#make my map just like any ggplot graph
world %>%
  ggplot() +
  geom_sf(aes(fill = pop_est)) +
  theme_bw()
  
#lets label Vienna, Austria
vienna.data <- tibble(longitude = 48.2082, latitude = 16.3738)


#make my map just like any ggplot graph with vienna point plotted
world %>%
  ggplot() +
  geom_sf(aes(fill = pop_est)) +
  geom_point(data = vienna.data, aes(x = latitude, y = longitude), color = "red",
             size = 3) +
  theme_bw()


#suppose I want a us map
#install.packages("usmap")
library(usmap)

plot_usmap()


###### neuro aside
install.packages("ggseg")
library(ggseg)

ggplot() +
  geom_brain(atlas = dk)

########


#lets scrape in some alcohol data

url <- "https://worldpopulationreview.com/state-rankings/alcohol-consumption-by-state"

library(rvest)

library(rayshader)

alcohol.data <- url %>%
  read_html() %>%
  html_element(xpath = '//*[@id="dataTable"]/div[1]/div/div[1]/div/div[1]/div[2]/table') %>%
  html_table()


#lets rename our columns to make plot us_map() happy
colnames(alcohol.data) <- c("state", "Alcohol")

map1 <- plot_usmap(data = alcohol.data, values = "Alcohol") +
  scale_fill_viridis_c() +
  labs(title = "Alcohol consumption in gallons", fill = "Alcohol consumption (in gallons)")


plot_gg(map1)




#interactive maps!
#install.packages("leaflet")
library(leaflet)

leaflet %>%
  addTiles()


#lets add some markers to our map/leaflet


#lets label Vienna, Austria

vienna.data <- tibble(latitude = 48.2082, longitude = 16.3738, name = "Vienna")

vienna.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~x, lng = ~y, label = ~name, popup = ~name)


#lets plot all of the Taco Bells in the US
fast.food <- read_csv("Datafiniti_Fast_Food_Restaurants.csv")

#lets cluster our markers
fast.food %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusteroptions = markerClusterOptions())

#I only want Taco Bell
fast.food %>% 
  filter(name == "Taco Bell") %>%
  filter(province %in% c("VT", "NH", "ME", "CT", "RI")) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(label = ~address)


#Lets make a choropleth (colored map), perhaps with some markers
#Introducing .geojson
install.packages("geojsonio")
library(geojsonio)

states <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_20m.json",
                       what = "sp")

states %>% 
  leaflet() %>%
  #addTiles() %>%
  addPolygons(color = "black") %>%
  setView(-96, 38, 3.5)

#Remake our alcohol consumption map using leaflet
states@data

#need to add the alcohol column to our states@data
states.copy <- states

states.copy@data <- states@data %>%
  left_join(alcohol.data, by = c("NAME" = "state"))
#dont store as states because it will get overwritten
#dont store as states@data because data set will be overwritten through recutsion 
  
#fill/color states by alcohol consumption

color1 <- colorBin(palette = "YlOrRd", domain = states.copy@data$Alcohol, bins = c(0,1,,2,3,4,5))

color2 <- colorNumeric(palette = "YlOrRd", domain = states.copy@data$Alcohol)

#lets check our colors
color1(states.copy@data$Alcohol)

states.copy %>%
  leaflet() %>%
  addPolygons(color = "black", fillColor = ~color2(Alcohol), fillOpacity = 1,
              opacity = 1) %>%
  setView(-96, 38, 3.5)


#lets label our states/polygons
states.copy %>%
  leaflet() %>%
  addPolygons(color = "black", fillColor = ~color2(Alcohol), fillOpacity = 1,
              opacity = 1, label = ~NAME, popup = ~GEO_ID) %>%
  setView(-96, 38, 3.5)



#Class 1/26/22

install.packages("shiny")
library(shiny)




