#test
library(shiny)
library(shinyWidgets)
library(ggseg)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinydashboard)

rawData <- read.csv("/Users/suj/Desktop/DataScience/PFC_project/spikeDataPFC.csv")

Data2 <- read.csv("/Users/suj/Desktop/DataScience/PFC_project/PFC_allData.csv")
Data3 <- read.csv("/Users/suj/Desktop/DataScience/PFC_project/PFC_allData2.csv")
Data4 <- read.csv("/Users/suj/Desktop/DataScience/PFC_project/PFC_allData3.csv")

rawData$spikeTimes = rawData$spikeTimes/1000


rawData_ee <- rawData %>%
  filter(rat == "ee")

rawData_ff <- rawData %>%
  filter(rat == "ff")

rawData_gg <- rawData %>%
  filter(rat == "gg")


rawData <- rawData%>%
  mutate(trial_number = 0)

rawData <- rawData%>%
  mutate(direction = 0)




#rat ee
rawData <- rawData%>%
  mutate(direction = 0)

rawData <- rawData%>%
  mutate(trial_number = 0)

rawData_1 <- rawData%>%
  filter(spikeTimes >= 64, spikeTimes <=105, rat == "ee")
rawData_1$trial_number = "1"
rawData_1$direction = "L"

rawData_2 <- rawData%>%
  filter(spikeTimes >= 311.0944, spikeTimes <=321.803, rat == "ee")
rawData_2$trial_number = "2"
rawData_2$direction = "R"

rawData_3 <- rawData%>%
  filter(spikeTimes >= 348.9648, spikeTimes <=358.465, rat == "ee")
rawData_3$trial_number = "3"
rawData_3$direction = "R"

rawData_4 <- rawData%>%
  filter(spikeTimes >= 391.8832, spikeTimes <=445.348, rat == "ee")
rawData_4$trial_number = "4"
rawData_4$direction = "L"

rawData_5 <- rawData%>%
  filter(spikeTimes >= 491.3032, spikeTimes <=503.602, rat == "ee")
rawData_5$trial_number = "5"
rawData_5$direction = "L"

rawData_6 <- rawData%>%
  filter(spikeTimes >= 638.7008, spikeTimes <=649.379, rat == "ee")
rawData_6$trial_number = "6"
rawData_6$direction = "R"

rawData_7 <- rawData%>%
  filter(spikeTimes >= 675.6448, spikeTimes <=691.999, rat == "ee")
rawData_7$trial_number = "7"
rawData_7$direction = "R"

rawData_8 <- rawData%>%
  filter(spikeTimes >= 711.1904, spikeTimes <=722.488, rat == "ee")
rawData_8$trial_number = "8"
rawData_8$direction = "R"

rawData_9 <- rawData%>%
  filter(spikeTimes >= 744.0280, spikeTimes <=768.811, rat == "ee")
rawData_9$trial_number = "9"
rawData_9$direction = "L"

rawData_10 <- rawData%>%
  filter(spikeTimes >= 785.0464, spikeTimes <=798.029, rat == "ee")
rawData_10$trial_number = "10"
rawData_10$direction = "R"

rawData_11 <- rawData%>%
  filter(spikeTimes >= 860.1576, spikeTimes <=871.157, rat == "ee")
rawData_11$trial_number = "11"
rawData_11$direction = "L"

rawData_12 <- rawData%>%
  filter(spikeTimes >= 904.4096, spikeTimes <=917.414, rat == "ee")
rawData_12$trial_number = "12"
rawData_12$direction = "L"

rawData_13 <- rawData%>%
  filter(spikeTimes >= 1165.4360, spikeTimes <=1183.957, rat == "ee")
rawData_13$trial_number = "13"
rawData_13$direction = "R"

rawData_14 <- rawData%>%
  filter(spikeTimes >= 1229.3016, spikeTimes <=1238.737, rat == "ee")
rawData_14$trial_number = "14"
rawData_14$direction = "L"

rawData_15 <- rawData%>%
  filter(spikeTimes >= 1305.9616, spikeTimes <=1319.149, rat == "ee")
rawData_15$trial_number = "15"
rawData_15$direction = "L"

rawData_16 <- rawData%>%
  filter(spikeTimes >= 1343.4128, spikeTimes <=1360.117, rat == "ee")
rawData_16$trial_number = "16"
rawData_16$direction = "R"

rawData_17 <- rawData%>%
  filter(spikeTimes >= 1384.1984, spikeTimes <=1397.521, rat == "ee")
rawData_17$trial_number = "17"
rawData_17$direction = "R"

rawData_18 <- rawData%>%
  filter(spikeTimes >= 1410.6936, spikeTimes <=1423.736, rat == "ee")
rawData_18$trial_number = "18"
rawData_18$direction = "R"

rawData_19 <- rawData%>%
  filter(spikeTimes >= 1440.1768, spikeTimes <=1461.382, rat == "ee")
rawData_19$trial_number = "19"
rawData_19$direction = "L"



#rat gg
rawData <- rawData%>%
  mutate(direction = 0)

rawData <- rawData%>%
  mutate(trial_number = 0)

rawData_1_gg <- rawData%>%
  filter(spikeTimes >= 65.7304, spikeTimes <=87.652, rat == "gg")
rawData_1_gg$trial_number = "1"
rawData_1_gg$direction = "L"

rawData_2_gg <- rawData%>%
  filter(spikeTimes >= 98.9440, spikeTimes <=124.251, rat == "gg")
rawData_2_gg$trial_number = "2"
rawData_2_gg$direction = "L"

rawData_3_gg <- rawData%>%
  filter(spikeTimes >= 133.4088, spikeTimes <=164.791, rat == "gg")
rawData_3_gg$trial_number = "3"
rawData_3_gg$direction = "L"

rawData_4_gg <- rawData%>%
  filter(spikeTimes >= 174.8552, spikeTimes <=191.837, rat == "gg")
rawData_4_gg$trial_number = "4"
rawData_4_gg$direction = "R"

rawData_5_gg <- rawData%>%
  filter(spikeTimes >= 202.7640, spikeTimes <=215.972, rat == "gg")
rawData_5_gg$trial_number = "5"
rawData_5_gg$direction = "R"

rawData_6_gg <- rawData%>%
  filter(spikeTimes >= 229.7448, spikeTimes <=253.637, rat == "gg")
rawData_6_gg$trial_number = "6"
rawData_6_gg$direction = "L"

rawData_7_gg <- rawData%>%
  filter(spikeTimes >= 260.8552, spikeTimes <=275.810, rat == "gg")
rawData_7_gg$trial_number = "7"
rawData_7_gg$direction = "R"

rawData_8_gg <- rawData%>%
  filter(spikeTimes >= 286.8232, spikeTimes <=299.005, rat == "gg")
rawData_8_gg$trial_number = "8"
rawData_8_gg$direction = "R"

rawData_9_gg <- rawData%>%
  filter(spikeTimes >= 320.0944, spikeTimes <=334.534, rat == "gg")
rawData_9_gg$trial_number = "9"
rawData_9_gg$direction = "R"

rawData_10_gg <- rawData%>%
  filter(spikeTimes >= 350.5160, spikeTimes <=383.821, rat == "gg")
rawData_10_gg$trial_number = "10"
rawData_10_gg$direction = "L"

rawData_11_gg <- rawData%>%
  filter(spikeTimes >= 398.0432, spikeTimes <=422.660, rat == "gg")
rawData_11_gg$trial_number = "11"
rawData_11_gg$direction = "L"

rawData_12_gg <- rawData%>%
  filter(spikeTimes >= 433.2632, spikeTimes <=443.245, rat == "gg")
rawData_12_gg$trial_number = "12"
rawData_12_gg$direction = "R"

rawData_13_gg <- rawData%>%
  filter(spikeTimes >= 500.3528, spikeTimes <=519.528, rat == "gg")
rawData_13_gg$trial_number = "13"
rawData_13_gg$direction = "L"

rawData_14_gg <- rawData%>%
  filter(spikeTimes >= 532.4632, spikeTimes <=544.354, rat == "gg")
rawData_14_gg$trial_number = "14"
rawData_14_gg$direction = "R"

rawData_15_gg <- rawData%>%
  filter(spikeTimes >= 558.0128, spikeTimes <=573.222, rat == "gg")
rawData_15_gg$trial_number = "15"
rawData_15_gg$direction = "R"

rawData_16_gg <- rawData%>%
  filter(spikeTimes >= 585.3160, spikeTimes <=609.702, rat == "gg")
rawData_16_gg$trial_number = "16"
rawData_16_gg$direction = "L"

rawData_17_gg <- rawData%>%
  filter(spikeTimes >= 619.4848, spikeTimes <=632.858, rat == "gg")
rawData_17_gg$trial_number = "17"
rawData_17_gg$direction = "R"

rawData_18_gg <- rawData%>%
  filter(spikeTimes >= 648.5200, spikeTimes <=668.574, rat == "gg")
rawData_18_gg$trial_number = "18"
rawData_18_gg$direction = "L"

rawData_19_gg <- rawData%>%
  filter(spikeTimes >= 680.8000, spikeTimes <=712.490, rat == "gg")
rawData_19_gg$trial_number = "19"
rawData_19_gg$direction = "L"

rawData_20_gg <- rawData%>%
  filter(spikeTimes >= 737.5632, spikeTimes <=751.825, rat == "gg")
rawData_20_gg$trial_number = "20"
rawData_20_gg$direction = "R"

rawData_21_gg <- rawData%>%
  filter(spikeTimes >= 782.4464, spikeTimes <=795.250, rat == "gg")
rawData_21_gg$trial_number = "21"
rawData_21_gg$direction = "R"

rawData_22_gg <- rawData%>%
  filter(spikeTimes >= 865.6648, spikeTimes <=885.094, rat == "gg")
rawData_22_gg$trial_number = "22"
rawData_22_gg$direction = "R"

rawData_23_gg <- rawData%>%
  filter(spikeTimes >= 899.0464, spikeTimes <=919.606, rat == "gg")
rawData_23_gg$trial_number = "23"
rawData_23_gg$direction = "L"

rawData_24_gg <- rawData%>%
  filter(spikeTimes >= 947.6296, spikeTimes <=957.905, rat == "gg")
rawData_24_gg$trial_number = "24"
rawData_24_gg$direction = "R"

rawData_25_gg <- rawData%>%
  filter(spikeTimes >= 979.9456, spikeTimes <=1003.404, rat == "gg")
rawData_25_gg$trial_number = "25"
rawData_25_gg$direction = "L"

rawData_26_gg <- rawData%>%
  filter(spikeTimes >= 1032.5688, spikeTimes <=1057.460, rat == "gg")
rawData_26_gg$trial_number = "26"
rawData_26_gg$direction = "L"

rawData_27_gg <- rawData%>%
  filter(spikeTimes >= 1293.9288, spikeTimes <=1306.113, rat == "gg")
rawData_27_gg$trial_number = "27"
rawData_27_gg$direction = "R"

rawData_28_gg <- rawData%>%
  filter(spikeTimes >= 1519.4208, spikeTimes <=1543.935, rat == "gg")
rawData_28_gg$trial_number = "28"
rawData_28_gg$direction = "L"

rawData_29_gg <- rawData%>%
  filter(spikeTimes >= 1761.0032, spikeTimes <=1771.834, rat == "gg")
rawData_29_gg$trial_number = "29"
rawData_29_gg$direction = "L"

rawData_30_gg <- rawData%>%
  filter(spikeTimes >= 1784.5104, spikeTimes <=1804.868, rat == "gg")
rawData_30_gg$trial_number = "30"
rawData_30_gg$direction = "R"

rawData_31_gg <- rawData%>%
  filter(spikeTimes >= 1834.1160, spikeTimes <=1850.062, rat == "gg")
rawData_31_gg$trial_number = "31"
rawData_31_gg$direction = "R"

rawData_32_gg <- rawData%>%
  filter(spikeTimes >= 1870.7008, spikeTimes <=1899.176, rat == "gg")
rawData_32_gg$trial_number = "32"
rawData_32_gg$direction = "L"

rawData_33_gg <- rawData%>%
  filter(spikeTimes >= 1935.1776, spikeTimes <=1994.455, rat == "gg")
rawData_33_gg$trial_number = "33"
rawData_33_gg$direction = "R"

rawData_34_gg <- rawData%>%
  filter(spikeTimes >= 2043.8376, spikeTimes <=2061.245, rat == "gg")
rawData_34_gg$trial_number = "34"
rawData_34_gg$direction = "R"

rawData_35_gg <- rawData%>%
  filter(spikeTimes >= 2079.9128, spikeTimes <=2094.120, rat == "gg")
rawData_35_gg$trial_number = "35"
rawData_35_gg$direction = "L"

rawData_36_gg <- rawData%>%
  filter(spikeTimes >= 2108.6368, spikeTimes <=2149.351, rat == "gg")
rawData_36_gg$trial_number = "36"
rawData_36_gg$direction = "L"

rawData_37_gg <- rawData%>%
  filter(spikeTimes >= 2158.2392, spikeTimes <=2172.513, rat == "gg")
rawData_37_gg$trial_number = "37"
rawData_37_gg$direction = "R"

rawData_38_gg <- rawData%>%
  filter(spikeTimes >= 2190.8192, spikeTimes <=2224.684, rat == "gg")
rawData_38_gg$trial_number = "38"
rawData_38_gg$direction = "L"

rawData_39_gg <- rawData%>%
  filter(spikeTimes >= 2246.9704, spikeTimes <=2291.165, rat == "gg")
rawData_39_gg$trial_number = "39"
rawData_39_gg$direction = "L"

rawData_40_gg <- rawData%>%
  filter(spikeTimes >= 2330.9664, spikeTimes <=2353.959, rat == "gg")
rawData_39_gg$trial_number = "40"
rawData_39_gg$direction = "R"


#rat ff
rawData_1_ff <- rawData%>%
  filter(spikeTimes >= 24.4544, spikeTimes <=62.1192, rat == "ff")
rawData_1_ff$trial_number = "1"
rawData_1_ff$direction = "R"

rawData_2_ff <- rawData%>%
  filter(spikeTimes >= 86.7384, spikeTimes <=94.9128, rat == "ff")
rawData_2_ff$trial_number = "2"
rawData_2_ff$direction = "R"

rawData_3_ff <- rawData%>%
  filter(spikeTimes >= 114.9328, spikeTimes <=122.1512, rat == "ff")
rawData_3_ff$trial_number = "3"
rawData_3_ff$direction = "R"

rawData_4_ff <- rawData%>%
  filter(spikeTimes >= 236.1344, spikeTimes <=243.3416, rat == "ff")
rawData_4_ff$trial_number = "4"
rawData_4_ff$direction = "L"

rawData_5_ff <- rawData%>%
  filter(spikeTimes >= 304.5976, spikeTimes <=311.3352, rat == "ff")
rawData_5_ff$trial_number = "5"
rawData_5_ff$direction = "L"

rawData_6_ff <- rawData%>%
  filter(spikeTimes >= 348.2144, spikeTimes <=356.0840, rat == "ff")
rawData_6_ff$trial_number = "6"
rawData_6_ff$direction = "R"

rawData_7_ff <- rawData%>%
  filter(spikeTimes >= 407.4632, spikeTimes <=417.9336, rat == "ff")
rawData_7_ff$trial_number = "7"
rawData_7_ff$direction = "R"

rawData_8_ff <- rawData%>%
  filter(spikeTimes >= 481.2640, spikeTimes <=487.1816, rat == "ff")
rawData_8_ff$trial_number = "8"
rawData_8_ff$direction = "L"

rawData_9_ff <- rawData%>%
  filter(spikeTimes >= 523.0872, spikeTimes <=530.3176, rat == "ff")
rawData_9_ff$trial_number = "9"
rawData_9_ff$direction = "L"

rawData_10_ff <- rawData%>%
  filter(spikeTimes >= 671.3632, spikeTimes <=680.2312, rat == "ff")
rawData_10_ff$trial_number = "10"
rawData_10_ff$direction = "L"

rawData_11_ff <- rawData%>%
  filter(spikeTimes >= 716.6680, spikeTimes <=725.0568, rat == "ff")
rawData_11_ff$trial_number = "11"
rawData_11_ff$direction = "R"

rawData_12_ff <- rawData%>%
  filter(spikeTimes >= 738.9528, spikeTimes <=758.1832, rat == "ff")
rawData_12_ff$trial_number = "12"
rawData_12_ff$direction = "R"

rawData_13_ff <- rawData%>%
  filter(spikeTimes >= 781.2968, spikeTimes <=788.1608, rat == "ff")
rawData_13_ff$trial_number = "13"
rawData_13_ff$direction = "L"

rawData_14_ff <- rawData%>%
  filter(spikeTimes >= 843.1480, spikeTimes <=849.9336, rat == "ff")
rawData_14_ff$trial_number = "14"
rawData_14_ff$direction = "L"

rawData_15_ff <- rawData%>%
  filter(spikeTimes >= 883.7336, spikeTimes <=890.7144, rat == "ff")
rawData_15_ff$trial_number = "15"
rawData_15_ff$direction = "R"

rawData_16_ff <- rawData%>%
  filter(spikeTimes >= 954.2872, spikeTimes <=960.7560, rat == "ff")
rawData_16_ff$trial_number = "16"
rawData_16_ff$direction = "L"

rawData_17_ff <- rawData%>%
  filter(spikeTimes >= 1071.0264, spikeTimes <=1081.3830, rat == "ff")
rawData_17_ff$trial_number = "17"
rawData_17_ff$direction = "R"

rawData_18_ff <- rawData%>%
  filter(spikeTimes >= 1125.2240, spikeTimes <=1139.1620, rat == "ff")
rawData_18_ff$trial_number = "18"
rawData_18_ff$direction = "R"

rawData_19_ff <- rawData%>%
  filter(spikeTimes >= 1182.1456, spikeTimes <=1189.6710, rat == "ff")
rawData_19_ff$trial_number = "19"
rawData_19_ff$direction = "L"

rawData_20_ff <- rawData%>%
  filter(spikeTimes >= 1247.2808, spikeTimes <=1569.7290, rat == "ff")
rawData_20_ff$trial_number = "20"
rawData_20_ff$direction = "R"

rawData_21_ff <- rawData%>%
  filter(spikeTimes >= 1710.1640, spikeTimes <=1721.9720, rat == "ff")
rawData_21_ff$trial_number = "21"
rawData_21_ff$direction = "R"

rawData_22_ff <- rawData%>%
  filter(spikeTimes >= 1850.7792, spikeTimes <=1860.8260, rat == "ff")
rawData_22_ff$trial_number = "22"
rawData_22_ff$direction = "R"

rawData_23_ff <- rawData%>%
  filter(spikeTimes >= 1881.1024, spikeTimes <=1890.5220, rat == "ff")
rawData_23_ff$trial_number = "23"
rawData_23_ff$direction = "L"

rawData_24_ff <- rawData%>%
  filter(spikeTimes >= 1936.4000, spikeTimes <=1944.2570, rat == "ff")
rawData_24_ff$trial_number = "24"
rawData_24_ff$direction = "L"

rawData_25_ff <- rawData%>%
  filter(spikeTimes >= 2006.3280, spikeTimes <=2016.5000, rat == "ff")
rawData_25_ff$trial_number = "25"
rawData_25_ff$direction = "R"

rawData_26_ff <- rawData%>%
  filter(spikeTimes >= 2037.8152, spikeTimes <=2055.7450, rat == "ff")
rawData_26_ff$trial_number = "26"
rawData_26_ff$direction = "R"

rawData_27_ff <- rawData%>%
  filter(spikeTimes >= 2086.2072, spikeTimes <=2094.5290, rat == "ff")
rawData_27_ff$trial_number = "27"
rawData_27_ff$direction = "L"

rawData_28_ff <- rawData%>%
  filter(spikeTimes >= 2156.9648, spikeTimes <=2164.4170, rat == "ff")
rawData_28_ff$trial_number = "28"
rawData_28_ff$direction = "L"

rawData_29_ff <- rawData%>%
  filter(spikeTimes >= 2204.5768, spikeTimes <=2216.1800, rat == "ff")
rawData_29_ff$trial_number = "29"
rawData_29_ff$direction = "R"

rawData_30_ff <- rawData%>%
  filter(spikeTimes >= 2242.8808, spikeTimes <=2251.6620, rat == "ff")
rawData_30_ff$trial_number = "30"
rawData_30_ff$direction = "L"

rawData_31_ff <- rawData%>%
  filter(spikeTimes >= 2290.8816, spikeTimes <=2299.0980, rat == "ff")
rawData_31_ff$trial_number = "31"
rawData_31_ff$direction = "L"

rawData_32_ff <- rawData%>%
  filter(spikeTimes >= 2323.9520, spikeTimes <=2336.3460, rat == "ff")
rawData_32_ff$trial_number = "32"
rawData_32_ff$direction = "R"

rawData_33_ff <- rawData%>%
  filter(spikeTimes >= 2405.8216, spikeTimes <=2418.1130, rat == "ff")
rawData_33_ff$trial_number = "33"
rawData_33_ff$direction = "R"

rawData_34_ff <- rawData%>%
  filter(spikeTimes >= 2466.1512, spikeTimes <=2473.0760, rat == "ff")
rawData_34_ff$trial_number = "34"
rawData_34_ff$direction = "L"

rawData_35_ff <- rawData%>%
  filter(spikeTimes >= 2525.2632, spikeTimes <=2533.0310, rat == "ff")
rawData_35_ff$trial_number = "35"
rawData_35_ff$direction = "L"

rawData_36_ff <- rawData%>%
  filter(spikeTimes >= 2622.4584, spikeTimes <=2634.8940, rat == "ff")
rawData_36_ff$trial_number = "36"
rawData_36_ff$direction = "R"

rawData_37_ff <- rawData%>%
  filter(spikeTimes >= 2674.3584, spikeTimes <=2681.7670, rat == "ff")
rawData_37_ff$trial_number = "37"
rawData_37_ff$direction = "L"

rawData_38_ff <- rawData%>%
  filter(spikeTimes >= 2726.1640, spikeTimes <=2733.9660, rat == "ff")
rawData_38_ff$trial_number = "38"
rawData_38_ff$direction = "R"

rawData_39_ff <- rawData%>%
  filter(spikeTimes >= 2788.4688, spikeTimes <=2798.3240, rat == "ff")
rawData_39_ff$trial_number = "39"
rawData_39_ff$direction = "L"



#rbind ff
rawData_ff_total <- rbind(rawData_1_ff, rawData_2_ff, rawData_3_ff, rawData_4_ff,
                          rawData_5_ff,rawData_6_ff,rawData_7_ff,rawData_8_ff,rawData_9_ff,
                          rawData_10_ff,rawData_11_ff,rawData_12_ff,rawData_13_ff,rawData_14_ff,
                          rawData_15_ff,rawData_16_ff,rawData_17_ff,rawData_18_ff,rawData_19_ff,
                          rawData_20_ff, rawData_21_ff, rawData_22_ff, rawData_23_ff,rawData_24_ff,
                          rawData_25_ff,rawData_26_ff,rawData_27_ff,rawData_28_ff,rawData_29_ff,
                          rawData_30_ff,rawData_31_ff, rawData_32_ff,rawData_33_ff,rawData_34_ff,
                          rawData_35_ff,rawData_36_ff,rawData_37_ff,rawData_38_ff,rawData_39_ff)

rawData_ff_total


#rbind gg
rawData_gg_total <- rbind(rawData_1_gg, rawData_2_gg, rawData_3_gg,rawData_4_gg,rawData_5_gg,
                          rawData_6_gg, rawData_7_gg,rawData_8_gg,rawData_9_gg,rawData_10_gg,
                          rawData_11_gg,rawData_12_gg,rawData_13_gg,rawData_14_gg,rawData_15_gg, 
                          rawData_16_gg,rawData_17_gg, rawData_18_gg,rawData_19_gg,rawData_20_gg,
                          rawData_21_gg,rawData_22_gg, rawData_23_gg,rawData_24_gg,rawData_25_gg,
                          rawData_26_gg,rawData_27_gg, rawData_28_gg,rawData_29_gg,rawData_30_gg,
                          rawData_31_gg, rawData_32_gg,rawData_33_gg,rawData_34_gg,rawData_35_gg,
                          rawData_36_gg,rawData_37_gg, rawData_38_gg, rawData_39_gg,rawData_40_gg)

rawData_gg_total


#rbind ee
rawData_ee_total <- rbind(rawData_1, rawData_2, rawData_3, rawData_4,rawData_5,rawData_6,
                          rawData_7,rawData_8,rawData_9,rawData_10,rawData_11,rawData_12, 
                          rawData_13,rawData_14,rawData_15,rawData_16,rawData_17,rawData_18,rawData_19)

rawData_ee_total



#outside trials
rawData_1_out <- rawData%>%
  filter(spikeTimes >= 0, spikeTimes <=64, rat == "ee")
rawData_1_out$trial_number = "1"
rawData_1_out$direction = "L"

rawData_2_out <- rawData%>%
  filter(spikeTimes >= 105, spikeTimes <=311.0944 , rat == "ee")
rawData_2_out$trial_number = "2"
rawData_2_out$direction = "R"

rawData_3_out <- rawData%>%
  filter(spikeTimes >= 321.803 , spikeTimes <=348.9648 , rat == "ee")
rawData_3_out$trial_number = "3"
rawData_3_out$direction = "R"

rawData_4_out <- rawData%>%
  filter(spikeTimes >= 358.465 , spikeTimes <=391.8832 , rat == "ee")
rawData_4_out$trial_number = "4"
rawData_4_out$direction = "L"

rawData_5_out <- rawData%>%
  filter(spikeTimes >= 445.348 , spikeTimes <=491.3032 , rat == "ee")
rawData_5_out$trial_number = "5"
rawData_5_out$direction = "L"

rawData_6_out <- rawData%>%
  filter(spikeTimes >= 503.602 , spikeTimes <= 638.7008 , rat == "ee")
rawData_6_out$trial_number = "6"
rawData_6_out$direction = "R"

rawData_7_out <- rawData%>%
  filter(spikeTimes >= 649.379 , spikeTimes <=675.6448 , rat == "ee")
rawData_7_out$trial_number = "7"
rawData_7_out$direction = "R"

rawData_8_out <- rawData%>%
  filter(spikeTimes >= 691.999 , spikeTimes <= 711.1904 , rat == "ee")
rawData_8_out$trial_number = "8"
rawData_8_out$direction = "R"

rawData_9_out <- rawData%>%
  filter(spikeTimes >= 722.488 , spikeTimes <=744.0280 , rat == "ee")
rawData_9_out$trial_number = "9"
rawData_9_out$direction = "L"

rawData_10_out <- rawData%>%
  filter(spikeTimes >= 768.811 , spikeTimes <=785.0464 , rat == "ee")
rawData_10_out$trial_number = "10"
rawData_10_out$direction = "R"

rawData_11_out <- rawData%>%
  filter(spikeTimes >= 798.029 , spikeTimes <=860.1576 , rat == "ee")
rawData_11_out$trial_number = "11"
rawData_11_out$direction = "L"

rawData_12_out <- rawData%>%
  filter(spikeTimes >= 871.157 , spikeTimes <=904.4096 , rat == "ee")
rawData_12_out$trial_number = "12"
rawData_12_out$direction = "L"

rawData_13_out <- rawData%>%
  filter(spikeTimes >= 917.414 , spikeTimes <= 1165.4360 , rat == "ee")
rawData_13_out$trial_number = "13"
rawData_13_out$direction = "R"

rawData_14_out <- rawData%>%
  filter(spikeTimes >= 1183.957 , spikeTimes <= 1229.3016 , rat == "ee")
rawData_14_out$trial_number = "14"
rawData_14_out$direction = "L"

rawData_15_out <- rawData%>%
  filter(spikeTimes >= 1238.737 , spikeTimes <= 1305.9616 , rat == "ee")
rawData_15_out$trial_number = "15"
rawData_15_out$direction = "L"

rawData_16_out <- rawData%>%
  filter(spikeTimes >= 1319.149 , spikeTimes <=1343.4128 , rat == "ee")
rawData_16_out$trial_number = "16"
rawData_16_out$direction = "R"

rawData_17_out <- rawData%>%
  filter(spikeTimes >= 1360.117 , spikeTimes <=1384.1984 , rat == "ee")
rawData_17_out$trial_number = "17"
rawData_17_out$direction = "R"

rawData_18_out <- rawData%>%
  filter(spikeTimes >=1397.521 , spikeTimes <=1410.6936 , rat == "ee")
rawData_18_out$trial_number = "18"
rawData_18_out$direction = "R"

rawData_19_out <- rawData%>%
  filter(spikeTimes >= 1423.736 , spikeTimes <= 1440.1768, rat == "ee")
rawData_19_out$trial_number = "19"
rawData_19_out$direction = "L"


#rbinding outside trial
rawData_ee_total_out <- rbind(rawData_1_out, rawData_2_out, rawData_3_out, rawData_4_out,
                              rawData_5_out,rawData_6_out,rawData_7_out,rawData_8_out,
                              rawData_9_out,rawData_10_out,rawData_11_out,rawData_12_out,
                              rawData_13_out,rawData_14_out,rawData_15_out,rawData_16_out,
                              rawData_17_out,rawData_17_out,rawData_19_out)

rawData_ee_total_out





ee_ff_gg_combined <- rbind(rawData_ff_total, rawData_ee_total,rawData_gg_total)


#{r ee firing rate}

N <- 78
slider <- 10
z <- cut(rawData_ee$spikeTimes, breaks =seq(0,1800,by= slider), dig.lab = 5)

data_f_ee_fire <- data.frame(table(z))

data_f_ee_fire <- data_f_ee_fire %>%
  mutate(fire_rate = data_f_ee_fire$Freq/(N*slider)) %>%
  mutate(bin_edges = seq(slider,1800,by= slider))



#{r ff firing rate}

N_ff <- 51

slider <- 10
z_ff <- cut(rawData_ff$spikeTimes, breaks =seq(0,3000,by= slider), dig.lab = 5)

data_f_ff_fire <- data.frame(table(z_ff))

data_f_ff_fire <- data_f_ff_fire %>%
  mutate(fire_rate_ff = data_f_ff_fire$Freq/(N_ff*slider)) %>%
  mutate(bin_edges_ff = seq(slider,3000,by= slider))






#{r gg firing rate}
N_gg <- 19
slider <- 10
z_gg <- cut(rawData_gg$spikeTimes, breaks =seq(0,2500,by= slider), dig.lab = 5)

data_f_gg_fire <- data.frame(table(z_gg))

data_f_gg_fire <- data_f_gg_fire %>%
  mutate(fire_rate_gg = data_f_gg_fire$Freq/(N_gg*slider))%>%
  mutate(bin_edges_gg = seq(slider,2500,by= slider))



#{r firing rate of ee rat showing L and R trials}
N <- 78
slider <- 20
trial_Times_ee_R <- Data2[Data2$trialTimes.4 == 1,1]
trial_Times_ee_L <- Data2[Data2$trialTimes.4 == 2,1]
z <- cut(rawData_ee$spikeTimes, breaks =seq(0,1800,by= slider), dig.lab = 5)

data_f_ee_fire <- data.frame(table(z))

data_f_ee_fire <- data_f_ee_fire %>%
  mutate(fire_rate = data_f_ee_fire$Freq/(N*slider)) %>%
  mutate(bin_edges = seq(slider,1800,by= slider))



#{r firing rate of ff gg ee during experiment}

z_total_ee <- cut(rawData_ee_total$spikeTimes, breaks =seq(0,1800,by= slider), dig.lab = 5)

data_f_ee_fire_total <- data.frame(table(z_total_ee))

data_f_ee_fire_total <- data_f_ee_fire_total %>%
  mutate(fire_rate = data_f_ee_fire_total$Freq/(N*slider))%>%
  mutate(bin_edges_ee = seq(slider,1800,by= slider))



#ff
z_total_ff <- cut(rawData_ff_total$spikeTimes, breaks =seq(0,3000,by= slider), dig.lab = 5)

data_f_ff_fire_total <- data.frame(table(z_total_ff))

data_f_ff_fire_total <- data_f_ff_fire_total %>%
  mutate(fire_rate = data_f_ff_fire_total$Freq/(N_ff*slider))%>%
  mutate(bin_edges_ff = seq(slider,3000,by= slider))


#gg
z_total_gg <- cut(rawData_gg_total$spikeTimes, breaks =seq(0,2500,by= slider), dig.lab = 5)

data_f_gg_fire_total <- data.frame(table(z_total_gg))

data_f_gg_fire_total <- data_f_gg_fire_total %>%
  mutate(fire_rate = data_f_gg_fire_total$Freq/(N_gg*slider))%>%
  mutate(bin_edges_gg = seq(slider,2500,by= slider))



#Firing Rate of left reward vs right reward (all rats) This shows synchrony of neurons. This is new synchrony graph
#{r firing rate of L vs R}
N_com <- 148
slider <- 10

ee_ff_gg_combined_left <- filter(ee_ff_gg_combined, direction == "L")

z_com <- cut(ee_ff_gg_combined_left$spikeTimes, breaks =seq(0,3000,by= slider), dig.lab = 5)

data_f_com_fire_left <- data.frame(table(z_com))

data_f_com_fire_left <- data_f_com_fire_left %>%
  mutate(fire_rate = data_f_com_fire_left$Freq/(N_com*slider))%>%
  mutate(bin_edges_com_l = seq(slider,3000,by= slider))



#right reward

ee_ff_gg_combined_right <- filter(ee_ff_gg_combined, direction == "R")

z_com_r <- cut(ee_ff_gg_combined_right$spikeTimes, breaks =seq(0,3000,by= slider), dig.lab = 5)

data_f_com_fire_right <- data.frame(table(z_com_r))

data_f_com_fire_right <- data_f_com_fire_right %>%
  mutate(fire_rate = data_f_com_fire_right$Freq/(N_com*slider)) %>%
  mutate(bin_edges_com_r = seq(slider,3000,by= slider))




ui <- dashboardPage(skin = "blue", title = "Exploring the Prefrontal Cortex",
                    dashboardHeader(title = "The Prefrontal Cortex"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Overview", tabName = "overview", icon = icon("clipboard-list")),
                        menuItem("Neuroscience Background", tabName = "background", icon = icon("brain")),
                        menuItem("Measuring Neural Activity", tabName = "measuring", icon = icon("ruler-combined")),
                        menuItem("Investigating Neural Activity", tabName = "investigate", icon = icon("search")),
                        menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
                        menuItem("Analysis", tabName = "analysis", icon = icon("question-circle"))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "overview",
                                titlePanel(h1("Overview", align = "center")),
                                br(), br(),
                                fluidRow(column(8, align = "center", offset = 2, img(src = "kindpng_2084151.png", width = "40%", height = "40%"))),
                                br(), br(),
                                fluidRow(column(12, align = "center", textOutput(outputId = "text1"))),
                                br(), br(), br(), 
                                titlePanel(h3("Citation Information", align = "center")),
                                p("Fujisawa S, Amarasingham A, Harrison MT, Peyrache, A., Buzsáki G. (2015)"),
                                p("Simultaneous electrophysiological recordings of ensembles of isolated neurons in rat medial prefrontal cortex and intermediate CA1 area of the hippocampus during a working memory task."),
                                p("CRCNS.org. http://dx.doi.org/10.6080/K01V5BWK")
                                
                        ),
                        tabItem(tabName = "background",
                                titlePanel(h1("Prefrontal Cortex Background", align = "center")),
                                br(), br(), br(),
                                fluidRow(column(8, align = "center", offset = 2, img(src = "PikPng.com_brain-png_602352.png", width = "45%", height = "45%"))),
                                br(), br(),
                                p("The major function of the PFC is cognitive control and performing the executive functions of the brain.", align = "center", style = "font-size:23px;"),
                                br(),
                                p("The prefrontal cortex is responsible for:", align = "center", style = "font-size:20px;"),
                                p("- Planning and decision making", align = "center", style = "font-size:16px;"),
                                p("- Sensory information and stimulus filtration", align = "center",style = "font-size:16px;"),
                                p("- Attention control and memory (working, prospective, short-term)", align = "center",style = "font-size:16px;"),
                                p("- Social behavior and personality expression", align = "center",style = "font-size:16px;"),
                                p("- Impulse control and inhibitory control", align = "center",style = "font-size:16px;"),
                                p("- Problem solving and setting goal-directed behavior", align = "center",style = "font-size:16px;"),
                                p("- Spatial information processing and task management", align = "center",style = "font-size:16px;"),
                                p("- Controlling other cortical areas", align = "center",style = "font-size:16px;"),
                                #fluidRow(column(12, align = "center", outputId = "text2")),
                                br(), br(), br(),
                                titlePanel(h3("Map of PFC", align = "center")),
                                br(),
                                fluidRow(column(12, align = "center", plotOutput(outputId = "plot", width = 700, height = 500)))
                        ),
                        tabItem(tabName = "measuring",
                                titlePanel(h1("Measuring Neural Activity", align = "center")),
                                br(), br(), br(),
                                fluidRow(column(8, align = "center", offset = 2, img(src = "electricity-clipart-brain-730249.png", width = "30%", height = "30%"))),
                                p("Neurons receive inputs from other cells that cause brief changes in its membrane potential by opening voltage-specific ion channels.", align = "center",style = "font-size:18px;"),
                                p("Once the voltage of a cell reaches a spiking threshold, it will fire an action potential, also known as a spike.", align = "center",style = "font-size:18px;"),
                                #fluidRow(column(12, align = "center", textOutput(outputId = "text3"))),
                                br(), br(), br(),
                                titlePanel(h3("Firing Rate", align = "center")),
                                br(),
                                p("A spike train is a sequence neuronal firing times", align = "center",style = "font-size:17px;"),
                                #fluidRow(column(12, align = "center", textOutput(outputId = "text4"))),
                                br(), 
                                fluidRow(column(8, align = "center", offset = 2, img(src = "Network firing rate.png", width = "55%", height = "55%"))),
                                br(), br(), br(),
                                titlePanel(h3("Spike Train", align = "center")),
                                br(),
                                p("The firing rate is the number of spikes generated by a neuron per unit of time", align = "center",style = "font-size:17px;"),
                                #fluidRow(column(12, align = "center", textOutput(outputId = "text5"))),
                                br(), 
                                fluidRow(column(8, align = "center", offset = 2, img(src = "Network Spike Train.png", width = "55%", height = "55%"))),
                                br(), br(), br(),
                                titlePanel(h3("Interspike Interval", align = "center")),
                                p("The interspike interval is the time between spikes", align = "center",style = "font-size:17px;"),
                                #fluidRow(column(12, align = "center", textOutput(outputId = "text6")),
                                br(), 
                                fluidRow(column(8, align = "center", offset = 2, img(src = "ISI stimulus.png", width = "55%", height = "55%"))),
                                br(), br(), br() 
                        ),
                        tabItem(tabName = "investigate",
                                titlePanel(h1("Investigating Neural Activity", align = "center")),
                                br(), br(),
                                fluidRow(column(8, align = "center", offset = 2, img(src = "121-1210232_innovation-clipart-brain-activity-study-brain.png", width = "40%", height = "40%"))),
                                titlePanel(h3("Research Experiment", align = "center")),
                                br(),
                                p("Goal: To determine if short term plasticity is at play in the spiking relationship in the medial prefrontal cortex during a working memory task", align = "center",style = "font-size:19px;"),
                                p("1. The rats first were given an odor cue of either chocolate or cheese", align = "center",style = "font-size:17px;"),
                                p("2. Each of the 3 rats would be released into a figure-eight T-maze that has either chocolate or cheese to the left or to the right", align = "center",style = "font-size:17px;"),
                                p("3. The spiking of the rats' neurons was recorded as each rat made the association and then selected a direction to get the food", align = "center",style = "font-size:17px;"),
                                #fluidRow(column(12, align = "center", textOutput(outputId = "text7"))),
                                br(), br(),
                                fluidRow(column(8, align = "center", offset = 2, img(src = "Ratmaze.png", width = "30%", height = "30%")))
                        ),
                        tabItem(tabName = "graphs",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId = "graphChoice",
                                                label = "What graph do you want?",
                                                choices = c("Overall Firing Rate of EE Rat",
                                                            "Overall Firing Rate of FF Rat","Overall Firing Rate of GG Rat",
                                                            "Overall Firing Rate of EE Rat with Direction",
                                                            "Firing Rate of EE Rat During Experiment",
                                                            "Firing Rate of FF Rat During Experiment",
                                                            "Firing Rate of GG Rat During Experiment",
                                                            "Interspike Interval of EE Rat Inside Trial",
                                                            "Interspike Interval of EE Rat Outside Trial",
                                                            "Overall Interspike Interval of EE Rat",
                                                            "Overall Interspike Interval of EE Rat",
                                                            "Overall Interspike Interval of EE Rat",
                                                            "Overall Firing Rate of Left Reward",
                                                            "Overall Firing Rate of Right Reward")),
                                    uiOutput("interaction"),
                                    uiOutput("analysis")),
                                  mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Plot", plotOutput(outputId = "plots")))
                                  )
                                  
                                )
                        ),
                        tabItem(tabName = "analysis",
                                titlePanel(h1("Analysis", align = "center")),
                                br(), br(), br(),
                                fluidRow(column(12, align = "center", textOutput(outputId = "text8"))),
                                fluidRow(column(12, align = "center", textOutput(outputId = "text9"))),
                                fluidRow(column(12, align = "center", textOutput(outputId = "text10")))
                        )
                      )
                    )
    )


  


#   tabsetPanel(type = "tabs", 
#               tabPanel(titlePanel(h1("The Prefrontal Cortex", align = "center")),
#                        HTML('<center><img src="PikPng.com_brain-png_602352.png" width="40%"></center>'),
#                        titlePanel(h2("Background", align = "center")),
#                        setBackgroundColor(color = "aliceblue"),
#                        p("The prefrontal cortex is responsible for:", align = "center", style = "font-size:20px;"),
#                        p("- Planning and decision making", align = "center", style = "font-size:16px;"),
#                        p("- Sensory information and stimulus filtration", align = "center",style = "font-size:16px;"),
#                        p("- Attention control and memory (working, prospective, short-term)", align = "center",style = "font-size:16px;"),
#                        p("- Social behavior and personality expression", align = "center",style = "font-size:16px;"),
#                        p("- Impulse control and inhibitory control", align = "center",style = "font-size:16px;"),
#                        p("- Problem solving and setting goal-directed behavior", align = "center",style = "font-size:16px;"),
#                        p("- Spatial information processing and task management", align = "center",style = "font-size:16px;"),
#                        p("- Controlling other cortical areas", align = "center",style = "font-size:16px;"),
#                        fluidRow(column(12, align = "center", plotOutput(outputId = "plot", width = 700, height = 500)))),
#               tabPanel(titlePanel(h2("Measuring Neural Activity", align = "center")),
#                        p("Neurons receive inputs from other cells that cause brief changes in its membrane potential by opening voltage-specific ion channels.", align = "center",style = "font-size:16px;"),
#                        p("Once the voltage of a cell reaches a spiking threshold, it will fire an action potential, also known as a spike.", align = "center",style = "font-size:16px;"),
#                        titlePanel(h3("Firing Rate", align = "center")),
#                        p("A spike train is a sequence neuronal firing times", align = "center",style = "font-size:16px;"),
#                        HTML('<center><img src="Network firing rate.png" width="35%"></center>'),
#                        titlePanel(h3("Spike Train", align = "center")),
#                        p("The firing rate is the number of spikes generated by a neuron per unit of time", align = "center",style = "font-size:16px;"),
#                        HTML('<center><img src="Network Spike Train.png" width="35%"></center>'),
#                        titlePanel(h3("Interspike Interval", align = "center")),
#                        p("The interspike interval is the time between spikes", align = "center",style = "font-size:16px;"),
#                        HTML('<center><img src="ISI stimulus.png" width="35%"></center>')),
#               tabPanel(titlePanel(h2("Investigating Neural Activity", align = "center")),
#                        titlePanel(h3("Research Experiment", align = "center")),
#                        p("Goal: To determine if short term plasticity is at play in the spiking relationship in the medial prefrontal cortex during a working memory task", align = "center",style = "font-size:16px;"),
#                        p("1. The rats first were given an odor cue of either chocolate or cheese", align = "center",style = "font-size:16px;"),
#                        p("2. Each of the 3 rats would be released into a figure-eight T-maze that has either chocolate or cheese to the left or to the right", align = "center",style = "font-size:16px;"),
#                        p("3. The spiking of the rats' neurons was recorded as each rat made the association and then selected a direction to get the food", align = "center",style = "font-size:16px;"),
#                        HTML('<center><img src="Ratmaze.png" width="28%"></center>'),
#                        titlePanel(h3("Analyzing the Data", align = "center")),
                       # sidebarLayout(
                       #   sidebarPanel(
                       #     selectInput(inputId = "graphChoice",
                       #                 label = "What graph do you want?",
                       #                 choices = c("Firing Rate for Rat EE",
                       #                             "Firing Rate for Rat FF","Firing Rate for Rat GG",
                       #                             "Firing Rate for Rat EE During Trial",
                       #                             "Firing Rate for Rat FF During Trial",
                       #                             "Firing Rate for Rat GG During Trial",
                       #                             "Firing Rate Combined for Left",
                       #                             "Firing Rate Combined for Right",
                       #                             "ISI for Rat EE Inside Trial",
                       #                             "ISI for Rat EE Outside Trial",
                       #                             "Overall ISI for Rat EE",
                       #                             "Overall ISI for Rat FF",
                       #                             "Overall ISI for Rat GG",
                       #                             "Synchrony for Rat EE Inside Trial",
                       #                             "Synchrony for Rat EE Outside Trial")),
                       #     uiOutput("interaction"),
                       #     uiOutput("analysis")),
                       #   mainPanel(
                       #     tabsetPanel(type = "tabs",
                       #                 tabPanel("Plot", plotOutput(outputId = "plots")),
                       #                 tabPanel("Analysis", textOutput(outputId = "analysis1")))
#                            
#                            
#                          )
#                        )
#                        
#               )
#   )
# )



server <- function(input, output, session) {
  
  
  
  output$text1 <- renderText({
    "This app is designed to give a background of the prefrontal cortex, discuss how neural data is measured, and investigate and analyze
    a real data set. The graphs made from the data set are interactive to allow the user to visualize the figures in different ways. 
    The data set is taken from a published article in Nature Neuroscience called 'Behavior-dependent short-term 
    assembly dynamics in the medial prefrontal cortex.' We specifically looked into the spike time data of three different 
    rats and the times of the trials that were conducted. The authors are Shigeyoshi Fujisawa, 
    Asohan Amarashingham, Matthew T Harrison, and Gyorgy Buzsaki. Fujisawa, Amarasingham, and Buzsaki work are the Center for 
    Molecular and Behavioral Neuroscience at Rutgers University at the time of publication. Harrison works in the Department of 
    Statistics at Carnegie Mellon University at the time of publication. Fujisawa has a PhD in neurophysiology, Amarashingham 
    has a PhD in applied mathematics, Harrison is a professor at Brown University in the applied mathematics area, and Buzsaki 
    has a MD and PhD at NYU. 
    "
  })
  
  # output$text2 <- renderText({
  #   "Put background info with neuroscience context here"
  # })
  # 
  # output$text3 <- renderText({
  #   "Put info with measuring stuff here"
  # })
  # 
  # output$text4 <- renderText({
  #   "Put  stuff here"
  # })
  # 
  # output$text5 <- renderText({
  #   "Put  stuff here"
  # })
  # 
  # output$text6 <- renderText({
  #   "Put  stuff here"
  # })
  # 
  # output$text7 <- renderText({
  #   "Goal: To determine if short term plasticity is at play in the spiking relationship in the medial prefrontal cortex during a working memory task
  #   1. The rats first were given an odor cue of either chocolate or cheese
  #   2. Each of the 3 rats would be released into a figure-eight T-maze that has either chocolate or cheese to the left or to the right
  #   3. The spiking of the rats' neurons was recorded as each rat made the association and then selected a direction to get the food"
  # })
  
  output$text8 <- renderText({
    "Put  stuff here"
  })
  
  output$text9 <- renderText({
    "Put  stuff here"
  })
  
  output$text10 <- renderText({
    "Put  stuff here"
  })
  
  

  
  output$plot <-renderPlot({
    parts = tibble(region = c("rostral middle frontal", "superior frontal",
                              "medial orbitofrontal","pars orbitalis", "pars opercularis", "pars triangularis",
                              "caudal middle frontal", "rostral anterior cingulate", "lateral orbitofrontal"), 
                   regions = sample(seq(from = 0,to = 10, by = 1), 9)
    )
    
    ggseg(parts, atlas = dk, 
          colour = "black",
          size = .1, 
          position = "stacked",
          mapping = aes(fill = regions))
  })
  
  
  
  output$interaction <- renderUI({
    
    if(input$graphChoice == "Firing Rate for Rat EE") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate for Rat FF") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate for Rat GG") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate for Rat EE During Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate for Rat FF During Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate for Rat GG During Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate Combined for Left") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Firing Rate Combined for Right") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    
    else if(input$graphChoice == "ISI for Rat EE Inside Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "ISI for Rat EE Outside Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Overall ISI for Rat EE") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Overall ISI for Rat FF") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Overall ISI for Rat GG") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 200,
                  min = 5,
                  max = 400)}
    
    else if(input$graphChoice == "Synchrony for Rat EE Inside Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 0.5,
                  min = 0.1,
                  max = 2)}
    
    else if(input$graphChoice == "Synchrony for Rat EE Outside Trial") {
      sliderInput(inputId = "binSize",
                  label = "Choose number of bins:",
                  value = 0.5,
                  min = 5,
                  max = 2)}
    
    
    
    
  })
  
  output$analysis <- renderUI({
    
    if(input$graphChoice == "Firing Rate for Rat EE") {
      
    }
  })
  
  output$plots <- renderPlot({
    
    #Overall Firing Rates
    #This graph shows the overall firing rate of ee rat over the course of the experiment. 
    #During the first 250 seconds of the experiment, the average firing rate was between 1.5Hz and 2.0Hz. 
    #During the last 250 seconds, the average firing rate was between 1.0Hz and 1.5Hz. Less neuronal activity throughout 
    #this working memory task experiment is indicative of the rat learning since neurons do not need to fire as often.
    
    
    
    #{r ee firing rate}
    
    if(input$graphChoice == "Overall Firing Rate of EE Rat") { 
    ggplot(data = data_f_ee_fire) + 
      geom_col(mapping = aes(x = bin_edges, y = fire_rate), fill = "steelblue") +
      labs(title = "Overall Firing Rate of EE Rat", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw()+
      theme(text = element_text(size = 12)) 
    }
    
    
    
    #{r ff firing rate}
    
    else if(input$graphChoice == "Overall Firing Rate of FF Rat") {
    ggplot(data = data_f_ff_fire) + 
      geom_col(mapping = aes(x = bin_edges_ff, y = fire_rate_ff), fill ="darkseagreen4") +
      labs(title = "Overall Firing Rate of FF Rat", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    
    
    #{r gg firing rate}
    
    else if(input$graphChoice == "Overall Firing Rate of GG Rat") {
    ggplot(data = data_f_gg_fire) + 
      geom_col(mapping = aes(x = bin_edges_gg, y = fire_rate_gg), fill = "mediumpurple4") +
      labs(title = "Overall Firing Rate of GG Rat", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12))
    }
    
    
    
    
    
    #Firing Rate of EE with L and R trials shown
    #The red dots represent trials in which the reward was to the right side and the green dots 
    #represent trials in which the reward was to the left. The transition from right reward to left reward 
    #was often accompanied by an increase in firing rate because the rat had to readjust to the change in reward direction. 
    #After several trials of alternating left and right reward, we see the firing rate for left reward decrease, even after 
    #subsequent trials of right reward (same vice versa). For example at 1250 seconds there is a left reward and right after 
    #there were a few trials of right reward and the firing rate dropped lower than both the previous left reward and lower 
    #than the firing rate at the 400 second right reward. This shows that the rat has learned since the firing rate has decreased 
    #by left reward and by right reward trials.
    
    #{r firing rate of ee rat showing L and R trials}
    
    
    else if(input$graphChoice == "Overall Firing Rate of EE Rat with Direction") {
    ggplot(data = data_f_ee_fire) + 
      geom_col(mapping = aes(x = bin_edges, y = fire_rate), fill = "steelblue") +
      labs(title = "Overall Firing Rate of EE Rat with Direction", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90))  + 
      annotate("point", x = trial_Times_ee_L, y = rep(0,length(trial_Times_ee_L)), color = "green") + 
      annotate("point", x = trial_Times_ee_R, y = rep(0,length(trial_Times_ee_R)), color = "red") + theme_bw()+ 
      theme(text = element_text(size = 12))
    }
    
    
    
    
    
    #Firing Rate of All three rats during experiment (in trial)
    
    #{r firing rate of ff gg ee during experiment}
    
    else if(input$graphChoice == "Firing Rate of EE Rat During Experiment") {
    ggplot(data = data_f_ee_fire_total) + 
      geom_col(mapping = aes(x = bin_edges_ee, y = fire_rate), fill = "steelblue") +
      labs(title = "Firing Rate of EE Rat During Experiment", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    #ff
    
    else if(input$graphChoice == "Firing Rate of FF Rat During Experiment") {
    ggplot(data = data_f_ff_fire_total) + 
      geom_col(mapping = aes(x = bin_edges_ff, y = fire_rate), fill = "seagreen4") +
      labs(title = "Firing Rate of FF Rat During Experiment", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    #gg
    
    else if(input$graphChoice == "Firing Rate of GG Rat During Experiment") {
    ggplot(data = data_f_gg_fire_total) + 
      geom_col(mapping = aes(x = bin_edges_gg, y = fire_rate), fill = "mediumpurple4") +
      labs(title = "Firing Rate of GG Rat During Experiment", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    
    
    
    #ISI of ee rat in trial and out trial
    
    #  The isi time increases slightly outside of trial. Expected since the rat is not actively searching for reward.
    #  These graphs show the interspike interval of all three rats in trial compared to out of trial. The ISI of outside trial was slightly higher than the inside trial. This difference may be due to the rat not actively engaged in a working memory task, so the timing between neuronal firing increased. During the experiment, we had expected shorter ISI times since the rat is actively learning.
    #  ```{r isi graphs ee in and out}
    
    else if(input$graphChoice == "Interspike Interval of EE Rat Inside Trial") {
    rawData_ee_total %>%
      group_by(rat) %>%
      summarise( spike_diff = diff(spikeTimes)) %>%
      ggplot() + 
      geom_histogram(mapping = aes(x = spike_diff), fill = "dodgerblue4", color = "black", bins =150) +
      labs(title = "Interspike Interval of EE Rat Inside Trial", y = "Count", x = "Time Difference (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() +scale_x_log10() + theme(text = element_text(size = 12)) 
    }
    
    
    
    else if(input$graphChoice == "Interspike Interval of EE Rat Outside Trial") {
    rawData_ee_total_out %>%
      group_by(rat) %>%
      summarise( spike_diff = diff(spikeTimes)) %>%
      ggplot() + 
      geom_histogram(mapping = aes(x = spike_diff), color = "black", fill = "dodgerblue3",bins=150) +
      labs(title = "Interspike Interval of EE Rat Outside Trial", y = "Count", x = "Time Difference (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() +scale_x_log10() + theme(text = element_text(size = 12)) 
    }
    
    
    
    
    
    
    #ISI of all three rats (includes in trial and out trial)
    #{r isi }
    
    else if(input$graphChoice == "Overall Interspike Interval of EE Rat") {
    rawData_ee %>%
      group_by(rat) %>%
      summarise( spike_diff = diff(spikeTimes)) %>%
      ggplot() + 
      geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "steelblue",bins = 50) +
      labs(title = "Overall Interspike Interval of EE Rat", y = "Count", x = "Time Difference (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    
    else if(input$graphChoice == "Overall Interspike Interval of FF Rat") {
    rawData_ff  %>%
      group_by(rat) %>%
      summarise( spike_diff = diff(spikeTimes)) %>%
      ggplot() + 
      geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "seagreen4", bins=50) +
      labs(title = "Overall Interspike Interval of FF Rat", y = "Count", x = "Time Difference (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    
    else if(input$graphChoice == "Overall Interspike Interval of GG Rat") {
    rawData_gg  %>%
      group_by(rat) %>%
      summarise( spike_diff = diff(spikeTimes)) %>%
      ggplot() + 
      geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "mediumpurple4", bins=50) +
      labs(title = "Overall Interspike Interval of GG Rat", y = "Count", x = "Time Difference (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12))
    }
    
    
    #Firing Rate of left reward vs right reward (all rats) This shows synchrony of neurons. This is new synchrony graph
    #{r firing rate of L vs R}
    
    
    else if(input$graphChoice == "Overall Firing Rate of Left Reward") {
    ggplot(data = data_f_com_fire_left) + 
      geom_col(mapping = aes(x = bin_edges_com_l, y = fire_rate), fill = "sandybrown") +
      labs(title = "Overall Firing Rate of Left Reward", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12))
    }
    
    #right reward
    
    else if(input$graphChoice == "Overall Firing Rate of Right Reward") {
    ggplot(data = data_f_com_fire_right) + 
      geom_col(mapping = aes(x = bin_edges_com_r, y = fire_rate), fill = "plum4") +
      labs(title = "Overall Firing Rate of Right Reward", y = "Firing rate (Hz)", x = "Time (sec)") + 
      theme(axis.text.x = element_text(angle = 90)) + theme_bw() + theme(text = element_text(size = 12)) 
    }
    
    
    
  })
    
  # #CV calculations and average firing rate by rat
  # 
  # #{r cv}
  # 
  # ee_sd <- sd(rawData_ee$spikeTimes)
  # ee_cv <- ee_sd/ee_mean
  # 
  # ff_sd <- sd(rawData_ff$spikeTimes)
  # ff_cv <- ff_sd/ff_mean
  # 
  # gg_sd <- sd(rawData_gg$spikeTimes)
  # gg_cv <- gg_sd/gg_mean
  # 
  # ee_cv
  # ff_cv
  # gg_cv
  # 
  # 
  # ee: 0.64
  # ff: 0.55
  # gg: 0.59
  # 
  # 
  # 
  # #{r average firing rates by rat}
  # 
  # ee_avg_fire <- count(rawData_ee)/max(rawData_ee$spikeTimes)
  # 
  # ff_avg_fire <- count(rawData_ff)/max(rawData_ff$spikeTimes)
  # 
  # gg_avg_fire <- count(rawData_gg)/max(rawData_gg$spikeTimes)
  # 
  # ee_avg_fire/N
  # ff_avg_fire/N_ff
  # gg_avg_fire/N_gg
  # 
  # ee: 1.38
  # ff: 2.22
  # gg: 1.09  
  # 
  # 
  
  
  
  
  
  
  
  
  
  # 
  # #Spike train during experiment
  # output$plots <- renderPlot({
  #   
  #   
  #   
  #   if(input$graphChoice == "Spike Train for Rat EE") {
  #     ggplot(data = rawData_ee_total) +
  #       geom_tile(aes(x=spikeTimes, y = "EE Rat" , width = 0.0005), color ="steelblue") +
  #       labs(title = "Spike Train of EE Rat Inside Trials", y = "Count", x = "Time (sec)") + theme_bw()
  #   }
  #   
  #   
  #   
  #   else if(input$graphChoice == "Spike Train for Rat FF") {
  #     ggplot(data = rawData_ff_total) +
  #       geom_tile(aes(y = "FF Rat", x=spikeTimes, width = 0.0005), color= "seagreen4") +
  #       labs(title = "Spike Train of FF Rat Inside Trials", y = "Count", x = "Time (sec") + theme_bw()
  #   }
  #   
  #   
  #   
  #   else if(input$graphChoice == "Spike Train for Rat GG") {
  #     ggplot(data = rawData_gg_total) +
  #       geom_tile(aes(y = "GG Rat", x=spikeTimes, width = 0.0005), color = "mediumpurple4") +
  #       labs(title = "Spike Train GG Rat Inside Trials", y = "Count", x = "Time (sec)") + theme_bw()
  #   }
  #   
  #   
  #   
  #   #overall firing rate of rats
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat EE") {
  #     rawData_ee%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "steelblue", bins = input$binSize)  +
  #       labs(title = "Overall Firing Rate of EE Rat ", y = "Firing rate (Hz)", x = "Time (ms)") + theme_bw()
  #   }
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat FF") {
  #     rawData_ff%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill ="darkseagreen4", bins = input$binSize) +
  #       labs(title = "Overall Firing Rate of FF Rat ", y = "Firing rate (Hz)", x = "Time (ms)") +theme_bw()
  #   }
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat GG") {
  #     rawData_gg%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "mediumpurple4", bins = input$binSize) +
  #       labs(title = "Overall Firing Rate of GG Rat ", y = "Firing rate (Hz)", x = "Time (ms)") + theme_bw()
  #   }
  #   
  #   
  #   
  #   
  #   
  #   #firing rate ee, ff, gg during experiment
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat EE During Trial") {
  #     rawData_ee_total%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "steelblue", bins = input$binSize) +
  #       labs(title = "Firing Rate of EE Rat During Experiment ", y = "Firing rate (Hz)", x = "Time (ms)") + theme_bw()
  #   }
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat FF During Trial") {
  #     rawData_ff_total%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black",fill = "seagreen4", bins = input$binSize) +
  #       labs(title = "Firing Rate of FF Rat During Experiment ", y = "Firing rate (Hz)", x = "Time (ms)") + theme_bw()
  #   }
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate for Rat GG During Trial") {
  #     rawData_gg_total%>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "mediumpurple4",bins = input$binSize) +
  #       labs(title = "Firing Rate of FF Rat During Experiment ", y = "Firing rate (Hz)", x = "Time (ms)") + theme_bw()
  #   }
  #   
  #   
  #   
  #   #firing rate L vs R
  #   
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate Combined for Left") {
  #     ee_ff_gg_combined%>%
  #       filter(direction == "L") %>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "sandybrown", bins = input$binSize) +
  #       labs(title = "Overall Firing Rate of Left Reward", y = "Count", x = "Time (sec)") + theme_bw()
  #   }
  #   
  #   
  #   else if(input$graphChoice == "Firing Rate Combined for Right") {
  #     ee_ff_gg_combined%>%
  #       filter(direction == "R") %>%
  #       ggplot() +
  #       geom_histogram(mapping = aes(x=spikeTimes),color = "black", fill = "plum4", bins = input$binSize) +
  #       labs(title = "Overall Firing Rate of Right Reward", y = "Count", x = "Time (sec)") + theme_bw()
  #   }
  #   
  #   
  #   
  #   #ISI graphs ee in and out
  #   
  #   else if(input$graphChoice == "ISI for Rat EE Inside Trial") {
  #     rawData_ee_total %>%
  #       group_by(rat) %>%
  #       summarise(spike_diff = diff(spikeTimes)) %>%
  #       ggplot() + 
  #       geom_histogram(mapping = aes(x = spike_diff), fill = "dodgerblue4", color = "black", bins =input$binSize) +
  #       labs(title = "Interspike Interval of EE Rat Inside Trial", y = "Count", x = "Time Difference (ms)") + 
  #       theme(axis.text.x = element_text(angle = 90)) + theme_bw() +scale_x_log10()
  #   }
  #   
  #   
  #   
  #   
  #   else if(input$graphChoice == "ISI for Rat EE Outside Trial") {
  #     rawData_ee_total_out %>%
  #       group_by(rat) %>%
  #       summarise(spike_diff = diff(spikeTimes)) %>%
  #       ggplot() + 
  #       geom_histogram(mapping = aes(x = spike_diff), color = "black", fill = "dodgerblue3",bins=input$binSize) +
  #       labs(title = "Interspike Interval of EE Rat Outside Trial", y = "Count", x = "Time Difference (ms)") + 
  #       theme(axis.text.x = element_text(angle = 90)) + theme_bw() +scale_x_log10()
  #   }
  #   
  #   
  #   
  #   
  #   #ISI graphs
  #   
  #   
  #   else if(input$graphChoice == "Overall ISI for Rat EE") {
  #     rawData_ee %>%
  #       group_by(rat) %>%
  #       summarise(spike_diff = diff(spikeTimes)) %>%
  #       ggplot() + 
  #       geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "steelblue",bins=input$binSize) +
  #       labs(title = "Overall Interspike Interval of EE Rat", y = "Count", x = "Time Difference (ms)") + 
  #       theme(axis.text.x = element_text(angle = 90)) + theme_bw()
  #   }
  #   
  #   
  #   
  #   else if(input$graphChoice == "Overall ISI for Rat FF") {
  #     rawData_ff  %>%
  #       group_by(rat) %>%
  #       summarise( spike_diff = diff(spikeTimes)) %>%
  #       ggplot() + 
  #       geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "seagreen4", bins=input$binSize) +
  #       labs(title = "Overall Interspike Interval of FF Rat", y = "Count", x = "Time Difference (ms)") + 
  #       theme(axis.text.x = element_text(angle = 90)) + theme_bw()
  #   }
  #   
  #   
  #   
  #   else if(input$graphChoice == "Overall ISI for Rat GG") {
  #     rawData_gg  %>%
  #       group_by(rat) %>%
  #       summarise( spike_diff = diff(spikeTimes)) %>%
  #       ggplot() + 
  #       geom_histogram(mapping = aes(x = spike_diff),color = "black", fill = "mediumpurple4", bins=input$binSize) +
  #       labs(title = "Overall Interspike Interval of GG Rat", y = "Count", x = "Time Difference (ms)") + 
  #       theme(axis.text.x = element_text(angle = 90)) + theme_bw()
  #   }
  #   
  #   
  #   
  #   
  #   
  #   #Synchrony inside vs outside for ee
  #   
  #   
  #   else if(input$graphChoice == "Synchrony for Rat EE Inside Trial") {
  #     ggplot(data = rawData_ee_total) +
  #       geom_histogram(aes(x = spikeTimes), binwidth = input$binSize, fill ="dodgerblue4") + 
  #       labs(title = "Neuronal Synchrony of EE Rat Inside Trials ",y ="Firing rate (Hz)",x = "Time (sec)")+
  #       theme_bw()
  #   }
  #   
  #   
  #   
  #   else if(input$graphChoice == "Synchrony for Rat EE Outside Trial") {
  #     ggplot(data = rawData_ee_total_out) +
  #       geom_histogram(aes(x = spikeTimes), binwidth = input$binSize, fill ="dodgerblue3") + 
  #       labs(title = "Neuronal Synchrony of EE Rat Outside Trials ",y ="Firing rate (Hz)",x = "Time (sec)")+
  #       theme_bw()
  #   }
  #   
  #   
  #   
  # })
  # 
  # #use for all graphs  + theme(text = element_text(size = 12)) 
  # 
  
}

shinyApp(ui = ui, server = server)







