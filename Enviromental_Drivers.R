library(dplyr)
library(ggplot2)
library(zoo)
library(vegan)
library(scales)
library(stringr)
library(readr)

waves_url <- "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Macrocystis%20pyrifera%20biomass%20and%20environmental%20drivers/kelp_no3_waves_quarterly_long.csv"
waves <- pred <- read_csv(url(waves_url))

waves <- waves %>% mutate(year = paste(year,quarter))
waves$year <- as.yearqtr(format(waves$year), "%Y%q")

###kelp and no3###
ggplot(waves, aes(no3, kelp)) +
  geom_point() + geom_smooth()

###kelp and no3 individually compared to year###
ggplot(waves, aes(year, kelp)) +
  geom_point() +
  scale_x_yearqtr(format = '%Y-%q')
ggplot(waves, aes(year, no3)) +
  geom_point() +
  scale_x_yearqtr(format = '%Y-%q')

#ratio without log10 scale#
waves$ratio <- waves$kelp/waves$no3
ggplot(waves, aes(year, ratio)) +
  geom_point() +
  scale_y_log10() +
  scale_x_yearqtr(format = '%Y-%q')
#ratio with log 10 scale#
waves$ratio <- waves$kelp/waves$no3
ggplot(waves, aes(year, ratio)) +
  geom_point() +
  scale_y_log10() +
  scale_x_yearqtr(format = '%Y-%q')

##USING MEANS###

waves_filtered <- waves %>% 
  group_by(year) %>%
  summarize(kelp = mean(kelp), no3 = mean(no3))

###kelp and no3###
ggplot(waves_filtered, aes(no3, kelp)) +
  geom_point() + geom_smooth()
ggplot(waves_filtered, aes(year, kelp)) +
  geom_point() + geom_smooth()

#ratio without log10 scale#
waves_filtered$ratio <- waves_filtered$kelp/waves_filtered$no3
ggplot(waves_filtered, aes(year, ratio)) +
  geom_point() +
  scale_x_yearqtr(format = '%Y-%q')

#ratio with log 10 scale#
ggplot(waves_filtered, aes(year, ratio)) +
  geom_point() +
  scale_y_log10() +
  scale_x_yearqtr(format = '%Y-%q')

ggplot(waves_filtered, aes(year, kelp)) +
  geom_point() + geom_smooth()
scale_x_yearqtr(format = '%Y-%q')
ggplot(waves_filtered, aes(year, no3)) +
  geom_point() +
  scale_x_yearqtr(format = '%Y-%q')

