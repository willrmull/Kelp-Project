library(dplyr)
library(ggplot2)
library(zoo)
library(vegan)
library(scales)
library(readr)

###Finding the richness of fish species###

###Filtering the dataset###
fish_url <- "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Monthly_Fish_All_Years_20240805.csv"
fish <- read_csv(url(fish_url))
fish <- subset(fish, COUNT > 0)
fish$DATE <- as.Date(fish$DATE, format = "%Y-%m-%d")
fish$DATE <- format(fish$DATE, "%Y-%m")

fish_richness <- fish %>% 
  group_by(DATE, SITE) %>%
  summarize(n = n_distinct(SP_CODE), na.rm = FALSE) %>%
  ungroup()

date <- fish_richness$DATE
fish_richness$DATE<- as.Date(paste(date, "-01", sep=""))

ggplot(fish_richness, aes(DATE, n, group = SITE, color = SITE), alpha = 0.3, ) +
  geom_point() +
  ylab("Species Present") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

###Graphs for each site###

###ABUR###

ggplot(subset(fish_richness, SITE == "ABUR"), aes(DATE, n), alpha = 0.3, ) +
  geom_point() +
  geom_smooth() +
  ylab("Species Present") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

###AQUE###

ggplot(subset(fish_richness, SITE == "AQUE"), aes(DATE, n), alpha = 0.3, ) +
  geom_point() +
  geom_smooth() +
  ylab("Species Present") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

###MOHK###

ggplot(subset(fish_richness, SITE == "MOHK"), aes(DATE, n), alpha = 0.3, ) +
  geom_point() +
  geom_smooth() +
  ylab("Species Present") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

