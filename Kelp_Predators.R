library(reshape)
library(tcltk2)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(reshape2)
library(zoo)
library(readr)

pred_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/byrnes_foodweb_table1%20(1).csv"
pred <- read_csv(url(pred_url))
#Alternative
pred <- read.csv("~/Documents/Fall 2024/Kelp Project/Kelp-Project/Datasets/byrnes_foodweb_table1 (1).csv")

###finds predators of giant kelp###
pred <- filter(pred, Prey == "Macrocystis pyrifera")


###WIP### ----Predator Fish-----
fish_url <- "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Monthly_Fish_All_Years_20240805.csv"
fish <- read_csv(url(fish_url))
fishpred <-subset(fish, SCIENTIFIC_NAME %in% pred$Prey)
fishpred  <- filter(fishpred , SCIENTIFIC_NAME == "Girella nigricans" | SCIENTIFIC_NAME == "Medialuna californiensis")
fishpred  <- filter(fishpred , COUNT > 0)

###WIP###----Urhcins-----######
urchin_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/SBS_Urchin_All_Years_20240823.csv"
urchins <- read_csv(url(urchin_url))
#ALternative
urchins <- read.csv("~/Documents/Fall 2024/Kelp Project/Kelp-Project/Datasets/SBS_Urchin_All_Years_20240823.csv")

###selecting relevant sites###

urchins  <- filter(urchins, SITE == "AQUE" |  SITE == "MOHK" |  SITE == "ABUR")

###Accounting for different species####

urchins_filtered <- urchins %>% 
  group_by(DATE, SITE, SCIENTIFIC_NAME) %>%
  summarize(count = sum(COUNT), size = sum(SIZE)) %>%
  ungroup()
urchins_filtered$DATE <- as.Date(urchins_filtered$DATE, "%Y-%m-%d")

###COUNT###

ggplot(urchins_filtered, aes(DATE, count, group = SITE, color = SITE), alpha = 0.3) +
  geom_point() +
  ylab("COUNT") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

###SIZE###`
ggplot(urchins_filtered, aes(DATE, size, group = SITE, color = SITE), alpha = 0.3) +
  geom_point() +
  ylab("Size") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

####Not acounting for different species###

urchins_filtered <- urchins %>% 
  group_by(DATE, SITE) %>%
  summarize(count = sum(COUNT), size = sum(SIZE)) %>%
  ungroup()
urchins_filtered$DATE <- as.Date(urchins_filtered$DATE, "%Y-%m-%d")

###COUNT###
ggplot(urchins_filtered, aes(DATE, count, group = SITE, color = SITE), alpha = 0.3) +
  geom_point() +
  ylab("COUNT") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

###SIZE###`
ggplot(urchins_filtered, aes(DATE, size, group = SITE, color = SITE), alpha = 0.3) +
  geom_point() +
  ylab("Size") +
  xlab("Date") +
  ggtitle("Fish Richness") +
  scale_x_date(date_labels = "%Y")

######################OTHER SPECIES###########################
filter(urchin_pred, Pred_taxon == "Urchins")

other_species_url <- "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Annual_All_Species_Biomass_at_transect_20240823.csv"
other_species = read_csv(url(other_species_url))

list <- c(pred$Predator)
other_species <- filter(other_species, SCIENTIFIC_NAME %in% list)

other_species <- subset(other_species, WM_GM2 >= 0)
other_species$DATE <- as.Date(other_species$DATE, format = "%Y-%m-%d")

ggplot(other_species, aes(DATE, WM_GM2, group = SCIENTIFIC_NAME, color = SITE), alpha = 0.3) +
  geom_point() +
  ylab("Wet WEight") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y%-m")

#####graphing based on mean wet weight######
other_species_filtered <- other_species %>% 
  group_by(DATE, SCIENTIFIC_NAME) %>%
  summarize(mean = mean(WM_GM2)) %>%
  ungroup()

ggplot(other_species_filtered, aes(DATE, mean, group = SCIENTIFIC_NAME), alpha = 0.3) +
  geom_point() +
  ylab("Wet Weight") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y%m")

#####Same thing but not accounting for species#####

other_species_filtered <- other_species %>% 
  group_by(DATE) %>%
  summarize(mean = mean(WM_GM2)) %>%
  ungroup()

ggplot(other_species_filtered, aes(DATE, mean), alpha = 0.3) +
  geom_point() +
  ylab("Size") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y%m")


