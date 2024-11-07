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

###finds predators of giant kelp###
pred <- filter(pred, Prey == "Macrocystis pyrifera")


###WIP### ----Predator Fish-----
fishpred <- read.csv("~/Documents/Fall 2024/Kelp Project/Datasets/Monthly_Fish_All_Years_20240805.csv")
fishpred <-subset(fishpred, SCIENTIFIC_NAME %in% pred$Prey)
fishpred  <- filter(fishpred , SCIENTIFIC_NAME == "Girella nigricans" | SCIENTIFIC_NAME == "Medialuna californiensis")
fishpred  <- filter(fishpred , COUNT > 0)

###WIP###----Urhcins-----######
urchin_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Net%20primary%20production%2C%20growth%20and%20standing%20crop%20of%20Macrocystis%20pyrifera/Macrocystis%20pyrifera%20net%20primary%20production%20and%20growth%20with%20SE_20240325.csv"
urchins <- read_csv(url(urchin_url))
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

other_species_url <- filter(urchin_pred, Pred_taxon == "Urchins")
other_species = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Annual_All_Species_Biomass_at_transect_20240823.csv"

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


