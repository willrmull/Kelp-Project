library(reshape)
library(tcltk2)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(reshape2)
library(zoo)
library(readr)

urchin_pred_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/byrnes_foodweb_table1%20(1).csv"
urchin_pred <- read_csv(url(urchin_pred_url))
#Alternative
urchin_pred <- read.csv("~/Documents/Fall 2024/Kelp Project/Kelp-Project/Datasets/byrnes_foodweb_table1 (1).csv")
###finds predators of urchin###

urchin_pred <- filter(urchin_pred, Pred_taxon == "Urchins")
urchin_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Annual_All_Species_Biomass_at_transect_20240823.csv"
urchin_counts <- read_csv(url(urchin_url))

#Alternative
urchin_counts<- read.csv("~/Documents/Fall 2024/Kelp Project/Kelp-Project/Datasets/SBS_Urchin_All_Years_20240823.csv")


list_urchins <- c(urchin_pred$Predator)
urchin_counts <- filter(urchin_counts, SCIENTIFIC_NAME %in% list_urchins)

urchin_counts <- subset(urchin_counts, WM_GM2 >= 0)
urchin_counts$DATE <- as.Date(urchin_counts$DATE, format = "%Y-%m-%d")
###Wet weight####

ggplot(urchin_counts, aes(DATE, WM_GM2, group = SCIENTIFIC_NAME, color = SITE), alpha = 0.3) +
  geom_point() +
  geom_smooth() +
  ylab("Wet WEight") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y%-m")

###Dry weight####
ggplot(urchin_counts, aes(DATE, DRY_GM2, group = SCIENTIFIC_NAME, color = SITE), alpha = 0.3) +
  geom_point() +
  geom_smooth() +
  ylab("Wet WEight") +
  xlab("Date") +
  scale_x_date(date_labels = "%Y%-m")

###Same thing but for sites of interest####

###Wet weight####
 urchins_filtered <- subset(urchin_counts, SITE =="ABUR" | SITE == "MOHK" | SITE == "AQUE")
 ggplot( urchins_filtered, aes(DATE, WM_GM2, group = SCIENTIFIC_NAME, color = SITE), alpha = 0.3) +
   geom_point() +
   geom_smooth() +
   ylab("Wet WEight") +
   xlab("Date") +
   scale_x_date(date_labels = "%Y%-m")
 
 ###Dry weight####
 ggplot( urchins_filtered, aes(DATE, DRY_GM2, group = SCIENTIFIC_NAME, color = SITE), alpha = 0.3) +
   geom_point() +
   geom_smooth() +
   ylab("Wet WEight") +
   xlab("Date") +
   scale_x_date(date_labels = "%Y%-m")
 
 