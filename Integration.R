library(vegan)
library(reshape)
library(tcltk2)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(data.table)
library(tibble)
library(sads)
library(reshape2)
library(zoo)
library(readr)

import_url <- "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Net%20primary%20production%2C%20growth%20and%20standing%20crop%20of%20Macrocystis%20pyrifera/Macrocystis%20pyrifera%20net%20primary%20production%20and%20growth%20with%20SE_20240325.csv"
import <- read_csv(url(import_url))
#Alternative
umport <- read.csv("~/Documents/Fall 2024/Kelp Project/Kelp-Project/Datasets/Net primary production, growth and standing crop of Macrocystis pyrifera/Macrocystis pyrifera net primary production and growth with SE_20240325.csv")

#Removes the Strings from Seasons
import$Season <- stringr::str_remove_all(import$Season, "-.*")

imp <- import %>% mutate(Year = paste(import$Year,import$Season))

imp$Year <- as.Date(as.yearqtr(format(imp$Year), "%Y%q"))
imp.new<- subset(npp, Growth_rate_dry >= 0)


# Slope Approximation -----------------------------------------------------
new.ABUR <- imp.new[imp.new$Site == "ABUR",]
new.ABUR$x <- 1:78
results_newABUR <- data.frame(matrix(NA, nrow = length(seq(1:78)), 
                                     ncol = length(seq(1:1))))
for (rowIdx in 1:nrow(results_newABUR)) {
  for (colIdx in 1:ncol(results_newABUR)) {
    results_newABUR[rowIdx, colIdx] <- ( new.ABUR$NPP_carbon[rowIdx+1] - new.ABUR$NPP_carbon[rowIdx]) / (new.ABUR$x[rowIdx+1] - new.ABUR$x[rowIdx]) # or whatever value you want here
  }
}

new.ABUR$results <- results_newABUR$matrix.NA..nrow...length.seq.1.78....ncol...length.seq.1.1...

new.MOHK <- imp.new[imp.new$Site == "MOHK",]
new.MOHK$x <- 1:81
results_newMOHK <- data.frame(matrix(NA, nrow = length(seq(1:81)), 
                                     ncol = length(seq(1:1))))
for (rowIdx in 1:nrow(results_newMOHK)) {
  for (colIdx in 1:ncol(results_newMOHK)) {
    results_newMOHK[rowIdx, colIdx] <- (new.MOHK$NPP_carbon[rowIdx+1] - new.MOHK$NPP_carbon[rowIdx]) / (new.MOHK$x[rowIdx+1] - new.MOHK$x[rowIdx]) # or whatever value you want here
  }
}

new.MOHK$results <- results_newMOHK$matrix.NA..nrow...length.seq.1.81....ncol...length.seq.1.1...

new.AQUE <- imp.new[imp.new$Site == "AQUE",]
new.AQUE$x <- 1:74
results_newAQUE <- data.frame(matrix(NA, nrow = length(seq(1:74)), 
                                     ncol = length(seq(1:1))))
for (rowIdx in 1:nrow(results_newAQUE)) {
  for (colIdx in 1:ncol(results_newAQUE)) {
    results_newAQUE[rowIdx, colIdx] <- (new.AQUE$NPP_carbon[rowIdx+1] - new.AQUE$NPP_carbon[rowIdx]) / (new.AQUE$x[rowIdx+1] - new.AQUE$x[rowIdx]) # or whatever value you want here
  }
}

new.AQUE$results <- results_newAQUE$matrix.NA..nrow...length.seq.1.74....ncol...length.seq.1.1...

recombine <- rbind(new.ABUR, new.AQUE, new.MOHK)

ggplot(recombine, aes(Year, results, group = Site, color = Site), alpha = 0.3) +
  geom_point() 
