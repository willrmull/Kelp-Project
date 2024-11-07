library(dplyr)
library(ggplot2)
library(zoo)
library(vegan)
library(scales)
library(devtools)

npp_url = "https://raw.githubusercontent.com/willrmull/Kelp-Project/refs/heads/main/Datasets/Net%20primary%20production%2C%20growth%20and%20standing%20crop%20of%20Macrocystis%20pyrifera/Macrocystis%20pyrifera%20net%20primary%20production%20and%20growth%20with%20SE_20240325.csv"
npp <- read_csv(url(npp_url))

#Removes the Strings from Seasons
npp$Season <- stringr::str_remove_all(npp$Season, "-.*")
npp <- npp %>% mutate(Year = paste(npp$Year,npp$Season))
npp$Year <- as.Date(as.yearqtr(format(npp$Year), "%Y%q"))
npp_filtered <- subset(npp, Growth_rate_dry >= 0)

#Growth

ggplot(npp_filtered, aes(Year, Growth_rate_carbon, group = Site, color = Site), alpha = 0.3) +
  geom_point() 

ggplot(npp_filtered, aes(Year, Growth_rate_nitrogen, group = Site, color = Site), alpha = 0.3) +
  geom_point()

ggplot(npp_filtered, aes(Year, Growth_rate_dry, group = Site, color = Site), alpha = 0.3) +
  geom_point() 

# NPP over time

ggplot(npp_filtered, aes(Year, NPP_dry, group = Site, color = Site), alpha = 0.3) +
  geom_point() 
ggplot(npp_filtered , aes(Year, NPP_carbon, group = Site, color = Site), alpha = 0.3) +
  geom_point() 
ggplot(npp_filtered , aes(Year, NPP_nitrogen, group = Site, color = Site), alpha = 0.3) +
  geom_point() 

####NPP dry vs NPP of carbon or nitrigen####
ggplot(npp_filtered , aes(NPP_dry, NPP_carbon, group = Site, color = Site), alpha = 0.3) +
  geom_point() 
npp_filtered$NPP_dry/npp_filtered$NPP_carbon
####NPP_Dry/NPP_Carbon over time####
dry.carbon <- npp_filtered$NPP_dry/npp_filtered$NPP_carbon

####Interesting graph####
ggplot(npp_filtered, aes(Year, dry.carbon, group = Site, color = Site), alpha = 0.3) +
  geom_point()

npp_filtered$NPPtoGrowth <- npp_filtered$NPP_dry/npp_filtered$Growth_rate_dry
ggplot(npp_filtered , aes(Year, NPPtoGrowth, group = Site, color = Site), alpha = 0.3) +
  geom_point() + geom_line()
ggplot(npp_filtered , aes(Growth_rate_dry, NPP_dry, group = Site, color = Site), alpha = 0.3) +
  geom_point() + geom_line()

#Growth and nutrients

ggplot(npp_filtered, aes(Growth_rate_dry, Growth_rate_carbon, group = Site, color = Site), alpha = 0.3) +
  geom_point() + geom_line()
ggplot(npp_filtered , aes(Growth_rate_dry, Growth_rate_nitrogen, group = Site, color = Site), alpha = 0.3) +
  geom_point() + geom_line()

