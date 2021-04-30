library(readr)        
library(dplyr)       
library(ggplot2)      
library(sf)           
library(terra)        
library(lubridate)

#IMPORT DATA

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)