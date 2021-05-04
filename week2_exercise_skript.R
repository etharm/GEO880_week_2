library(readr)        
library(dplyr)       
library(ggplot2)      
library(sf)           
library(terra)        
library(lubridate)
library(purrr)
library(zoo)
library(tidyr)
library(scales)

#IMPORT DATA

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
View(wildschwein_BE)

#TASK 1: GETTING AN OVERVIEW

# grouping animals by ID
wildschwein_BE <- group_by(wildschwein_BE,TierID)

# calculate time lag

# error: character string is not in a standard unambiguous format
#wildschwein_BE <- wildschwein_BE %>%
  #mutate(timelag = as.integer(difftime(lead(DatetimeUTC), DatetimeUTC), units = "secs"))

# modify time format of Column DateTimeUTC
wildschwein_BE$DateTimeUTC <- format(as.POSIXct(strptime(wildschwein_BE$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
wildschwein_BE$DateTimeUTC <- ymd_hm(wildschwein_BE$DateTimeUTC)


# calculate and and new column with time lag
as.integer(difftime(lead(wildschwein_BE$DateTimeUTC),wildschwein_BE$DateTimeUTC, units = "secs"))
wildschwein_BE$timelag <- as.integer(difftime(lead(wildschwein_BE$DateTimeUTC),wildschwein_BE$DateTimeUTC, units = "secs"))

wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timeLag = as.integer(difftime(lead(DateTimeUTC), DateTimeUTC, units = "secs")))

# How many individuals were tracked?
individuals <- length(unique(wildschwein_BE$TierID)) #3 individuals were tracked...
individuals

individalsNames <- unique(wildschwein_BE$TierName) #... with names Sabi, Rosa and Ruth
individalsNames

# For how long were the individual tracked? Are there gaps?
# Were all individuals tracked concurrently or sequentially?
# What is the temporal sampling interval between the locations?

wildschwein_BE %>%
  group_by(TierName) %>%
  summarise(min_time = min(DateTimeUTC), max_time= max(DateTimeUTC), Tracking_Period = difftime(max_time, min_time), min_lag = min(timelag, na.rm=T), max_lag = max(timelag, na.rm=T), median_lag = median(timelag, na.rm=T))

# Tracking Period varies between 235 and 339 days.The largest gap is 60360s
# Rosa and Ruth have the same start tracking day (concurrently) but Ruth got tracked around a month longer. Ruth and Sabi have the same end tracking day but Sabi had a much longer tracking period.
# Rosa and Ruth got tracked after Sabi (sequentially). But no tracking period is exactly like another one
# There is no strict sampling interval

# Visualisation of tracking duration
ggplot(wildschwein_BE, aes(x=DateTimeUTC, y=TierID)) + geom_point() +scale_x_datetime(date_labels = "%b-%Y")

# Visualisation histogram of limelag
wildschwein_BE %>%
  filter(timelag < 15000) %>%
  filter(timelag > 0) %>% #Why negative values, even though grouping by TierID
  ggplot(., aes(timelag)) +
  geom_histogram(binwidth = 100) +
  scale_y_log10()


timelagCount <- data.frame(table(wildschwein_BE$timelag))
timelagCount <- timelagCount[-c(1,2,3),] #Why negative values, even though grouping by TierID

# Visualisation change in time lag (Sep14-Jan15)
wildschwein_BE %>%
  filter(timelag < 30000) %>%
  filter(timelag > 0) %>%
  ggplot(., aes(DateTimeUTC, timelag, col = TierID)) +
  geom_point() +
  scale_x_datetime(limits = c(as.POSIXct("2014-09-01 00:00 UTC"), as.POSIXct("2015-01-01 00:00 UTC"))) +
  geom_line(size = 0.1)



