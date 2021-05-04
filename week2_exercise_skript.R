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
library(sp)


library(raster)
library(geosphere)
library(rgeos)
#-------------------------------------------------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------------------------------------------------

# TASK 2: DERIVING MOVEMENT PARAMETERS I: SPEED

# calculate step length (distance between two gps points)
wildschwein_BE <-  wildschwein_BE %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))


# caluclate speed (distance/time)
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)
#-------------------------------------------------------------------------------------------------------------------------

# TASK 3: CROSS-SCALE MOVEMENT ANALYSIS

#import data
caro60 <- read_delim("caro60.csv",",")
caro60 <- st_as_sf(caro60, coords = c("E", "N"), crs = 2056, remove = FALSE)
nrow(caro60) #200

# timelag, steplength and speed for caro60

# modify time format of Column DateTimeUTC
caro60$DateTimeUTC <- format(as.POSIXct(strptime(caro60$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
caro60$DateTimeUTC <- ymd_hm(caro60$DateTimeUTC)


# calculate and and new column with time lag
caro60$timelag <- as.integer(difftime(lead(caro60$DateTimeUTC),caro60$DateTimeUTC, units = "secs"))

#steplength
caro60 <-  caro60 %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

#speed
caro60 <- caro60 %>%
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)



#Subsetting
caro_3 <- caro60[seq(1,nrow(caro60),by=3),] #every 3rd
caro_6 <- caro60[seq(1,nrow(caro60),by=6),] #every 6th
caro_9 <- caro60[seq(1,nrow(caro60),by=9),] #every 9th
nrow(caro_3) #67
nrow(caro_6) #34
nrow(caro_9) #23

# timelag, steplength and speed for caro_3

# modify time format of Column DateTimeUTC
caro_3$DateTimeUTC <- format(as.POSIXct(strptime(caro_3$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
caro_3$DateTimeUTC <- ymd_hm(caro_3$DateTimeUTC)


# calculate and and new column with time lag
caro_3$timelag <- as.integer(difftime(lead(caro_3$DateTimeUTC),caro_3$DateTimeUTC, units = "secs"))

#steplength
caro_3 <-  caro_3 %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

#speed
caro_3 <- caro_3 %>%
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)

# timelag, steplength and speed for caro_6

# modify time format of Column DateTimeUTC
caro_6$DateTimeUTC <- format(as.POSIXct(strptime(caro_6$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
caro_6$DateTimeUTC <- ymd_hm(caro_6$DateTimeUTC)


# calculate and and new column with time lag
caro_6$timelag <- as.integer(difftime(lead(caro_6$DateTimeUTC),caro_6$DateTimeUTC, units = "secs"))

#steplength
caro_6 <-  caro_6 %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

#speed
caro_6 <- caro_6 %>%
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)

# timelag, steplength and speed for caro_9

# modify time format of Column DateTimeUTC
caro_9$DateTimeUTC <- format(as.POSIXct(strptime(caro_9$DatetimeUTC, "%d.%m.%Y %H:%M", tz="UTC")), format = "%Y-%m-%d %H:%M")
caro_9$DateTimeUTC <- ymd_hm(caro_9$DateTimeUTC)


# calculate and and new column with time lag
caro_9$timelag <- as.integer(difftime(lead(caro_9$DateTimeUTC),caro_9$DateTimeUTC, units = "secs"))

#steplength
caro_9 <-  caro_9 %>%
  group_by(TierName) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

#speed
caro_9 <- caro_9 %>%
  group_by(TierName) %>%
  mutate(speed = steplength/timelag)

#Visualisation of coarser datasets

#from points to line
caro60_line <- caro60 %>% group_by(TierName) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
caro_3_line <- caro_3 %>% group_by(TierName) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
caro_6_line <- caro_6 %>% group_by(TierName) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")
caro_9_line <- caro_9 %>% group_by(TierName) %>% summarise(do_union = FALSE) %>% st_cast("LINESTRING")

#plots

#3min
ggplot() +
  geom_sf(data = caro60, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro60_line, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro_3, aes(color = "3 minutes"), alpha = 1)+
  geom_sf(data = caro_3_line, aes(color = "3 minutes"), alpha = 1)+
  coord_sf(datum = 2056) +
  scale_color_manual(name = "Trajectory", breaks = c("1 minute", "3 minutes"), values = c("1 minute" = "mistyrose", "3 minutes" = "turquoise")) +
  ggtitle("Comparing original- with 3 minutes-resampled data") +
  labs(x="E", y="N") #How to show only every second tick on both axis? and change alpha in legend?

#6min
ggplot() +
  geom_sf(data = caro60, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro60_line, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro_6, aes(color = "6 minutes"), alpha = 1)+
  geom_sf(data = caro_6_line, aes(color = "6 minutes"), alpha = 1)+
  coord_sf(datum = 2056) +
  scale_color_manual(name = "Trajectory", breaks = c("1 minute", "6 minutes"), values = c("1 minute" = "mistyrose", "6 minutes" = "turquoise")) +
  ggtitle("Comparing original- with 6 minutes-resampled data") +
  labs(x="E", y="N") #How to show only every second tick on both axis? and change alpha in legend?

#9min
ggplot() +
  geom_sf(data = caro60, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro60_line, aes(color = "1 minute"), alpha = 0.8) +
  geom_sf(data = caro_9, aes(color = "9 minutes"), alpha = 1)+
  geom_sf(data = caro_9_line, aes(color = "9 minutes"), alpha = 1)+
  coord_sf(datum = 2056) +
  scale_color_manual(name = "Trajectory", breaks = c("1 minute", "9 minutes"), values = c("1 minute" = "mistyrose", "9 minutes" = "turquoise")) +
  ggtitle("Comparing original- with 9 minutes-resampled data") +
  labs(x="E", y="N") #How to show only every second tick on both axis? and change alpha in legend?


#Less movement is visible. Connections between more stationary sections are reduced to straight lines. Stationary sections has smaller diameter


#Comparison of speed at different sampling intervals
ggplot() +
  geom_line(data= caro60, aes(x=DateTimeUTC, y= speed, color="1 minute")) +
  geom_line(data= caro_3, aes(x=DateTimeUTC, y= speed, color="3 minutes")) +
  geom_line(data= caro_6, aes(x=DateTimeUTC, y= speed, color="6 minutes")) +
  geom_line(data= caro_9, aes(x=DateTimeUTC, y= speed, color="9 minutes")) +
  scale_color_manual(name = "colour", breaks = c("1 minute", "3 minutes", "6 minutes", "9 minutes"), values = c("1 minute" = "red", "3 minutes" = "green", "6 minutes" = "blue", "9 minutes" = "purple")) +
  ggtitle("Comparing derived speed at different sampling intervals")+
  labs(x= "Time", y="Speed (m/s)")
#-------------------------------------------------------------------------------------------------------------------------


#TASK 4: DERIVING MOVEMENT PARAMETERS II: ROLLING WINDOW FUNCTIONS

example <- rnorm(10)
rollmean(example,k = 3,fill = NA,align = "left")
rollmean(example,k = 4,fill = NA,align = "left")

caroRW <- caro60 %>%
  mutate(RW3 = rollmean(speed, k= 3, fill = NA, align = "left"),
         RW4 = rollmean(speed, k= 4, fill = NA, align = "left"),
         RW10 = rollmean(speed, k= 10, fill = NA, align = "left"),
         RW20 = rollmean(speed, k= 20, fill = NA, align = "left"),
         RW30 = rollmean(speed, k= 30, fill = NA, align = "left"),
         RW50 = rollmean(speed, k= 50, fill = NA, align = "left"))



ggplot(data = caroRW)+
  geom_line(aes(x = DateTimeUTC, y = speed, color ="True"))+
  geom_line(aes(x = DateTimeUTC, y = RW4, color ="RW4"))+
  geom_line(aes(x = DateTimeUTC, y = RW10, color ="RW10"))+
  geom_line(aes(x = DateTimeUTC, y = RW20, color ="RW20"))+
  geom_line(aes(x = DateTimeUTC, y = RW30, color ="RW30"))+
  geom_line(aes(x = DateTimeUTC, y = RW50, color ="RW50"))+
  scale_color_manual(name = "Rolling Window", breaks = c("True","RW3", "RW4","RW10", "RW20", "RW30", "RW50"), values = c("True" = "orange", "RW3" = "orangered1", "RW4" = "orchid", "RW10" = "turquoise4", "RW20" = "steelblue", "RW30" = "springgreen", "RW50" = "maroon")) +
  ggtitle("Varying Rolling Window sizes applied on speed")+
  labs(x= "Time", y="Speed (m/s)")
  


# If the rolling window is not too large the varying characteristics are still visible. But if the rolling window is too large, you will get a mean over the whole time period
# Analogy: Smoothing image with 9x9 window and calculating the average. But if the window is as large as the image (pixelxpixel) the image will only show one value

#-------------------------------------------------------------------------------------------------------------------------

