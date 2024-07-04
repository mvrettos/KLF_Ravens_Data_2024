#' Daily probability of presence at locations in park
#' Varalika Jain

#------------------------------------------------------------------------------
####----(i) LOAD LIBRARIES----####
library(rgdal)
library(sp)
library(terra)
library(move)
library(lubridate)
library(dplyr)
library(sf)
library(tidyr)
library(chron)
library(ggplot2)
library(reshape)
library(plyr)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(ii) DOWNLOAD DATA FROM MOVEBANK (MOVE OBJECT)----####
#' Permissions required to download data
#' Input movebank login details
login <- movebankLogin()

#' Source data from GPS tagged ravens
ravens <- getMovebankData(study="Common Ravens in the Eastern Alps",
                          
                          login=login,
                          timestamp_start = "20230101000000000", 
                          timestamp_end = "20230115000000000",
                          removeDuplicatedTimestamps=TRUE)

#' Assigning the correct time zone (accounts for daylight savings)
timestamps(ravens) <- with_tz(timestamps(ravens), tz="Europe/Vienna")

#' Check the timezone
head(timestamps(ravens))

#' Check individuals
levels(ravens@trackId)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iii) CONVERT MOVE OBJECT TO DATAFRAME----####
ravens_df <- as(ravens, "data.frame")
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(iv) SELECTING DAYTIME POINTS---####
####----(1) Mark real dawn and dusk----#### 
#' Create a dataframe with timestamps and location for 'suncalc' package
sun <- data.frame(date= as.Date(ravens_df$timestamps, tz = "Europe/Vienna"), 
                  lat=ravens_df$location_lat, lon=ravens_df$location_long)


devtools::install_github("datastorm-open/suncalc")
require(suncalc)

#' Caluclate sunrise and sunset times
sunrise <-getSunlightTimes(data=sun, keep="sunrise", tz = "Europe/Vienna") 
sunset <- getSunlightTimes(data=sun, keep="sunsetStart", tz = "Europe/Vienna") 

#' Creating a new column for the move object, where fixes that have timestamps
#' before sunrise and after sunset (i.e., night) should be marked as 1, else 0
ravens_df$night_day <- ifelse(ravens_df$timestamps < sunrise$sunrise | 
                                ravens_df$timestamps > sunset$sunset,
                              night <- 1, night <- 0)

#' Proportion of night to daytime points by month
ravens_df %>% 
  mutate(month = format(as.Date(timestamps), "%m")) %>%
  ggplot(aes(month))+geom_bar(aes(fill = as.factor(night_day)), position = "fill")
#' bias of night time points (1) in winter months with less daylight

####----(2) Retain GPS locations only in the day----####
ravens_day <- ravens_df %>% filter(night_day == 0)
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
####----(v) MODIFY DATAFRAME FOR REQUIRED INFO----####
#' Filter columns needed
head(ravens_day)
ravens_filter <- ravens_day[,c("location_lat",
                               "location_long",
                               "timestamps",
                               "local_identifier")]

####----(1) Creating columns:date & time----####
#' Check to see if timestamps are still in local time
head(ravens_filter$timestamps)

# Create a column with the date 
ravens_filter$date <- as.Date(ravens_filter$timestamps, tz = "Europe/Vienna")

# Create a column with the time 
ravens_filter$time <- format(ravens_filter$timestamps, format = "%H:%M:%S")


#' Local identifier as factor
class(ravens_filter$local_identifier)
ravens_filter$local_identifier <- as.factor(ravens_filter$local_identifier)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vi) FOUR MONTHS POST RELEASE----####
####----(1) Subset data to four months post release---####
#" release dates
release_dates <- as.Date(c("2017-10-10", "2018-09-04",
                           "2019-08-30", "2020-09-03", "2021-09-06",
                           "2022-09-09"), format="%Y-%m-%d")

dates <- as.data.frame(release_dates)
dates$release_dates <- as.Date(dates$release_dates, format = "%Y-%m-%d")
dates$month_release <- release_dates + 122 #four months post-release


#' Filter raven movement data to those four months post-release
ravens_month <- ravens_filter %>% subset(date >= dates$release_dates & 
                                           date < dates$month_release)

#' add release date column to data
ravens_month$release_date <- 
  ifelse(ravens_month$date >= dates$release_dates[1] & 
           ravens_month$date < dates$month_release[1], paste0(dates$release_dates[1]),
         ifelse(ravens_month$date >= dates$release_dates[2] & 
                  ravens_month$date < dates$month_release[2], paste0(dates$release_dates[2]),
                ifelse(ravens_month$date >= dates$release_dates[3] & 
                         ravens_month$date < dates$month_release[3], paste0(dates$release_dates[3]),
                       ifelse(ravens_month$date >= dates$release_dates[4] & 
                                ravens_month$date < dates$month_release[4], paste0(dates$release_dates[4]),
                              ifelse(ravens_month$date >= dates$release_dates[5] & 
                                       ravens_month$date < dates$month_release[5], paste0(dates$release_dates[5]),
                                     paste0(dates$release_dates[6]))))))

#' nest by release date 
ravens_month_nested <- ravens_month %>% nest(data = -c(release_date))

####----(2) Group by bi-weekly intervals from release date----####
ravens_biweek <- list()

for (i in 1:nrow(ravens_month_nested)){
  ravens_biweek[[i]] <- ravens_month_nested$data[[i]] %>%
    mutate(week = cut(date, breaks=seq.Date(min(date), 
                                            max(date)+14,
                                            by = 14)))
}

ravens_biweek_df <- do.call(rbind.data.frame, ravens_biweek)

#'check the weeks
levels(ravens_biweek_df$week)
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
####----(vii) INTERSECTION WITH LOCATIONS WITHIN PARK----####
####----(1) Convert raven data into Austria Lambert projection---####
range(ravens_biweek_df$location_long)
ravens_coords <- ravens_biweek_df

coordinates(ravens_coords) <-  c("location_long", "location_lat")
proj4string(ravens_coords) <- CRS("+proj=longlat +datum=WGS84")
ravens_proj <- spTransform(ravens_coords, CRS("EPSG:3416"))
#' for more info, see https://epsg.io/3416 

####----(2) Convert raven data (Lambert projection) into an sf object----####
presence_sf <- st_as_sf(x = ravens_proj, 
                        coords = c("location_long", "location_lat"),
                        crs = "EPSG:3416")

####----(3) Convert location data into an Lambert projection----####
#' Read in the coordinates for the park locations of interest
park_coords <- read.csv("FI_GPS coordinates.csv")

coordinates(park_coords) <- c("Longitude", "Latitute")
proj4string(park_coords) <- CRS("+proj=longlat +datum=WGS84")
park_coords_utm <- spTransform(park_coords, CRS("EPSG:3416"))
park_coords_utm <- as.data.frame(park_coords_utm)
head(park_coords_utm)

####----(4) Convert location data (Lambert projection) into an sf object----####
park_sf <- st_as_sf(x = park_coords_utm, 
                    coords = c("coords.x1", "coords.x2"),
                    crs = "EPSG:3416")
head(park_sf)

####----(5) Get the geom of location values----####
geom_locs = st_geometry(park_sf, CRS("EPSG:3416"))
head(geom_locs)

####----(6) Checking the projections----####
projection(presence_sf)
projection(park_sf)
projection(geom_locs)

####----(7) Set buffers----####
#' Dist is in meters, setting a buffer for 100m
park_buf <- st_buffer(geom_locs, dist = 100)

####----(8) Calculating the intersection between fixes and buffers----####
#' For each GPS fix, calculate whether or not it intersects with any of the bufs
#' #' If it does not intersect, in the intersection column, paste " ", else "AFS"
presence_sf <- st_set_crs(presence_sf, "EPSG:3416")

#' checks to make sure everything is correct
projection(presence_sf)
projection(park_buf)
class(presence_sf$geometry)

#' intersection
intersect_dat <- presence_sf %>% mutate(
  intersection = as.character(st_intersects(geometry, park_buf)),
  intersect = as.numeric(intersection),
  location = if_else(is.na(intersect), "0", paste0("1"))) 
  
intersect_df <- as.data.frame(intersect_dat)
levels(as.factor(intersect_df$intersect))

#' merge intersection data with raven data again 
presence_intersect <- merge.data.frame(intersect_df, ravens_filter, 
                                       by = c("local_identifier", "timestamps", "date","time"))


presence_intersect$intersect <- as.factor(presence_intersect$intersect)
presence_intersect$intersect <- revalue(presence_intersect$intersect, c("1" = "horse",
                                                                        "2" = "bear-wolf",
                                                                        "3" = "wildboar",
                                                                        "4" = "deer",
                                                                        "5" = "cliff"))
head(presence_intersect)
#------------------------------------------------------------------------------

