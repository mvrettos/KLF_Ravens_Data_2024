#The purpose of this script is to understand how different AFS are exploited by the ravens
all.dat <- read.csv("all.data.csv")
head(all.dat)

#Packages 
library(recurse)
library(move)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(raster)
library(scales)
library(viridis)
library(lubridate)
library(reshape2)
library(raster)
library(rworldmap)
library(maptools)
library(cluster)

head(all.dat$timestamps)
library(lubridate)
all.dat$timestamps <- ymd_hms(all.dat$timestamps, tz="Europe/Vienna")

library(sp)
all.dat.coords <- all.dat[c("location_long", "location_lat", "timestamps", "local_identifier")]
coordinates(all.dat.coords) <- c("location_long", "location_lat")
proj4string(all.dat.coords) <- CRS("+proj=longlat +datum=WGS84")
all.dat.UTM <- spTransform(all.dat.coords, CRS("+proj=utm +zone=33 ellps=WGS84"))
all.dat.UTM <- as.data.frame(all.dat.UTM)
all.dat.UTM <- all.dat.UTM[c(3,4,1,2)]
head(all.dat.UTM)

#Read in location data 
locs <- read.csv("AFSs.csv")

#Convert location data into UTM projection
locs.coords <- locs[c("Name", "long", "lat", "Type")]
coordinates(locs.coords) <- c("long", "lat")
proj4string(locs.coords) <- CRS("+proj=longlat +datum=WGS84")
locs.UTM <- spTransform(locs.coords, CRS("+proj=utm +zone=33 ellps=WGS84"))
#Convert back to data frame 
locs.UTM <- as.data.frame(locs.UTM)
head(locs.UTM)

#save(list = ls(all = T), file = 'recurse.RData')

#Splitting up the data into categories to set specific radii
locs.wildparks <- subset(locs.UTM, Type == c("Wildpark", "KL"))


####WILDPARKS####
#Revists for just wildparks 
library(recurse)
locs.wildparks <- droplevels(locs.wildparks)
locs.wildparks <- split(locs.wildparks, locs.wildparks$Name)

wildparks <- rapply(locs.wildparks, length, how = "list")
wp.stats <- rapply(locs.wildparks, length, how = "list")

for( i in (1:length(locs.wildparks))){
  wildparks[[i]] <- getRecursionsAtLocations(
    all.dat.UTM,
    locs.wildparks[[i]][3:4], #Selecting just for lat and long columns
    radius = 80, #Select radii according to AFS type
    threshold = 0, #a time difference (in units timeunits) to ignore excursions outside the radius.
    timeunits = c("hours"),
    verbose = TRUE
  )
  wp.stats[[i]] <- wildparks[[i]]$revisitStats #Creates a df with the revisit stats
}


####DUMPS####
locs.dumps <- locs.UTM %>% filter (Type == c("Compost"))
#Revists for just wildparks 
locs.dumps <- droplevels(locs.dumps)
locs.dumps <- split(locs.dumps, locs.dumps$Name)

dumps <- rapply(locs.dumps, length, how = "list")
d.stats <- rapply(locs.dumps, length, how = "list")

for( i in (1:length(locs.dumps))){
  dumps[[i]] <- getRecursionsAtLocations(
    all.dat.UTM,
    locs.dumps[[i]][3:4], #Selecting just for lat and long columns
    radius = 100, #Select radii according to AFS type
    threshold = 0, #a time difference (in units timeunits) to ignore excursions outside the radius.
    timeunits = c("hours"),
    verbose = TRUE
  )
d.stats[[i]] <- dumps[[i]]$revisitStats #Creates a df with the revisit stats
}

####HOTELS####
locs.hotels <- locs.UTM %>% filter (Type == c("Hotel"))
#Revists for just wildparks 
locs.hotels <- droplevels(locs.hotels)
locs.hotels <- split(locs.hotels, locs.hotels$Name)

hotels <- rapply(locs.hotels, length, how = "list")
h.stats <- rapply(locs.hotels, length, how = "list")

for( i in (1:length(locs.hotels))){
  hotels[[i]] <- getRecursionsAtLocations(
    all.dat.UTM,
    locs.hotels[[i]][3:4], #Selecting just for lat and long columns
    radius = 70, #Select radii according to AFS type
    threshold = 0, #a time difference (in units timeunits) to ignore excursions outside the radius.
    timeunits = c("hours"),
    verbose = TRUE
  )
  h.stats[[i]] <- hotels[[i]]$revisitStats #Creates a df with the revisit stats
}

####FARMS####
locs.farms <- locs.UTM %>% filter (Type == c("Farm", "Supermarket"))

#Revists for just hotels 
locs.farms <- droplevels(locs.farms)
locs.farms <- split(locs.farms, locs.farms$Name)

farms <- rapply(locs.farms, length, how = "list")
f.stats <- rapply(locs.farms, length, how = "list")

for( i in (1:length(locs.farms))){
  farms[[i]] <- getRecursionsAtLocations(
    all.dat.UTM,
    locs.farms[[i]][3:4], #Selecting just for lat and long columns
    radius = 150, #Select radii according to AFS type
    threshold = 0, #a time difference (in units timeunits) to ignore excursions outside the radius.
    timeunits = c("hours"),
    verbose = TRUE
  )
  f.stats[[i]] <- farms[[i]]$revisitStats #Creates a df with the revisit stats
}

####SHOPS####
locs.shops <- locs.UTM %>% filter (Type == c("Shopping"))

#Revists for just wildparks 
locs.shops <- droplevels(locs.shops)
locs.shops <- split(locs.shops, locs.shops$Name)

shops <- rapply(locs.shops, length, how = "list")
s.stats <- rapply(locs.shops, length, how = "list")

for( i in (1:length(locs.shops))){
  shops[[i]] <- getRecursionsAtLocations(
    all.dat.UTM,
    locs.shops[[i]][3:4], #Selecting just for lat and long columns
    radius = 250, #Select radii according to AFS type
    threshold = 0, #a time difference (in units timeunits) to ignore excursions outside the radius.
    timeunits = c("hours"),
    verbose = TRUE
  )
  s.stats[[i]] <- shops[[i]]$revisitStats #Creates a df with the revisit stats
}


#Combine a list of dataframes

revisit.df <- bind_rows(c(wp.stats, d.stats, f.stats, h.stats, s.stats), .id = "column_label")
#write.csv(revisit.df, "revisit_df.csv")


revisit.df <- read.csv("revisit_df.csv")
revisit.df$entranceTime <- ymd_hms(revisit.df$entranceTime, tz="Europe/Vienna")
revisit.df$exitTime <- ymd_hms(revisit.df$exitTime, tz="Europe/Vienna")

head(revisit.df)
# to exclude very short duration visits - what is a short duration for the ravens?
#revisit.df = revisit.df[revisit.df$timeInside > 0.1,]

#extract year of when AFS was visited
revisit.df$year = year(revisit.df$entranceTime)
#Extract day of year of when AFS was visited 
revisit.df$doy_enter = yday(revisit.df$entranceTime) #yday returns the day of the year as a decimal number (01-366)

#extract month of when AFS was visited
revisit.df$month = month(revisit.df$entranceTime)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
revisit.df$monthAbb <- mymonths[ revisit.df$month ]

#Extract day of month of when AFS was visited 
revisit.df$dom_enter = mday(revisit.df$entranceTime) #mday returns the day of the month as a decimal number (01-31)

#Extract hour AFS was entered
revisit.df$hour_enter = hour(revisit.df$entranceTime)

#Entrance time and exit time
revisit.df$time_enter = hour(revisit.df$entranceTime) + minute(revisit.df$entranceTime) / 60 + second(revisit.df$entranceTime) / 60 / 60
revisit.df$time_exit = hour(revisit.df$exitTime) + minute(revisit.df$exitTime) / 60 + second(revisit.df$exitTime) / 60 / 60

#Overnight or not
#wp.stats$overnight = factor(as.logical(yday(wp.stats$exitTime) - yday(wp.stats$entranceTime)))
#levels(wp.stats$overnight) = c("no", "yes")

#### SUMMARIES ####
revisit.df$season <- ifelse(revisit.df$entranceTime < "2017-09-23 00:00:00", "sum0",
                         ifelse (revisit.df$entranceTime >= "2017-09-23 00:00:00" & revisit.df$entranceTime < "2017-12-21 00:00:00" , "aut1",
                                 ifelse (revisit.df$entranceTime >= "2017-12-21 00:00:00" & revisit.df$entranceTime< "2018-03-21 00:00:00" , "win1",
                                         ifelse (revisit.df$entranceTime >= "2018-03-21 00:00:00" & revisit.df$entranceTime < "2018-06-21 00:00:00" , "spr1",
                                                 ifelse (revisit.df$entranceTime >= "2018-06-21 00:00:00" & revisit.df$entranceTime < "2018-09-23 00:00:00" , "sum1",
                                                         ifelse (revisit.df$entranceTime >= "2018-09-23 00:00:00" & revisit.df$entranceTime < "2018-12-21 00:00:00" , "aut2",
                                                                 ifelse (revisit.df$entranceTime >= "2018-12-21 00:00:00" & revisit.df$entranceTime < "2019-03-21 00:00:00" , "win2",
                                                                         ifelse (revisit.df$entranceTime >= "2019-03-21 00:00:00" & revisit.df$entranceTime < "2019-06-21 00:00:00" , "spr2",
                                                                                 ifelse (revisit.df$entranceTime >= "2019-06-21 00:00:00" & revisit.df$entranceTime < "2019-09-23 00:00:00" , "sum2",    
                                                                                         ifelse (revisit.df$entranceTime >= "2019-09-23 00:00:00" & revisit.df$entranceTime < "2019-12-21 00:00:00" , "aut3",
                                                                                                 ifelse (revisit.df$entranceTime >= "2019-12-21 00:00:00" & revisit.df$entranceTime < "2020-03-21 00:00:00" , "win3", "spr3")))))))))))
#' the number of AFS visit by an individual per season
library(dplyr)
revisit_ind<- revisit.df %>% group_by(season, id, column_label) %>% tally()
revisit_ind<- revisit_ind[,-4]
revisit_ind<- revisit_ind %>% group_by(season, id) %>% tally()
#write.csv(revisit_ind, "revisit_ind.csv")

#' the AVERAGE time spent inside AFS (across all different ones) by an individual per season 
timein_ind <- aggregate(timeInside ~ season + id + column_label, data = revisit.df, mean)
timeexit_ind <- aggregate(time_exit ~ season + id + column_label, data = revisit.df, mean)
timeenter_ind <-aggregate(time_enter ~ season + id + column_label, data = revisit.df, mean)
time_ind <- merge(timein_ind, timeexit_ind) %>%
  merge(timeenter_ind)

revisit_sites <- merge.data.frame(revisit_ind, time_ind, by.x = c("id","season", "column_label"),by.y = c("id","season", "column_label"),
                           all.x = TRUE, sort = TRUE)


write.csv(revisit_sites, "revisit_sites.csv")

library(plyr)
#' Total number of individuals at each AFS per season and year/ overall
#ind_afs <- aggregate(id ~ season + column_label, data = revisit.df, count)
#listx<- as.data.frame(ind_afs$id)
ind_afs_table <- read.csv("site_no_ind.csv")
library(reshape2)
ind_afs_matrix <- dcast(ind_afs_table, site ~ season)


ggplot(ind_afs_table, aes(x = site, y = no_ind, col = season)) + 
  geom_point()+
  xlab("Foraging site") + ylab("number of individuals") + 
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))


#'Number of revisits for each location (as a total across all individuals) 
#'per season and year / overall
#'
revisit_afs<- revisit.df %>% group_by(season, column_label) %>% tally()


#'Average time spent in each AFS (as a total across all individuals) 
timein_afs <- aggregate(timeInside ~ season + column_label, data = revisit.df, mean)
timeexit_afs <- aggregate(time_exit ~ season + column_label, data = revisit.df, mean)
timeenter_afs <-aggregate(time_enter ~ season + column_label, data = revisit.df, mean)
time_afs <- merge(timein_afs, timeexit_afs) %>%
  merge(timeenter_afs)


ggplot(time_afs, aes(x = column_label, y = timeInside, col = season)) + 
  geom_point()+
  xlab("Foraging site") + ylab("mean time inside in hrs") + 
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

ggplot(revisit.df, aes(x=column_label, y = timeInside, fill = season)) + 
  geom_boxplot()+
  xlab("month") + ylab("time spent inside") + 
  theme_classic()+
  coord_cartesian(ylim= c(0,2))+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))



#####IGNORE THE PLOTS FOR NOW ####
#Plotting the revisitation frequency for individuals (per month) for each yr & site
sites <- subset(revisit.df, column_label == c("Foraging1", "Foraging2", "Foraging3", "Foraging4"))
sites <- droplevels(sites)
ggplot(sites, aes(x=monthAbb, y = timeInside)) + 
  geom_boxplot()+
  facet_grid(column_label ~ year, scales = "free") + 
  xlab("month") + ylab("time spent inside") + 
  theme_classic()
 


# plot entrance time by year and month
ttime = ggplot(wp.stats, aes(x = monthAbb)) + 
  geom_bar(stat = "count", color = "darkgray", fill = "gray") +
  facet_grid(site ~ year) + 
  xlab("visit day of month") + ylab("revisit frequency") + 
  theme_classic()
ttime 



# visit duration by entrance time of day
binhour = ggplot(wp.stats, aes(x = time_enter, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_color_brewer(palette = "Dark2", name = 'overnight') +
  xlab("visit entrance time") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = overnight)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(0, 1), legend.position = c(0, 1))
binhour

#-------------------------------------
# does visit duration vary through breeding season?
#-------------------------------------
library(RColorBrewer)

colourCount = length(unique(wp.stats$id))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

bindoy = ggplot(wp.stats, aes(x = month, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_fill_manual(values = getPalette(colourCount), name = 'individual') +
  xlab("visit entrance day of year") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = id)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(1, 1), legend.position = c(1,1)) +
  facet_grid(~ site)
bindoy

#-------------------------------------
# residence time
#-------------------------------------

# calculate approximate residence time by rounding to nearest hour (data is hourly)
begin = round(wp.stats$time_enter)
duration = round(wp.stats$timeInside)
hours = data.frame(site1 = rep(0, 24),
                   site2 = rep(0, 24),
                   site3 = rep(0, 24),
                   site4 = rep(0, 24))

for (i in 1:nrow(wp.stats))
{
  #idxs will be 0-23, so add 1 for array indexing
  idxs = (begin[i] + 0:(duration[i])) %% 24
  hours[idxs + 1, wp.stats$coordIdx[i]] = hours[idxs + 1, wp.stats$coordIdx[i]] + 1
}

# reformat data for plotting with reshape2
hours$hour = factor(row.names(hours), levels = as.character(1:24), ordered = TRUE)
hours.melt = melt(hours, value.name="Count", variable.name="site", na.rm = TRUE)

restime = ggplot(hours.melt, aes(x = hour, y = Count)) +
  geom_bar(stat = "identity", color = "darkgray", fill = "gray") +
  xlab("hour of day") + ylab("total hours") + 
  scale_x_discrete(breaks = 1:24, 
                   labels = c("1", "", "", "", "", "6", "", "", "", "", "", "12", "", "", "", "", "", "18", "", "", "", "", "", "24")) +
  theme_classic()
restime + facet_grid(~ site) 

#-------------------------------------
# time since last visit
#-------------------------------------

# does time spend vary based on recency of last visit? i.e. less than a day, two days, or longer
wp.stats$timesince = cut(wp.stats$timeSinceLastVisit, 
                         breaks = c(0, 24, 48, 2000), labels = c("<24h", "24-48h", ">48h"))

boxtimesince = ggplot(na.omit(wp.stats), aes(x = timesince, y = timeInside, fill = site)) + 
  geom_boxplot() + ylim(0, 50) + 
  xlab("time since last visit") + ylab("visit duration (h)") +
  theme_classic() +
  theme(legend.justification = c(1, 1), legend.position = c(0.9, 0.9) ) 
boxtimesince 


