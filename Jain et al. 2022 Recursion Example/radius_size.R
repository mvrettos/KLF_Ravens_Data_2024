#### (I) Radius selection using the recurse package for one ####

library(recurse)
require(scales)
require(sp)
require(lubridate)
par(mfrow=c(1,1))

#### (1) Downloading the data from R: specify dates & remove duplicated timestamps ####

library(move)
login <- movebankLogin()

#RavensMove is a movestack object (multiple individuals)
RavensMove <- getMovebankData(study="Common Ravens in the Eastern Alps", login=login,
                              timestamp_start = "20200101000000000", 
                              timestamp_end = "20240531000000000",
                              removeDuplicatedTimestamps=TRUE)

#### (2) Assigning the correct time zone ####

RavensLocal <- RavensMove
timestamps(RavensLocal) <- with_tz(timestamps(RavensLocal), tz="Europe/Vienna")
RavensLocal@timestamps[1]

#### (3) Selecting single individual ####

#Subset an individual and plot
indv <- RavensLocal[[4]]

plot(indv$location_long, indv$location_lat, col = viridis_pal()(nrow(indv)), pch = 20, 
     xlab = "long", ylab = "lat", asp = 1)
#Longitude on the x axis and latitude on the y axis

points(x= 13.948700, y =47.805300, col = 'red') #Cumberland wildpark

#The units for the radius are those of the (x,y) coordinates (e.g., meters in the case of a UTM projection)

#Convert to UTM projection for meters
indv.utm <- spTransform(indv, CRSobj="+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs", centre = TRUE) 

plot(indv.utm$location_long, indv.utm$location_lat, col = viridis_pal()(nrow(indv)), pch = 20,
     xlab = "UTM easting", ylab = "UTM northing", asp = 1)
#UTM easting on x axis, UTM northing on Y axis

points(x= 421283.55, y =5295195.56, col = 'red') #Cumberland wildpark
#points(x= 421338.02, y =5298315.00, col = 'red') #Cumberland wildpark


# information to select radius
# calculate time and distance of each step
timeDiff = diff(as.numeric(indv.utm$timestamp) / 3600) # in hours
distDiff = sqrt(diff(indv.utm$location_long)^2 + diff(indv.utm$location_lat)^2)

# step lengths eliminating those that cross the year boundary (~ over 6 months apart)

hist(distDiff, xlim=c(0,100), breaks = 1000)
#Most step lengths are less than 40....


#effect of radius on wildpark

locations = data.frame(x = c(421283.55), y = c(5295195.56))
#locations = data.frame(x = c(421338.02), y = c(5298315.00))

locvisit = getRecursionsAtLocations(indv.utm, locations, 1000) 
locvisit$revisits


for (i in 1:100) {
  locvisit$revisits[i] = getRecursionsAtLocations(indv.utm, locations, i) 
}


radius <- c(1:100)
plot(radius,locvisit$revisits)

lapply((as.numeric(locvisit$revisits)) , diff)

plot( radius[-1],diff(as.numeric(locvisit$revisits)))



#Extraction locations that were revisited

#indvisit = getRecursions(indv.utm, 50) 

#par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
#plot(indvisit, indv.utm)

#hist(indvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 50m)")
#summary(indvisit$revisits)



#### (II) Radius selection using the recurse package for all individuals and all locations/ full dataset ####

require(move)
login <- movebankLogin()

AllMove <- getMovebankData(study="Corvus corax, Common Raven - Eastern Alps", login=login,
                           timestamp_start = "20200401000000000", timestamp_end="20200430000000000",
                           removeDuplicatedTimestamps=TRUE)

#Assigning the correct time zone
library(lubridate)
AllLocal <- AllMove
timestamps(AllLocal) <- with_tz(timestamps(AllLocal), tz="Europe/Vienna")
AllLocal@timestamps[1]


plot(AllLocal$location_long, AllLocal$location_lat, col = viridis_pal()(nrow(AllLocal)), pch = 20, 
     xlab = "long", ylab = "lat", asp = 1)
#Longitude on the x axis and latitude on the y axis

#Loading the lat long of all identified foraging locations

locs <- read.csv('AFSs.csv')

#The units for the radius are those of the (x,y) coordinates (e.g., meters in the case of a UTM projection)

#Convert to UTM projection for meters
ravens.utm <- spTransform(RavensLocal, CRSobj="+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs", centre = TRUE) 

#Loading the locations file
#locs <- locs[c(5,4)] #just to get rid of empty columns 

#Converting to UTM
locations<- locs[c(2,3)]
coordinates(locations) <- c("long", "lat")
proj4string(locations) <- CRS("+proj=longlat +datum=WGS84") 


locations.utm <- spTransform(locations, CRSobj="+proj=utm +zone=33 +ellps=WGS84 +units=m +no_defs", centre = TRUE)


plot(ravens.utm$location_long, ravens.utm$location_lat, col = viridis_pal()(nrow(ravens.utm)), pch = 20,
     xlab = "UTM easting", ylab = "UTM northing", asp = 1)

#UTM easting on x axis, UTM northing on Y axis
points(locations.utm@coords, col = 'red')


# information to select radius
# calculate time and distance of each step
timeDiff = diff(as.numeric(ravens.utm$timestamp) / 3600) # in hours
distDiff = sqrt(diff(ravens.utm$location_long)^2 + diff(ravens.utm$location_lat)^2)

# step lengths eliminating those that cross the year boundary (~ over 6 months apart)

hist(distDiff, xlim=c(0,100), breaks = 100000)
#Most step lengths are less than 40....


#effect of radius on locations 
locvisit = list()

locations.utm@coords[1,]

################### WORK ON THIS FOR LOOP TOMORROW ##########
locations.df <- as.data.frame(locations.utm@coords)

for (i in 1:150) {
  locvisit$revisits[i] = getRecursionsAtLocations(ravens.utm, locations.df, i) 
}


revisits <- as.data.frame(locvisit$revisits)

radius <- c(1:150)

list_of_titles <- paste(locs$Name)

par(mfrow = c(2,2))
for (j in 1:length(revisits)){
  plot(radius,revisits[j,], main = list_of_titles[j])
}


lapply((as.numeric(locvisit$revisits)) , diff)

plot( radius[-1],diff(as.numeric(locvisit$revisits)))



#Extraction locations that were revisited

##indvisit = getRecursions(indv.utm, 50) 

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plot(indvisit, indv.utm)

hist(indvisit$revisits, breaks = 20, main = "", xlab = "Revisits (radius = 50m)")
summary(indvisit$revisits)






