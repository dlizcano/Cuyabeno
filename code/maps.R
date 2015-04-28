
library(ggplot2)
library(ggmap)
library(lubridate)
library(dplyr)
library(maptools)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)

# calculate midle point
latitude<-cuyabeno.raw$Lat
longitude<-cuyabeno.raw$Lon
Mach_location<-as.data.frame(cbind(latitude, longitude))

# calculate midle point
Mach_meanLat<-mean(cuyabeno.raw$Lat, na.rm = T) 
Mach_meanLong<-mean(cuyabeno.raw$Lon, na.rm = T)
Mach_location.mean<-c(Mach_meanLong, Mach_meanLat)



#Plot all

plot(x=cuyabeno.raw$Lon,y=cuyabeno.raw$Lat,asp=1) #Boring !

coordinates(Mach_location) <- ~longitude+latitude #make sppatial data fram
geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
utm <- CRS("+proj=utm +zone=17 +south +ellps=WGS84") #def cord
proj4string(Mach_location)<-utm # set cords
loc<-as.data.frame(spTransform(Mach_location,CRSobj = geo))
Mach_location.mean<-c(mean(loc[,1]),mean(loc[,2]))

cuyabeno.raw<-cbind(cuyabeno.raw, loc)

## more elaborated map using ggmap
require(ggmap) # same as ggplot2 but for maps

###################################
#### cuyabeno.raw ······
##################################

wmap.base.m = qmap(Mach_location.mean, zoom = 12) #Get a base map from google with location in the center

wmap.base.m 

wmap.m<- wmap.base.m + geom_point(data = cuyabeno.raw, 
                                  aes(longitude, latitude), 
                                  size = I(1), 
                                  color = "gray", na.rm = TRUE)
#Plot plus text
g1<-wmap.m + geom_text(data = cuyabeno.raw, 
                   aes(longitude, latitude,label=(camera_trap), size=7),
                   angle = 0,
                   alpha = 1/4,
                   na.rm = TRUE) 

overlay <- stat_density2d(
  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
  bins = 6, geom = "polygon",
  data = cuyabeno.raw) 

# con google
g3<- g1 + overlay + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~ Familia, ncol = 4)
 





