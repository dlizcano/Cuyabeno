
######### load packages
library(lubridate)
library(dplyr)
library(xtable)
library(maptools)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)


############################## get source code
source("code/calendar.R")
source("code/f_matrix2_code.R")
############################################

############################################
################ Load Enrique Data set
cuyabeno.raw<-read.csv("Data/FaunaCuyabeno.csv")
############################################
# make sure the columns have the same names and date format

#############################
#### date fix
#############################

# unique(year(cuyabeno.raw$camera_trap_start_time))
cuyabeno.raw$photo_date2<-as.Date(as.character(cuyabeno.raw$Fecha), "%d-%m-%Y")
cuyabeno.raw$Sampling.Period<-2013
cuyabeno.raw$camera_trap_start_date<-as.Date(substr(as.character(cuyabeno.raw$camera_trap_start_date), start=1, stop=11), "%d-%m-%Y")
cuyabeno.raw$camera_trap_end_date<-as.Date(substr(as.character(cuyabeno.raw$camera_trap_end_date), start=1, stop=11), "%d-%m-%Y")



########## extract yr and month
cuyabeno.raw$year<-year(cuyabeno.raw$photo_date2)
cuyabeno.raw$month<-month(cuyabeno.raw$photo_date2)
# problem?
# which(cuyabeno.raw$year == "2011")


####################################
# make photo calendar type
####################################

 f.calendar.yr(dataset = cuyabeno.raw, yr_toplot = 1)



#######################
### make matrix per species names
######################


mat.per.sp<-f.matrix.creator2(data = cuyabeno.raw,year = 2013)
sp.names<-names(mat.per.sp) # species names

# counting how many (total) records per species by all days
cont.per.sp<-data.frame(row.names = sp.names)
for (i in 1:length(mat.per.sp)){
  cont.per.sp[i,1]<-sum(apply(as.data.frame(mat.per.sp [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
}

# cont.per.sp$especie<-paste("$", rownames(cont.per.sp), "$",sep="")
cont.per.sp$especie<-paste("", rownames(cont.per.sp), "",sep="")
colnames(cont.per.sp)<-c("Numero_de_fotos","especie")
wanttex <- xtable(arrange(df = cont.per.sp, desc(Numero_de_fotos)))
#print(wanttex,sanitize.text.function=function(str)gsub("_","\\_",str,fixed=TRUE), floating=FALSE)
print(wanttex,floating=TRUE)


# Localizacion de las camaras


```{r googlemap,cache=TRUE,warning=FALSE,eval=TRUE, echo=FALSE}
library(raster)
library(rgdal)
library(dismo)
library(biomod2)
library(spatstat)
library(sp)
library(dplyr)
library(maptools)

# calculate midle point
latitude<-cuyabeno.raw$Lat
longitude<-cuyabeno.raw$Lon
Mach_location<-as.data.frame(cbind(latitude, longitude))

# calculate midle point
Mach_meanLat<-mean(cuyabeno.raw$Lat, na.rm = T) 
Mach_meanLong<-mean(cuyabeno.raw$Lon, na.rm = T)
Mach_location.mean<-c(Mach_meanLong, Mach_meanLat)


coordinates(Mach_location) <- ~longitude+latitude #make sppatial data fram
geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
utm <- CRS("+proj=utm +zone=17 +south +ellps=WGS84") #def cord
proj4string(Mach_location)<-utm # set cords
loc<-as.data.frame(spTransform(Mach_location,CRSobj = geo))
colnames(loc)<-c("longitude", "latitude")
Mach_location.mean<-c(mean(loc[,1]),mean(loc[,2]))

cuyabeno.raw<-cbind(cuyabeno.raw, loc)

## more elaborated map using ggmap
require(ggmap) # same as ggplot2 but for maps

###################################
#### cuyabeno.raw ······
##################################

wmap.base.m = qmap(Mach_location.mean, zoom = 12) #Get a base map from google with location in the center

# see the map
# wmap.base.m 

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
  bins = 5, geom = "polygon",
  data = cuyabeno.raw) 

# con google
g3<- g1 + overlay + scale_fill_gradient(low = "blue", high = "red") + facet_wrap(~ Orden, ncol = 3)
 
 
# plot maps
plot(g1)
plot(g3) # per Orden




# Covariables 


long<-unique(cuyabeno.raw$longitude)
lati<-unique(cuyabeno.raw$latitude)
centercoord<-c(mean(subset(long, long<=1)),mean(unique(subset(lati, lati<=1))))
coordsubset<-subset(cuyabeno.raw,select = c(camera_trap,longitude,latitude,Habitat,Tecnico))

#################################
# get elevation
################################

rm(unmarked) # in case is already loaded
elevation<-getData('SRTM',lon=centercoord[1], lat=centercoord[2],download = TRUE, path="elev_cuyabeno") # in conflict with Unmarked !!! 

 # read elevation from disk
# elevation<- raster("C://Users//Diego//Documents//CodigoR//ULEAM//Cuyabeno//elev_cuyabeno//srtm_21_13.tif")

cam.cords<-as.data.frame(distinct(coordsubset))
coordinates(cam.cords) <- ~longitude+latitude #make sppatial data fram
geo <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #def cord
proj4string(cam.cords)<-geo # set cords

# FIX EXTEND MANUALLY 
e<-extent (-75.8000, -75.70000, -0.3611207, -0.2433732)
elevation.crop<-crop(elevation, e)

plot(elevation.crop)
plot(cam.cords, add=T, col="red")
title(main="Altitud", sub="en color rojo se muestra donde se instalaron las camaras")


cam.cords.sp<-SpatialPoints(cam.cords)
proj4string(cam.cords.sp)<-geo 
# etract values
elev.ovr <- extract(elevation.crop, cam.cords.sp, method='bilinear')

# add to table
cam.cords$elev<-elev.ovr





############################
# river
############################

# river <- readShapeSpatial("C:/Users/Diego/Documents/CodigoR/ULEAM/Infor_Caract/shp/cuyabeno_roadsclip.shp")
# names(roadpol)<-"dist_rd"
# proj4string(roadpol)<-geo
# dist_rd<-over(x = cam.cords, y = roadpol)
# add to table
# cam.cords$dist_rd<-as.numeric(dist_rd[,1])

# roadpol.ow<-as(as(roadpol, "SpatialPolygons"), "owin") # make owin
# cam.and.covs<-as.data.frame(cam.cords)

# plot(roadpol, col=topo.colors(65))
# plot(cam.cords, add=T, col="red")
# title(main="Distancia a las carreteras", sub="en color rojo se muestra donde se instalaron las camaras")

##################################
## Estructura Veget
##################################

# est.veget<-read.csv("C:/Users/Diego/Documents/CodigoR/ULEAM/Infor_Caract/Data/estructVeget.csv") # read table
# cam.and.covs<-cbind(cam.and.covs,est.veget) #Paste covs

cam.and.covs<-as.data.frame(cam.cords)

################################
# remove raster package. In conflict with unmarked
rm (raster)
################################


#### fUNCTION TO plot unmarked data frames per species
f.sp.occu.plot.mat <- function(sp_number){
    ########################
    ### shrink to 15
    ########################
    
    library(unmarked)
    
    # colapse
    # sp15<-f.shrink.matrix.to15(matrix = mat.per.sp[[sp_number]])
    sp15<-mat.per.sp[[sp_number]] # sin colapsar
    
    
    ########################
    ### make unmarked object 
    ########################
    
    sp_UMF <- unmarkedFrameOccu(sp15)
    
    plot(sp_UMF, panels=1)
    # title(main=as.character(sp.names[sp_number]))
     
}


#### fUNCTION TO make unmarked data frames per species and basic models
f.sp.occu.models <- function(sp_number){
    ########################
    ### shrink to 15
    ########################
    
    library(unmarked)
    
    # sp15<-f.shrink.matrix.to15(matrix = mat.per.sp[[sp_number]])
    sp15<-mat.per.sp[[sp_number]] # sin colapsar
    
    ########################
    ### make unmarked object 
    ########################
    
    sp_UMF <- unmarkedFrameOccu(sp15)
    
    # plot(sp_UMF, panels=1)
    # title(main=as.character(sp.names[sp_number]))
     
    # add some  covariates
    siteCovs(sp_UMF) <- cam.and.covs
    
    #######################
    ## occu models 
    #######################
    
    #  covariates of detection and occupancy in that order.
    fm0 <- occu(~ 1 ~ 1, sp_UMF) 
    fm1 <- occu(~ 1 ~ Habitat, sp_UMF)
    fm2 <- occu(~ elev ~ elev, sp_UMF)
    fm3 <- occu(~ elev ~ 1, sp_UMF)
    fm4 <- occu(~ elev ~ Habitat, sp_UMF)
    fm5 <- occu(~ Habitat ~ 1, sp_UMF)

    
    # put the names of each model
    models <- fitList(
      'p(.)psi(.)' = fm0,
      'p(.)psi(Habitat)' = fm1,
      'p(elev)psi(elev)' = fm2,
      'p(elev)psi(.)' = fm3,
      'p(elev)psi(Habitat)' = fm4,
      'p(Habitat)psi(.)' = fm5
      )
    
    ms <- modSel(models)
    # (ms)
    
    #This part store some models coeficients in a table (mat_models) to compare on screen
  ms_AIC_models<-as.data.frame(ms@ Full[1], row.names = NULL) #store model name
  modelo<-paste("_", as.character(as.character(sp.names[sp_number])), 
                "_", " models", sep="") # fix model name addin species
  ma_nPars<-as.data.frame(ms@Full$nPars) #store parameter number
  ms_AIC_values<- as.data.frame(ms@Full$AIC) #store AIC values
  ms_AIC_delta<- as.data.frame(ms@Full$delta) #store AIC delta values
  ms_AIC_AICwt<- as.data.frame(ms@Full$AICwt) #store AIC wt values
  ms_AIC_cumultw<-as.data.frame(ms@Full$cumltvWt) #store model name
  ms_m<-as.data.frame(row.names(ms_AIC_models)) #store m number
  ms_formula<- as.data.frame(ms@Full$formula) #store model formula
  mat_models <- cbind(ms_AIC_models, ma_nPars, ms_AIC_values, ms_AIC_delta, ms_AIC_AICwt, ms_AIC_cumultw) #paste in matrix
  colnames(mat_models)<-c(modelo, "nPars",'AIC', "delta", "AICwt", "cumltvWt") # change row names

  ##Print los 7 primeros modelos
  xtable(mat_models)
  # print(spname)
  # print (mat_models[c(1:7),])
  # as.character(sp.names[sp_number])
    
    
    
}





#Modelos de ocupacion por especie

## La Guanta (_Cuniculus paca_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 1)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=1]))
f.sp.occu.models(sp_number = 1)




## El Venado (_Mazama americana_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 2)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=2]))
f.sp.occu.models(sp_number = 2)





## El Tapir (_Tapirus terrestris_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 4)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=4]))
f.sp.occu.models(sp_number = 4)



## El Venado (_Mazama gouazoubira_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 7)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=7]))
f.sp.occu.models(sp_number = 7)



## La Guatusa (_Dasyprocta fuliginosa_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 5)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=5]))
f.sp.occu.models(sp_number = 5)



## El Ocelote (_Leopardus pardalis_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 11)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=11]))
f.sp.occu.models(sp_number = 11)




## El Pecari (_Pecari tajacu_)
### Matriz de datos colapsada sin colapsar
 f.sp.occu.plot.mat(sp_number = 16)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=16]))
f.sp.occu.models(sp_number = 16)



## El Pecari (_Tayassu pecari_)
### Matriz de datos colapsada sin colapsar

 f.sp.occu.plot.mat(sp_number = 26)
### Seleccion de Modelos
print(as.character(sp.names[sp_number=26]))
f.sp.occu.models(sp_number = 26)

