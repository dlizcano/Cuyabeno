

# Jorge Ahumada. Tropical Ecology Assessment and Monitoring Network. Conservation International
# Code developed on 2010/07/02 - 2010/12/01
# Modified by Diego J. Lizcano 2013
require(reshape2)
require(plyr)
require(ggplot2)
#script to process raw TEAM files and get them ready for analysis
f.readin.fix.data<-function(){
  require(lubridate)
  data<-read.csv(file.choose(),h=T,skip=62)
  data<-f.fix.data(data)
  #make sure date info makes sense
  data
}


#function to create binary matrices for all species at a site and sampling period. Matrix has a 1 if the species was seen in a day a 0 if not seen and NA if not sampled
#The function requires data from one sampling event and will return a list composed of 0,1 matrices, one matrix for each species.

#THIS FUNCTION WORKS WITH NEW TEAM DATA ONLY - do not use with legacy TEAM data
# this works one year at a time. Separate data in different years first
f.matrix.creator2<-function(data,year){
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$camera_trap)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$binomial)
  #start and end dates of sampling periods
  data<-data[data$Sampling.Period==year,]
  min<-min(data$camera_trap_start_date)
  max<-max(data$camera_trap_end_date)
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$camera_trap_start_date),data$camera_trap,unique)
  nms<-names(start.dates)
  # start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.character(data$camera_trap_end_date),data$camera_trap,unique)
  # end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==as.Date(start.dates[j], format = "%Y-%m-%d"))
    hi<-which(date.header==as.Date(end.dates[j], format = "%Y-%m-%d"))
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<- 0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$photo_date2[indx]
    cameras<-data$camera_trap[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}

##############################################
