
######### get packages
library(lubridate)
library(dplyr)


############################## get source
source("code/calendar.R")

############################################
cuyabeno.raw<-read.csv("Data/FaunaCuyabeno.csv")
#
#############################
#### date fix
#############################

# unique(year(cuyabeno.raw$camera_trap_start_time))
cuyabeno.raw$photo_date2<-as.Date(as.character(cuyabeno.raw$Fecha), "%d-%m-%Y")

cuyabeno.raw$Sampling.Period<-2013

cuyabeno.raw$camera_trap_start_date<-as.Date(substr(as.character(cuyabeno.raw$Inicio), start=1, stop=11), "%d-%m-%Y")
cuyabeno.raw$camera_trap_end_date<-as.Date(substr(as.character(cuyabeno.raw$Fin), start=1, stop=11), "%d-%m-%Y")



########## extract yr and month
cuyabeno.raw$year<-year(cuyabeno.raw$photo_date2)
cuyabeno.raw$month<-month(cuyabeno.raw$photo_date2)
# problem?
# which(cuyabeno.raw$year == "2011")


####################################
# make photo calendar type
####################################

f.calendar.yr(dataset = cuyabeno.raw, yr_toplot = 1)
# f.calendar.yr(dataset = cuyabeno.raw, yr_toplot = 2)




####################################
# make mat per species
####################################

mat.per.sp<-f.matrix.creator2(data = cuyabeno.raw,year = 2014)
sp.names<-names(mat.per.sp) # species names

# counting how many (total) records per species by day
cont.per.sp<-data.frame(row.names = sp.names)
for (i in 1:length(mat.per.sp)){
  cont.per.sp[i,1]<-sum(apply(as.data.frame(mat.per.sp [[i]]),FUN=sum,na.rm=T, MARGIN = 1))
}
cont.per.sp



##########################
## get just animals
##########################
data<-subset(cuyabeno.raw, photo_type=="Animal")


data_animal<-tbl_df(filter(cuyabeno.raw, year==2014))
counts_cam_date <- count(data_animal, camera_trap, photo_date)
colnames(counts_cam_date)<-c("camera_trap", "dates",  "counts")
counts_cam_date$dates<-as.Date(counts_cam_date$dates,format = "%d-%b-%Y")
gg.calendar(counts_cam_date)


# Delete 2011 provisional
index<-which(data$camera_trap_start_date == "2011-11-11")
cuales<-data[index,]
data<-data[-index,]



# to do
yr2014<-f.matrix.creator2(data = cuyabeno.raw, year = 2014) #### fix names in function

##########################################
##### export camera points
##########################################





cam_point<-unique(cuyabeno.raw$camera_array)

coordinates (cuyabeno.raw$longitude, cuyabeno.raw$latitude)
