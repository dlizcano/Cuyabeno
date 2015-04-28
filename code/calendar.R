



# setwd ("C:\\Users\\Diego\\Documents\\CodigoR\\Tanzania_report")
#wd<-"C:\\Users\\Diego\\Documents\\CodigoR\\Tanzania_report"

# data<-read.csv("C:\\Users\\Diego\\Documents\\CodigoR\\Tanzania_report\\TV-20121209083303_2129.csv",skip=62)
# station.name<-as.character(unique(data$Site.Name))


# #data$Observation.Date = as.Date(data$Sampling.Period)
# start.date<-min(data$camera_trap_start_date)
# end.date<-max(data$camera_trap_end_date)
# # SiteName<-unique(data$Site.Name)
# # Data.Set.Creator<-unique(data$Data.Set.Creator.Scientist)
# Data.Set.Creator.Institutions<-unique(data$Data.Set.Creator.Institutions)
# ### select mammals ecluding Homo sapiens
# Mammals<-subset.data.frame(data,subset=Class == "MAMMALIA" & Species != "sapiens")# select Mammals
# Mammals$binom.mammal<-paste(Mammals$Genus, Mammals$Species, sep=" ")
# Mammals$As.Pos<-as.POSIXct(paste(Mammals$photo_date2, Mammals$Photo.Time,sep=" "))
# Mammals$round_hour<-round_date(Mammals$As.Pos, "hour") #redondea a la hora
# ListMammal<-as.data.frame(unique(Mammals$Species))
# MammalNumber<-nrow(ListMammal)
# Birds<-subset.data.frame(data,subset=Class == "AVES")# select Aves
# ListBirds<-as.data.frame(unique(Birds$Species))
# BirdNumber<-nrow(ListBirds)

# dates<-seq.Date(start.date, end.date ,by = "years")
# yr<-as.numeric(substr(dates, 1, 4))
# no.yr<-(max(yr)-min(yr))+1
# 
# ####Location
# meanLat<-mean(Mammals$Latitude) - 0.05
# meanLong<-mean(Mammals$Longitude)
# location<-c(meanLong, meanLat)
# 
# 
# ###Limites
# minLat<-min(Mammals$Latitude)
# maxLat<-max(Mammals$Latitude)
# minLong<-min(Mammals$Longitude)
# maxLong<-max(Mammals$Longitude)
# limx1<-maxLong-0.15
# limx2<-minLong+0.15
# limy1<-minLat-0.02
# limy2<-maxLat+0.02
# 
# #### Fix Temp
# Temper<-Mammals$Temperature
# IndexToFaren<-which(Temper>40)
# 
# #### Fix Moon
# lunallena<-which(Mammals$Moon.Phase == "Full")
# #if Mammals$Moon.Phase
# 

################################
### Function to make calendar ##
################################


# dataset<-machalilla.raw
# yr_toplot<-2

f.calendar.yr <- function(dataset,yr_toplot) { ##########start ploting function
  
  #load packages
  require(reshape)
  require(zoo)
  require(ggplot2)
  require(chron)
  require(ggmap)
  require(grid)
  require(xtable)
  require(Hmisc)
  # require(quantmod)
  require(reshape2)
  require(plyr)
  require(scales)
  require(lubridate)
  
  start.date<-min(dataset$camera_trap_start_date)
  end.date<-max(dataset$camera_trap_end_date)
  dates<-seq.Date(start.date, end.date ,by = "years")
  yr<-as.numeric(substr(dates, 1, 4))
  no.yr<-(max(yr)-min(yr))+1
  
  years<- as.numeric(substr(as.Date(dataset$photo_date2),1,4))
  months<- as.numeric(substr(as.Date(dataset$photo_date2),6,7))
  days<- as.numeric(substr(as.Date(dataset$photo_date2),9,10))
  time<-strptime(dataset$photo_time,format="%H:%M:%S")
  data2<-cbind(dataset,years,time)  
  
  data2$Date.Start<-data2$camera_trap_start_date
  data2$Date.End<-data2$camera_trap_end_date
  cameras<-as.character(unique(x=data2$camera_trap))
  camvec<-unique(x=data2$camera_trap)
  ncam<-dim(as.data.frame(camvec))
  ncam<-as.numeric(ncam[1])
  
  #Crea matrix con conteos de solo photos por yr
  #yr01<-subset(x=data2,year==yr[1]
  
  list.cams.ini.fin<-dim(ncam) #inicializa la lista que guarda inicio fin
  for (z in 1:ncam){
    
    allyrs_cam<-subset(data2,camera_trap==c(cameras[z])) # camara 1 todos los anios
    secfotocam<-as.data.frame(allyrs_cam$photo_date2) # secuencia de fechas de camara1
    perdaycam<-as.data.frame(table(allyrs_cam$photo_date2,dnn="photo_date2")) #numero de fotos por dia cam1
    perdaycam$foto.type<-"Photo"
    
    #add fecha inicio de cameras [1]
    cam01.start<-as.data.frame(unique(subsub<-subset(data2,camera_trap ==cameras[z],select=Date.Start))) 
    cam01.start$Freq<-1
    cam01.start$foto.type<-"Start"
    colnames(cam01.start)<-c("Date.End","Freq","foto.type")
    cam01.end<-as.data.frame(unique(subsub<-subset(data2,camera_trap ==cameras[z],select=Date.End)))
    cam01.end$Freq<-1
    cam01.end$foto.type<-"End"
    mat.start.end.cam01<-rbind.data.frame(cam01.start,cam01.end)
    mat.start.end.cam01$Date.End<-as.factor(mat.start.end.cam01$Date.End)
    colnames(mat.start.end.cam01)<-c("photo_date2","Freq","foto.type")
    
    mat.cam01<-rbind(perdaycam,mat.start.end.cam01) #paste camara 
    mat.cam01$camera_trap<-cameras[z]
    list.cams.ini.fin[[z]]<-list(mat.cam01)### guarda en una lista las matrices 
  } #### end loop que adiciona start end
  
  #### loop to extract start end de list a tabla
  mat.all.cams.ini.fin<-subset(as.data.frame(list.cams.ini.fin[[1]]),Freq >=1)
  for(z in 2:(ncam)){ #### paste list in data frame
    mat.all.cams.ini.fin<-rbind(mat.all.cams.ini.fin, subset(as.data.frame(list.cams.ini.fin[[z]]), Freq>=1))
  }
  
  data4<-mat.all.cams.ini.fin
  ##### fix dates
  # We will facet by year ~ month, and each subgraph will
  # show week-of-month versus weekday
  # the year is simple
  data4$year<-as.numeric(as.POSIXlt(data4$photo_date2)$year+1900)
  
  # the month too 
  data4$month<-as.numeric(as.POSIXlt(data4$photo_date2)$mon+1)
  # but turn months into ordered facors to control the appearance/ordering in the presentation
  data4$monthf<-factor(data4$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
  # the day of week is again easily found
  data4$weekday = as.POSIXlt(data4$photo_date2)$wday
  
  # again turn into factors to control appearance/abbreviation and ordering
  # I use the reverse function rev here to order the week top down in the graph
  # you can cut it out to reverse week order
  data4$weekdayf<-factor(data4$weekday,levels=rev(0:6),
                         labels=rev(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
  
  # the monthweek part is a bit trickier 
  # first a factor which cuts the data into month chunks
  data4$yearmonth<-as.yearmon(data4$photo_date2)
  data4$yearmonthf<-factor(data4$yearmonth)
  # then find the "week of year" for each day
  data4$week <- as.numeric(format(strftime(data4$photo_date2,"%W"))) ###########
  # and now for each monthblock we normalize the week to start at 1 
  data4<-ddply(data4,.(yearmonthf),transform,monthweek=1+week-min(week))
  # add dia del anio
  data4$dayofmonth<-days(data4$photo_date2)
  # add unos
  data4$one<-1
  ### fechas de lubridate
  data4$week2 = as.POSIXlt(data4$photo_date2)$yday %/% 7 + 1
  data4$wday2 = as.POSIXlt(data4$photo_date2)$wday
  data4$dayofyr <- yday(data4$photo_date2)
  
  ###get sub sets
  ### get years of sampling 
  dates<-seq(as.Date(start.date), as.Date(end.date),"1 years")
  yr<-unique(data4$year)# yr<-as.numeric(substr(dates, 1, 4))
  no.yr<-(max(yr)-min(yr))+1
  
  yr09<-subset(x=data4,year==yr[yr_toplot]) ##### select year to plot
  cameras<-unique(x=yr09$camera_trap)
  camvec<-as.vector(cameras[1:60]) #camras como vectores   ######### edit this part if there are camaras missing ######  
  
  camera.max.per.yr<-min(which(is.na(camvec))) - 1 # 
  ######### edit this part if there are camaras missing ######  
  yr09_array1<-subset(yr09,camera_trap %in% c(camvec[1:camera.max.per.yr])) #Subset de varias camaras en 1 yr
  
  momin<-min(yr09_array1$month)
  momax<-max(yr09_array1$month)
  modif<-momax-momin
  fillings<-c("gray80","white","gray80","white","gray80","white","gray80","white","gray80","white","gray80","white")
  fillings2<-as.vector(fillings[momin:momax])
  
  ### part to get month fillings by odd even month
  colfil<-yr09_array1$month
  is.even <- function(x) x %% 2 == 0 # function odd even
  par.nones<-is.even(colfil)
  yr09_array1$colfill<-ifelse(par.nones==TRUE,"gray80","white" )
  minday<- min(yr09_array1$dayofyr) 
  
  ###### Grafica 
  titlegraph<-paste ("Time-Series Photo Calendar ", yr[yr_toplot], sep="")
  
  momintext<-as.character(month(ymd(080101) + months(momin), label = TRUE, abbr = TRUE))
  nextmonth1<-as.character(month(ymd(080101) + months(momin+2), label = TRUE, abbr = TRUE))
  nextmonth2<-as.character(month(ymd(080101) + months(momin+3), label = TRUE, abbr = TRUE))
  
  
  momin2<-momin
  momin3<-momin+2
  momin4<-momin+3
  momin5<-momin+4
  monthstart1<-sum(days_in_month(c(1:momin2)))+1
  monthstart2<-sum(days_in_month(c(1:momin3)))+1
  monthstart3<-sum(days_in_month(c(1:momin4)))+1
  monthstart4<-sum(days_in_month(c(1:momin5)))+1
  
  #   
  #   # rect_left <- c(0,60,120,180, 240, 300)
  #   rectangles <- data.frame(
  #     xmin = rect_left,
  #     xmax = rect_left + as.numeric(days_in_month(momin+1)),
  #     ymin = 0.4,
  #     ymax = 1.6
  #   )
  
  if (modif==2) {rect_left<-c(monthstart1)
                 labelmonth1 <-momintext
                 
                 # rect_left <- c(0,60,120,180, 240, 300)
                 rectangles <- data.frame(
                   xmin = rect_left,
                   xmax = rect_left + as.numeric(days_in_month(momin+1)),
                   ymin = 0.4,
                   ymax = 1.6
                 )
                 # edit here if the 3 arrays are in a single year or two years
                 cal1<- ggplot() +  
                   geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                             fill="gray80", alpha=0.5) + 
                   annotate("text", x = monthstart1+15, y = 1, label = labelmonth1, colour = "gray75",size = 5) 
                 # annotate("text", x = monthstart1+75, y = 1, label = labelmonth2, colour = "gray75",size = 5) #+
                 # annotate("text", x = monthstart1+138, y = 1, label = labelmonth3, colour = "gray75",size = 5) 
  }
  if (modif==3) {rect_left<-c(monthstart1,monthstart2)
                 labelmonth1 <-momintext
                 labelmonth2 <-nextmonth1
                 
                 # rect_left <- c(0,60,120,180, 240, 300)
                 rectangles <- data.frame(
                   xmin = rect_left,
                   xmax = rect_left + as.numeric(days_in_month(momin+1)),
                   ymin = 0.4,
                   ymax = 1.6
                 )
                 # edit here if the 3 arrays are in a single year or two years
                 cal1<- ggplot() +  
                   geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                             fill="gray80", alpha=0.5) + 
                   annotate("text", x = monthstart1+15, y = 1, label = labelmonth1, colour = "gray75",size = 5) +
                   annotate("text", x = monthstart1+75, y = 1, label = labelmonth2, colour = "gray75",size = 5) #+
                 # annotate("text", x = monthstart1+138, y = 1, label = labelmonth3, colour = "gray75",size = 5) 
  }
  if (modif==4) {rect_left<-c(monthstart1,monthstart2,monthstart3) 
                 labelmonth1 <-momintext
                 labelmonth2 <-nextmonth1
                 labelmonth3 <-nextmonth2
                 
                 # rect_left <- c(0,60,120,180, 240, 300)
                 rectangles <- data.frame(
                   xmin = rect_left,
                   xmax = rect_left + as.numeric(days_in_month(momin+1)),
                   ymin = 0.4,
                   ymax = 1.6
                 )
                 # edit here if the 3 arrays are in a single year or two years
                 cal1<- ggplot() +  
                   geom_rect(data=rectangles, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                             fill="gray80", alpha=0.5) + 
                   annotate("text", x = monthstart1+15, y = 1, label = labelmonth1, colour = "gray75",size = 5) +
                   annotate("text", x = monthstart1+75, y = 1, label = labelmonth2, colour = "gray75",size = 5) #+
                 annotate("text", x = monthstart1+138, y = 1, label = labelmonth3, colour = "gray75",size = 5) 
  }
  if (modif==5) {rect_left<-c(monthstart1,monthstart2,monthstart3,monthstart4)}
  
  
  
  
  
  
  #geom_tile(data=yr09_array1, aes(monthf,one, fill = colfill)) +
  #scale_fill_manual(values=c("gray80", "white"), guide = F)
  
  cal2<-cal1 + geom_tile(data=yr09_array1, aes(dayofyr,one, fill = foto.type)) +   
    scale_fill_manual(guide = guide_legend(title = "Record Type"),values=c("red", "steelblue", "green")) 
  
  cal3<- cal2 + facet_grid(camera_trap ~.) +
    labs(title = titlegraph) +  ylab("") + xlab("Day of the year")
  
  
  
  cal4<- cal3 +  theme_bw() # remueve el background gris
  
  cal5<- cal4 + theme(strip.text.y = element_text(colour = "black", angle = 0, size = 8,
                                                  hjust = 0.5, vjust = 0.5)) +
    theme(panel.background = element_rect(colour = NA)) +
    theme(panel.grid.major = element_blank()) + # borra las lineas guia
    theme(panel.grid.minor = element_blank()) + # borra las lineas guia
    theme(axis.title.y = element_blank()) + # borra titulo eje y
    theme(axis.text.y = element_blank()) + # Borra texto del eje y
    theme(axis.ticks.y = element_blank()) + # Borra ticks  del eje y
    theme(legend.position = "bottom", legend.box = "horizontal")
  
  
  print(cal5)           
  
} #####end of function  
