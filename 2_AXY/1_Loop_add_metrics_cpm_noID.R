#clear R
rm(list=ls()) 


library(dplyr)          
library(tidyr)          
library(lubridate)
library(data.table)
library(cpm)
library(zoo)


op <- options(digits.secs=6)
memory.limit(size=5000000000)

# This script cuts the trip with cpm and then calculates summary metrics of energy expenditure for each segment. 
# Adapted script  from  PM Collins (2015), later modified by B Clark (2018) for the Grupo Aves Marinas Course, Dec 2020

#SET_WORKING_DIRECTORY

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Set the required segment length
freq <- 25# The Frequency of accelerometry data (Hz)
secs <- 2 # The number of seconds over which to calculate the desired metrics.

numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 


setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_trips")



PHAAET_files <- list.files(pattern = '*.csv')
print(PHAAET_files)


axy_metrics <- function(file_name){

  #file_name <-PHAAET_files[3]

  
  file.error<- try(fread(file_name, header=T, sep=","), silent= T)
  if(length(file.error)==1){axy_data<-read.csv(file_name, header=T, sep=";")} else {axy_data<-fread(file_name, header=T, sep=",")}
  
  rm(file.error)
  ####transform pressure to depth and calculate pitch and roll 
  
  #Calculate pitch & roll
  axy_data$pitch<-atan((axy_data$X/(sqrt((axy_data$Y*axy_data$Y)+(axy_data$Z*axy_data$Z)))))*(180/pi)#Calculate pitch
  axy_data$roll<-atan((axy_data$Y/(sqrt((axy_data$X*axy_data$X)+(axy_data$Z*axy_data$Z)))))*(180/pi) #Calculate roll
  
  

  
  ####Calculate depth and spread pressure
  axy_data$Pressure<- zoo::na.approx(axy_data$Pressure, rule = 2)
  axy_data$Approx_depth_negative_mode<-0.01*(getmode(round(axy_data$Pressure))-axy_data$Pressure)
  
  
  ####remove duplicates (happens in some trips, not sure why)
  
  axy_data<-axy_data%>% distinct()
  
  axy_data<-axy_data%>% arrange(Timestamp_mod_axy)
  
  
  
  
  
####get change points in data, this will split the axy data when there is a change of state in X
  
result<-processStream(axy_data$X, "GLR", ARL0=50000, startup=25)
axy_data$observation <- 1:nrow(axy_data)
change_points<-as_tibble(result$changePoints)
change_points$section_cpm<-1:nrow(change_points)
axy_data<-left_join(axy_data, change_points, by = c("observation" = "value"))
axy_data[1]$section_cpm<-0

axy_data<-axy_data %>%
  fill(section_cpm,  .direction = "down")

#Label segments 1 second segments, for running mean
row_num <- floor(nrow(axy_data)/numrows)*numrows
axy_data <- axy_data[1:row_num,]
axy_data$segID <- rep(1:(row_num/numrows),each = numrows)
axy_data$bird_seg <- paste(axy_data$ring,axy_data$segID,sep="_")#####1 second segments 


axy_data$bird_seg<- as.factor(axy_data$bird_seg)
axy_data$section_cpm<-as.factor(axy_data$section_cpm)
#Calculate the metrics for each segment
####using one second blocks to calcualte the static acceleration 
#and the cpm to calculate the other values 
Static=axy_data%>%
  group_by(bird_seg)%>%
  summarise(
    StaticX=sum(zoo::rollapply(X,numrows,mean,fill=NA),na.rm = T),
    StaticY=sum(zoo::rollapply(Y,numrows,mean,fill=NA),na.rm = T),
    StaticZ=sum(zoo::rollapply(Z,numrows,mean,fill=NA),na.rm = T))%>%
  ungroup()



axy_data=axy_data%>%
  left_join(.,Static,by="bird_seg")


sz=axy_data%>%
  dplyr::mutate(
    ####Calculates DBA for each axis. 
    DynamicX=X-StaticX,
    DynamicY=Y-StaticY,
    DynamicZ=Z-StaticZ,
    #Combines the DBA into ODBA or VeDBA
    ODBA=abs(DynamicX)+abs(DynamicY)+abs(DynamicZ),
    VeDBA=sqrt((DynamicX^2)+(DynamicY^2)+(DynamicZ^2)))%>%
  
  
  dplyr::group_by(ring_dep_trip, section_cpm)%>%
  dplyr::summarise(
    #id's
    
    #start of the seg
    start_seg=min(Timestamp_mod_axy),
    end_seg=max(Timestamp_mod_axy)+0.04,
    #OBDA/VeDBA
    mean_ODBA=mean(ODBA),
    max_ODBA=max(ODBA),
    min_ODBA=min(ODBA),
    sd_ODBA=sd(ODBA),
  
    mean_VeDBA=mean(VeDBA),
    sd_VeDBA=sd(VeDBA),
    max_VeDBA=max(ODBA),
    min_VeDBA=min(VeDBA),
    
    #Surge
    meanX_surge=mean(X),
    stdevX_surge=sd(X),
    minX_surge=min(X),
    maxX_surge=max(X),
    sum_positive_X_surge=sum(X[which(X>0)]),
    sum_negative_X_surge=sum(X[which(X<0)]),
    #Sway
    meanY_sway=mean(Y),
    stdevY_sway=sd(Y),
    minY_sway=min(Y),
    maxY_sway=max(Y),
    sum_positive_Y_sway=sum(Y[which(Y>0)]),
    sum_negative_Y_sway=sum(Y[which(Y<0)]),
    #heave
    meanZ_heave=mean(Z),
    stdevZ_heave=sd(Z),
    minZ_heave=min(Z),
    maxZ_heave=max(Z),
    sum_positive_Z_heave=sum(Z[which(Z>0)]),
    sum_negative_Z_heave=sum(Z[which(Z<0)]),
    #pitch
    pitch_mean=mean(pitch),
    pitch_stdev=sd(pitch),
    min_pitch=min(pitch),
    max_pitch=max(pitch),
    sum_positive_pitch=sum(pitch[which(pitch>0)]),
    sum_negative_pitch=sum(pitch[which(pitch<0)]),
    #roll
    roll_mean=mean(roll),
    roll_stdev=sd(roll),
    min_roll=min(roll),
    max_roll=max(roll),
    sum_positive_roll=sum(roll[which(roll>0)]),
    sum_negative_roll=sum(roll[which(roll<0)]),
    #depth
    depth_mean=mean(Approx_depth_negative_mode),
    depth_stdev=sd(Approx_depth_negative_mode),
    min_depth=min(Approx_depth_negative_mode),
    max_depth=max(Approx_depth_negative_mode),
    sum_positive_depth=sum(Approx_depth_negative_mode[which(Approx_depth_negative_mode>0)]),
    sum_negative_depth=sum(Approx_depth_negative_mode[which(Approx_depth_negative_mode<0)]))%>%
  select(ring_dep_trip, section_cpm,start_seg, end_seg, everything())%>%
  arrange(start_seg)


sz$end_seg<-ymd_hms(ifelse(is.na(lead(sz$start_seg)), as.character(sz$end_seg), as.character(lead(sz$start_seg))))
  
write.csv(sz,paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_summarised_cpm/", unique(axy_data$ring_dep_trip), "_metrics_with_depth_cpm_perseg.csv"),row.names=F)

gc()
rm(axy_data)

}

lapply(PHAAET_files, axy_metrics)






