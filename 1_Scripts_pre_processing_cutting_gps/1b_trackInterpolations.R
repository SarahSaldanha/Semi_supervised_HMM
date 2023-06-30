rm(list = ls())

setwd("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper_Chapter_1/Data_no_ID")

Sys.setenv(TZ='UTC')

library('ggplot2')
library("dplyr")
library('lubridate') # dateTime_mod functions

library('marmap') # mapping tools

#library('maptools') # mapping tools
library("viridis")# viridis colour map

#library("adehabitatLT")
#library("rgdal")
#library("sp")
library("trip")
library("oce")

options(expressions = 20000)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## folders

if (file.exists(paste("./", "2_trackInterpolations", sep = "")) == FALSE){
  dir.create(paste("./", "2_trackInterpolations", sep = ""))
}

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Read in data to test interpolations on

masterBirds <- readRDS("./1_assignedBlocks/extractedTrips_allBirds_withContBlocks_noID.rds")
length(unique(masterBirds$ring_dep_trip))
length(unique(masterBirds$contBlock))

####make sure your date is in dateTime_mod
masterBirds$dateTime_mod<-ymd_hms(masterBirds$dateTime_mod)

######order your data
masterBirds <- masterBirds[order(masterBirds$ring_dep_trip, masterBirds$dateTime_mod), ]



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Interpolate function

interpFunc <- function(track, interpStep, method = c( "linear", "cubic")){

  if(difftime(max(track$dateTime_mod), min(track$dateTime_mod), units = "secs") > 5*(interpStep) & nrow(track) > 5){
    
  newTime <- seq(from = ceiling_date(min(track$dateTime_mod, na.rm = T), unit = "minute"),
                 to = floor_date(max(track$dateTime_mod, na.rm = T), unit = "minute"),
                 by = interpStep)
  
  if(method == "cubic"){
    trackNEW <- data.frame(lon = spline(track$dateTime_mod, track$lon, xout = newTime, method = "natural")$y,
                                lat = spline(track$dateTime_mod, track$lat, xout = newTime, method = "natural")$y,
                                dateTime_mod = newTime)  
    }
  
  if(method == "linear"){
    trackNEW <- data.frame(lon = approx(track$dateTime_mod, track$lon, xout = newTime)$y,
                           lat = approx(track$dateTime_mod, track$lat, xout = newTime)$y,
                           dateTime_mod = newTime 
                           )
                             
  }
  
  if(length(track$ring_dep_trip) > 0){
    trackNEW$ring_dep_trip <- rep(track$ring_dep_trip[1], nrow(trackNEW))
  }
  if(length(track$tripID2) > 0){
    trackNEW$tripID2 <- rep(track$tripID2[1], nrow(trackNEW))
  }
  if(length(track$contBlock) > 0){
    trackNEW$contBlock <- rep(track$contBlock[1], nrow(trackNEW))
  }
 
  
  return(trackNEW)
  rm(newTime, trackNEW)
  
  }
  
}
    
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Interpolate data at 300 seconds using linear and cubic spline



track <- masterBirds


linearTracks300 <- lapply(split(track, as.factor(track$contBlock)), function(x) interpFunc(x, interpStep = 300, method = "linear"))   ####interstep here is the number of seconds of the interpolation. You can choose to use a "linear" or "cubic" interpolation. After visual inspection we chose to use linear interpolation for our study. Note that gaps of over 20 minutes are not interpolated over and if we have a continuous block of less than 5 positions it will be lost (HMMs need at least 5 positions per trip to run, and we are essentially running each contBlock as a seperate trip)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## unlist to giant data frame

#####here is where I loose contBlocks.... i dont understand why?

library(data.table)
linearTracks300_ALL <- rbindlist(linearTracks300)
length(unique(linearTracks300_ALL$contBlock)) ####less contBlocks here because of short contBlocks- 


length(unique(linearTracks300_ALL$ring_dep_trip)) #####loose some tracks because the contBlocks were too short
length(unique(track$ring_dep_trip))-length(unique(linearTracks300_ALL$ring_dep_trip)) ####loose 10

#Check these to make sure everything is ok
Lost_trips<- subset(track, !(ring_dep_trip %in%  unique(linearTracks300_ALL$ring_dep_trip)))

table(Lost_trips$ring_dep_trip)




linearTracks300_ALL <- linearTracks300_ALL[order(linearTracks300_ALL$contBlock, linearTracks300_ALL$dateTime_mod), ]






##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## save out


##%%%%%%%%%%%%%%%%%%%
##  linear
write.table(linearTracks300_ALL, paste("./2_trackInterpolations/interpolatedTRACKS_linear300sec_noID.csv", sep = ""), col.names = T, row.names = F, sep = ",")
saveRDS(linearTracks300_ALL, paste("./2_trackInterpolations/interpolatedTRACKS_linear300sec_noID.rds", sep = ""))


length(unique(linearTracks300_ALL$contBlock))
