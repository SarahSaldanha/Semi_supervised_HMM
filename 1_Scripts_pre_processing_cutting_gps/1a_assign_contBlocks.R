
rm(list = ls())

#set working directory
setwd("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper_Chapter_1/Data_no_ID")
Sys.setenv(TZ='UTC')

#read in necessary packages
library("dplyr")
library('lubridate')


options(expressions = 20000)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## folders

if (file.exists(paste("./", "1_assignedBlocks", sep = "")) == FALSE){
  dir.create(paste("./", "1_assignedBlocks", sep = ""))
}

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Read in data

masterBirds <- readRDS("./start_data_GPS_noID.rds")
head(masterBirds)

#Make sure dates are in the right format

masterBirds$dateTime_mod<-ymd_hms(masterBirds$dateTime_mod)

####ORDER DATA
masterBirds <- masterBirds[order(masterBirds$ring_dep_trip,  masterBirds$dateTime_mod), ]

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  mark points of new deployment, new trip or time gap of more than 20 minutes because we will not run the interpolations and  HMMs over gaps larger than 20 minutes...

cutPointsTrip<-which(!( masterBirds$ring_dep_trip == lag(masterBirds$ring_dep_trip)))

cutPointsDuration <- which(c(NA, diff(masterBirds$fixInterval) > 1200 | diff(masterBirds$fixInterval) < -1200))

cutPoints <- sort(unique(c(cutPointsTrip, cutPointsDuration)))
rm( cutPointsTrip, cutPointsDuration)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## assign continuous blocks

JJ <- 1
contBlock <- rep(NA, nrow(masterBirds))
contBlock[1:(cutPoints[1]-1)] <- JJ
for(II in 2:length(cutPoints)){
  JJ <- JJ+1
  contBlock[cutPoints[II-1]:(cutPoints[II]-1)] <- JJ
}
contBlock[cutPoints[II] : length(contBlock)] <- JJ +1
rm(JJ, II)

####Check if everything worked out. If true then everything is good
identical(cutPoints, which(c(NA, diff(contBlock)) > 0))

###add in the continuous blocks
masterBirds$contBlock <- contBlock


####order before saving
masterBirds <- masterBirds[order(masterBirds$ring_dep_trip, masterBirds$dateTime_mod), ]



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## write out data

length(unique(masterBirds$ring_dep_trip))
length(unique(masterBirds$contBlock))


write.table(masterBirds, paste("./1_assignedBlocks/extractedTrips_allBirds_withContBlocks_noID.csv", sep = ""), col.names = T, row.names = F, sep = ",")
saveRDS(masterBirds, paste("./1_assignedBlocks/extractedTrips_allBirds_withContBlocks_noID.rds", sep = ""))



