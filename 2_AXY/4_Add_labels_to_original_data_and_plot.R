#Attaches labels from randomforest to the acc data with correct timestamps
#Then plots out the full dataset with behaviours coloured for checking the randomforest predictionss

# Adapted from script written 11/9/17
# Beth Clark bethany.clark@birdlife.org
# Grupo Aves Marinas Course, Dec 2020

#Set up ####
library(ggplot2)
library(randomForest)
library (stringr)
library(dplyr)
library(lubridate)
library(tidyr)
rm(list=ls())

#set the seglength
seglength <- 50

filepath <- "D:Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/"
acclabels<-readRDS(paste0(filepath,"Predicted_trips/", "Acc_16_trips_17082021_full_acc_labels_cmp.rds"))

head(acclabels)

#acclabels$tripID<-str_sub(acclabels$bird_seg2, end = -((nchar(acclabels$segID2)+2)))

list_trips<- unique(paste0(acclabels$tripID, ".csv"))

setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/cut_trips_final")


trip_7501394_PHAAET_rec16112018_PRincon_S1_1<- read.csv("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/cut_trips_final/7501394_PHAAET_rec16112018_PRincon_S1_1.csv", header = T )


label_7501394_PHAAET_rec16112018_PRincon_S1_1<-subset(acclabels, tripID == "7501394_PHAAET_rec16112018_PRincon_S1_1")


memory.limit(size=9999999999)


###GET AXY DA









accdata <- 
  do.call(rbind,lapply(list_trips, read.csv))


######so I need to merge based on the start seg and fill downwards

label_behaviour_only<- dplyr::select(acclabels, start_seg, pred_behaviour, tripID, pred_behaviour_clear_0.65)

accdata$Timestamp3<-ymd_hms(accdata$Timestamp)

accdata2<-merge(accdata, label_behaviour_only, by.x = c("tripID", "Timestamp"), by.y = c("tripID", "start_seg"), all.x =T)


accdata3 <- accdata2 %>% fill(pred_behaviour) %>% fill(grouped_pred_behaviour)




#Plot ####
#number of graphs of 10,000 points
num_graphs <- ceiling(nrow(accdata3)/10000);num_graphs

#Create a new folder to store the graphs ######################### GOLD!
plotfolder <- paste0(filepath,"RFplots/")
#plotfolder <- paste0(filepath,"RFplots2/")
dir.create(plotfolder)

op <- options(digits.secs=6)



accdata3$Timestamp<-ymd_hms(accdata3$Timestamp)





table(accdata3$grouped_pred_behaviour)

#Plots 10,000 records per graph (100 2-second sections)
#Plots as many graphs as we need
for (i in 1:num_graphs){
  #i=12
  accdata_seg1 <- accdata3[1+((i-1)*10000):((i)*10000),]
  
  ggplot(accdata_seg1, aes(Timestamp,X, colour=(as.character(pred_behaviour)))) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    coord_cartesian(ylim = c(min(accdata3$X)-1, max(accdata3$X)+1)) +
    scale_x_continuous(expand = c(0, 0),minor_breaks = seq(0,10000,2)) +
    ylab(expression("Surge Acceleration (" ~ ms^-1 ~ ")")) +
    xlab("Time (seconds)") +
    labs(colour = "Behaviour") +
    theme(legend.position = c(0.85,0.8)) +
    theme(legend.text=element_text(size=8)) +
    scale_colour_manual(values=c("Flapping"="#f23c3c", "Dive"="#a678f0", 
                                 "Glidding"="#aaf078", "On_Water"="#098cd8", 
                                 "Takeoff"="#ffb82b","Active_On_Water"="darkblue", 
                                 "Active_On_Land"="chocolate4", "Jercky_Flight" ="black", "On_Land" ="burlywood" ))
  
  ggsave(paste0(plotfolder,"/plot",i,".png"))
}










