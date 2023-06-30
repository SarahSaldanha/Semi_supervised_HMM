#clear R
rm(list=ls()) 

library(dplyr)          ## data manipulation
library(tidyverse)          ## data manipulation
library(lubridate)      ## data manipulation date & time
library(data.table)

op <- options(digits.secs=6)

memory.limit(size=5000000000)


setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_labelled_WD_noID/")


# This script takes the files lablled in Framework4 or R, individually names each segment of behaviour,
# removes the unlabelled rows, takes the first 100 rows of each segment to create a training dataset
# Adapted from script written 18/2/18
# Beth Clark bethany.clark@birdlife.org
# Grupo Aves Marinas Course, Dec 2020

#Setup ####





#set folder
filepath <- "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_labelled_WD_noID/"

#Set the require segment length
seglength <- 25 #50 = 2seconds for my 25 hz data




Original_files <- list.files(pattern = '*.csv')
print(Original_files)

# 
# #Set the require segment length
# freq <- 25# The Frequency of accelerometry data (Hz)
# secs <- 2 # The number of seconds over which to calculate the desired metrics.
# # The manuscript says to use 1 second intervals, but Phil said "to capture 
# # gliding flight as well I've found that a longer period is needed."
# numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 






#####This for trips without subsamples only 

Add_behaviour_and_labelled_segments <- function(file_name){
  #file_name <-Original_files[1]
  #Read in the file that you read in to Framework4 (as Framework4 messes up date format when it saves)
  
  acc.labels<-fread(paste0(filepath, file_name), header=T, sep=",")
  acc.labels$index <- 1:nrow(acc.labels) ### add an index column with indicating the row number 
  acc.labels$Timestamp_mod_axy_merge<-acc.labels$Timestamp_mod_axy

  
  #Read in the output file from Framework4
  acc.data<- fread(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_summarised_cpm/", str_sub(file_name, 10,-5),  "_metrics_with_depth_cpm_perseg.csv"))
  
  
  acc.data$start_seg_merge<-acc.data$start_seg
  acc.data$end_seg_merge<-acc.data$end_seg

  
  
  
  #####now merge on the fact that the timestamp is between the cpm 
  together<-acc.data[acc.labels, on = .(ring_dep_trip == ring_dep_trip, start_seg_merge <= Timestamp_mod_axy_merge, end_seg_merge > Timestamp_mod_axy_merge)]
  
  together<-select(together, -start_seg_merge, -end_seg_merge)
  
  
  
  ####now summarise this based on the 
  
  together<-subset(together, !is.na(section_cpm))
  together$section_cpm<-as.factor(together$section_cpm)
  
  sum_together<-together %>%
    group_by(ring_dep_trip, section_cpm, start_seg, end_seg, mean_ODBA, sd_ODBA,mean_VeDBA,sd_VeDBA,meanX_surge,stdevX_surge,minX_surge, maxX_surge, sum_positive_X_surge, sum_negative_X_surge, meanY_sway, stdevY_sway, minY_sway, maxY_sway, sum_positive_Y_sway, sum_negative_Y_sway, meanZ_heave, stdevZ_heave, minZ_heave, maxZ_heave, sum_positive_Z_heave, sum_negative_Z_heave, pitch_mean, pitch_stdev, min_pitch, max_pitch, sum_positive_pitch,sum_negative_pitch,roll_mean, roll_stdev,  min_roll , max_roll, sum_positive_roll, sum_negative_roll ,depth_mean ,depth_stdev ,min_depth ,max_depth ,sum_positive_depth, sum_negative_depth, behaviour)%>%
    tally()%>%
    spread(behaviour, n)%>%
    mutate_at(c('Dive','Flapping', 'none', 'Water'), ~replace_na(.,0))%>%
    ungroup()
  
  
  
  sum_together$NBbehaviour_tot<-sum_together$Dive + sum_together$Flapping + sum_together$none +sum_together$Water 
  sum_together$Final_Behaviour<- ifelse(sum_together$Dive == sum_together$NBbehaviour_tot, "Dive", ifelse(sum_together$Flapping == sum_together$NBbehaviour_tot, "Flapping", ifelse(sum_together$Water == sum_together$NBbehaviour_tot, "Water", "none")))
  
  table(sum_together$Final_Behaviour)
  

  fwrite(sum_together,paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_training_dataset_wd_ready_for_RF/", file_name, "_training.csv"),row.names=F)
  
} 
  
lapply(Original_files, Add_behaviour_and_labelled_segments)
  
#####################
