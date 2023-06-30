# Acc randomforest behavioural classification
# Adapted from script written 18/2/18
# Beth Clark bethany.clark@birdlife.org
# Grupo Aves Marinas Course, Dec 2020

#Set up ####
rm(list=ls())

library(dplyr)          ## data manipulation
library(tidyr)          ## data manipulation
library(lubridate)      ## data manipulation date & time
library(data.table)
library(tidyverse)

#install.packages("randomForest")
library(randomForest)



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_training_dataset_wd_ready_for_RF")


#Read in the training dataset and the full dataset



Original_files <- list.files()
print(Original_files)



myTrainingData <- rbindlist(fill=TRUE,lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_training_dataset_wd_ready_for_RF/"), fread))

unique(subset(myTrainingData, !is.na(Land))$ring_dep_trip)

####I have a couple trips with land. Clip the trips to remove tese sections 
other<-subset(myTrainingData, !(ring_dep_trip %in% unique(subset(myTrainingData, !is.na(Land))$ring_dep_trip)))
trim1<-max(subset(myTrainingData, ring_dep_trip == "ID_190_433_1001" & !is.na(Land))$section_cpm)
track1<-subset(myTrainingData, ring_dep_trip == "ID_190_433_1001")
track1<-subset(track1,section_cpm > trim1)

trim2<-max(subset(myTrainingData, ring_dep_trip == "ID_190_433_1000" & !is.na(Land))$section_cpm)
track2<-subset(myTrainingData, ring_dep_trip == "ID_190_433_1000")
track2<-subset(track2,section_cpm > trim2)

trim3<-max(subset(myTrainingData, ring_dep_trip == "ID_341_425_975" & !is.na(Land))$section_cpm)
track3<-subset(myTrainingData, ring_dep_trip == "ID_341_425_975")
track3<-subset(track3,section_cpm > trim3)

myTrainingData<-rbind(other, track1, track2, track3)


myTrainingData<-select(myTrainingData, -Land)



length(unique(myTrainingData$ring_dep_trip))









table(myTrainingData$Final_Behaviour)







myTrainingData$behaviour2 <- ifelse(myTrainingData$Final_Behaviour == "none", NA, as.character(myTrainingData$Final_Behaviour))


myTrainingData$sum_abs_depth<-abs(myTrainingData$sum_negative_depth)+ myTrainingData$sum_positive_depth
myTrainingData$sum_abs_X_surge<-abs(myTrainingData$sum_negative_X_surge)+ myTrainingData$sum_positive_X_surge
myTrainingData$sum_abs_Z_heave<-abs(myTrainingData$sum_negative_Z_heave)+ myTrainingData$sum_positive_Z_heave
myTrainingData$sum_abs_Y_sway<-abs(myTrainingData$sum_negative_Y_sway)+ myTrainingData$sum_positive_Y_sway
myTrainingData$sum_abs_pitch<-abs(myTrainingData$sum_negative_pitch)+ myTrainingData$sum_positive_pitch  
myTrainingData$sum_abs_roll<-abs(myTrainingData$sum_negative_roll)+ myTrainingData$sum_positive_roll

myTrainingData2<-subset(myTrainingData, !is.na(behaviour2))




myTrainingData2<-myTrainingData2[complete.cases(myTrainingData2), ]

#Running a random forest model is simple

#The format is similar to a linear model



###


myTrainingData2$behaviour2<-as.factor(as.character(myTrainingData2$behaviour2))

table(myTrainingData2$behaviour2)

#####this data set is too big for the model ( run into memory issues)
####remove small sections of a behaviour

####

myTrainingData3<-subset(myTrainingData2,NBbehaviour_tot >24)

table(myTrainingData3$behaviour2)




#Run a random forest #### ####






####close everything and re-open the session to run the model? 
saveRDS(myTrainingData3,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TRAINING_DATA_RF_SUBSET.rds" )
saveRDS(myTrainingData2,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TRAINING_DATA_FULL.rds" )







myTrainingData3<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TRAINING_DATA_RF_SUBSET.rds")
myTrainingData2<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TRAINING_DATA_FULL.rds")


myTrainingData3<-subset(myTrainingData2,NBbehaviour_tot >24)

table(myTrainingData3$behaviour2)

gc()

memory.limit(size = 30000000000)



#OOB = The error rate within the data set 
#100-error rate = within sample accuracy 

#!note! this could be very different to the error rate on new data

#Confusion matrix tells you which catergories are well estimated 
#and which they get mixed up with

#Check if it matters for your research question if two behaviours are 
#mixed up.

#To improve, you can target new training sample at important
#catergories with high class error rates.

#You could also calculate some new metrics that might help
#E.g. cumulative negative and positive x/y/z
#     min/max/variation in ODBA/VeDBA
#     wingbeat frequency (or step/flipper/etc.)

#Optimise mtry ####

#We can optimise the number of variable tried at each branch of the descision tree
#check for the best mtry value, it has the lowest error rate






OOB<-rep(NA,15)
for (i in 1:15) {
  OOB[i] <- randomForest(behaviour2 ~ mean_ODBA + mean_VeDBA + sd_ODBA +sd_VeDBA +
                           meanX_surge + stdevX_surge + minX_surge + maxX_surge + sum_positive_X_surge + sum_negative_X_surge +
                           meanY_sway + stdevY_sway + minY_sway + maxY_sway + 
                           sum_positive_Y_sway + sum_negative_Y_sway +
                           meanZ_heave + stdevZ_heave + minZ_heave + maxZ_heave + 
                           sum_positive_Z_heave + sum_negative_Z_heave +
                           pitch_mean + pitch_stdev + min_pitch + max_pitch + sum_positive_pitch + sum_negative_pitch +  roll_mean + roll_stdev + min_roll + max_roll +sum_positive_roll + sum_negative_roll + depth_mean + depth_stdev + min_depth + max_depth + sum_positive_depth + sum_abs_depth +sum_abs_X_surge + sum_abs_Y_sway + sum_abs_pitch + sum_abs_roll, ntree = 1000, mtry = i, data = myTrainingData3)$err.rate[1000]*100
  print(paste(i,OOB[i]))
}



which.min(OOB) #

#We can plot the importance of each variable to our classification







####run best model 


accRF <- randomForest(behaviour2 ~ mean_ODBA + mean_VeDBA + sd_ODBA +sd_VeDBA +
                        meanX_surge + stdevX_surge + minX_surge + maxX_surge + sum_positive_X_surge +        sum_negative_X_surge +sum_abs_X_surge+
                        meanY_sway + stdevY_sway + minY_sway + maxY_sway + 
                        sum_positive_Y_sway + sum_negative_Y_sway +
                        meanZ_heave + stdevZ_heave + minZ_heave + maxZ_heave + 
                        sum_positive_Z_heave + sum_negative_Z_heave +
                        pitch_mean + pitch_stdev + min_pitch + max_pitch + sum_positive_pitch + sum_negative_pitch +  roll_mean + roll_stdev + min_roll + max_roll +sum_positive_roll + sum_negative_roll + depth_mean + depth_stdev + min_depth + max_depth + sum_positive_depth + sum_abs_depth +sum_abs_Z_heave+ sum_abs_Y_sway + sum_abs_pitch + sum_abs_roll, 
                      ntree = 1000, mtry = 7, data = myTrainingData3, proximity = FALSE)

accRF

varImpPlot(accRF)



### read in and cut each of the full trips. 

accRF$predicted
accRF$votes



# 
# 
# #Use predict to add labels to the full dataset ####
# myTrainingData2$pred_behaviour <- predict(accRF,myTrainingData2) 
# pred_behaviour_prob <- predict(accRF,myTrainingData2, type = "prob" ) 
# 
# pred_behaviour_prob<-as.data.frame(pred_behaviour_prob)
# 
# myTrainingData<-cbind(myTrainingData,pred_behaviour_prob)
# 



######so from here lets create a table with the number of segments with each behaviour identified and then the mean plus sd of each of the characteristics including the segment length. 

myTrainingData3$seg_length<-myTrainingData3$end_seg- myTrainingData3$start_seg


#####now summarize the mean and sd of all the variables based on the behaviours. 

summary_variables<-myTrainingData3 %>%
  dplyr::group_by(behaviour2) %>%
  dplyr::summarize(mean_seg_length = mean(seg_length, na.rm = T),
                   sd_seg_length = sd(seg_length, na.rm = T),
                   mean_ODBA = mean(mean_ODBA),
                   sd_ODBA = sd(mean_ODBA, na.rm = T),
                   mean_VeDBA = mean(mean_VeDBA, na.rm = T),
                   sd_VeDBA = sd(mean_VeDBA, na.rm = T),
                   mean_sd_ODBA = mean(sd_ODBA, na.rm = T),
                   sd_sd_ODBA = sd(sd_ODBA, na.rm = T),
                   mean_sd_VeDBA = mean(sd_VeDBA, na.rm = T),
                   sd_sd_VeDBA = sd(sd_VeDBA, na.rm = T),
                   mean_meanX_surge = mean(meanX_surge),
                   sd_meanX_surge= sd(meanX_surge),
                   mean_stdevX_surge = mean(stdevX_surge),
                   sd_stdevX_surge = sd(stdevX_surge),
                   mean_minX_surge = mean(minX_surge),
                   sd_minX_surge = sd(minX_surge),
                   mean_maxX_surge = mean(maxX_surge),
                   sd_maxX_surge = sd(maxX_surge),
                   mean_sum_positive_X_surge = mean(sum_positive_X_surge),
                   sd_sum_positive_X_surge = sd(sum_positive_X_surge),
                   mean_sum_negative_X_surge = mean(sum_negative_X_surge),
                   sd_sum_negative_X_surge= sd(sum_negative_X_surge),
                   mean_meanY_sway = mean(meanY_sway),
                   sd_meanY_sway = sd(meanY_sway),
                   mean_stdevY_sway = mean(stdevY_sway),
                   sd_stdevY_sway = sd(stdevY_sway),
                   mean_minY_sway= mean(minY_sway),
                   sd_minY_sway = sd(minY_sway),
                   mean_sum_positive_Y_sway = mean(sum_positive_Y_sway),
                   sd_sum_positive_Y_sway = sd(sum_positive_Y_sway),
                   mean_maxY_sway = mean(maxY_sway),
                   sd_maxY_sway = sd(maxY_sway),
                   mean_sum_negative_Y_sway = mean(sum_negative_Y_sway),
                   sd_sum_negative_Y_sway = sd(sum_negative_Y_sway),
                   mean_meanZ_heave = mean(meanZ_heave),
                   sd_meanZ_heave = sd(meanZ_heave),
                   mean_stdevZ_heave = mean(stdevZ_heave),
                   sd_stdevZ_heave = sd(stdevZ_heave),
                   mean_minZ_heave = mean(minZ_heave),
                   sd_minZ_heave = sd(minZ_heave),
                   mean_maxZ_heave = mean(maxZ_heave),
                   sd_maxZ_heave = sd(maxZ_heave),
                   mean_sum_positive_Z_heave = mean(sum_positive_Z_heave),
                   sd_sum_positive_Z_heave = sd(sum_positive_Z_heave),
                   mean_sum_negative_Z_heave = mean(sum_negative_Z_heave),
                   sd_sum_negative_Z_heave = sd(sum_negative_Z_heave),
                   mean_pitch_mean = mean(pitch_mean),
                   sd_pitch_mean = sd(pitch_mean),
                   mean_pitch_stdev = mean(pitch_stdev),
                   sd_pitch_stdev = sd(pitch_stdev),
                   mean_min_pitch = mean(min_pitch),
                   sd_min_pitch = sd(min_pitch),
                   mean_max_pitch = mean(max_pitch),
                   sd_max_pitch = sd(max_pitch),
                   mean_sum_positive_pitch = mean(sum_positive_pitch),
                   sd_sum_positive_pitch = sd(sum_positive_pitch),
                   mean_sum_negative_pitch = mean(sum_negative_pitch),
                   sd_sum_negative_pitch = sd(sum_negative_pitch),
                   mean_roll_mean = mean(roll_mean),
                   sd_roll_mean = sd(roll_mean),
                   mean_roll_stdev = mean(roll_stdev),
                   sd_roll_stdev = sd(roll_stdev),
                   mean_min_roll = mean(min_roll),
                   sd_min_roll = sd(min_roll),
                   mean_max_roll = mean(max_roll),
                   sd_max_roll = sd(max_roll),
                   mean_sum_positive_roll = mean(sum_positive_roll),
                   sd_sum_positive_roll = sd(sum_positive_roll),
                   mean_sum_negative_roll = mean(sum_negative_roll),
                   sd_sum_negative_roll = sd(sum_negative_roll),
                   mean_depth_mean = mean(depth_mean),
                   sd_depth_mean = sd(depth_mean),
                   mean_depth_stdev = mean(depth_stdev),
                   sd_depth_stdev = sd(depth_stdev),
                   mean_min_depth = mean(min_depth),
                   sd_min_depth = sd(min_depth),
                   mean_max_depth = mean(max_depth),
                   sd_max_depth = sd(max_depth),
                   mean_sum_positive_depth = mean(sum_positive_depth),
                   sd_sum_positive_depth = sd(sum_positive_depth),
                   mean_sum_negative_depth= mean(sum_negative_depth),
                   sd_sum_negative_depth = sd(sum_negative_depth),
                   mean_sum_abs_depth= mean(sum_abs_depth),
                   sd_sum_abs_depth = sd(sum_abs_depth),
                   mean_sum_abs_X_surge= mean(sum_abs_X_surge),
                   sd_sum_abs_X_surge = sd(sum_abs_X_surge),
                   mean_sum_abs_Y_sway= mean(sum_abs_Y_sway),
                   sd_sum_abs_Y_sway = sd(sum_abs_Y_sway),
                   mean_sum_abs_pitch= mean(sum_abs_pitch),
                   sd_sum_abs_pitch = sd(sum_abs_pitch),
                   mean_sum_abs_roll= mean(sum_abs_roll),
                   sd_sum_abs_roll = sd(sum_abs_roll), 
                   mean_sum_abs_heave = mean(sum_abs_Z_heave),
                   sd_sum_abs_heave = sd (sum_abs_Z_heave))

write.csv(summary_variables, "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/rf_segment_summary_over_1s.csv")








































####Now we need to read in the actuall full dataset and set it up just like the training dataset so that we can extrapolate the model over everything. 



Original_files <- list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_summarised_cpm/", pattern = '*metrics_with_depth_cpm_perseg.csv')
print(Original_files)

# 
# first_loop<-Original_files[c(8, 9,10,11, 12, 17, 18, 26, 28,29,30,31, 32,33, 36,37)]
# 
# seccond_loop<-Original_files[c(1:7,13, 14, 15,16,19:25, 27, 34,35, 38:52)]

#first_loop<-Original_files[c(10,33,37)]
library(data.table)
library(lubridate)
#####Create a second loop to read-in previous files, remove the none behaviours, extract the start of each segment 
op <- options(digits.secs=6)
run_model_over_each_trip <- function(file_name){
  #file_name <-Original_files[1]
  
  #Read in the full dataset (1 trip) 
  full_acc<-fread(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_summarised_cpm//", file_name), header=T, sep=",")
  full_acc$start_seg<-ymd_hms(full_acc$start_seg)
  
  full_acc$sum_abs_depth<-abs(full_acc$sum_negative_depth)+ full_acc$sum_positive_depth
  full_acc$sum_abs_X_surge<-abs(full_acc$sum_negative_X_surge)+ full_acc$sum_positive_X_surge
  full_acc$sum_abs_Y_sway<-abs(full_acc$sum_negative_Y_sway)+ full_acc$sum_positive_Y_sway
  full_acc$sum_abs_Z_heave<-abs(full_acc$sum_negative_Z_heave)+ full_acc$sum_positive_Z_heave
  full_acc$sum_abs_pitch<-abs(full_acc$sum_negative_pitch)+ full_acc$sum_positive_pitch  
  full_acc$sum_abs_roll<-abs(full_acc$sum_negative_roll)+ full_acc$sum_positive_roll
  
  full_acc$pred_behaviour <- predict(accRF,full_acc) 
  pred_behaviour_prob <- predict(accRF,full_acc, type = "prob" ) 
  
  pred_behaviour_prob<-as.data.frame(pred_behaviour_prob)
  
  full_acc<-cbind(full_acc,pred_behaviour_prob)
  
  #####now create the 0.65 cut off 
  full_acc$pred_behaviour_clear_0.65<- as.factor(ifelse(full_acc$Dive >0.65 & full_acc$Dive >full_acc$Flapping & full_acc$Dive >full_acc$Water, "Dive", ifelse ( full_acc$Flapping>0.65 & full_acc$Flapping >full_acc$Dive & full_acc$Flapping >full_acc$Water,  "Flapping",ifelse(full_acc$Water>0.65  & full_acc$Water >full_acc$Dive & full_acc$Water > full_acc$Flapping, "Water", NA))))
  
  
  #####Now do the logic correction. This only makes sense with same seg length analyses. 
  # 
  # full_acc$Labels3 <- as.character(full_acc$pred_behaviour_clear_0.65)
  # full_acc$Labels4 <- as.character(full_acc$pred_behaviour_clear_0.65)
  # 
  # table(full_acc$Labels3, exclude = NULL)
  # 
  # 
  # full_acc$Labels3<-ifelse(is.na(full_acc$Labels3), "unknown", full_acc$Labels3)
  # 
  # 
  # #remove single instances of flapping flight within water sections
  # for (i in 2:(nrow(full_acc)-1)){
  #   if(full_acc$Labels3[i] == "Flapping" & 
  #      full_acc$Labels3[i-1] == "Water"   & 
  #      full_acc$Labels3[i+1] == "Water"){
  #     full_acc$Labels4[i] <- "Water"
  #   }
  # }
  # 
  # #remove single instances of flapping flight within water sections
  # for (i in 2:(nrow(full_acc)-1)){
  #   if(full_acc$Labels3[i] == "Water" & 
  #      full_acc$Labels3[i-1] == "Flapping"  & 
  #      full_acc$Labels3[i+1] == "Flapping"){
  #     full_acc$Labels4[i] <- "Flapping"
  #   }
  # }
  # 
  # table(full_acc$Labels4)
  
  saveRDS(full_acc,paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Predicted_trips_axy/", str_sub(file_name, end= -5),  "_predicted.rds"))
  write.csv(full_acc,paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Predicted_trips_axy/", str_sub(file_name, end= -5),  "_predicted.csv"),row.names=F)
 
}

lapply(Original_files, run_model_over_each_trip)

























######The following code was used to make a plot of the 0.65 cut off 



vector_0.33_0.99<-seq(0.1:1.0, by = 0.01)


df = NULL



for (ii in vector_0.33_0.99) {
  #ii <- 0.31
pred_behaviour <- as.factor(ifelse(myTrainingData$Dive > ii & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>ii & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water> ii & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

trial<-count(pred_behaviour)
# transpose
t_trial <- transpose(trial)

# get row and colnames in order
colnames(t_trial) <- trial[,1]
t_trial<-t_trial[2,]
t_trial$Accuracy <- as.numeric(ii)
t_trial$Dive <-as.numeric(t_trial$Dive)
t_trial$Flapping <-as.numeric(t_trial$Flapping)
t_trial$Water <-as.numeric(t_trial$Water)

df = rbind(df, data.frame(t_trial))
}

head(df)
str(df)

df<-select(df, Dive, Flapping, Water, Unknown = NA., Accuracy)

ggplot(df, aes(x=Accuracy)) + 
  theme_bw() +
  geom_line(aes(y = Dive), color = "red", size =2) + 
  geom_line(aes(y = Flapping), color="cyan", size =2)  +
  geom_line(aes(y = Water), color="lightgoldenrod", size =2)  +
  geom_line(aes(y = as.numeric(Unknown)), color="grey", size =2) +
  ylab("Classified Positions")
  
  
df


###

df$prop_classified<-(df$Dive+df$Flapping+df$Water)/(21211+704322+2558389+201)


ggplot(df, aes(x=Accuracy)) + 
  theme_bw() +
  geom_line(aes(y = prop_classified), color = "red", size =2)






myTrainingData$pred_behaviour_clear_0.8<- as.factor(ifelse(myTrainingData$Dive >0.8 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.8 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.8  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water" , NA))))

myTrainingData$pred_behaviour_clear_0.85<- as.factor(ifelse(myTrainingData$Dive >0.85 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.85 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.85  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))


myTrainingData$pred_behaviour_clear_0.9<- as.factor(ifelse(myTrainingData$Dive >0.9 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.9 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.9  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))


myTrainingData$pred_behaviour_clear_0.95<- as.factor(ifelse(myTrainingData$Dive >0.95 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.95 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.95  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))


myTrainingData$pred_behaviour_clear_0.7<- as.factor(ifelse(myTrainingData$Dive >0.7 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.7 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.7  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.75<- as.factor(ifelse(myTrainingData$Dive >0.75 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.75 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.75  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.6<- as.factor(ifelse(myTrainingData$Dive >0.6 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.6 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.6  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.65<- as.factor(ifelse(myTrainingData$Dive >0.65 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.65 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.65  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.5<- as.factor(ifelse(myTrainingData$Dive >0.5 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.5 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.5  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.55<- as.factor(ifelse(myTrainingData$Dive >0.55 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.55 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.55  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.4<- as.factor(ifelse(myTrainingData$Dive >0.4 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.4 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.4  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.45<- as.factor(ifelse(myTrainingData$Dive >0.45 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.45 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.45  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))



myTrainingData$pred_behaviour_clear_0.3<- as.factor(ifelse(myTrainingData$Dive >0.3 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.3 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.3  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.35<- as.factor(ifelse(myTrainingData$Dive >0.35 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.35 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.35  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))


myTrainingData$pred_behaviour_clear_0.2<- as.factor(ifelse(myTrainingData$Dive >0.2 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.2 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.2  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.25<- as.factor(ifelse(myTrainingData$Dive >0.25 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.25 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.25  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))


myTrainingData$pred_behaviour_clear_0.1<- as.factor(ifelse(myTrainingData$Dive >0.1 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.1 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.1  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.15<- as.factor(ifelse(myTrainingData$Dive >0.15 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.15 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.15  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))

myTrainingData$pred_behaviour_clear_0.05<- as.factor(ifelse(myTrainingData$Dive >0.05 & myTrainingData$Dive >myTrainingData$Flapping & myTrainingData$Dive >myTrainingData$Water, "Dive", ifelse ( myTrainingData$Flapping>0.05 & myTrainingData$Flapping >myTrainingData$Dive & myTrainingData$Flapping >myTrainingData$Water,  "Flapping",ifelse(myTrainingData$Water>0.05  & myTrainingData$Water >myTrainingData$Dive & myTrainingData$Water > myTrainingData$Flapping, "Water", NA))))





myTrainingData$behaviour<-as.factor(myTrainingData$behaviour)
table(myTrainingData$pred_behaviour_clear_0.95)
table(myTrainingData$pred_behaviour_clear_0.9)
table(myTrainingData$pred_behaviour_clear_0.85)
table(myTrainingData$pred_behaviour_clear_0.8)
table(myTrainingData$pred_behaviour_clear_0.75)
table(myTrainingData$pred_behaviour_clear_0.7)
table(myTrainingData$pred_behaviour_clear_0.65)
table(myTrainingData$pred_behaviour_clear_0.6)
table(myTrainingData$pred_behaviour_clear_0.55)
table(myTrainingData$pred_behaviour_clear_0.5)
table(myTrainingData$pred_behaviour_clear_0.45)
table(myTrainingData$pred_behaviour_clear_0.4)
table(myTrainingData$pred_behaviour_clear_0.35)
table(myTrainingData$pred_behaviour_clear_0.3)
table(myTrainingData$pred_behaviour_clear_0.25)
table(myTrainingData$pred_behaviour_clear_0.2)
table(myTrainingData$pred_behaviour_clear_0.15)
table(myTrainingData$pred_behaviour_clear_0.1)
table(myTrainingData$pred_behaviour_clear_0.05)
table(myTrainingData$pred_behaviour_clear)
table(myTrainingData$pred_behaviour)
table(myTrainingData$behaviour)

table(myTrainingData$behaviour2)


####make a plot of this










cm<-table(myTrainingData$behaviour2, myTrainingData$pred_behaviour)

cm<-table(myTrainingData$behaviour, myTrainingData$pred_behaviour_clear_0.8)



saveRDS(myTrainingData,paste0(filepath,"Predicted_trips/Acc_16_trips_17082021_full_acc_labels_cmp.rds"))



####save this out



























































#write.csv(full_acc,paste0(filepath,"4Acc_sample_full_acc_labels2.csv"), header = T)
