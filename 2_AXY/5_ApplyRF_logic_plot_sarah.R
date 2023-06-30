#Attaches labels from randomforest to the acc data with correct timestamps

#Use logical correction to improve accuracy

#Then plots out the full dataset with behaviours coloured 
#for checking the randomforest predictionss

# Adapted from script written 24/5/19
# Beth Clark bethany.clark@birdlife.org
# Grupo Aves Marinas Course, Dec 2020

#Set up ####
rm(list=ls())
library(randomForest);library(ggplot2)
library(grid)
library(ggplot2)
library(randomForest)
library (stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)

#set the seglength
seglength <- 50



filepath <- "E:/Dropbox/PhD/All.GPSandAXYS/axys_all_files/Trips/"
acclabels<-readRDS("E:/Dropbox/PhD/All.GPSandAXYS/axys_all_files/Trips/Predicted_trips/Acc_16_trips_17082021_full_acc_labels_cmp.rds")
head(acclabels)




#acclabels$tripID<-str_sub(acclabels$bird_seg2, end = -((nchar(acclabels$segID2)+2)))

list_trips<-unique(paste0(acclabels$tripID, ".csv"))
  
  


setwd("E:/Dropbox/PhD/All.GPSandAXYS/axys_all_files/Trips/TRIPS_WITH_WET_DRY/")



Original_files <- list.files(pattern = '*.csv')
print(Original_files)
####load in the files manually because they have different columns (i dont know why..)

####read in all data and cut bits on land
memory.limit(size=99999999999999)

t8201689_rec09042021_1_WET_DRY<-fread("8201689_PHAAET_rec09042021_ICima_ninho_27_21_S1_1_WET_DRY.csv", nrow = 1000000)
common_columns<-names(t8201689_rec09042021_1_WET_DRY)[1:63]


t8201689_rec09042021_1_WET_DRY<-select(t8201689_rec09042021_1_WET_DRY, all_of(common_columns))

t7502351_rec08032021_1_WET_DRY<-fread("7502351_PHAAET_rec08032021_ICima_ninho_3_21_S1_1_WET_DRY_corrected.csv", nrow = 1000000)
t7502351_rec08032021_1_WET_DRY<-select(t7502351_rec08032021_1_WET_DRY, all_of(common_columns))

all_wd<-rbind(t8201689_rec09042021_1_WET_DRY, t7502351_rec08032021_1_WET_DRY)

rm(t8201689_rec09042021_1_WET_DRY, t7502351_rec08032021_1_WET_DRY)



t7502351_rec20042021_1_WET_DRY<-fread("7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_1_WET_DRY_corrected.csv")
t7502351_rec20042021_1_WET_DRY<-select(t7502351_rec20042021_1_WET_DRY,all_of(common_columns))


all_wd<-rbind(all_wd, t7502351_rec20042021_1_WET_DRY)

rm(t7502351_rec20042021_1_WET_DRY)

t7502351_rec20042021_2_WET_DRY<-fread("7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_2_WET_DRY_corrected.csv", nrow = 1000000)
t7502351_rec20042021_2_WET_DRY<-select(t7502351_rec20042021_2_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t7502351_rec20042021_2_WET_DRY)
rm(t7502351_rec20042021_2_WET_DRY)

t7502351_rec20042021_3_WET_DRY<-fread("7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_3_WET_DRY_corrected.csv")
t7502351_rec20042021_3_WET_DRY<-select(t7502351_rec20042021_3_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t7502351_rec20042021_3_WET_DRY)
rm(t7502351_rec20042021_3_WET_DRY)

t7502351_rec20042021_4_WET_DRY<-fread("7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_4_WET_DRY_corrected.csv")
t7502351_rec20042021_4_WET_DRY<-select(t7502351_rec20042021_4_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t7502351_rec20042021_4_WET_DRY)
rm(t7502351_rec20042021_4_WET_DRY)

t8200501_rec08042021_1_WET_DRY<-fread("8200501_PHAAET_rec08042021_ICima_ninho_3_37_S1_1_WET_DRY_corrected.csv", nrow = 1000000)
t8200501_rec08042021_1_WET_DRY<-select(t8200501_rec08042021_1_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t8200501_rec08042021_1_WET_DRY)
rm(t8200501_rec08042021_1_WET_DRY)

t8200501_rec08042021_2_WET_DRY<-fread("8200501_PHAAET_rec08042021_ICima_ninho_3_37_S1_2_WET_DRY_corrected.csv")
t8200501_rec08042021_2_WET_DRY<-select(t8200501_rec08042021_2_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t8200501_rec08042021_2_WET_DRY)
rm(t8200501_rec08042021_2_WET_DRY)


t8201658_rec11032021_1_WET_DRY<-fread("8201658_PHAAET_rec11032021_ICima_ninho_46_36_S1_1_WET_DRY.csv", nrow = 1000000)
t8201658_rec11032021_1_WET_DRY<-select(t8201658_rec11032021_1_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201658_rec11032021_1_WET_DRY)
rm(t8201658_rec11032021_1_WET_DRY)

t8201666_rec04022021_1_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_1_WET_DRY.csv", nrow = 1000000)
t8201666_rec04022021_1_WET_DRY<-select(t8201666_rec04022021_1_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201666_rec04022021_1_WET_DRY)
rm(t8201666_rec04022021_1_WET_DRY)

t8201666_rec04022021_2_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_2_WET_DRY.csv")
t8201666_rec04022021_2_WET_DRY<-select(t8201666_rec04022021_2_WET_DRY, all_of(common_columns))

all_wd<-rbind(all_wd, t8201666_rec04022021_2_WET_DRY)
rm(t8201666_rec04022021_2_WET_DRY)



t8201666_rec04022021_3_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_3_WET_DRY.csv")
t8201666_rec04022021_3_WET_DRY<-select(t8201666_rec04022021_3_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201666_rec04022021_3_WET_DRY)
rm(t8201666_rec04022021_3_WET_DRY)


t8201666_rec04022021_4_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_4_WET_DRY_corrected.csv")
t8201666_rec04022021_4_WET_DRY<-select(t8201666_rec04022021_4_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201666_rec04022021_4_WET_DRY)
rm(t8201666_rec04022021_4_WET_DRY)


t8201666_rec04022021_5_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_5_WET_DRY.csv")
t8201666_rec04022021_5_WET_DRY<-select(t8201666_rec04022021_5_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201666_rec04022021_5_WET_DRY)
rm(t8201666_rec04022021_5_WET_DRY)


t8201666_rec04022021_6_WET_DRY<-fread("8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_6_WET_DRY_corrected.csv", nrow = 1000000)
t8201666_rec04022021_6_WET_DRY<-select(t8201666_rec04022021_6_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201666_rec04022021_6_WET_DRY)
rm(t8201666_rec04022021_6_WET_DRY)


t8201674_rec10022021_1_WET_DRY<-fread("8201674_PHAAET_rec10022021_ICima_ninho_31_14_S1_1_WET_DRY_corrected.csv")
t8201674_rec10022021_1_WET_DRY<-select(t8201674_rec10022021_1_WET_DRY, all_of(common_columns))


all_wd<-rbind(all_wd, t8201674_rec10022021_1_WET_DRY)
rm(t8201674_rec10022021_1_WET_DRY)



unique(all_wd$tripID)







#list_datasets<-lapply(Original_files, fread, fill=TRUE, na.strings="")


#accdata_8_15 <- 
#  do.call(rbind,lapply(list_trips[8:15], read.csv))
#accdata_16_23 <- 
#  do.call(rbind,lapply(list_trips[16:23], read.csv))
#accdata_24_31 <- 
#  do.call(rbind,lapply(list_trips[24:31], read.csv))
#accdata_32_34 <- 
#  do.call(rbind,lapply(list_trips[32:34], read.csv))
#accdata_35_39 <- 
#  do.call(rbind,lapply(list_trips[35:39], read.csv))
#accdata_40_43 <- 
#  do.call(rbind,lapply(list_trips[40:43], read.csv))








acclabels$Labels <- as.character(acclabels$pred_behaviour)
acclabels$Labels2 <- as.character(acclabels$pred_behaviour)
acclabels$Labels3 <- as.character(acclabels$pred_behaviour_clear_0.65)
acclabels$Labels4 <- as.character(acclabels$pred_behaviour_clear_0.65)

table(acclabels$Labels2, exclude = NULL)
table(acclabels$Labels3, exclude = NULL)


acclabels$Labels3<-ifelse(is.na(acclabels$Labels3), "unknown", acclabels$Labels3)


#remove single instances of flapping flight within water sections
for (i in 2:(nrow(acclabels)-1)){
  if(acclabels$Labels3[i] == "Flapping" & 
   acclabels$Labels3[i-1] == "Water"   & 
     acclabels$Labels3[i+1] == "Water"){
    acclabels$Labels4[i] <- "Water"
  }
}

#remove single instances of flapping flight within water sections
for (i in 2:(nrow(acclabels)-1)){
  if(acclabels$Labels3[i] == "Water" & 
     acclabels$Labels3[i-1] == "Flapping"  & 
     acclabels$Labels3[i+1] == "Flapping"){
    acclabels$Labels4[i] <- "Flapping"
  }
}

table(acclabels$Labels4)










# 
# 
# water_behaviours <- c("On_Water","Active_On_Water")
# water_behaviours2 <- c("On_Water","Active_On_Water")
# flight_behaviours <- c("Flapping","Glidding", "Jercky_Flight")
# land_behaviours <-  c("On_Land","Active_On_Land")
# 
# #remove single instances of passive flight within On_Water sections
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels[i] == "Glidding" & 
#      acclabels$Labels[i-1] %in% water_behaviours & 
#      acclabels$Labels[i+1] %in% water_behaviours){
#     acclabels$Labels2[i] <- "On_Water"
#   }
# }
# 
# #remove single instances of water within flight sections
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels[i] %in% water_behaviours & 
#      acclabels$Labels[i-1] %in% flight_behaviours & 
#      acclabels$Labels[i+1] %in% flight_behaviours){
#     acclabels$Labels2[i] <- "unknown flight"
#   }
# }
# 
# ##remove single instances of water behaviours between flight and dives logic3
# #for (i in 2:(nrow(acclabels)-1)){
# #  if(acclabels$Labels[i] %in% water_behaviours & 
# #     acclabels$Labels[i-1] %in% flight_behaviours & 
# #     acclabels$Labels[i+1] == "Dive"){
# #    acclabels$Labels2[i] <- "unknown flight"
# #  }
# #}
# 
# #remove single instances of water behaviours after takeoff before flight logic4
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels[i] %in% water_behaviours & 
#      acclabels$Labels[i-1] == "Takeoff" & 
#      acclabels$Labels[i+1] %in% flight_behaviours){
#     acclabels$Labels2[i] <- "unknown flight"
#   }
# }
# 
# #second level
# flight_behaviours2 <- c("Flapping","Glidding","unknown flight", "Jercky_Flight")
# acclabels$Labels3 <- acclabels$Labels2
# 
# #remove single flight between waters
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels2[i]  %in% flight_behaviours2 & 
#      acclabels$Labels2[i-1] %in% water_behaviours & 
#      acclabels$Labels2[i+1] %in% water_behaviours){
#     acclabels$Labels3[i] <- "On_Water"
#   }
# }
# 
# #remove single water between flight
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels2[i]  %in% water_behaviours2 & 
#      acclabels$Labels2[i-1] %in% flight_behaviours2 & 
#      flight_behaviours2[i+1] %in% flight_behaviours2){
#     acclabels$Labels3[i] <- "unknown flight"
#   }
# }
# 
# 
# 
# #remove single flight between land
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels2[i]  %in% flight_behaviours2 & 
#      acclabels$Labels2[i-1] %in% land_behaviours & 
#      acclabels$Labels2[i+1] %in% land_behaviours){
#     acclabels$Labels3[i] <- "Active_On_Land"
#   }
# }
# 
# 
# #remove single water between land
# for (i in 2:(nrow(acclabels)-1)){
#   if(acclabels$Labels2[i]  %in% water_behaviours2 & 
#      acclabels$Labels2[i-1] %in% land_behaviours & 
#      acclabels$Labels2[i+1] %in% land_behaviours){
#     acclabels$Labels3[i] <- "On_Land"
#   }
# }
# 
# 
# 
# 
# 
# 




#remove single instances of dive within rest on water sections logic5 after other corrections
#for (i in 2:(nrow(acclabels)-1)){
#  if(acclabels$Labels2[i] == "Dive" & 
#     acclabels$Labels2[i-1] == "On_water" & 
#     acclabels$Labels2[i+1] == "water"){
#    acclabels$Labels3[i] <- "preen"
#  }
#}













head(acclabels)
table(acclabels$Labels)
table(acclabels$Labels2)
table(acclabels$Labels3)
table(acclabels$Labels4)

save.image(file = "AccLabels_logical_correction.RData")

saveRDS(acclabels, file = "acclabels_wet_dry_cmp.rds")


label_behaviour_only<- dplyr::select(acclabels,start_seg, behaviour, behaviour2, pred_behaviour_clear_0.65, pred_behaviour, Dive, Flapping,  Water,  Labels4, tripID)





label_behaviour_only$start_seg<- ymd_hms(label_behaviour_only$start_seg)

all_wd$Timestamp3<-ymd_hms(all_wd$Timestamp)

accdata2<-merge(all_wd, label_behaviour_only, by.x = c("tripID", "Timestamp3"), by.y = c("tripID", "start_seg"), all.x =T)

rm(all_wd)



head(accdata2)

saveRDS(accdata3, file = "accdata3_wet_dry_cmp.rds")


accdata3 <- as.data.frame(accdata2) %>% fill(pred_behaviour) %>% fill(behaviour) %>% fill(behaviour2) %>% fill(pred_behaviour_clear_0.65) %>% fill(Dive) %>% fill(Flapping)%>% fill(Water)%>% fill(Labels4)









acclabels$
accdata3<-merge(accdata2, label_behaviour_only, by.x = c("tripID", "Timestamp"), by.y = c("tripID", "start_seg"), all.x =T)





head(accdata3)
str(accdata3)

accdata3$Timestamp3<-ymd_hms(accdata3$Timestamp)


#number of graphs of 10,000 points
num_graphs <- ceiling(nrow(accdata3)/10000)

#Create a new folder to store the graphs
plotfolder <- paste0(filepath,"RFplots_logic_2/")
dir.create(plotfolder)

for (i in 1:300){
  #i<-2
  
  accdata_seg1 <- accdata3[1+((i-1)*10000):((i)*10000),]
  accdata_seg1$rowID <- 1:nrow(accdata_seg1)
  
  
  p1<-ggplot(accdata_seg1, aes(Timestamp3,X, colour=(as.character(Labels4)))) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    coord_cartesian(ylim = c(min(accdata3$X)-1, max(accdata3$X)+1)) +
    scale_x_continuous(expand = c(0, 0),minor_breaks = seq(0,10000,2)) +
    ylab(expression("Surge(" ~ ms^-1 ~ ")")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(colour = "Behaviour") +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=6)) +
    theme(legend.title = element_blank(), legend.margin=margin(0,0,0,0),
  legend.box.margin=margin(-10,-10,-10,-10))+
    scale_colour_manual(values=c("Flapping"="cyan", "Dive"="red", 
                                 "Water"="darkblue"))
  
  
  
  
  p2<-ggplot(accdata_seg1, aes(Timestamp3,Z, colour=(as.character(Labels4)))) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    coord_cartesian(ylim = c(min(accdata3$Z)-1, max(accdata3$Z)+1)) +
    scale_x_continuous(expand = c(0, 0),minor_breaks = seq(0,10000,2)) +
    ylab(expression("Heave(" ~ ms^-1 ~ ")")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(colour = "Behaviour") +
    theme(legend.position = "none") +
    scale_colour_manual(values=c("Flapping"="cyan", "Dive"="red", 
                                 "Water"="darkblue" ))
  
  
  p3<-ggplot(accdata_seg1, aes(Timestamp3,Y, colour=(as.character(Labels4)))) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    coord_cartesian(ylim = c(min(accdata3$Z)-1, max(accdata3$Z)+1)) +
    scale_x_continuous(expand = c(0, 0),minor_breaks = seq(0,10000,2)) +
    ylab(expression("Sway(" ~ ms^-1 ~ ")")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    labs(colour = "Behaviour") +
    theme(legend.position = "none") +
    scale_colour_manual(values=c("Flapping"="cyan", "Dive"="red", 
                                 "Water"="darkblue" ))
  
  
  
  
  
  
  p4<-ggplot(accdata_seg1, aes(Timestamp3,Approx_depth_negative_mode, colour=(as.character(Labels4)))) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    coord_cartesian(ylim = c(min(accdata3$Approx_depth_negative_mode)-0.5, max(accdata3$Approx_depth_negative_mode)+0.5)) +
    scale_x_continuous(expand = c(0, 0),minor_breaks = seq(0,10000,2)) +
    ylab("Depth ("~m~")") +
    xlab("Time (seconds)") +
    labs(colour = "Behaviour") +
    theme(legend.position = "none") +
    scale_colour_manual(values=c("Flapping"="cyan", "Dive"="red", 
                                 "Water"="darkblue" ))
  
  
  
  
  plots<-list(p1, p2, p3, p4)
  grob<-marrangeGrob (plots, nrow=4, ncol=1, list(top=NULL))
  
  ggsave(paste(plotfolder,"plot",i,".png",sep=""), grob)
}

#Save the 
write.csv(accdata3,paste("D:/Users/me/Documents/Sync/Documents2TB/Analysis/AccCamClassification/R_randomForest_May19/RFLabels/",
                         birdID,"_full_acc_rotated_labels_",trainingdat,
                         "_final_cmp.csv",sep=""), row.names = F)


getwd()

