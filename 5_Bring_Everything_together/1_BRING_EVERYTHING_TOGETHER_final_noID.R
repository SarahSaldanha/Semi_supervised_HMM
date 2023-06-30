
library(dplyr)          ## data manipulation
library(tidyverse)          ## data manipulation
library(lubridate)      ## data manipulation date & time
library(ggplot2)        ## visualization
library(data.table)

op <- options(digits.secs=6)

memory.limit(size=99999999)




setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

####Here we bring together all info of the GPS, AXY, WETDRY, TDR and merge together to make one big dataset with known states. 

#### We need to bring in the data from the axy Random Forest models and match it with the gps data all in one go... or else the datasets are too big 



Original_files <- list.files(path = "./Predicted_trips_axy/", pattern = '*_cpm_perseg_predicted.rds')
print(Original_files)


####READ IN ALL GPS DATA TO START 



birdTrack <- readRDS("./Data_no_ID_GPS/2_trackInterpolations/interpolatedTRACKS_linear300sec_noID.rds")
birdTrack$dateTime_gps<-birdTrack$dateTime_mod




#####This first function reads-in previous GPS files and summarize the amount of behaviours predicted by the random forest.
op <- options(digits.secs=6)
summarize_axy <- function(file_name){
  #file_name <-Original_files[4]
  
  #Read in the full dataset (1 trip) 
  full_acc<- readRDS(paste0("./Predicted_trips_axy/", file_name))
  full_acc$start_seg_round<-floor_date(full_acc$start_seg,unit = "seconds")
  full_acc$seg_length<-full_acc$end_seg-full_acc$start_seg
  
  trial<-full_acc %>%
    mutate( tot_trip_length = difftime(max(end_seg), min (start_seg), units = "secs")) %>%
    group_by(pred_behaviour_clear_0.65,ring_dep_trip, tot_trip_length) %>%
    dplyr::summarise(number_of_segments = n(),
                     tot_seg_length = sum(seg_length, na.rm = T))
  
  
  
  saveRDS(trial,paste0("./Summary_RF_CLASSIFICATION_per_trip/", unique(trial$ring_dep_trip),  "_gps_axy.rds"))
  write.csv(trial,paste0("./Summary_RF_CLASSIFICATION_per_trip/", unique(trial$ring_dep_trip),  "_gps_axy.csv"),row.names=F)
  
}


lapply(Original_files, summarize_axy)



#####A second function  to read-in previous files, remove the none-behaviours, extract the start of each segment 
op <- options(digits.secs=6)
summarize_axy_to_gps <- function(file_name){
  #file_name <-Original_files[4]
  #file_name <- "ID_135_180_444_metrics_with_depth_cpm_perseg_predicted.rds"
  #Read in the full dataset (1 trip) 

  full_acc<- readRDS(paste0("./Predicted_trips_axy/", file_name))
  ###there are some duplicated times. remove these 
  full_acc<-full_acc%>% distinct(start_seg, end_seg, ring_dep_trip,  .keep_all = TRUE)
  
  
  full_acc$seg_length<-full_acc$end_seg-full_acc$start_seg
  
  bird<-subset(birdTrack, ring_dep_trip == unique(full_acc$ring_dep_trip))
  
  table(full_acc$pred_behaviour_clear_0.65)
  
  
  
  ###merged with gps use data.table to merge to closest GPS position before the axy timestamp. 
  setDT(full_acc)
  setDT(bird)
  
  #add joining datetime variable 
  full_acc[, join_time := start_seg ]
  bird[, join_time := dateTime_mod ]
  
  setkey(full_acc, ring_dep_trip , join_time)
  setkey(bird, ring_dep_trip , join_time)
  
  
  #update df1 by reference with a rolling join
  combined<-bird[full_acc, roll = T]
  
  ###add any dateTimes from the gps data that are lost 
  lost<-subset(bird, !(dateTime_gps %in%  combined$dateTime_gps))
  
  combined<-rbind(lost, combined, fill = T)
  
  
  
  
  #arrange by difftime and start seg
  
  combined<- combined %>% arrange(dateTime_gps, start_seg)
  
  ####fill downwards
  
  combined<-combined  %>% group_by(ring_dep_trip)%>% fill(names(combined)[9:67], .direction = "down") %>% ungroup()
  


  ####now summarize to each gps position 
  ###get proportion NA, on water, diving, flapping
  #####extract data I need for the HMMs

  ###### Turn TDR INTO DIVES ####this is only a rough dive estimate
  
  combined<-combined[with(combined, order(ring_dep_trip, start_seg)),]
  combined$dive_tdr_01<-ifelse(combined$min_depth < -0.2,  1, 0)
  
  
  #####REMOVE AXY DATA OVER 5 MINUTES AFTER GPS POSITIONS
  
  combined<-subset(combined, end_seg < max(bird$dateTime_mod)+300)
  
  
  #####now collapse this to every interpolated GPS position
  
  combined$Labels2_w_01<-ifelse(combined$pred_behaviour_clear_0.65 =="Water", 1, 0)
  combined$Labels2_F_01<-ifelse(combined$pred_behaviour_clear_0.65 =="Flapping", 1, 0)
  combined$Labels2_D_01<-ifelse(combined$pred_behaviour_clear_0.65 =="Dive", 1, 0)
  combined$Labels2_U_01<-ifelse(is.na(combined$pred_behaviour_clear_0.65), 1, 0)
  
  #split cpm sections that cross the gps times
  combined<-combined%>% arrange(dateTime_gps, start_seg)
  combined$lead_dateTime_gps<-lead(combined$dateTime_gps)
  combined$lag_dateTime_gps<-lag(combined$dateTime_gps)
  
  
  ####cut segments to timestamps of gps 
  
  combined$start_seg<-ymd_hms(ifelse(combined$start_seg >= combined$dateTime_gps, as.character(combined$start_seg), as.character(combined$dateTime_gps)))
  
  combined$end_seg<-ymd_hms(ifelse(combined$end_seg < lead(combined$start_seg), as.character(combined$end_seg), as.character(lead(combined$start_seg))))
  
  combined$cross_over_segment<-ifelse(combined$end_seg > lead(combined$dateTime_gps) & !(lead(combined$dateTime_gps) ==combined$dateTime_gps), 1, 0)
  combined$lead_lat<-lead(combined$lat)
  combined$lead_lon<-lead(combined$lon)
  
  cross<-subset(combined, cross_over_segment == 1)
  cross<-subset(cross, as.numeric(lead_dateTime_gps- dateTime_gps) <301)
  cross$seg_length<- cross$end_seg-(cross$lead_dateTime_gps)
  cross$end_seg<-cross$lead_dateTime_gps+cross$seg_length
  cross$start_seg <- cross$lead_dateTime_gps
  cross$dateTime_gps<-cross$lead_dateTime_gps
  cross$lat<-cross$lead_lat
  cross$lon<-cross$lead_lon

  cross<-left_join(cross, bird, ring_dep_trip  = ring_dep_trip , dateTime_gps = dateTime_gps, lon = lon, lat = lat, daTime_mod = dateTime_mod, tripID2 = tripID2, contBlock = contBlock)
  
  #now shorten end times of combined if there is the start of a cross over
  
  combined$end_seg<-fifelse(combined$cross_over_segment == 1, combined$lead_dateTime_gps, combined$end_seg)

  
  combined<-rbind(cross, combined)
  combined<-combined%>% arrange (dateTime_gps, start_seg)
  
  combined<-combined%>% distinct(start_seg, lon, lat, dateTime_mod, ring_dep_trip, contBlock,  .keep_all = TRUE)
  
  
#   
# combined$end_seg<-ymd_hms(ifelse(combined$end_seg > lead(combined$start_seg), as.character(lead(combined$start_seg)), as.character(combined$end_seg)))
# combined<-combined%>% arrange (dateTime_gps, start_seg)

  
  combined$seg_length<-combined$end_seg-combined$start_seg
  
  
  combined<-subset(combined, start_seg - dateTime_gps <= 300.00 & end_seg-dateTime_gps <=300 )
  hist(as.numeric(combined$seg_length))

  
  
  combined$end_seg<-ymd_hms(ifelse(combined$end_seg < combined$start_seg, as.character(lead(combined$start_seg)), as.character(combined$end_seg)))
  combined$seg_length<-combined$end_seg-combined$start_seg
  
  
####the proportion relative to each segment 
  
  HMM_DATA<-combined %>%
    group_by(ring_dep_trip,  contBlock,lon, lat, dateTime_gps)%>%
    dplyr::summarize(prop_wet = NA, 
              dur_wet = NA, 
              dur_dry = NA,
              sum_seg_length = sum(seg_length, na.rm = T),
              time_water_acc = sum(Labels2_w_01*seg_length, na.rm = T),
              time_Flapping_acc = sum(Labels2_F_01*seg_length, na.rm = T),
              time_dives_acc = sum(Labels2_D_01*seg_length, na.rm = T),
              number_dives_acc = sum(Labels2_D_01, na.rm = T), 
              prop_water = as.numeric(sum(Labels2_w_01*seg_length, na.rm = T))/ as.numeric(sum_seg_length), 
              prop_flying= as.numeric(sum(Labels2_F_01*seg_length, na.rm = T))/as.numeric(sum_seg_length), 
              number_dives_tdr = sum(dive_tdr_01, na.rm = T)) %>% 
    
    ungroup()
  
  HMM_DATA$dive_01_acc<- ifelse(HMM_DATA$number_dives_acc == 0, 0, 1)
  HMM_DATA$dive_tdr_01<-ifelse(HMM_DATA$number_dives_tdr > 0, 1, 0)
  HMM_DATA<-HMM_DATA %>% arrange(dateTime_gps)
  
  
  
  HMM_DATA$time_diff<-as.numeric(HMM_DATA$dateTime_gps- lag(HMM_DATA$dateTime_gps))
  HMM_DATA$time_diff<-ifelse(HMM_DATA$contBlock == lag(HMM_DATA$contBlock), HMM_DATA$time_diff, NA)
  
  
  table(HMM_DATA$time_diff)
  
  HMM_DATA$contBlock<-ifelse(HMM_DATA$time_diff >300,  lead(HMM_DATA$contBlock), HMM_DATA$contBlock)
  #re-summarise
  
  HMM_DATA<-HMM_DATA %>%
    group_by(ring_dep_trip,  contBlock,lon, lat, dateTime_gps)%>%
    dplyr::summarize(prop_wet = NA, 
                     dur_wet = NA, 
                     dur_dry = NA,
                     sum_seg_length = sum(sum_seg_length, na.rm = T),
                     time_water_acc = sum(as.numeric(time_water_acc), na.rm = T),
                     time_Flapping_acc = sum(as.numeric(time_Flapping_acc), na.rm = T),
                     time_dives_acc = sum(as.numeric(time_dives_acc), na.rm = T),
                     number_dives_acc = sum(number_dives_acc, na.rm = T), 
                     prop_water = as.numeric(time_water_acc)/ as.numeric(sum_seg_length), 
                     prop_flying= as.numeric(time_Flapping_acc)/as.numeric(sum_seg_length), 
                     number_dives_tdr = sum(number_dives_tdr, na.rm = T)) %>% 
    
    ungroup()
  
  HMM_DATA$dive_01_acc<- ifelse(HMM_DATA$number_dives_acc == 0, 0, 1)
  HMM_DATA$dive_tdr_01<-ifelse(HMM_DATA$number_dives_tdr > 0, 1, 0)
  HMM_DATA<-HMM_DATA %>% arrange(dateTime_gps)
  
  
  
  
  
     
  ####save this out! 
  
  
  
  saveRDS(HMM_DATA,paste0("./combined_datasets/gps_axy/", unique(HMM_DATA$ring_dep_trip),  "_gps_axy.rds"))
  write.csv(HMM_DATA,paste0("./combined_datasets/gps_axy/", unique(HMM_DATA$ring_dep_trip),  "_gps_axy.csv"),row.names=F)

  }
  

lapply(Original_files, summarize_axy_to_gps)



####bring in the data with wet dry only 



wd<-fread("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/geos/complete_geos_noID.csv")


head(wd)

length(unique(wd$ring_dep_trip))


####floor time to 1 min so that I can merge the data since GPS always rounded to minutes

wd$time_mod_geo_round<-floor_date(wd$time_mod_geo,unit = "minutes")






wd<-select(wd, -V1, -tripID2)


bird<-subset(birdTrack, ring_dep_trip %in% unique(wd$ring_dep_trip))
bird$bird_time_ID<-paste0(bird$ring_dep_trip, "_", bird$dateTime_gps)
###merged with gps use data.table to merge to closest GPS position before the axy timestamp. 
setDT(wd)
setDT(bird)

#add joining datetime variable 
wd[, join_time := time_mod_geo ]
bird[, join_time := dateTime_mod ]

setkey(wd, ring_dep_trip , join_time)
setkey(bird, ring_dep_trip , join_time)


#update df1 by reference with a rolling join
combined<-bird[wd, roll = T]

###add any dateTimes from the gps data that are lost 


lost<-subset(bird, !(bird_time_ID %in%  combined$bird_time_ID))

combined<-rbind(lost, combined, fill = T)












#combined<-left_join(wd, birdTrack,   by = c("time_mod_geo_round" = "dateTime_mod", "ring_dep_trip" = "ring_dep_trip"))


#combined<-combined[with(combined, order(ring_dep_trip, dateTime_gps, time_mod_geo_round, time_mod_geo)),]

###fill_downards

# combined <- as.data.frame(combined) %>% fill(lon) %>% fill(lat) %>% fill(contBlock)%>% fill(ring_dep_trip)%>% fill(dateTime_gps)

combined<-subset(combined, !(is.na(dateTime_gps) | is.na(lat)))

####now summarize to each gps position 

combined<-combined[with(combined, order(ring_dep_trip, dateTime_gps)),]



combined$act01<-ifelse(combined$act == "dry", 1, 0)








####Extract `proportion wet from wd data

HMM_DATA2<-combined %>%
  group_by(ring_dep_trip, contBlock, lon, lat, dateTime_gps)%>%
  dplyr::summarize(dur_dry = sum(act01*duract_sec), 
                   dur_wet = sum(duract_sec)-dur_dry,
                   prop_wet = dur_wet/sum(duract_sec), 
                   sum_seg_length = NA,
                   time_water_acc = NA,
                   time_Flapping_acc = NA,
                   time_dives_acc = NA,
                   number_dives_acc = NA, 
                   prop_water = NA,
                   prop_flying= NA, 
                   number_dives_tdr = NA) %>% 
  
  ungroup()

HMM_DATA2$dive_01_acc<- NA
HMM_DATA2$dive_tdr_01<-NA


HMM_DATA2<-HMM_DATA2%>% arrange(ring_dep_trip, dateTime_gps)


###add any dateTimes from the gps data that are lost 

HMM_DATA2$time_diff<-as.numeric(HMM_DATA2$dateTime_gps- lag(HMM_DATA2$dateTime_gps))
HMM_DATA2$time_diff<-ifelse(HMM_DATA2$contBlock == lag(HMM_DATA2$contBlock), HMM_DATA2$time_diff, NA)

table(HMM_DATA2$time_diff)


####So technically these datasets should all have the same columns now! So read in all the .rds files of the HMM Data1 and merge together 

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/gps_axy")

AXY_files <- rbindlist(fill=TRUE,lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/gps_axy/", pattern = '_gps_axy.rds'), readRDS))



# ######some axy data information is split on two rows. Need to merge these
# 
# AXY_files<-AXY_files%>%
#   group_by( ring_dep_trip, dateTime_gps, contBlock, lon, lat, dur_wet , dur_dry, prop_wet) %>%
#   summarize (
#     sum_seg_length = sum(sum_seg_length), 
#              time_water_acc = sum(time_water_acc, na.rm = T),
#              time_Flapping_acc = sum(time_Flapping_acc, na.rm = T), 
#              time_dives_acc = sum(time_dives_acc, na.rm = T), 
#              number_dives_acc = sum(number_dives_acc, na.rm = T), 
#              prop_water = as.numeric(time_water_acc)/as.numeric(sum_seg_length), 
#              prop_flying = as.numeric(time_Flapping_acc)/as.numeric(sum_seg_length), 
#              number_dives_tdr = sum(number_dives_tdr, na.rm = T), 
#              dive_01_acc = ifelse(number_dives_acc > 0, 1, 0), 
#              dive_tdr_01 = ifelse(number_dives_tdr > 0, 1, 0)) %>%
#   
#   ungroup()





duplicated<-subset(AXY_files, ring_dep_trip %in% unique(HMM_DATA2$ring_dep_trip)) 
length(unique(duplicated$ring_dep_trip))
non.duplicated<-subset(AXY_files, !ring_dep_trip %in% unique(HMM_DATA2$ring_dep_trip))
length(unique(non.duplicated$ring_dep_trip))

non.duplicated$Tag_type<-"axy"

table(duplicated$ring_dep_trip) ####WORKING NOW!!!

####so now get dataset with duplicated wd data and merge it with duplicated 

TO_MERGE<-subset(HMM_DATA2, ring_dep_trip %in% unique(duplicated$ring_dep_trip))

TO_MERGE<-select(TO_MERGE, ring_dep_trip, dateTime_gps, dur_wet, dur_dry, prop_wet)
duplicated<- select(duplicated, -dur_wet, -dur_dry, -prop_wet)


duplicated<-merge(TO_MERGE, duplicated, by = c("ring_dep_trip", "dateTime_gps"), all = T)

duplicated$Tag_type<-"wd_axy"



axy_wd<-rbind(duplicated, non.duplicated)

plot(axy_wd$prop_wet, axy_wd$prop_water)




length(unique(axy_wd$ring_dep_trip))###61,  missing the wd_data alone 
####

wd_alone<-subset(HMM_DATA2, !(ring_dep_trip %in% AXY_files$ring_dep_trip))
wd_alone$Tag_type<-"wd"
wd_alone<-select(wd_alone, -time_diff)







axy_wd<-rbind(axy_wd, wd_alone)
length(unique(axy_wd$ring_dep_trip)) ####152

###now bring in gps data without wd or axy data



####t

nowd_axy<-subset(birdTrack, !ring_dep_trip %in% unique(axy_wd$ring_dep_trip))

nowd_axy$Tag_type<-"gps_only"

nowd_axy<-select(nowd_axy,ring_dep_trip, dateTime_gps, contBlock, lon, lat, Tag_type)



all<-rbind(nowd_axy, axy_wd, fill = T)

length(unique(all$ring_dep_trip))


all%>%group_by(Tag_type,ring_dep_trip) %>% tally() %>% group_by(Tag_type) %>%summarize(n= n())




#####why missing lat lon? 

missing_lat_long<-subset(all, is.na(lat))
length(unique(missing_lat_long$ring_dep_trip))

all<-subset(all, !is.na(lat))

####################Now set things up for the model with the known states!

names(all)

trial<-subset(all, Tag_type =="wd_axy")



plot(trial$prop_wet, trial$prop_flying)

length(unique(subset(all, !is.na(prop_wet))$ring_dep_trip))####make sure I have all the geos
length(unique(subset(all, !is.na(prop_water))$ring_dep_trip))####make sure I have all the axys









all$prop_known_axy<- as.numeric(all$time_water_acc +all$time_Flapping_acc +all$time_dives_acc)/300



# to_check<-subset(all, prop_wet ==1 & prop_water < 0.1) ####This occurs only once when wet dry data only covers 6s of the gps data. --> make wet dry nas
# all[482947,]$prop_wet<-NA

to_check2<-subset(all, prop_water ==1 & prop_wet < 0.1) ####13 POSTITIONS DRY BUT ACC CATEGORIZED AS ON WATER. GLIDDING? 





ggplot(all, aes(x=prop_wet, y=prop_water,  alpha=prop_known_axy)) +
  geom_point()+theme_bw()



all<-all[with(all, order(ring_dep_trip, dateTime_gps)),]
all$time_diff<-all$dateTime_gps-lag(all$dateTime_gps)
all$time_diff<-ifelse(all$contBlock == lag(all$contBlock), as.numeric(all$time_diff), NA)  ####some ISSUES WITH THE INTERPOLATIONS HERE. 
table(all$time_diff)
###some diff_times are quie a bit off 

error_diff_time<-subset(all, !is.na(time_diff) & !time_diff == 300)
unique(error_diff_time$ring_dep_trip)


# 
# #so there are 2 time intervals of 0 of NA which we need to correct to see why there are zeros in the dataset. 
# #0 these seem to come from repeated rows FROM WET DRY SO REMOVE THESE
# 
# all$sum_time_wd <-as.numeric(all$dur_wet)+ as.numeric(all$dur_dry)
# 
# all2<-all %>% 
#   group_by(ring_dep_trip, dateTime_gps) %>% 
#   arrange(desc(sum_time_wd)) %>% 
#   slice(1) %>% 
#   ungroup()
# 
# ###RECALCULATE TIME_DIFF
# all$time_diff<-all$dateTime_gps-lag(all$dateTime_gps)
# all$time_diff<-ifelse(all$contBlock == lag(all$contBlock), as.numeric(all$time_diff), NA) 
# 
# table(all$time_diff)

####so now there are just two positions with 3 min time diff 
unique(subset(all, !(time_diff == 300))$ring_dep_trip)

ID_136_181_447<-subset(all, ring_dep_trip =="ID_136_181_447")
ID_345_436_1005<-subset(all, ring_dep_trip == "ID_345_436_1005")
subset(all, !(time_diff == 300))

###switch contBlocks for these ttwo. 

all$contBlock<-ifelse(all$time_diff == 180, lead(all$contBlock), all$contBlock )


###re-check 

all<-all %>% arrange(ring_dep_trip, dateTime_gps )

all$time_diff<-all$dateTime_gps-lag(all$dateTime_gps)
all$time_diff<-ifelse(all$contBlock == lag(all$contBlock), as.numeric(all$time_diff), NA) 
table(all$time_diff)



all<-all[with(all, order(ring_dep_trip, dateTime_gps)),]

table(all$time_diff)


unique(subset(all, !is.na(prop_wet))$ring_dep_trip)####make sure I have all the geos 
unique(subset(all, !is.na(prop_water))$ring_dep_trip)####make sure I have all the axys


###whyyyyy NA CONTBLOCKS 

subset(all, is.na(contBlock))

birdTrack$contBlock2<-birdTrack$contBlock
trial<-select(birdTrack, ring_dep_trip, dateTime_gps = dateTime_mod, lat, lon, contBlock2)

all<-merge(trial, all, by = c("ring_dep_trip", "dateTime_gps", "lat", "lon"), all = T)
table(all$contBlock == all$contBlock2)

all$contBlock<-ifelse(is.na(all$contBlock), all$contBlock2, all$contBlock)
all<-select(all, -contBlock2)

all2<-all %>% group_by(ring_dep_trip,dateTime_gps, lat, lon, contBlock) %>% distinct()


saveRDS(all2,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined.rds" )


ggplot(subset(all, prop_known_axy >0.95), aes(x=prop_wet, y=prop_water,  alpha=prop_known_axy)) +
  geom_point()+theme_bw()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     fit the data to a hmm
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

