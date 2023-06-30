
library(dplyr)          ## data manipulation
library(tidyr)          ## data manipulation
library(lubridate)      ## data manipulation date & time
library(ggplot2)        ## visualization
library(viridis)        ## colour palette
library(RColorBrewer)   ## colour palette
library(sf)             ## data manipulation spatial data
library(kableExtra)
library(janitor)
library(raster)
library(RStoolbox)
library(lubridate)
library(stringr)
library(reshape2)
library(chron)
library(assertthat)
library(maptools)
library (stringr)
library (rgdal)
library(lubridate)
library(zoo)
library(plotly)
library(plyr)
library(geosphere)
library(data.table)

op <- options(digits.secs=6)

memory.limit(size=99999999)

all<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined_TDR.rds"  )
head(all)
length(unique(all$ring_dep_trip))


####plotting dives for acc
hist(subset(all, number_dives_acc>0)$number_dives_acc, breaks = seq(from = 0, to =2000, by = 1), xlim= c(0,100))


ggplot(subset(all, number_dives_acc>0), aes(x = number_dives_acc))+
  geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=1) + 
  geom_vline(xintercept = 40,  color = "red", size=1)+
  scale_x_continuous(name= "number of dives from accelerometery", breaks = seq(from = 0, to = 2000, by = 250  ))+
  scale_y_continuous(name= "number of positions", breaks = seq(from = 0, to = 600, by = 100 ))+
  theme_bw()


ggplot(subset(all, number_dives_acc>0), aes(x = number_dives_acc))+
  geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=1) + 
  geom_vline(xintercept = 40,  color = "red", size=1)+
  scale_x_continuous(name= "number of dives from accelerometery", limits = c(0, 150), breaks = seq(from = 0, to = 150, by = 10))+
  scale_y_continuous(name= "number of positions", breaks = seq(from = 0, to = 600, by = 100 ))+
  theme_bw()



ggplot(subset(all, TDR_dives>0), aes(x = TDR_dives))+
  geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=1) + 
  geom_vline(xintercept = 40,  color = "red", size=1)+
  scale_x_continuous(name= "number of dives from Time Depth Recorders", breaks = seq(from = 0, to = 100, by = 10))+
  scale_y_continuous(name= "number of positions")+
  theme_bw()



ggplot(subset(all, number_dives_tdr>0), aes(x = number_dives_tdr))+
  geom_histogram(fill="black", colour="black", alpha = 0.25, binwidth=1) + 
  geom_vline(xintercept = 40,  color = "red", size=1)+
  scale_x_continuous(name= "number of dives from Time Depth Recorders", limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 10))+
  scale_y_continuous(name= "number of positions", breaks = seq(from = 0, to = 100, by = 10 ))+
  theme_bw()







hist(subset(all, number_dives_tdr>0)$number_dives_tdr, breaks = seq(from = 0, to =2000, by = 1), xlim= c(0,100))




hist(subset(all, number_dives_acc>0)$number_dives_tdr, breaks = 200)

quantile(subset(all, number_dives_tdr>0)$number_dives_tdr, 0.95, na.rm = T) ###

hist(subset(all, number_dives_acc>0 & number_dives_acc< 100)$number_dives_acc, breaks= 100)
hist(subset(all, number_dives_tdr>0& number_dives_tdr< 100)$number_dives_tdr, breaks= 100)


plot(all$dateTime_gps, all$number_dives_tdr)


##### mean +- sd duration of dives ####we have this in the tdr code

###to set up the known states, I had some issues with iflese statements and so to simplify things I am going to seperate the datasets first into wd, axy, wd + axy and finally, gps only. 


######???now order ####If I want data from Madeleine I need to backtrack to the older version of all final.... without the TDR- 
# all<-all[
#   with(all, order(ring_dep_trip, dateTime_gps)),]
# allcv<-subset(all, !island == "Madeleine")
# 
# allwd_axy<-unique(subset(all, !is.na(prop_wet) & !is.na(prop_known_axy))$ring_dep_trip)
# allwd<-setdiff(unique(subset(allcv, !is.na(prop_wet))$ring_dep_trip), allwd_axy)
# allaxy<-setdiff(unique(subset(allcv, !is.na(prop_known_axy))$ring_dep_trip), allwd_axy)
# all_gps<-unique(subset(allcv, ! ring_dep_trip %in% c(allwd_axy, allwd, allaxy))$ring_dep_trip)
# 
# allcv$Tag_type<- ifelse(allcv$ring_dep_trip %in% allwd_axy, "wd_axy", ifelse(allcv$ring_dep_trip %in% allwd, "wd", ifelse(allcv$ring_dep_trip %in% allaxy, "axy", "gps_only")))

allwd_axy<-subset(all, Tag_type == "wd_axy")
allwd<-subset(all, Tag_type == "wd")
allaxy<-subset(all, Tag_type == "axy")
allgps_only<-subset(all, Tag_type == "gps_only")

####now identify true states for each before putting it back together
allgps_only$true_state_act_inactive<-NA
allgps_only$true_state<-NA

##wd
allwd$true_state_act_inactive<-ifelse(allwd$prop_wet ==1, 1, ifelse(allwd$prop_wet ==0, 3, NA))
allwd$true_state<-ifelse(allwd$prop_wet ==1, 1, NA)

table(allwd$true_state_act_inactive)

###axy
allaxy$true_state_act_inactive<-ifelse(allaxy$number_dives_acc %in% c(1:40) | allaxy$TDR_dives %in% c(1:40), 2, ifelse (allaxy$prop_known_axy < 0.95, NA, ifelse(allaxy$prop_water > 0.5, 1, ifelse(allaxy$prop_flying==1, 3, NA))))
                                       
allaxy$true_state<-ifelse(allaxy$number_dives_acc %in% c(1:40) | allaxy$TDR_dives %in% c(1:40), 2, ifelse (allaxy$prop_known_axy < 0.95, NA, ifelse(allaxy$prop_water > 0.5, 1, NA)))
                
table(allaxy$true_state_act_inactive)

#axy_wd
allwd_axy$true_state_act_inactive<-ifelse(allwd_axy$number_dives_acc %in% c(1:40) | allwd_axy$TDR_dives %in% c(1:40), 2,ifelse(allwd_axy$prop_wet ==1, 1, ifelse(allwd_axy$prop_wet == 0, 3, ifelse(allwd_axy$prop_known_axy < 0.95, NA, ifelse(allwd_axy$prop_water > 0.5, 1, ifelse(allwd_axy$prop_flying == 1, 3, NA))))))
                                                 
allwd_axy$true_state<-ifelse(allwd_axy$number_dives_acc %in% c(1:40) | allwd_axy$number_dives_tdr %in% c(1:40), 2,ifelse(allwd_axy$prop_wet ==1, 1, ifelse(allwd_axy$prop_known_axy < 0.95, NA, ifelse(allwd_axy$prop_water > 0.5, 1, NA))))

table(allwd_axy$true_state_act_inactive)

allcv<-rbind(allwd_axy, allwd, allaxy, allgps_only)

#axy_ks<-subset(allcv, allcv$prop_known_axy >0.95 & allcv$prop_water > 0.5 & is.na(allcv$prop_wet) & allcv$true_state_act_inactive ==1)

table(allcv$true_state_act_inactive)




####make a loop to plot how the number of true states change based on the thresholds.             
# 
# 
# ii<-0.5         
# 
# dd<-table(ifelse(all$number_dives_acc %in% c(1:40) | all$number_dives_tdr %in% c(1:40), 2, ifelse(all$prop_wet ==1, 1, ifelse(all$prop_wet == 0, 2.5, ifelse(all$prop_water > ii  & all$prop_known_axy > 0.95, 1, ifelse(all$prop_flying >ii  & all$prop_known_axy > 0.95 , 2.5, NA))))))
#   
#   
#   
#   
# for (ii in seq(0.51,1, by = 0.01)){
#   #ii <- 0.5
#  ddd<-table(ifelse(all$number_dives_acc %in% c(1:40) | all$number_dives_tdr %in% c(1:40), 2, ifelse(all$prop_wet ==1, 1, ifelse(all$prop_wet == 0, 2.5, ifelse(all$prop_water > ii  & all$prop_known_axy > 0.95, 1, ifelse(all$prop_flying >ii & all$prop_known_axy > 0.95 , 2.5, NA))))))
#  dd<-rbind(dd, ddd)
# }
#   
# dd<-cbind(as.data.frame(dd), seq(.5, 1, by=0.01))
# 
# names(dd)<-c("rest", "forage", "travel", "threshold")
# 
# plot(dd$threshold, dd$rest)
# plot(dd$threshold, dd$travel)
# 
# 
# 
# 
# ggplot(dd, aes(x = threshold, y = rest))+
#          geom_point()+
#          geom_line()+
#   scale_y_continuous(name= "resting known states")+
#          theme_bw()
# 
# 
# ggplot(dd, aes(x = threshold, y = travel))+
#   geom_point()+
#   geom_line()+
#   scale_y_continuous(name= "travelling known states")+
#   theme_bw()
#          

##############





table(allcv$true_state_act_inactive, useNA ="always")   
table(allcv$true_state, useNA ="always")                                



allcv$row_ID<-c(1:nrow(allcv))



###set it to NA when the propwet and propWater disagree
####figure out how to swith the true states of these issues to NA
###going to do some cheap codding here because ifelse is giving me problems 



issue1<-subset(allcv, prop_water >0.5 & prop_known_axy>0.94  & prop_wet == 0) ####15
issue2<-subset(allcv, prop_water <0.5 & prop_known_axy>0.94 & prop_wet == 1)###0

issue3<-subset(allcv, prop_flying ==1 & prop_known_axy>0.94  & prop_wet == 1) ##0
issue4<-subset(allcv, prop_flying <0.5 & prop_known_axy>0.94 & prop_wet == 0)##15




issue5<-subset(allcv, prop_water == 0 & prop_wet == 1) ###0
issues<-rbind(issue1, issue2, issue3, issue4,issue5 )


allcv$true_state<-ifelse(allcv$ring_dep_trip %in% issues$ring_dep_trip & allcv$dateTime_gps %in% issues$dateTime_gps, NA, allcv$true_state)
allcv$true_state_act_inactive<-ifelse(allcv$ring_dep_trip %in% issues$ring_dep_trip & allcv$dateTime_gps %in% issues$dateTime_gps, NA, allcv$true_state_act_inactive)


table(allcv$true_state_act_inactive)


#subset(allcv,contBlock == 4081 )#####REMOVE THIS BECAUSE IT IS TOO SHORT

issue<-subset(allcv, !true_state == true_state_act_inactive )


####set these to NA... 

allcv$true_state<-ifelse(allcv$row_ID %in% issue$row_ID, NA, allcv$true_state)
table(allcv$true_state)

allcv$true_state_act_inactive<-ifelse(allcv$row_ID %in% issue$row_ID, NA, allcv$true_state_act_inactive)
table(allcv$true_state_act_inactive)

 ####SAVE THIS OUT SO THAT IT IS READY FOR THE HMM                        
# 
# allcv%>%
#   dplyr::group_by(Tag_type) %>%
#   dplyr::summarize(n=n_distinct(ring))
# 
# 
# allcv$deployment<-str_sub(allcv$ring_dep_trip, 1, -3)
#   
# allcv%>%
#   dplyr::group_by(Tag_type) %>%
#   dplyr::summarize(n=n_distinct(str_sub(ring_dep_trip, 1, -3)))
# 
# allcv%>%
#   dplyr::group_by(Tag_type) %>%
#   dplyr::summarize(n=n_distinct(ring_dep_trip))
# 
# 
# allcv%>%
#   dplyr::group_by(Tag_type, true_state_act_inactive) %>%
#   dplyr::tally()

weird<-subset(allcv, prop_wet == 1 & true_state == 2)

allcv$true_state<-ifelse(allcv$row_ID %in% weird$row_ID, NA, allcv$true_state)

allcv$true_state_act_inactive<-ifelse(allcv$row_ID %in% weird$row_ID, NA, allcv$true_state_act_inactive)
table(allcv$true_state_act_inactive)


Acc_not_tdr<-subset(allcv, true_state == 2 & TDR_dives == 0)
Acc_not_tdr_0<-subset(Acc_not_tdr, prop_wet == 0)
####1624.... so mainly they connect
hist(Acc_not_tdr$prop_wet)

allcv$true_state<-ifelse(allcv$row_ID %in% Acc_not_tdr_0$row_ID, NA, allcv$true_state)

allcv$true_state_act_inactive<-ifelse(allcv$row_ID %in% Acc_not_tdr_0$row_ID, NA, allcv$true_state_act_inactive)
table(allcv$true_state_act_inactive)

####create boxplot of known states based on prop wet

allcv%>%
  dplyr::group_by(true_state_act_inactive) %>%
  dplyr::summarize(mean=mean(prop_wet, na.rm=T))





# 
# ###set all non-tdr dives to unknown for some trips since almost the entire trip was classified as foraging and clearly should be resting during some periods
# 
# 
# trips<-c("8200473_PHAAET_rec24052019_PRincon_S2_INCOMPLETE_1", "8200718_PHAAET_rec08032019_PRincon_1", "8200718_PHAAET_rec08032019_PRincon_2", "8200718_PHAAET_rec08032019_PRincon_3","8200718_PHAAET_rec08032019_PRincon_4", "8202087_PHAAET_rec08032021_ICima_ninho_30_23_S1_4"  )
# 
# allcv_rest<-subset(allcv, !(ring_dep_trip %in%  trips))
# a<-subset(allcv, ring_dep_trip %in%  trips)
# a$true_state_act_inactive<-ifelse(a$dive_tdr_01  == 0 & a$true_state_act_inactive == 2, NA, a$true_state_act_inactive )
# a$true_state<-ifelse(a$dive_tdr_01  == 0 & a$true_state == 2, NA, a$true_state )
# 
# allcv<-rbind(allcv_rest, a)

###check proportion of points marked as foraging per trip and visually inspect trips with tons of foraging
detach(package:plyr)

a<-allcv %>%
  dplyr::group_by(ring_dep_trip,true_state_act_inactive)%>%
  tally()%>%
  ungroup()

b<-a %>%
  dplyr::group_by(ring_dep_trip)%>%
  dplyr::summarize(N= sum(n))

a<-subset(a,true_state_act_inactive =="2" )


c<-merge(a, b, by="ring_dep_trip")
c$prop_foraging<-c$n/c$N
c<-subset(c,!is.na(prop_foraging))

hist(c$prop_foraging, breaks = 100)

c<-subset(c, prop_foraging >0.2)
trips<-unique(c$ring_dep_trip)


#after removing the prop_wet == 1 ones...

####lets make these NA'S as well... i think the axy foraging positions are not always right. 









saveRDS(allcv,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined_TRUE_STATES_SET.rds" )

#allcv<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined_TRUE_STATES_SET.rds" )


# 
# 
# 
# allwd<-subset(allcv, Tag_type == "wd" | Tag_type == "wd_axy")
# allaxy<-subset(allcv, Tag_type == "axy" | Tag_type == "wd_axy")
# allgps_only<-subset(allcv, Tag_type == "gps_only")
# 
# table(allaxy$TDRdive01, allaxy$true_state_act_inactive)
# table(allwd$prop_wet ==1,allwd$true_state_act_inactive)
# table((allaxy$prop_known_axy > 0.94 & allaxy$prop_water > 0.5),allaxy$true_state_act_inactive)
# 
# 
# table((allcv$prop_known_axy > 0.94 & allcv$prop_water > 0.5& allcv$prop_wet ==1 ),allcv$true_state_act_inactive)
# 
# 
# table((allaxy$prop_known_axy > 0.94 & allaxy$prop_flying==1 ),allaxy$true_state_act_inactive)
# 
# 
# table((allcv$prop_known_axy > 0.94 & allcv$prop_flying  == 1 & allcv$prop_wet ==0 ),allcv$true_state_act_inactive)
# 
# #calculations for rest
# table(allcv$prop_wet ==1 & allcv$prop_known_axy > 0.94 & allcv$prop_water > 0.5 & allcv$true_state_act_inactive ==1)
# table(allcv$prop_wet ==1 & allcv$true_state_act_inactive ==1)
# table(allcv$prop_known_axy > 0.94 & allcv$prop_water > 0.5 & allcv$true_state_act_inactive ==1)
# 
# 
# ##calculations for travel. 
# 
# table(allcv$prop_wet ==0 & allcv$prop_known_axy > 0.94 & allcv$prop_flying == 1 & allcv$true_state_act_inactive ==3)
# table(allcv$prop_wet ==0 & allcv$true_state_act_inactive ==3)
# table(allcv$prop_known_axy > 0.94 & allcv$prop_flying == 1 & allcv$true_state_act_inactive ==3)
# 
# #####calculation of concurrent behaviours 
# 
# 
# a<-subset(allcv, prop_known_axy > 0.94 & prop_water > 0.5 & is.na(true_state_act_inactive))
# a<-subset(allcv, prop_wet ==1 & is.na(true_state_act_inactive))
# a<-subset(allcv, prop_known_axy > 0.94 & prop_flying == 1 & is.na(true_state_act_inactive))
# a<-subset(allcv, prop_wet ==0 & is.na(true_state_act_inactive))
# 
# 
# 
# 
# 
# table(allcv$prop_known_axy > 0.94 & allcv$prop_flying == 1 & !allcv$true_state_act_inactive ==3)
# table(!allcv$prop_known_axy > 0.94 & !allcv$prop_water > 1 & allcv$true_state_act_inactive ==1)
# 
# 
# table(!allcv$prop_wet ==1 & allcv$true_state_act_inactive ==1)
# 
# 
# 
# 
# 
# ##wd
# allwd$true_state_act_inactive<-ifelse(allwd$prop_wet ==1, 1, ifelse(allwd$prop_wet ==0, 3, NA))
# allwd$true_state<-ifelse(allwd$prop_wet ==1, 1, NA)
# 
# table(allwd$true_state_act_inactive)
# 
# ###axy
# allaxy$true_state_act_inactive<-ifelse(allaxy$number_dives_acc %in% c(1:40) | allaxy$TDR_dives %in% c(1:40), 2, ifelse (allaxy$prop_known_axy < 0.95, NA, ifelse(allaxy$prop_water > 0.5, 1, ifelse(allaxy$prop_flying==1, 3, NA))))
# 
# allaxy$true_state<-ifelse(allaxy$number_dives_acc %in% c(1:40) | allaxy$TDR_dives %in% c(1:40), 2, ifelse (allaxy$prop_known_axy < 0.95, NA, ifelse(allaxy$prop_water > 0.5, 1, NA)))
# 
# table(allaxy$true_state_act_inactive)
# 
# #axy_wd
# allwd_axy$true_state_act_inactive<-ifelse(allwd_axy$number_dives_acc %in% c(1:40) | allwd_axy$TDR_dives %in% c(1:40), 2,ifelse(allwd_axy$prop_wet ==1, 1, ifelse(allwd_axy$prop_wet == 0, 3, ifelse(allwd_axy$prop_known_axy < 0.95, NA, ifelse(allwd_axy$prop_water > 0.5, 1, ifelse(allwd_axy$prop_flying == 1, 3, NA))))))
# 
# allwd_axy$true_state<-ifelse(allwd_axy$number_dives_acc %in% c(1:40) | allwd_axy$number_dives_tdr %in% c(1:40), 2,ifelse(allwd_axy$prop_wet ==1, 1, ifelse(allwd_axy$prop_known_axy < 0.95, NA, ifelse(allwd_axy$prop_water > 0.5, 1, NA))))
# 
# table(allwd_axy$true_state_act_inactive)
# 
# allcv<-rbind(allwd_axy, allwd, allaxy, allgps_only)
# 
# table(allcv$true_state_act_inactive)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# length(unique(tdr$ring_dep_trip))
# 
# original<-readRDS("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_paper/final_datal_wetdry_axy_combined.rds" )
# 
# 
# 
# lost<-subset(original, !(ring_dep_trip %in% unique(tdr$ring_dep_trip))) ####these are the ones from senegall! 
# 
# length(unique(original$ring_dep_trip))
# 
# 
# ####lets check some trips with strange classifications
# 
# unique(allcv$ring_dep_trip)
# unique(c$ring_dep_trip)
# 
# a<-subset(allcv, ring_dep_trip == "8202087_PHAAET_rec08032021_ICima_ninho_30_23_S1_4")
# a$true_state_act_inactive<-as.factor(a$true_state_act_inactive)
# 
# library(plotly)
# 
# p<-ggplot(data = a, aes(x = lon, y = lat)) + 
#   # add in birds
#   geom_path(data = a, aes(lon, lat), size = 0.5, alpha = 0.5) +  
#   geom_point(data = a, aes(lon, lat, color = true_state_act_inactive)) +
#   scale_colour_manual(values=c( "red","#FFCC33", "cyan", "grey45"))+
#   xlab("lon") +
#   ylab("lat") +
#   theme_bw()
#   
# 
# ggplotly(p)
# 
# 
