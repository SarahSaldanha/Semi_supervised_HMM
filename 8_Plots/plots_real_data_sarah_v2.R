library(gamm4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mgcv)
library(lubridate)
library(stringr)
library(reshape)
library(lme4)
library(mgcv)


realz<-readRDS("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper/HMMS/fitHMM_dataOut/2D_4STATE_AXY_WET_TDR_CONSTRAINTS/all_02102021.rds")

Sys.setenv(TZ = "GMT")
#memory.size()
#memory.limit(99999999)

#realz<-readRDS("D:/Dropbox/sarah_fenha/Saved_RData_Sarah/2Behaviour_real_data_only_CHLA3MONTHLY_CHLAmontlhy_CHLAdaily_SSTdaily_SSTmonthly_SST3MONTH_SST7DAY_and_FSLE1day_FSLE7DAY_depth.rds")


#saveRDS(realz, "D:/Dropbox/sarah_fenha/Saved_RData_Sarah/2Behaviour_real_data_only_CHLA3MONTHLY_CHLAmontlhy_CHLAdaily_SSTdaily_SSTmonthly_SST3MONTH_SST7DAY_and_FSLE1day_FSLE7DAY_depth.rds")

####simple boxplots of time foraging vs sex, lifecyclestatus

theme_base <- theme(axis.title = element_text(size=12), 
                   axis.text = element_text(size=10),
                   legend.title = element_text(size=12),
                   legend.text= element_text(size=10),
                   legend.key.size = unit(0.75, "cm"),
                   panel.grid = element_blank(), panel.border = element_blank(),
                   panel.background = element_rect(fill = "white", colour = "black"))


####for each model, make a column with the probability of the selected behaviour

realz$Full_4_stateCSA_highest<-ifelse(realz$Full_4_stateCSA_resting >= realz$Full_4_stateCSA_preening & realz$Full_4_stateCSA_resting >= realz$Full_4_stateCSA_foraging & realz$Full_4_stateCSA_resting >= realz$Full_4_stateCSA_traveling,realz$Full_4_stateCSA_resting, 
                                   ifelse(realz$Full_4_stateCSA_preening >= realz$Full_4_stateCSA_resting & realz$Full_4_stateCSA_preening >= realz$Full_4_stateCSA_foraging & realz$Full_4_stateCSA_preening >= realz$Full_4_stateCSA_traveling,realz$Full_4_stateCSA_preening, ifelse
(realz$Full_4_stateCSA_foraging >= realz$Full_4_stateCSA_resting & realz$Full_4_stateCSA_foraging >= realz$Full_4_stateCSA_preening & realz$Full_4_stateCSA_foraging >= realz$Full_4_stateCSA_traveling,realz$Full_4_stateCSA_foraging, realz$Full_4_stateCSA_traveling)))


hist(realz$Full_4_stateCSA_highest, breaks = 200)






#####lets make some plots to check out which model results make more sense


ggplot(realz, aes(x= as.factor(fitTrack_FULL_4state_constraint_step_and_angle),y= prop_wet, alpha = Full_4_stateCSA_highest)) +
  geom_boxplot()+
  theme_base +
  theme(legend.position = "none")+
  xlab("States") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time Wet")+
  geom_jitter(shape=16, position=position_jitter(0.2), size = 0.05)

ggplot(realz, aes(x= as.factor(fitTrack_FULL_4state_constraint_step_and_angle_travelling),y= prop_wet)) +
  geom_boxplot()+
  theme_base +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.5)


ggplot(realz, aes(x= as.factor(fitTrack_FULL_4state_constraint_step),y= prop_wet)) +
  geom_boxplot()+
  theme_base +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.5)

ggplot(realz, aes(x= as.factor(fitTrack_FULL_5state_constraint_step),y= prop_wet, alpha = )) +
  geom_boxplot()+
  theme_base +
  geom_jitter(shape=16, position=position_jitter(0.2), alpha = 0.5)







####Missing the sex and the nest stage. 

####bring these in. 


####need to add on the breeding stage of each

birdInfo <- read.csv("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Most_recent_GPS_info/birdInfo_tropicbirds_ALL.csv")

birdInfo<-dplyr::select(birdInfo, date_recovery, lifeCycleStatus, ring)

birdInfo$date_recovery<-dmy(birdInfo$date_recovery)

realz<-merge(birdInfo, realz, by= c("ring", "date_recovery"), all.y = T)

nrow(subset(realz, is.na(lifeCycleStatus)))



####looks like almost everything has a breeding phase here.... well missing gps so this might change


####bring in Sex


sex <- read.csv("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Most_recent_GPS_info/Matriu_Sexat_Molecular_GLOBAL.csv")

head(sex)

sex<-dplyr::select(sex, ring = Anilla, Sex)
sex$ring<-as.character(sex$ring)
head(sex)
realz<-merge(sex, realz, by= "ring", all.y = T)


table(realz$Sex)

#######################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

####prop_WET BEHAVIOURAL GAMMS TO CHOOSE THE MODEL
pwet<-subset(realz, !is.na(prop_wet))


behav_Full_5_state_resting<- gamm(Full_5_state_resting ~ s(prop_wet), 
                           random=list(ring=~1),
                           method="REML", 
                           data=pwet)
p1<-plot(behav_Full_5_state_resting$gam)
behav_Full_5_state_resting_gam <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)


behav_Full_5_state_preening<- gamm(Full_5_state_preening ~ s(prop_wet), 
                                  random=list(ring=~1),
                                  method="REML", 
                                  data=pwet)
p1<-plot(behav_Full_5_state_preening$gam)
behav_Full_5_state_preening_gam <- data.frame(x=p1[[1]]$x, 
                                             y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                             se=p1[[1]]$se)

behav_Full_5_state_int_foraging<- gamm(Full_5_state_int_foraging ~ s(prop_wet), 
                                   random=list(ring=~1),
                                   method="REML", 
                                   data=pwet)
p1<-plot(behav_Full_5_state_int_foraging$gam)
behav_Full_5_state_preening_gam <- data.frame(x=p1[[1]]$x, 
                                              y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                              se=p1[[1]]$se)

behav_Full_5_state_foraging<- gamm(Full_5_state_foraging ~ s(prop_wet), 
                                       random=list(ring=~1),
                                       method="REML", 
                                       data=pwet)
p1<-plot(behav_Full_5_state_foraging$gam)
behav_Full_5_state_foraging_gam <- data.frame(x=p1[[1]]$x, 
                                                  y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                  se=p1[[1]]$se)
behav_Full_5_state_travelling<- gamm(Full_5_state_travelling ~ s(prop_wet), 
                                       random=list(ring=~1),
                                       method="REML", 
                                       data=pwet)
p1<-plot(behav_Full_5_state_travelling$gam)
behav_Full_5_state_traveling_gam <- data.frame(x=p1[[1]]$x, 
                                                  y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                  se=p1[[1]]$se)



######now for the 4 state models: 


behav_Full_4_stateCSAT_traveling<- gamm(Full_4_stateCSAT_traveling ~ s(prop_wet), 
                                 random=list(ring=~1),
                                 method="REML", 
                                 data=pwet)
p1<-plot(behav_Full_4_stateCSAT_traveling$gam)
behav_Full_4_stateCSAT_traveling_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)

Full_4_stateCSAT_preening<- gamm(Full_4_stateCSAT_preening ~ s(prop_wet), 
                                 random=list(ring=~1),
                                 method="REML", 
                                 data=pwet)
p1<-plot(behav_Full_4_stateCSAT_preening$gam)
behav_Full_4_stateCSAT_preening_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)


behav_Full_4_stateCSAT_foraging<- gamm(Full_4_stateCSAT_foraging ~ s(prop_wet), 
                                       random=list(ring=~1),
                                       method="REML", 
                                       data=pwet)
p1<-plot(behav_Full_4_stateCSAT_foraging$gam)
behav_Full_4_stateCSAT_foraging_gam <- data.frame(x=p1[[1]]$x, 
                                                      y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                      se=p1[[1]]$se)

behav_Full_4_stateCSAT_resting<- gamm(Full_4_stateCSAT_resting ~ s(prop_wet), 
                                   random=list(ring=~1),
                                   method="REML", 
                                   data=pwet)
p1<-plot(behav_Full_4_stateCSAT_resting$gam)
behav_Full_4_stateCSAT_resting_gam <- data.frame(x=p1[[1]]$x, 
                                                      y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                      se=p1[[1]]$se)


########################################################################################










behav_Full_4_stateCS_traveling<- gamm(Full_4_stateCS_traveling ~ s(prop_wet), 
                                       random=list(ring=~1),
                                       method="REML", 
                                       data=pwet)
p1<-plot(behav_Full_4_stateCS_traveling$gam)
behav_Full_4_stateCS_traveling_gam <- data.frame(x=p1[[1]]$x, 
                                                      y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                      se=p1[[1]]$se)

behav_Full_4_stateCS_preening<- gamm(Full_4_stateCS_preening ~ s(prop_wet), 
                                 random=list(ring=~1),
                                 method="REML", 
                                 data=pwet)
p1<-plot(behav_Full_4_stateCS_preening$gam)
behav_Full_4_stateCS_preening_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)


behav_Full_4_stateCS_foraging<- gamm(Full_4_stateCS_foraging ~ s(prop_wet), 
                                     random=list(ring=~1),
                                     method="REML", 
                                     data=pwet)
p1<-plot(behav_Full_4_stateCS_foraging$gam)
behav_Full_4_stateCS_foraging_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)

behav_Full_4_stateCS_resting<- gamm(Full_4_stateCS_resting ~ s(prop_wet), 
                                random=list(ring=~1),
                                method="REML", 
                                data=pwet)
p1<-plot(behav_Full_4_stateCS_resting$gam)
behav_Full_4_stateCS_resting_gam <- data.frame(x=p1[[1]]$x, 
                                                   y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                   se=p1[[1]]$se)


################################################################################################



behav_Full_4_stateCSA_traveling<- gamm(Full_4_stateCSA_traveling ~ s(prop_wet), 
                                       random=list(ring=~1),
                                       method="REML", 
                                       data=pwet)
p1<-plot(behav_Full_4_stateCSA_traveling$gam)
behav_Full_4_stateCSA_traveling_gam <- data.frame(x=p1[[1]]$x, 
                                                      y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                      se=p1[[1]]$se)

behav_Full_4_stateCSA_preening<- gamm(Full_4_stateCSA_preening ~ s(prop_wet), 
                                 random=list(ring=~1),
                                 method="REML", 
                                 data=pwet)
p1<-plot(behav_Full_4_stateCSA_preening$gam)
behav_Full_4_stateCSA_preening_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)


behav_Full_4_stateCSA_foraging<- gamm(Full_4_stateCSA_foraging ~ s(prop_wet), 
                                     random=list(ring=~1),
                                     method="REML", 
                                     data=pwet)
p1<-plot(behav_Full_4_stateCSA_foraging$gam)
behav_Full_4_stateCSA_foraging_gam <- data.frame(x=p1[[1]]$x, 
                                                    y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                    se=p1[[1]]$se)

behav_Full_4_stateCSA_resting<- gamm(Full_4_stateCSA_resting ~ s(prop_wet), 
                                random=list(ring=~1),
                                method="REML", 
                                data=pwet)
p1<-plot(behav_Full_4_stateCSA_resting$gam)
behav_Full_4_stateCSA_resting_gam <- data.frame(x=p1[[1]]$x, 
                                                   y=as.numeric(p1[[1]]$fit)+ mean (pwet$prop_wet, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                                   se=p1[[1]]$se)

########now lets plot these. 

########4states model CS

gam_plot_v_Full_4_stateCS <- 
  ggplot()+
  ## phenology lines
  ## points
#  geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCS_foraging), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=behav_Full_4_stateCS_foraging_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCS_foraging_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  #########

#geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCS_resting), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=behav_Full_4_stateCS_resting_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCS_resting_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  #####

#geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCS_preening), alpha=0.1, size = 0.01, colour = "navyblue")+
  ## gam
  geom_line(data=behav_Full_4_stateCS_preening_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCS_preening_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "navyblue")+
  
  ##########
#geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCS_traveling), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=behav_Full_4_stateCS_traveling_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCS_traveling_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  
  
  scale_x_continuous(name ="Probability of Behaviour", limits = c(0, 1))+
 # scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time Wet")+
  theme_base

#######

gam_plot_v_Full_4_stateCSA <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSA_foraging), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=behav_Full_4_stateCSA_foraging_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSA_foraging_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  #########

geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSA_resting), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=behav_Full_4_stateCSA_resting_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSA_resting_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  #####

geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSA_preening), alpha=0.1, size = 0.01, colour = "navyblue")+
  ## gam
  geom_line(data=behav_Full_4_stateCSA_preening_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSA_preening_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "navyblue")+
  
  ##########
geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSA_traveling), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=behav_Full_4_stateCSA_traveling_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSA_traveling_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  
  
  scale_x_continuous(name ="Probability of Behaviour", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time Wet")+
  theme_base





#######

gam_plot_v_Full_4_stateCSAT <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSAT_foraging), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=behav_Full_4_stateCSAT_foraging_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSAT_foraging_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  #########

geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSAT_resting), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=behav_Full_4_stateCSAT_resting_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSAT_resting_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  #####

geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSAT_preening), alpha=0.1, size = 0.01, colour = "navyblue")+
  ## gam
  geom_line(data=behav_Full_4_stateCSAT_preening_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSAT_preening_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "navyblue")+
  
  ##########
geom_point(data=pwet, aes(x=prop_wet, y=Full_4_stateCSAT_traveling), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=behav_Full_4_stateCSAT_traveling_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_4_stateCSAT_traveling_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  
  
  scale_x_continuous(name ="Probability of Behaviour", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time Wet")+
  theme_base


#############################


##### 5 STATE MODEL

gam_plot_v_Full_5_state <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=pwet, aes(x=prop_wet, y=Full_5_state_foraging), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=behav_Full_5_state_foraging_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_5_state_foraging_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  #####
geom_point(data=pwet, aes(x=prop_wet, y=Full_5_state_int_foraging), alpha=0.1, size = 0.01, colour = "violet")+
  ## gam
  geom_line(data=behav_Full_5_state_preening_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_5_state_preening_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violet") +
  

  
  
  #########

geom_point(data=pwet, aes(x=prop_wet, y=Full_5_state_resting), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=behav_Full_5_state_resting_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_5_state_resting_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  #####

geom_point(data=pwet, aes(x=prop_wet, y=Full_5_state_preening), alpha=0.1, size = 0.01, colour = "navyblue")+
  ## gam
  geom_line(data=behav_Full_5_state_preening_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_5_state_preening_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "navyblue")+
  
  ##########
geom_point(data=pwet, aes(x=prop_wet, y=Full_5_state_travelling), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=behav_Full_5_state_traveling_gam, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=behav_Full_5_state_traveling_gam, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  
  
  scale_x_continuous(name ="Probability of Behaviour", limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time Wet")+
  theme_base











































gps_all<-realz
gps_all<-subset(realz, complete =="c")

gps_all <-gps_all[order(gps_all$tripunique, gps_all$dateTime_gps),]

#####we should make the plots of the the `probabilities vs prop wet from the gls 


##################################################################################################################################################

#gps_all <- cbind( gps_all, colsplit( as.character(gps_all$dateTime), split = ":", names = c("h","m","s" ))) # by clock

gps_all$h<-as.numeric(hour(gps_all$dateTime_gps))
gps_all$m<-as.numeric(minute(gps_all$dateTime_gps))
gps_all$s<-as.numeric(second(gps_all$dateTime_gps))

gps_all$decimal_hour<-gps_all$h +gps_all$m/60 + gps_all$s/3600

gps_all$behaviour<-gps_all$fitTrack_FULL_4state_constraint_step_and_angle
table(gps_all$behaviour)



gps_all<- gps_all %>%
  group_by(tripunique)  %>%
  dplyr::mutate(previous_time = lag(dateTime_gps))%>%
  ungroup()

gps_all<- gps_all %>%
  dplyr::group_by(tripunique)  %>%
  dplyr::mutate(time_interval_s = as.numeric(difftime(dateTime_gps, previous_time, units = "secs")))%>%
  ungroup()

str(gps_all)

brood<-subset(gps_all,lifeCycleStatus=="chick-rearing")
#brood$ring

beh_proportion <- aggregate (brood$time_interval_s, by=list(brood$ring, brood$tripunique,brood$island, brood$behaviour,brood$h),sum)
colnames (beh_proportion) <- c("ring","tripunique","island", "behaviour","hour","beh_sum")
library(plyr)
proportion_bro<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum))) %>%
dplyr::ungroup()



beh_proportion2 <- aggregate (brood$time_interval_s, by=list(brood$ring, brood$tripunique,brood$island, brood$behaviour),sum)
colnames (beh_proportion2) <- c("ring","tripunique","island", "behaviour","beh_sum")






proportion_trip_bro<-beh_proportion2 %>%
  dplyr::group_by(tripunique, behaviour, island) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum))) %>%
  dplyr::ungroup()


proportion_trip_bro$behaviour<-ifelse(proportion_trip_bro$behaviour == 1, "resting", ifelse(proportion_trip_bro$behaviour == 2, "preening", ifelse(proportion_trip_bro$behaviour == 3, "foraging","travelling")))
proportion_trip_bro$behaviour

proportion_trip_bro$behaviour<-factor(proportion_trip_bro$behaviour, levels = c("resting", "preening", "foraging", "travelling"))



####boxplot of the proportion of each behaviour per trip by colony

ggplot(proportion_trip_bro, aes(x=island, y=proportion, fill= behaviour)) + 
  geom_boxplot()+ facet_wrap(~behaviour)

rest_bro<-subset(proportion_bro,behaviour=="1")
rest_bro$behaviour <-"1"

#######faire le gamm pour brooding and incubation



rest_bro$ID<-rest_bro$tripunique
###########

behav_rest_brooding<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
             # the k is the number of nodes (?)
             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
             method="REML",  # I do not know what f*** is this
             data=rest_bro) # our dataframe



p1<-plot(behav_rest_brooding$gam)



resting_bro <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (rest_bro$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_bro<-subset(proportion_bro,behaviour=="4")

#######faire le gamm pour brooding and incubation



travel_bro$ID<-travel_bro$tripunique
###########

behav_travel_brooding<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
             # the k is the number of nodes (?)
             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
             method="REML",  # I do not know what f*** is this
             data=travel_bro) # our dataframe




p1<-plot(behav_travel_brooding$gam)



traveling_bro <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (travel_bro$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)








###################################################################################################################################################################################################################
foraging_bro<-subset(proportion_bro,behaviour=="3")

#######faire le gamm pour brooding and incubation


foraging_bro$ID<-foraging_bro$tripunique

###########

behav_foraging_brooding<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                             # the k is the number of nodes (?)
                             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                             method="REML",  # I do not know what f*** is this
                             data=foraging_bro) # our dataframe



p1<-plot(behav_foraging_brooding$gam)


for_bro <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (foraging_bro$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)




###################################################################################################################################################################################################################


preening_bro<-subset(proportion_bro,behaviour=="2")

#######faire le gamm pour brooding and incubation



preening_bro$ID<-preening_bro$tripunique
###########

int_behav_preening_brooding<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=preening_bro) # our dataframe




p1<-plot(int_behav_preening_brooding$gam)



pre_bro <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (preening_bro$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)

############################################################################################################################################################################################################################################################################################
# 
# 
# foraging_bro<-subset(proportion_bro,behaviour=="3")
# 
# foraging_bro$ID<-foraging_bro$tripunique
# ###########
# 
# behav_foraging_brooding<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
#                                    # the k is the number of nodes (?)
#                                    random=list(ring=~1), # the random factor that we want to take into account (always a list!)
#                                    method="REML",  # I do not know what f*** is this
#                                    data=foraging_bro) # our dataframe
# 
# 
# 
# 
# 
# p1<-plot(behav_foraging_brooding$gam)



# for_bro <- data.frame(x=p1[[1]]$x, 
#                              y=as.numeric(p1[[1]]$fit)+ mean (foraging_bro$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
#                              se=p1[[1]]$se)
# 



####################################################################################################################################################################################################################################################################################################################################################################################################################################

inc<-subset(gps_all,lifeCycleStatus=="incubation")
#inc$ring

beh_proportion <- aggregate (inc$time_interval_s, by=list(inc$ring, inc$tripunique, inc$behaviour,inc$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
library(plyr)
proportion_inc<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_inc<-subset(proportion_inc,behaviour=="1")

#######faire le gamm pour inc and incubation



rest_inc$ID<-rest_inc$tripunique
###########

behav_rest_inc<- gamm(proportion ~ s(hour,bs = "cc",  k=23),
                           random=list(ring=~1), 
                           method="REML",  
                           data=rest_inc) # our dataframe




p1<-plot(behav_rest_inc$gam)



resting_inc <- data.frame(x=p1[[1]]$x, 
                      y=as.numeric(p1[[1]]$fit)+ mean (rest_inc$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                      se=p1[[1]]$se)






###################################################################################################################################################################################################################
travel_inc<-subset(proportion_inc,behaviour=="4")

#######faire le gamm pour inc and incubation



travel_inc$ID<-travel_inc$tripunique
###########

behav_travel_inc<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                             # the k is the number of nodes (?)
                             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                             method="REML",  # I do not know what f*** is this
                             data=travel_inc) # our dataframe




p1<-plot(behav_travel_inc$gam)



tra_inc <- data.frame(x=p1[[1]]$x, 
                      y=as.numeric(p1[[1]]$fit)+ mean (travel_inc$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                      se=p1[[1]]$se)







###################################################################################################################################################################################################################
foraging_inc<-subset(proportion_inc,behaviour=="3")

#######faire le gamm pour inc and incubation



foraging_inc$ID<-foraging_inc$tripunique
###########

behav_foraging_inc<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=foraging_inc) # our dataframe





p1<-plot(behav_foraging_inc$gam)



for_inc <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (foraging_inc$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)


###################################################################################################################################################################################################################


preening_inc<-subset(proportion_inc,behaviour=="2")

#######faire le gamm pour inc and incubation



preening_inc$ID<-preening_inc$tripunique
###########

behav_preeening_inc<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=preening_inc) # our dataframe



p1<-plot(behav_preeening_inc$gam)



pre_inc <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (preening_inc$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)





############################################################################################################################################################################################################################################################################################

# 
# foraging_inc<-subset(proportion_inc,behaviour=="3")
# 
# foraging_inc$ID<-foraging_inc$tripunique
# ###########
# 
# behav_foraging_inc<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
#                                    # the k is the number of nodes (?)
#                                    random=list(ring=~1), # the random factor that we want to take into account (always a list!)
#                                    method="REML",  # I do not know what f*** is this
#                                    data=foraging_inc) # our dataframe
# 
# 
# 
# p1<-plot(behav_foraging_inc$gam)
# 
# for_inc <- data.frame(x=p1[[1]]$x, 
#                       y=as.numeric(p1[[1]]$fit)+ mean (foraging_inc$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
#                       se=p1[[1]]$se)
# 



#################################################################################################################################################################################################################???

#MAKE SOME PLOTS

##################################################################################################################################################################################################################




gam_plot_foraging_only <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "darkred")+
  ## gam
  geom_line(data=for_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "darkred")+
  theme_base +
  
  ########
#########
  geom_point(data=preening_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "red")+
  ## gam
  geom_line(data=pre_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "red")+
  scale_x_continuous(name ="Hour")+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  coord_cartesian(xlim=c(0, 23), ylim=c(0, 1))


#############################################



gam_plot_behaviour_inc <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=for_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
#########

geom_point(data=rest_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=resting_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+

##########
geom_point(data=travel_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=tra_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=tra_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  
  geom_point(data=preening_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orangered")+
  ## gam
  geom_line(data=pre_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "orangered")+
  theme_base



###################################

gam_plot_behaviour_bro <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=preening_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=pre_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  #########

geom_point(data=rest_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=resting_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  
  ##########
geom_point(data=travel_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=traveling_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "cyan")+
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  
  geom_point(data=foraging_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orangered")+
  ## gam
  geom_line(data=for_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "orangered")+
  theme_base

##########################################################################################################################################

#compare foraging for brooding and incubation

gam_plot_foraging_inc_brood <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=for_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=foraging_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=for_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_int_preening_inc_brood <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=preening_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pre_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=preening_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=pre_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base







gam_plot_int_traveling_inc_brood <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=travel_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=tra_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=tra_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue")+
  geom_point(data=travel_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=traveling_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown")+

  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base







gam_plot_int_rest_inc_brood <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=rest_inc, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=resting_inc, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_inc, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=rest_bro, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "rosybrown")+
  ## gam
  geom_line(data=resting_bro, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_bro, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "rosybrown")+
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base





#########################################################################################################################################################################################################################################################################################################################################################Make the plots of sex
#######################################################################################################################################################################################################################################################






male<-subset(gps_all,Sex=="1")
length(unique(male$tripunique)) #####356
length(unique(male$ring)) ####100

#male$ring

beh_proportion <- aggregate (male$time_interval_s, by=list(male$ring, male$tripunique, male$behaviour,male$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_male<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))%>%
  dplyr::ungroup()



rest_male<-subset(proportion_male,behaviour=="1")

#######faire le gamm pour maleing and incubation



rest_male$ID<-rest_male$tripunique
###########

behav_rest_male<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                           # the k is the number of nodes (?)
                           random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                           method="REML",  # I do not know what f*** is this
                           data=rest_male) # our dataframe



p1<-plot(behav_rest_male$gam)



resting_male <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (rest_male$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_male<-subset(proportion_male,behaviour=="4")

#######faire le gamm pour maleing and incubation



travel_male$ID<-travel_male$tripunique
###########

behav_travel_male<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                             # the k is the number of nodes (?)
                             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                             method="REML",  # I do not know what f*** is this
                             data=travel_male) # our dataframe




p1<-plot(behav_travel_male$gam)



traveling_male <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (travel_male$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)


#############################################################################################################################################################################################


foraging_male<-subset(proportion_male,behaviour=="3")

#######faire le gamm pour maleing and incubation



foraging_male$ID<-foraging_male$tripunique
###########

behav_foraging_male<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=foraging_male) # our dataframe




p1<-plot(behav_foraging_male$gam)



for_male <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (foraging_male$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)

############################################################################################################


preening_male<-subset(proportion_male,behaviour=="2")

#######faire le gamm pour maleing and incubation



preening_male$ID<-preening_male$tripunique
###########

behav_preening_male<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=preening_male) # our dataframe




p1<-plot(behav_preening_male$gam)



pre_male <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_male$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)




#################################################################################################################################################################################################################################################_



Female<-subset(gps_all,Sex=="2")
length(unique(Female$tripunique)) #####316
length(unique(Female$ring)) ##106

#Female$ring

beh_proportion <- aggregate (Female$time_interval_s, by=list(Female$ring, Female$tripunique, Female$behaviour,Female$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_Female<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_Female<-subset(proportion_Female,behaviour=="1")

#######faire le gamm pour Femaleing and incubation



rest_Female$ID<-rest_Female$tripunique
###########

behav_rest_Female<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                       # the k is the number of nodes (?)
                       random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                       method="REML",  # I do not know what f*** is this
                       data=rest_Female) # our dataframe



p1<-plot(behav_rest_Female$gam)



resting_Female <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (rest_Female$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_Female<-subset(proportion_Female,behaviour=="4")
table(travel_Female$behaviour)

#######faire le gamm pour Female and incubation



travel_Female$ID<-travel_Female$tripunique
###########

behav_travel_Female<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                         # the k is the number of nodes (?)
                         random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                         method="REML",  # I do not know what f*** is this
                         data=travel_Female) # our dataframe




p1<-plot(behav_travel_Female$gam)



traveling_Female <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (travel_Female$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)


#############################################################################################################################################################################################

foraging_Female<-subset(proportion_Female,behaviour=="3")

#######faire le gamm pour Femaleing and incubation



foraging_Female$ID<-foraging_Female$tripunique
###########

behav_foraging_Female<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=foraging_Female) # our dataframe




p1<-plot(behav_foraging_Female$gam)



for_Female <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_Female$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)

############################################################################################################


preening_Female<-subset(proportion_Female,behaviour=="2")

#######faire le gamm pour Femaleing and incubation



preening_Female$ID<-preening_Female$tripunique
###########

behav_preening_Female<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=preening_Female) # our dataframe




p1<-plot(behav_preening_Female$gam)



pre_Female <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_Female$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)




gam_plot_foraging_sex <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_male, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=for_male, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_male, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=foraging_Female, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=for_Female, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_Female, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_preening_sex <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=preening_male, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pre_male, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_male, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=preening_Female, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=pre_Female, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_Female, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base







gam_plot_traveling_sex <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=travel_male, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=traveling_male, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_male, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue")+
  geom_point(data=travel_Female, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=traveling_Female, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_Female, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="Hour", limits = c(0, 23))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base







gam_plot_int_rest_sex<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=rest_male, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=resting_male, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_male, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=rest_Female, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=resting_Female, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_Female, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base


################################################################################################################################################################################################################################################################################################################################

#now by colony 

####################################################################################################################################################################################################################################################

################################################################################################################################################################################################################################################


Boavista<-subset(gps_all,island=="Boavista") #Boavista     ICima Madeleine      Raso       Sal 
length(unique(Boavista$tripunique)) #####327
length(unique(Boavista$ring)) ####107

#Boavista$ring

beh_proportion <- aggregate (Boavista$time_interval_s, by=list(Boavista$ring, Boavista$tripunique, Boavista$behaviour,Boavista$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_Boavista<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_Boavista<-subset(proportion_Boavista,behaviour=="1")

#######faire le gamm pour Boavistaing and incubation



rest_Boavista$ID<-rest_Boavista$tripunique
###########

behav_rest_Boavista<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                         # the k is the number of nodes (?)
                         random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                         method="REML",  # I do not know what f*** is this
                         data=rest_Boavista) # our dataframe



p1<-plot(behav_rest_Boavista$gam)



resting_Boavista <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (rest_Boavista$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_Boavista<-subset(proportion_Boavista,behaviour=="4")

#######faire le gamm pour Boavistaing and incubation



travel_Boavista$ID<-travel_Boavista$tripunique
###########

behav_travel_Boavista<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                           # the k is the number of nodes (?)
                           random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                           method="REML",  # I do not know what f*** is this
                           data=travel_Boavista) # our dataframe




p1<-plot(behav_travel_Boavista$gam)



traveling_Boavista <- data.frame(x=p1[[1]]$x, 
                                y=as.numeric(p1[[1]]$fit)+ mean (travel_Boavista$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                se=p1[[1]]$se)


#############################################################################################################################################################################################


preening_Boavista<-subset(proportion_Boavista,behaviour=="2")

#######faire le gamm pour Boavistaing and incubation



preening_Boavista$ID<-preening_Boavista$tripunique
###########

behav_preening_Boavista<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                 # the k is the number of nodes (?)
                                 random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                 method="REML",  # I do not know what f*** is this
                                 data=preening_Boavista) # our dataframe




p1<-plot(behav_preening_Boavista$gam)



pre_Boavista <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (preening_Boavista$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)

############################################################################################################


foraging_Boavista<-subset(proportion_Boavista,behaviour=="3")

#######faire le gamm pour Boavistaing and incubation



foraging_Boavista$ID<-foraging_Boavista$tripunique
###########

behav_foraging_Boavista<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                 # the k is the number of nodes (?)
                                 random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                                 method="REML",  # I do not know what f*** is this
                                 data=foraging_Boavista) # our dataframe




p1<-plot(behav_foraging_Boavista$gam)




for_Boavista <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (foraging_Boavista$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)


#####



ICima<-subset(gps_all,island=="ICima") #ICima     ICima Madeleine      Raso       Sal 
length(unique(ICima$tripunique)) ###111
length(unique(ICima$ring)) ####44

#ICima$ring

beh_proportion <- aggregate (ICima$time_interval_s, by=list(ICima$ring, ICima$tripunique, ICima$behaviour,ICima$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_ICima<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_ICima<-subset(proportion_ICima,behaviour=="1")

#######faire le gamm pour ICimaing and incubation



rest_ICima$ID<-rest_ICima$tripunique
###########

behav_rest_ICima<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                           # the k is the number of nodes (?)
                           random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                           method="REML",  # I do not know what f*** is this
                           data=rest_ICima) # our dataframe



p1<-plot(behav_rest_ICima$gam)



resting_ICima <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (rest_ICima$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_ICima<-subset(proportion_ICima,behaviour=="4")

#######faire le gamm pour ICimaing and incubation



travel_ICima$ID<-travel_ICima$tripunique
###########

behav_travel_ICima<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                             # the k is the number of nodes (?)
                             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                             method="REML",  # I do not know what f*** is this
                             data=travel_ICima) # our dataframe




p1<-plot(behav_travel_ICima$gam)



traveling_ICima <- data.frame(x=p1[[1]]$x, 
                                  y=as.numeric(p1[[1]]$fit)+ mean (travel_ICima$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                  se=p1[[1]]$se)


#############################################################################################################################################################################################


preening_ICima<-subset(proportion_ICima,behaviour=="2")

#######faire le gamm pour ICimaing and incubation



preening_ICima$ID<-preening_ICima$tripunique
###########

behav_preening_ICima<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=preening_ICima) # our dataframe




p1<-plot(behav_preening_ICima$gam)



pre_ICima <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (preening_ICima$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)

############################################################################################################


foraging_ICima<-subset(proportion_ICima,behaviour=="3")

#######faire le gamm pour ICimaing and incubation



foraging_ICima$ID<-foraging_ICima$tripunique
###########

behav_foraging_ICima<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=foraging_ICima) # our dataframe




p1<-plot(behav_foraging_ICima$gam)



for_ICima <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (foraging_ICima$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)









####################################



Raso<-subset(gps_all,island=="Raso") #Raso     Raso Madeleine      Raso       Sal 
length(unique(Raso$tripunique)) ####134
length(unique(Raso$ring)) ####53

#Raso$ring

beh_proportion <- aggregate (Raso$time_interval_s, by=list(Raso$ring, Raso$tripunique, Raso$behaviour,Raso$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_Raso<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_Raso<-subset(proportion_Raso,behaviour=="1")

#######faire le gamm pour Rasoing and incubation



rest_Raso$ID<-rest_Raso$tripunique
###########

behav_rest_Raso<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                        # the k is the number of nodes (?)
                        random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                        method="REML",  # I do not know what f*** is this
                        data=rest_Raso) # our dataframe



p1<-plot(behav_rest_Raso$gam)



resting_Raso <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (rest_Raso$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_Raso<-subset(proportion_Raso,behaviour=="4")

#######faire le gamm pour Rasoing and incubation



travel_Raso$ID<-travel_Raso$tripunique
###########

behav_travel_Raso<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                          # the k is the number of nodes (?)
                          random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                          method="REML",  # I do not know what f*** is this
                          data=travel_Raso) # our dataframe




p1<-plot(behav_travel_Raso$gam)



traveling_Raso <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (travel_Raso$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)


#############################################################################################################################################################################################


preening_Raso<-subset(proportion_Raso,behaviour=="2")

#######faire le gamm pour Rasoing and incubation



preening_Raso$ID<-preening_Raso$tripunique
###########

behav_preening_Raso<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                # the k is the number of nodes (?)
                                random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                method="REML",  # I do not know what f*** is this
                                data=preening_Raso) # our dataframe




p1<-plot(behav_preening_Raso$gam)



pre_Raso <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (preening_Raso$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)

############################################################################################################


foraging_Raso<-subset(proportion_Raso,behaviour=="3")

#######faire le gamm pour Rasoing and incubation



foraging_Raso$ID<-foraging_Raso$tripunique
###########

behav_foraging_Raso<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                                # the k is the number of nodes (?)
                                random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                                method="REML",  # I do not know what f*** is this
                                data=foraging_Raso) # our dataframe




p1<-plot(behav_foraging_Raso$gam)



for_Raso <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (foraging_Raso$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)



##################################################
####################################################


Sal<-subset(gps_all,island=="Sal") #Sal     Sal Madeleine      Sal       Sal 
length(unique(Sal$tripunique)) #####324
length(unique(Sal$ring)) ####105

#Sal$ring

beh_proportion <- aggregate (Sal$time_interval_s, by=list(Sal$ring, Sal$tripunique, Sal$behaviour,Sal$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_Sal<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_Sal<-subset(proportion_Sal,behaviour=="1")

#######faire le gamm pour Saling and incubation



rest_Sal$ID<-rest_Sal$tripunique
###########

behav_rest_Sal<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                       # the k is the number of nodes (?)
                       random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                       method="REML",  # I do not know what f*** is this
                       data=rest_Sal) # our dataframe



p1<-plot(behav_rest_Sal$gam)



resting_Sal <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (rest_Sal$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_Sal<-subset(proportion_Sal,behaviour=="4")

#######faire le gamm pour Saling and incubation



travel_Sal$ID<-travel_Sal$tripunique
###########

behav_travel_Sal<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                         # the k is the number of nodes (?)
                         random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                         method="REML",  # I do not know what f*** is this
                         data=travel_Sal) # our dataframe




p1<-plot(behav_travel_Sal$gam)



traveling_Sal <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (travel_Sal$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)


#############################################################################################################################################################################################


preening_Sal<-subset(proportion_Sal,behaviour=="2")

#######faire le gamm pour Saling and incubation



preening_Sal$ID<-preening_Sal$tripunique
###########

behav_preening_Sal<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=preening_Sal) # our dataframe




p1<-plot(behav_preening_Sal$gam)



pre_Sal <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_Sal$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)

############################################################################################################


foraging_Sal<-subset(proportion_Sal,behaviour=="3")

#######faire le gamm pour Saling and incubation



foraging_Sal$ID<-foraging_Sal$tripunique
###########

behav_foraging_Sal<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=foraging_Sal) # our dataframe




p1<-plot(behav_foraging_Sal$gam)



for_Sal <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_Sal$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)


################################################################################################################################################################################################################################################################################################################################################################



##################################################
####################################################


Madeleine<-subset(gps_all,island=="Madeleine") #Madeleine     Madeleine Madeleine      Madeleine       Madeleine 
length(unique(Madeleine$tripunique)) #####324
length(unique(Madeleine$ring)) ####105

#Madeleine$ring

beh_proportion <- aggregate (Madeleine$time_interval_s, by=list(Madeleine$ring, Madeleine$tripunique, Madeleine$behaviour,Madeleine$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_Madeleine<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_Madeleine<-subset(proportion_Madeleine,behaviour=="1")

#######faire le gamm pour Madeleineing and incubation



rest_Madeleine$ID<-rest_Madeleine$tripunique
###########

behav_rest_Madeleine<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                      # the k is the number of nodes (?)
                      random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                      method="REML",  # I do not know what f*** is this
                      data=rest_Madeleine) # our dataframe



p1<-plot(behav_rest_Madeleine$gam)



resting_Madeleine <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (rest_Madeleine$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_Madeleine<-subset(proportion_Madeleine,behaviour=="4")

#######faire le gamm pour Madeleineing and incubation



travel_Madeleine$ID<-travel_Madeleine$tripunique
###########

behav_travel_Madeleine<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                        # the k is the number of nodes (?)
                        random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                        method="REML",  # I do not know what f*** is this
                        data=travel_Madeleine) # our dataframe




p1<-plot(behav_travel_Madeleine$gam)



traveling_Madeleine <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (travel_Madeleine$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)


#############################################################################################################################################################################################


preening_Madeleine<-subset(proportion_Madeleine,behaviour=="2")

#######faire le gamm pour Madeleineing and incubation



preening_Madeleine$ID<-preening_Madeleine$tripunique
###########

behav_preening_Madeleine<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                          # the k is the number of nodes (?)
                          random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                          method="REML",  # I do not know what f*** is this
                          data=preening_Madeleine) # our dataframe




p1<-plot(behav_preening_Madeleine$gam)



pre_Madeleine <- data.frame(x=p1[[1]]$x, 
                      y=as.numeric(p1[[1]]$fit)+ mean (preening_Madeleine$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                      se=p1[[1]]$se)

############################################################################################################


foraging_Madeleine<-subset(proportion_Madeleine,behaviour=="3")

#######faire le gamm pour Madeleineing and incubation



foraging_Madeleine$ID<-foraging_Madeleine$tripunique
###########

behav_foraging_Madeleine<- gamm(proportion ~ s(hour,bs = "cc",  k=23), # the formula: the response variable ~ the independent variable
                          # the k is the number of nodes (?)
                          random=list(ring=~1), # the random factor that we want to take exto account (always a list!)
                          method="REML",  # I do not know what f*** is this
                          data=foraging_Madeleine) # our dataframe




p1<-plot(behav_foraging_Madeleine$gam)



for_Madeleine <- data.frame(x=p1[[1]]$x, 
                      y=as.numeric(p1[[1]]$fit)+ mean (foraging_Madeleine$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                      se=p1[[1]]$se)




































gam_plot_foraging_colony <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_Boavista, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=for_Boavista, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_Boavista, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "blue") +
  
  geom_point(data=foraging_Sal, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=for_Sal, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_Sal, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "cyan") +
  
  geom_point(data=foraging_Raso, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=for_Raso, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_Raso, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "violetred") +
  
  geom_point(data=foraging_ICima, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orange")+
  ## gam
  geom_line(data=for_ICima, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_ICima, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "orange") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_preening_colony <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=preening_Boavista, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pre_Boavista, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_Boavista, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "blue") +
  
  geom_point(data=preening_Sal, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=pre_Sal, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_Sal, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "cyan") +
  
  geom_point(data=preening_Raso, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=pre_Raso, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_Raso, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "violetred") +
  
  geom_point(data=preening_ICima, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orange")+
  ## gam
  geom_line(data=pre_ICima, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_ICima, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "orange") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base





gam_plot_travel_colony <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=travel_Boavista, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=traveling_Boavista, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_Boavista, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "blue") +
  
  geom_point(data=travel_Sal, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=traveling_Sal, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_Sal, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "cyan") +
  
  geom_point(data=travel_Raso, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=traveling_Raso, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_Raso, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "violetred") +
  
  geom_point(data=travel_ICima, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orange")+
  ## gam
  geom_line(data=traveling_ICima, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_ICima, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "orange") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_rest_colony <- 
  ggplot()+
  ## phenology lines
  ## points
  #geom_point(data=rest_Boavista, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=resting_Boavista, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_Boavista, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "blue") +
  
 # geom_point(data=rest_Sal, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "cyan")+
  ## gam
  geom_line(data=resting_Sal, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_Sal, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "cyan") +
  
  #geom_point(data=rest_Raso, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=resting_Raso, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_Raso, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "violetred") +
  
  #geom_point(data=rest_ICima, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "orange")+
  ## gam
  geom_line(data=resting_ICima, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_ICima, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "orange") +
  
  #geom_point(data=rest_Madeleine, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "black")+
  ## gam
  geom_line(data=resting_Madeleine, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_Madeleine, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "black") +
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base














#####################################################################################################################################################################################################################################################################################################################################################################################################################Now by month
###############################################################################################################################################################################################################################################################################################################################
library(circular)

circular.colors(n=12, m=0, M=2*pi)




gps_all$month<-month(gps_all$dateTime_gps)

January<-subset(gps_all,month==1) #January     ICima Madeleine      Raso       Sal 
length(unique(January$tripunique)) #####129
length(unique(January$ring)) ####61

#January$ring

beh_proportion <- aggregate (January$time_interval_s, by=list(January$ring, January$tripunique, January$behaviour,January$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_January<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_January<-subset(proportion_January,behaviour=="1")

###########

behav_rest_January<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                           random=list(ring=~1), 
                           method="REML", 
                           data=rest_January) 

p1<-plot(behav_rest_January$gam)

resting_January <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (rest_January$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)
########

travel_January<-subset(proportion_January,behaviour=="4")

###########

behav_travel_January<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                             random=list(ring=~1),
                             method="REML", 
                             data=travel_January) 
p1<-plot(behav_travel_January$gam)

traveling_January <- data.frame(x=p1[[1]]$x, 
                                  y=as.numeric(p1[[1]]$fit)+ mean (travel_January$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                  se=p1[[1]]$se)

#######

preening_January<-subset(proportion_January,behaviour=="2")

###########

behav_preening_January<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                   random=list(ring=~1),  
                                   method="REML",  
                                   data=preening_January)

p1<-plot(behav_preening_January$gam)


pre_January <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (preening_January$proportion, na.rm=TRUE),
                               se=p1[[1]]$se)

###########

foraging_January<-subset(proportion_January,behaviour=="3")

###########

behav_foraging_January<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                   random=list(ring=~1), 
                                   method="REML",  
                                   data=foraging_January)

p1<-plot(behav_foraging_January$gam)

for_January <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (foraging_January$proportion, na.rm=TRUE), 
                               se=p1[[1]]$se)


#####
#####
####
#####

gps_all$month<-month(gps_all$dateTime_gps)

February<-subset(gps_all,month==2) #February        
length(unique(February$tripunique)) #####113
length(unique(February$ring)) ####52

#February$ring

beh_proportion <- aggregate (February$time_interval_s, by=list(February$ring, February$tripunique, February$behaviour,February$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_February<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_February<-subset(proportion_February,behaviour=="1")

###########

behav_rest_February<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                          random=list(ring=~1), 
                          method="REML", 
                          data=rest_February) 

p1<-plot(behav_rest_February$gam)

resting_February <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (rest_February$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)
########

travel_February<-subset(proportion_February,behaviour=="4")

###########

behav_travel_February<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                            random=list(ring=~1),
                            method="REML", 
                            data=travel_February) 
p1<-plot(behav_travel_February$gam)

traveling_February <- data.frame(x=p1[[1]]$x, 
                                 y=as.numeric(p1[[1]]$fit)+ mean (travel_February$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                 se=p1[[1]]$se)

#######

preening_February<-subset(proportion_February,behaviour=="2")

###########

behav_preening_February<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                  random=list(ring=~1),  
                                  method="REML",  
                                  data=preening_February)

p1<-plot(behav_preening_February$gam)


pre_February <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (preening_February$proportion, na.rm=TRUE),
                              se=p1[[1]]$se)

###########

foraging_February<-subset(proportion_February,behaviour=="3")

###########

behav_foraging_February<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                  random=list(ring=~1), 
                                  method="REML",  
                                  data=foraging_February)

p1<-plot(behav_foraging_February$gam)

for_February <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (foraging_February$proportion, na.rm=TRUE), 
                              se=p1[[1]]$se)
######


##############################################################################################################################################################################################################################################################################################################################################

gps_all$month<-month(gps_all$dateTime_gps)

March<-subset(gps_all,month==2) #March        
length(unique(March$tripunique)) #####113
length(unique(March$ring)) ####52

#March$ring

beh_proportion <- aggregate (March$time_interval_s, by=list(March$ring, March$tripunique, March$behaviour,March$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_March<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_March<-subset(proportion_March,behaviour=="1")

###########

behav_rest_March<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                           random=list(ring=~1), 
                           method="REML", 
                           data=rest_March) 

p1<-plot(behav_rest_March$gam)

resting_March <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (rest_March$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)
########

travel_March<-subset(proportion_March,behaviour=="4")

###########

behav_travel_March<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                             random=list(ring=~1),
                             method="REML", 
                             data=travel_March) 
p1<-plot(behav_travel_March$gam)

traveling_March <- data.frame(x=p1[[1]]$x, 
                                 y=as.numeric(p1[[1]]$fit)+ mean (travel_March$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                 se=p1[[1]]$se)

#######

preening_March<-subset(proportion_March,behaviour=="2")

###########

behav_preening_March<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1),  
                               method="REML",  
                               data=preening_March)

p1<-plot(behav_preening_March$gam)


pre_March <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_March$proportion, na.rm=TRUE),
                           se=p1[[1]]$se)

###########

foraging_March<-subset(proportion_March,behaviour=="3")

###########

behav_foraging_March<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1), 
                               method="REML",  
                               data=foraging_March)

p1<-plot(behav_foraging_March$gam)

for_March <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_March$proportion, na.rm=TRUE), 
                           se=p1[[1]]$se)
######














####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

April<-subset(gps_all,month==4) #April        
length(unique(April$tripunique)) #####32
length(unique(April$ring)) ####15

#April$ring

beh_proportion <- aggregate (April$time_interval_s, by=list(April$ring, April$tripunique, April$behaviour,April$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_April<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_April<-subset(proportion_April,behaviour=="1")

###########

behav_rest_April<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                        random=list(ring=~1), 
                        method="REML", 
                        data=rest_April) 

p1<-plot(behav_rest_April$gam)

resting_April <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (rest_April$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)
########

travel_April<-subset(proportion_April,behaviour=="4")

###########

behav_travel_April<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                          random=list(ring=~1),
                          method="REML", 
                          data=travel_April) 
p1<-plot(behav_travel_April$gam)

traveling_April <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (travel_April$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)

#######

preening_April<-subset(proportion_April,behaviour=="2")

###########

behav_preening_April<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1),  
                                method="REML",  
                                data=preening_April)

p1<-plot(behav_preening_April$gam)


pre_April <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (preening_April$proportion, na.rm=TRUE),
                            se=p1[[1]]$se)

###########

foraging_April<-subset(proportion_April,behaviour=="3")

###########

behav_foraging_April<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1), 
                                method="REML",  
                                data=foraging_April)

p1<-plot(behav_foraging_April$gam)

for_April <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (foraging_April$proportion, na.rm=TRUE), 
                            se=p1[[1]]$se)

######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

May<-subset(gps_all,month==5) #May        
length(unique(May$tripunique)) #####78
length(unique(May$ring)) ####33

#May$ring

beh_proportion <- aggregate (May$time_interval_s, by=list(May$ring, May$tripunique, May$behaviour,May$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_May<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_May<-subset(proportion_May,behaviour=="1")

###########

behav_rest_May<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                        random=list(ring=~1), 
                        method="REML", 
                        data=rest_May) 

p1<-plot(behav_rest_May$gam)

resting_May <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (rest_May$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)
########

travel_May<-subset(proportion_May,behaviour=="4")

###########

behav_travel_May<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                          random=list(ring=~1),
                          method="REML", 
                          data=travel_May) 
p1<-plot(behav_travel_May$gam)

traveling_May <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (travel_May$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)

#######

preening_May<-subset(proportion_May,behaviour=="2")

###########

behav_preening_May<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1),  
                                method="REML",  
                                data=preening_May)

p1<-plot(behav_preening_May$gam)


pre_May <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (preening_May$proportion, na.rm=TRUE),
                            se=p1[[1]]$se)

###########

foraging_May<-subset(proportion_May,behaviour=="3")

###########

behav_foraging_May<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1), 
                                method="REML",  
                                data=foraging_May)

p1<-plot(behav_foraging_May$gam)

for_May <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (foraging_May$proportion, na.rm=TRUE), 
                            se=p1[[1]]$se)


######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

June<-subset(gps_all,month==6) #June        
length(unique(June$tripunique)) #####55
length(unique(June$ring)) ####19

#June$ring

beh_proportion <- aggregate (June$time_interval_s, by=list(June$ring, June$tripunique, June$behaviour,June$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_June<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_June<-subset(proportion_June,behaviour=="1")

###########

behav_rest_June<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                        random=list(ring=~1), 
                        method="REML", 
                        data=rest_June) 

p1<-plot(behav_rest_June$gam)

resting_June <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (rest_June$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                            se=p1[[1]]$se)
########

travel_June<-subset(proportion_June,behaviour=="4")

###########

behav_travel_June<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                          random=list(ring=~1),
                          method="REML", 
                          data=travel_June) 
p1<-plot(behav_travel_June$gam)

traveling_June <- data.frame(x=p1[[1]]$x, 
                               y=as.numeric(p1[[1]]$fit)+ mean (travel_June$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                               se=p1[[1]]$se)

#######

preening_June<-subset(proportion_June,behaviour=="2")

###########

behav_preening_June<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1),  
                                method="REML",  
                                data=preening_June)

p1<-plot(behav_preening_June$gam)


pre_June <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (preening_June$proportion, na.rm=TRUE),
                            se=p1[[1]]$se)

###########

foraging_June<-subset(proportion_June,behaviour=="3")

###########

behav_foraging_June<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                random=list(ring=~1), 
                                method="REML",  
                                data=foraging_June)

p1<-plot(behav_foraging_June$gam)

for_June <- data.frame(x=p1[[1]]$x, 
                            y=as.numeric(p1[[1]]$fit)+ mean (foraging_June$proportion, na.rm=TRUE), 
                            se=p1[[1]]$se)



######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

July<-subset(gps_all,month==7) #July        
length(unique(July$tripunique)) #####13
length(unique(July$ring)) ####7

#July$ring

beh_proportion <- aggregate (July$time_interval_s, by=list(July$ring, July$tripunique, July$behaviour,July$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_July<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_July<-subset(proportion_July,behaviour=="1")

###########

behav_rest_July<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                       random=list(ring=~1), 
                       method="REML", 
                       data=rest_July) 

p1<-plot(behav_rest_July$gam)

resting_July <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (rest_July$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)
########

travel_July<-subset(proportion_July,behaviour=="4")

###########

behav_travel_July<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                         random=list(ring=~1),
                         method="REML", 
                         data=travel_July) 
p1<-plot(behav_travel_July$gam)

traveling_July <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (travel_July$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)

#######

preening_July<-subset(proportion_July,behaviour=="2")

###########

behav_preening_July<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1),  
                               method="REML",  
                               data=preening_July)

p1<-plot(behav_preening_July$gam)


pre_July <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_July$proportion, na.rm=TRUE),
                           se=p1[[1]]$se)

###########

foraging_July<-subset(proportion_July,behaviour=="3")

###########

behav_foraging_July<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1), 
                               method="REML",  
                               data=foraging_July)

p1<-plot(behav_foraging_July$gam)

for_July <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_July$proportion, na.rm=TRUE), 
                           se=p1[[1]]$se)

######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

August<-subset(gps_all,month==8) #August        
length(unique(August$tripunique)) #####55
length(unique(August$ring)) ####19

#August$ring

beh_proportion <- aggregate (August$time_interval_s, by=list(August$ring, August$tripunique, August$behaviour,August$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_August<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_August<-subset(proportion_August,behaviour=="1")

###########

behav_rest_August<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                       random=list(ring=~1), 
                       method="REML", 
                       data=rest_August) 

p1<-plot(behav_rest_August$gam)

resting_August <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (rest_August$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)
########

travel_August<-subset(proportion_August,behaviour=="4")

###########

behav_travel_August<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                         random=list(ring=~1),
                         method="REML", 
                         data=travel_August) 
p1<-plot(behav_travel_August$gam)

traveling_August <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (travel_August$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)

#######

preening_August<-subset(proportion_August,behaviour=="2")

###########

behav_preening_August<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1),  
                               method="REML",  
                               data=preening_August)

p1<-plot(behav_preening_August$gam)


pre_August <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_August$proportion, na.rm=TRUE),
                           se=p1[[1]]$se)

###########

foraging_August<-subset(proportion_August,behaviour=="3")

###########

behav_foraging_August<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1), 
                               method="REML",  
                               data=foraging_August)

p1<-plot(behav_foraging_August$gam)

for_August <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_August$proportion, na.rm=TRUE), 
                           se=p1[[1]]$se)


######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

September<-subset(gps_all,month==8) #September        
length(unique(September$tripunique)) #####113
length(unique(September$ring)) ####52

#September$ring

beh_proportion <- aggregate (September$time_interval_s, by=list(September$ring, September$tripunique, September$behaviour,September$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_September<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_September<-subset(proportion_September,behaviour=="1")

###########

behav_rest_September<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                       random=list(ring=~1), 
                       method="REML", 
                       data=rest_September) 

p1<-plot(behav_rest_September$gam)

resting_September <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (rest_September$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                           se=p1[[1]]$se)
########

travel_September<-subset(proportion_September,behaviour=="4")

###########

behav_travel_September<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                         random=list(ring=~1),
                         method="REML", 
                         data=travel_September) 
p1<-plot(behav_travel_September$gam)

traveling_September <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (travel_September$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)

#######

preening_September<-subset(proportion_September,behaviour=="2")

###########

behav_preening_September<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1),  
                               method="REML",  
                               data=preening_September)

p1<-plot(behav_preening_September$gam)


pre_September <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (preening_September$proportion, na.rm=TRUE),
                           se=p1[[1]]$se)

###########

foraging_September<-subset(proportion_September,behaviour=="3")

###########

behav_foraging_September<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                               random=list(ring=~1), 
                               method="REML",  
                               data=foraging_September)

p1<-plot(behav_foraging_September$gam)

for_September <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (foraging_September$proportion, na.rm=TRUE), 
                           se=p1[[1]]$se)


######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

October<-subset(gps_all,month==10) #October        
length(unique(October$tripunique)) #####113
length(unique(October$ring)) ####52

#October$ring

beh_proportion <- aggregate (October$time_interval_s, by=list(October$ring, October$tripunique, October$behaviour,October$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_October<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_October<-subset(proportion_October,behaviour=="1")

###########

behav_rest_October<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                         random=list(ring=~1), 
                         method="REML", 
                         data=rest_October) 

p1<-plot(behav_rest_October$gam)

resting_October <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (rest_October$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)
########

travel_October<-subset(proportion_October,behaviour=="4")

###########

behav_travel_October<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                           random=list(ring=~1),
                           method="REML", 
                           data=travel_October) 
p1<-plot(behav_travel_October$gam)

traveling_October <- data.frame(x=p1[[1]]$x, 
                                y=as.numeric(p1[[1]]$fit)+ mean (travel_October$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                se=p1[[1]]$se)

#######

preening_October<-subset(proportion_October,behaviour=="2")

###########

behav_preening_October<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                     -            random=list(ring=~1),  
                                 method="REML",  
                                 data=preening_October)

p1<-plot(behav_preening_October$gam)


pre_October <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (preening_October$proportion, na.rm=TRUE),
                             se=p1[[1]]$se)

###########

foraging_October<-subset(proportion_October,behaviour=="3")

###########

behav_foraging_October<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                 random=list(ring=~1), 
                                 method="REML",  
                                 data=foraging_October)

p1<-plot(behav_foraging_October$gam)

for_October <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (foraging_October$proportion, na.rm=TRUE), 
                             se=p1[[1]]$se)



######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

November<-subset(gps_all,month==11) #November        
length(unique(November$tripunique)) #####113
length(unique(November$ring)) ####52

#November$ring

beh_proportion <- aggregate (November$time_interval_s, by=list(November$ring, November$tripunique, November$behaviour,November$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_November<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_November<-subset(proportion_November,behaviour=="1")

###########

behav_rest_November<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                            random=list(ring=~1), 
                            method="REML", 
                            data=rest_November) 

p1<-plot(behav_rest_November$gam)

resting_November <- data.frame(x=p1[[1]]$x, 
                                y=as.numeric(p1[[1]]$fit)+ mean (rest_November$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                se=p1[[1]]$se)
########

travel_November<-subset(proportion_November,behaviour=="4")

###########

behav_travel_November<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                              random=list(ring=~1),
                              method="REML", 
                              data=travel_November) 
p1<-plot(behav_travel_November$gam)

traveling_November <- data.frame(x=p1[[1]]$x, 
                                   y=as.numeric(p1[[1]]$fit)+ mean (travel_November$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                   se=p1[[1]]$se)

#######

preening_November<-subset(proportion_November,behaviour=="2")

###########

behav_preening_November<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                    random=list(ring=~1),  
                                    method="REML",  
                                    data=preening_November)

p1<-plot(behav_preening_November$gam)


pre_November <- data.frame(x=p1[[1]]$x, 
                                y=as.numeric(p1[[1]]$fit)+ mean (preening_November$proportion, na.rm=TRUE),
                                se=p1[[1]]$se)

###########

foraging_November<-subset(proportion_November,behaviour=="3")

###########

behav_foraging_November<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                    random=list(ring=~1), 
                                    method="REML",  
                                    data=foraging_November)

p1<-plot(behav_foraging_November$gam)

for_November <- data.frame(x=p1[[1]]$x, 
                                y=as.numeric(p1[[1]]$fit)+ mean (foraging_November$proportion, na.rm=TRUE), 
                                se=p1[[1]]$se)


######
####
####
####

gps_all$month<-month(gps_all$dateTime_gps)

December<-subset(gps_all,month==12) #December        
length(unique(December$tripunique)) #####113
length(unique(December$ring)) ####52

#December$ring

beh_proportion <- aggregate (December$time_interval_s, by=list(December$ring, December$tripunique, December$behaviour,December$h),sum)
colnames (beh_proportion) <- c("ring","tripunique", "behaviour","hour","beh_sum")
proportion_December<-beh_proportion %>%
  dplyr::group_by(hour, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_December<-subset(proportion_December,behaviour=="1")

###########

behav_rest_December<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                          random=list(ring=~1), 
                          method="REML", 
                          data=rest_December) 

p1<-plot(behav_rest_December$gam)

resting_December <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (rest_December$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)
########

travel_December<-subset(proportion_December,behaviour=="4")

###########

behav_travel_December<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                            random=list(ring=~1),
                            method="REML", 
                            data=travel_December) 
p1<-plot(behav_travel_December$gam)

traveling_December <- data.frame(x=p1[[1]]$x, 
                                 y=as.numeric(p1[[1]]$fit)+ mean (travel_December$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                 se=p1[[1]]$se)

#######

preening_December<-subset(proportion_December,behaviour=="2")

###########

behav_preening_December<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                  random=list(ring=~1),  
                                  method="REML",  
                                  data=preening_December)

p1<-plot(behav_preening_December$gam)


pre_December <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (preening_December$proportion, na.rm=TRUE),
                              se=p1[[1]]$se)

###########

foraging_December<-subset(proportion_December,behaviour=="3")

###########

behav_foraging_December<- gamm(proportion ~ s(hour,bs = "cc",  k=23), 
                                  random=list(ring=~1), 
                                  method="REML",  
                                  data=foraging_December)

p1<-plot(behav_foraging_December$gam)

for_December <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (foraging_December$proportion, na.rm=TRUE), 
                              se=p1[[1]]$se)






gam_plot_foraging_month <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=foraging_January, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#FF0000")+
  ## gam
  geom_line(data=for_January, aes(x=x, y=y), colour="grey75", linetype="solid")+
  geom_ribbon(data=for_January, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#FF0000") +
  geom_point(data=foraging_February, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#FF8B00")+
  ## gam
  geom_line(data=for_February, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_February, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#FF8B00") +
  
  geom_point(data=foraging_March, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#E8FF00")+
  ## gam
  geom_line(data=for_March, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_March, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#E8FF00") +
  
  geom_point(data=foraging_April, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#5DFF00")+
  ## gam
  geom_line(data=for_April, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_April, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#5DFF00") +
  
  geom_point(data=foraging_May, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#00FF2E")+
  ## gam
  geom_line(data=for_May, aes(x=x, y=y), colour="grey75", linetype="solid")+
  geom_ribbon(data=for_May, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#00FF2E") +
  geom_point(data=foraging_June, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#00FFB9")+
  ## gam
  geom_line(data=for_June, aes(x=x, y=y), colour="grey75", linetype="solid")+
  geom_ribbon(data=for_June, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#00FFB9") +
  geom_point(data=foraging_July, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#00B9FF")+
  ## gam
  geom_line(data=for_July, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_July, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#00B9FF") +
  geom_point(data=foraging_August, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#002EFF")+
  ## gam
  geom_line(data=for_August, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_August, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#002EFF") +
  

  geom_point(data=foraging_September, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#5D00FF")+
  ## gam
  geom_line(data=for_September, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  
  
  geom_ribbon(data=for_September, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#5D00FF") +
  geom_point(data=foraging_October, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#E800FF")+
  ## gam
  geom_line(data=for_October, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_October, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#E800FF") +
  geom_point(data=foraging_November, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#FF008B")+
  ## gam
  geom_line(data=for_November, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_November, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#FF008B") +

  geom_point(data=foraging_December, aes(x=hour, y=proportion), alpha=0.1, size = 0.01, colour = "#FF0000")+
  ## gam
  geom_line(data=for_December, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_December, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.2, fill = "#FF0000") +

  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base


####make a boxplot of prop_foraging by month?? 













##############################################################################################################################################################################################################Summarize per day to get how the proportion of different behaviours varry throughout the year 
##############################################################################################################################################################################################################
gps_all$doy <- as.numeric(strftime(gps_all$dateTime_gps, format = "%j"))


names(gps_all)

####take only daytime for there
day01<-gps_all
day01<-subset(gps_all, day01 == 1)

female2<-subset(day01,Sex=="female")
#brood$ring

beh_proportion <- female2 %>%
  group_by(ring, tripunique, behaviour, doy)%>%
  dplyr::summarize(beh_sum = sum(time_interval_s), na.rm=TRUE)
  
beh_proportion$beh_sum<-ifelse(is.na(beh_proportion$beh_sum), 0, beh_proportion$beh_sum)


proportion_female2<-beh_proportion %>%
  dplyr::group_by(doy, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_female2<-subset(proportion_female2,behaviour=="1")

#######faire le gamm pour brooding and incubation

###########

behav_rest_female2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                           # the k is the number of nodes (?)
                           random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                           method="REML",  # I do not know what f*** is this
                           data=rest_female2) # our dataframe



p1<-plot(behav_rest_female2$gam)



resting_female2 <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (rest_female2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)







###################################################################################################################################################################################################################
travel_female2<-subset(proportion_female2,behaviour=="4")
###########

behav_travel_female2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                             # the k is the number of nodes (?)
                             random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                             method="REML",  # I do not know what f*** is this
                             data=travel_female2) # our dataframe


summary(behav_travel_female2$gam)

p1<-plot(behav_travel_female2$gam)



traveling_female2 <- data.frame(x=p1[[1]]$x, 
                             y=as.numeric(p1[[1]]$fit)+ mean (travel_female2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                             se=p1[[1]]$se)



foraging_female2<-subset(proportion_female2,behaviour=="2" |behaviour=="3")

#######faire le gamm pour female2 and incubation


behav_foraging_female2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                               # the k is the number of nodes (?)
                               random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                               method="REML",  # I do not know what f*** is this
                               data=foraging_female2) # our dataframe



p1<-plot(behav_foraging_female2$gam)


for_female2 <- data.frame(x=p1[[1]]$x, 
                      y=as.numeric(p1[[1]]$fit)+ mean (foraging_female2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                      se=p1[[1]]$se)




###################################################################################################################################################################################################################


preening_female2<-subset(proportion_female2,behaviour=="2")

#######faire le gamm pour female2 and incubation



preening_female2$ID<-preening_female2$tripunique
###########

behav_preening_female2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=preening_female2) # our dataframe




p1<-plot(behav_preening_female2$gam)



pre_female2 <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (preening_female2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)

##################################################################


foraging_female2<-subset(proportion_female2,behaviour=="3")

foraging_female2$ID<-foraging_female2$tripunique
###########

behav_foraging_female2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                                   # the k is the number of nodes (?)
                                   random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                   method="REML",  # I do not know what f*** is this
                                   data=foraging_female2) # our dataframe





p1<-plot(behav_foraging_female2$gam)



for_female2 <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (foraging_female2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)


######

#####

#####

#####

male2<-subset(day01,Sex=="male")
#brood$ring

beh_proportion <- male2 %>%
  group_by(ring, tripunique, behaviour, doy)%>%
  dplyr::summarize(beh_sum = sum(time_interval_s))

beh_proportion$beh_sum<-ifelse(is.na(beh_proportion$beh_sum), 0, beh_proportion$beh_sum)


proportion_male2<-beh_proportion %>%
  dplyr::group_by(doy, tripunique) %>%
  dplyr::mutate(proportion=(beh_sum/sum(beh_sum)))



rest_male2<-subset(proportion_male2,behaviour=="1")

#######faire le gamm pour brooding and incubation



rest_male2$ID<-rest_male2$tripunique
###########

behav_rest_male2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                          # the k is the number of nodes (?)
                          random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                          method="REML",  # I do not know what f*** is this
                          data=rest_male2) # our dataframe


summary(behav_rest_male2$gam)
p1<-plot(behav_rest_male2$lme)



resting_male2 <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (rest_male2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)

###################################################################################################################################################################################################################
travel_male2<-subset(proportion_male2,behaviour=="4")
###########

behav_travel_male2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                            # the k is the number of nodes (?)
                            random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                            method="REML",  # I do not know what f*** is this
                            data=travel_male2) # our dataframe


summary(behav_travel_male2$gam)

p1<-plot(behav_travel_male2$gam)



traveling_male2 <- data.frame(x=p1[[1]]$x, 
                                 y=as.numeric(p1[[1]]$fit)+ mean (travel_male2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                                 se=p1[[1]]$se)



foraging_male2<-subset(proportion_male2,behaviour=="2" |behaviour=="3")

#######faire le gamm pour male2 and incubation


behav_foraging_male2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                              # the k is the number of nodes (?)
                              random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                              method="REML",  # I do not know what f*** is this
                              data=foraging_male2) # our dataframe



p1<-plot(behav_foraging_male2$gam)


for_male2 <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (foraging_male2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                          se=p1[[1]]$se)




###################################################################################################################################################################################################################


preening_male2<-subset(proportion_male2,behaviour=="2")

#######faire le gamm pour male2 and incubation



preening_male2$ID<-preening_male2$tripunique
###########

behav_preening_male2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                                  # the k is the number of nodes (?)
                                  random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                  method="REML",  # I do not know what f*** is this
                                  data=preening_male2) # our dataframe




p1<-plot(behav_preening_male2$gam)



pre_male2 <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (preening_male2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)

##################################################################


foraging_male2<-subset(proportion_male2,behaviour=="3")

foraging_male2$ID<-foraging_male2$tripunique
###########

behav_foraging_male2<- gamm(proportion ~ s(doy,bs = "cc",  k=36), # the formula: the response variable ~ the independent variable
                                  # the k is the number of nodes (?)
                                  random=list(ring=~1), # the random factor that we want to take into account (always a list!)
                                  method="REML",  # I do not know what f*** is this
                                  data=foraging_male2) # our dataframe





p1<-plot(behav_foraging_male2$gam)



for_male2 <- data.frame(x=p1[[1]]$x, 
                              y=as.numeric(p1[[1]]$fit)+ mean (foraging_male2$proportion, na.rm=TRUE), #mean(calc_act_hour_hot$prop_wet_hour_mean) 
                              se=p1[[1]]$se)

#####################lets see how we can plot these 



gam_plot_int_rest_sex<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=rest_male2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=resting_male2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_male2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=rest_female2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=resting_female2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=resting_female2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="DOY", limits = c(0, 365))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_preening_sex<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=preening_male2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pre_male2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_male2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=preening_female2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=pre_female2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pre_female2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="DOY", limits = c(0, 365))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base



gam_plot_foraging_sex<- 
  ggplot()+
  ## phenology lines
  ## poexts
  geom_point(data=foraging_male2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=for_male2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_male2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=foraging_female2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=for_female2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=for_female2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="DOY", limits = c(0, 365))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base




gam_plot_travel_sex<- 
  ggplot()+
  ## phenology lines
  ## potravels
  geom_point(data=travel_male2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=traveling_male2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_male2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  geom_point(data=travel_female2, aes(x=doy, y=proportion), alpha=0.1, size = 0.01, colour = "violetred")+
  ## gam
  geom_line(data=traveling_female2, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=traveling_female2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "violetred")+
  
  scale_x_continuous(name ="DOY", limits = c(0, 365))+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Proportion of Time")+
  theme_base


#########################################################################################################################################################################################################################################################################################################################################Prop wet models and plots 
#####################################################################################################################################################################################################################################################################


all<-readRDS("D:/Dropbox/sarah_fenha/gps_geo_1m_organized_propwet.rds")

all<-subset(all, !geo == "J464001" & 	!geo == "BD818001" 	& 	!geo == "BJ486001" ) #####FIGURE OUT WHY THIS DIDNT WORK LATER

all$PropWet<-ifelse(all$DurDry > 1200 | all$DurWet > 1200, NA,all$PropWet )  ### there are 116 points like this where the time wet or dry is huge... probably because of gaps in the tracking data. Turn these to NA

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



all<-all[with(all, order(geo,contBlock, dt)),]
all$date<-date(all$dt)
library(suncalc)

sun<-getSunlightTimes(data = all, 
                      keep = c("dusk", "dawn", "sunrise", "sunset", "night", "nightEnd"), tz = "GMT")


all<- merge(all, sun, by = c("date", "lat", "lon"))


sun2<-getSunlightPosition(data = all)

all<-merge(all, sun2, by = c("date", "lat", "lon"))


all$day01<-ifelse(all$dt > all$dawn & all$dt < all$dusk, 1, 0) ####better use this??

all$day<-ifelse(all$day01 ==1, "day", "night")


all$time<- strftime(all$dt, format="%H:%M:%S")
all$time <- as.POSIXct(all$time, format="%H:%M:%S") ###bullshit date


require(lubridate)
clockS = function(t){hour(t)*3600+minute(t)*60+second(t)}
all$time<-clockS(all$time) #seconds since midnight
all$time<-all$time/3600 #hour since midnight


all$time_round<- round(all$time)


mean_sun_altitude <- all %>% 
  group_by(time_round)%>% 
  dplyr::summarize (mean_sun_altitude =mean(altitude))

mean_sun_altitude$time_end<-as.numeric(mean_sun_altitude$time_round)+1



plot_time<-ggplot(all, aes(x=time, y=PropWet)) + 
  geom_point()+
  geom_smooth() +
  theme_base + 
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), labels = c( "00:00", "2:00", "4:00","6:00", "8:00","10:00", "12:00","14:00", "16:00","18:00", "20:00", "22:00"))

#####hoping to put this as the background of the gam
gg <- ggplot()
gg <- gg + geom_rect(data=mean_sun_altitude, aes(ymin=0, ymax=1, xmin=time_round,
                                                 xmax=time_end, fill=mean_sun_altitude), alpha =0.5) + scale_fill_gradient2(low = "darkgrey", mid = "lightgrey", high = "white", midpoint = 0)





sex<-unique(select(gps_all, tripunique, Sex))

all<-left_join(all, sex, by= "tripunique")


birdinfo<-read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Most_recent_GPS_info/birdInfo_tropicbirds_ALL.csv")

birdinfo<-dplyr::select(birdinfo,refID=refID1, ring)

all<-left_join(all, birdinfo, by="refID")

all$doy <- as.numeric(strftime(all$dt, format = "%j"))

all$month<-month(all$dt)

#library(gamm4)

all<-subset(all, !is.na(PropWet))
all_model<-subset(all, !(lifeCycleStatus =="breeding"))
all_model$propwet01<-ifelse(all_model$PropWet > 0, 1, 0)


M1 <- gamm(propwet01~ island + Sex+ lifeCycleStatus + s(time,bs = "cc",  k=23),random=list(ring=~1), family = "binomial", method="REML",  data=all_model)
AIC(M1$lme)

M2 <- gamm(propwet01~ Sex + lifeCycleStatus +  s(time,bs = "cc",  k=23),random=list(ring=~1), family = "binomial", method="REML",  data=all_model)

AIC(M2$lme)

M3 <- gamm4(propwet01~ Sex +  s(time,bs = "cc",  k=23),random=list(ring=~1), family = "binomial", method="REML",  data=all_model)

AIC(M1$lme, M2$lme, M3$lme)

p1<-plot(M2$gam)

summary(M1$gam)
summary(M1$lme)






all_model_inc<-subset(all_model, lifeCycleStatus == "incubation")
all_model_chick<-subset(all_model, lifeCycleStatus == "chick-rearing")





pwet_inc<- gamm(propwet01 ~  s(time, bs = "cc",  k=23),random=list(ring=~1), method="REML",data=all_model_inc) # our dataframe

pwet_chick<- gamm(propwet01 ~  s(time, bs = "cc",  k=23),random=list(ring=~1), method="REML",data=all_model_chick) # our dataframe


p1<-plot(pwet_inc$gam)


pwet_inc_val <- data.frame(x=p1[[1]]$x, 
                          y=as.numeric(p1[[1]]$fit)+ mean (all_model_inc$propwet01, na.rm=TRUE), se=p1[[1]]$se)

p1<-plot(pwet_chick$gam)

pwet_chick_val <- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (all_model_chick$propwet01, na.rm=TRUE), se=p1[[1]]$se)




gam_plot_breeding_stage_propWet <- 
  ggplot()+
  geom_point(data=all_model_inc, aes(x=time, y=propwet01), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pwet_inc_val, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pwet_inc_val, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  #########

geom_point(data=all_model_chick, aes(x=time, y=propwet01), alpha=0.1, size = 0.01, colour = "gold")+
  ## gam
  geom_line(data=pwet_chick_val, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pwet_chick_val, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "gold")+
  
  scale_x_continuous(name ="Hour", limits = c(0, 24), breaks = seq(0, 24, 3))+
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25), name= "Proportion wet")+
  theme_base



















all_moon<-getMoonIllumination(date = all$date, keep = c("fraction", "phase","angle"))

all<-merge(all, all_moon, by= "date")
all_model<- merge(all_model, all_moon, by = "date")

night_all<-subset(all, day01 == 0)
night_all_model<-subset(all_model, day01 == 0)
night_all_model$fraction_corrected_for_anlge <-ifelse(night_all_model$angle <0, 0,night_all_model$fraction )


gam.moon <- gamm(propwet01~  s(fraction_corrected_for_anlge,  k=10), random=list(ring=~1), family = "binomial", method="REML",  data=night_all_model)


p1<-plot(gam.moon$gam)
pwet_moon<- data.frame(x=p1[[1]]$x, 
                           y=as.numeric(p1[[1]]$fit)+ mean (night_all_model$propwet01, na.rm=TRUE), se=p1[[1]]$se)

 
  ggplot()+
  geom_point(data=night_all_model, aes(x=time, y=Prop), alpha=0.1, size = 0.01, colour = "blue")+
  ## gam
  geom_line(data=pwet_moon, aes(x=x, y=y), colour="grey75", linetype="solid")+
  
  geom_ribbon(data=pwet_moon, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.3, fill = "blue") +
  
  #########


  m <- glmer(propwet01 ~ fraction_corrected_for_anlge +
               (1 | ring), data = night_all_model, family = binomial, control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 10)

plot(m) ####nope





#######################################################################################################################################################################################################We can now check whether the real data has different env variables in the different behaviours

head(realz)


hist(realz$fsle_7days, breaks = 100) ###normal but maybe get rid of super negative values? 
hist(realz$CHLA_3month , breaks = 100) ###gamma
hist(realz$SST_3month, breaks = 50) ###sorta normal?


realz$behaviour<-realz$fitTrack_FULL_4state_constraints_propWet_FIXPAR_known_state


tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/real_behaviour_fsle.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = realz, aes(x = behaviour, y = fsle_7days))  + theme_base + geom_jitter(color = "grey", size = 0.2, alpha = 0.9) + geom_boxplot(alpha =0.2)
dev.off()

realz_fsle<-subset(realz, !is.na(realz) | ! is.na (behaviour))
flse_M1 <- lme(fsle_7days~ behaviour + (1|ring),  data= realz_fsle)
summary(flse_M1)

tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/real_behaviour_CHLA_3month.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = realz, aes(x = behaviour, y = CHLA_3month))  + theme_base + geom_jitter(color = "grey", size = 0.2, alpha = 0.9) + geom_boxplot(alpha =0.2)
dev.off()



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/real_behaviour_SST_3month.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = realz, aes(x = behaviour, y = SST_3month))  + theme_base + geom_jitter(color = "grey", size = 0.2, alpha = 0.9) + geom_boxplot(alpha =0.2)
dev.off()


tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/real_behaviour_CHLA_monthly.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = realz, aes(x = behaviour, y = CHLA_monthly))  + theme_base + geom_jitter(color = "grey", size = 0.2, alpha = 0.9) + geom_boxplot(alpha =0.2)
dev.off()





############################################read in wind direction and speed






tidy_ww<-readRDS("D:/Dropbox/sarah_fenha/Saved_RData_Sarah/Wind_dataframe_sarah.rds")
tidy_ww<-select(tidy_ww, date = time, round_lat = lat, round_lon= lon, dir, speed)
tidy_ww$date <- ymd(tidy_ww$date)
realz$date<-ymd(date(realz$dateTime_gps))

realz$round_lat<-plyr::round_any(realz$lat, 0.5)
realz$round_lon <- plyr::round_any(realz$lon, 0.5)

realz<-left_join(realz, tidy_ww, by= c("date", "round_lat", "round_lon"))

realz$behaviour<-realz$fitTrack_FULL_4state_constraints_propWet_FIXPAR_known_state

tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/real_behaviour_wind.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = realz, aes(x = behaviour, y = speed))  + theme_base + geom_jitter(color = "grey", size = 0.2, alpha = 0.9) + geom_boxplot(alpha =0.2)
dev.off()

#######now lets model the different env variables vs 


gps_all<-subset(realz, complete =="c")


####calculate bearing 

gps_all<-gps_all %>% distinct(time, lat, lon, tripunique, .keep_all = TRUE)


gps_all <- gps_all %>% arrange(tripunique, time)

gps_all<-gps_all %>%
  group_by(tripunique)%>%
  mutate(lat_next= lead(lat, default = NA))%>%
  mutate(lon_next= lead(lon, default = NA))%>%
  ungroup() ####lead instead of lag if I want it to the next point

library(data.table)
gps_all<-data.table(gps_all)
gps_all[, lat_next := replace(lat_next, .N, NA), by = tripunique]
gps_all[, lon_next := replace(lon_next, .N, NA), by = tripunique]

#gps_all<-gps_all %>%
#  group_by(tripunique) %>%
#  mutate(lat_lag= lag(lat, default = first(lat), order_by = time))%>%
#  mutate(lon_lag= lag(lon, default = first(lon), order_by = time))%>%
#  ungroup() ####lead instead of lag if I want it to the next point



####calculate bearing between one position and the next
A<-data.matrix(dplyr::select(gps_all,  lon, lat))
B<-data.matrix(dplyr::select(gps_all, lon_next, lat_next))

library(geosphere)

gps_all$bear_next_2<- bearing(A,B)
gps_all$bear_next<- bearing(A)
hist(gps_all$bear_next)
hist(gps_all$bear_next_2)


####replace last values of each unique trip by NA (so it doesn't calculate bearing between trips)
gps_all[, bear_next := replace(bear_next, .N, NA), by = tripunique]
gps_all[, bear_next := replace(bear_next, .N, NA), by = tripunique]



gps_all$bear_next360<-ifelse(gps_all$bear_next >= 0, gps_all$bear_next , gps_all$bear_next +360 )

hist(gps_all$bear_next360)



#summary(gps_all$bear_next360)
  
  
#  ####calculate bearing between one position and the next
#B<-data.matrix(dplyr::select(gps_all,  lon, lat))
#A<-data.matrix(dplyr::select(gps_all, lon_lag, lat_lag))

#gps_all$bear_lag<- bearing(A, B)


all<-read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/All/all_ready_for_HMMs.csv", sep=";")

all$dt<-ymd_hms(all$dt)
all$date<-ymd_hms(all$date)
all$dusk<-ymd_hms(all$dusk)
all$sunrise<-ymd_hms(all$sunrise)
all$sunset<-ymd_hms(all$sunset)
all$night<-ymd_hms(all$night)

all$nightEnd<-ymd_hms(all$nightEnd)
all$date_hour<-ymd_hms(all$date_hour)

####we need the speed and the step length to be in the same format. right now speed is m/s

all<-all[with(all, order(geo,contBlock, dt)),]
all<-select(all, tripunique, lat, lon, time = dt,  speed_current = speed, dir_current = dir)





gps_all<-left_join(gps_all, all, by=c("tripunique", "lat", "lon", "time"))


gps_all<-gps_all %>%
  arrange(tripunique, time) %>%
  group_by(tripunique) %>%
  mutate(diff = abs(time - lead(time)),
         diff_secs = abs(as.numeric(diff, units = 'secs')))


gps_all$bird_speed<-gps_all$step/300 *1000

gps_all$bird_speed<- ifelse(gps_all$diff_secs > 300, NA, gps_all$bird_speed)

summary(gps_all$bird_speed)



gps_all$behaviour<-gps_all$fitTrack_FULL_4state_constraints_propWet_FIXPAR_known_state

gps_all$round_bearing<-ceiling((gps_all$bear_next360+0.000000001)/10) *10
gps_all$round_current<-ceiling((gps_all$dir_current+0.000000001)/10) *10
gps_all$round_wind<-ceiling((gps_all$dir+0.000000001)/10) *10

gps_plots<-gps_all%>%
  dplyr::group_by(round_bearing) %>%
  dplyr::summarize(number_angles= length(lat) )

ggplot(gps_plots, aes(x = round_bearing, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing from each point to the next while resting") +
  theme_bw()



gps_plots_current<-gps_all%>%
  dplyr::group_by(round_current) %>%
  dplyr::summarize(number_angles= length(bear_next) )


ggplot(gps_plots_current, aes(x = round_current, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing of currents") +
  theme_bw()


gps_plots_wind<-gps_all%>%
  dplyr::group_by(round_wind) %>%
  dplyr::summarize(number_angles= length(bear_next) )


ggplot(gps_plots_wind, aes(x = round_wind, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing of wind") +
  theme_bw()




resting<-subset(gps_all, behaviour == "1")

gps_plots_resting<-resting%>%
  dplyr::group_by(round_bearing) %>%
  dplyr::summarize(number_angles= length(bear_next) )


ggplot(gps_plots_resting, aes(x = round_bearing, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing from each point to the next while resting") +
  theme_bw()



traveling<-subset(gps_all, behaviour == "4")

hist(traveling$bear_next360)


gps_plots_traveling<-traveling%>%
  dplyr::group_by(round_bearing) %>%
  dplyr::summarize(number_angles= length(bear_next) )


ggplot(gps_plots_traveling, aes(x = round_bearing, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing from each point to the next while traveling") +
  theme_bw()




for<-subset(gps_all, behaviour == "3")

hist(for$bear_next360)


for<-for%>%
  dplyr::group_by(round_bearing) %>%
  dplyr::summarize(number_angles= length(bear_next) )


ggplot(for, aes(x = round_bearing, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "darkblue", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing from each point to the next in 3") +
  theme_bw()























angle_diff <- function(theta1, theta2){
  theta <- abs(theta1 - theta2) %% 360 
  return(ifelse(theta > 180, 360 - theta, theta))
}


####calculate difference between wind bearing and bearing of a traveling point

gps_all$bearing_diff<- angle_diff(gps_all$dir, gps_all$bear_next360)

traveling$bearing_diff<- angle_diff(traveling$dir, traveling$bear_next360)



summary(gps_all$bearing_diff)
hist(gps_all$bearing_diff, breaks= 50)






foraging<-subset(gps_all, behaviour == "3")


gps_plots_foraging<-foraging%>%
  dplyr::group_by(round_bearing) %>%
  dplyr::summarize(number_angles= length(bear_next) )




ggplot(gps_plots_current, aes(x = round_current, y = number_angles)) +
  coord_polar(theta = "x", start = -.01) +
  geom_bar(stat = "identity", fill = "maroon4", width = 8) +
  #geom_hline(yintercept = seq(0, 12000, by = 1000), color = "grey80", size = 0.3) +
  scale_x_continuous( breaks = c(0, 10,20,30,40, 50, 60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360), expand = c(.002,0)) +
  labs(x = "Bearing", y = "Number of points", title = "Bearing from each point to the next while traveling") +
  theme_bw()



ggplot(resting, aes(x=bear_next360)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0.1, 359.9)) + theme_bw()

ggplot(int_foraging, aes(x=bear_next360)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 359)) + theme_bw()


ggplot(foraging, aes(x=bear_next360)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 359)) + theme_bw()


ggplot(traveling, aes(x=bear_next360)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 359)) + theme_bw()



ggplot(subset(resting, approximate_speed_bird<0.8),aes(x=approximate_speed_bird, y = speed_current)) + geom_point(alpha =0.5) +
  geom_smooth(method='lm', formula= y~x)+
  theme_base





ggplot(subset(int_foraging, approximate_speed_bird<1),aes(x=approximate_speed_bird, y = speed_current)) + geom_point(alpha =0.5) +
  geom_smooth(method='lm', formula= y~x)+
  theme_base

ggplot(subset(traveling, approximate_speed_bird<20), aes(x=approximate_speed_bird, y = speed_current)) + geom_point(alpha =0.5) +
  geom_point(alpha =0.5) +
  geom_smooth(method='lm', formula= y~x)+
  theme_base


##############################################################################################################################################################################################################################Now for the wind 

####calculate bearing 
read(realz)

###get first row per trip

first<-realz[!duplicated(realz$tripunique),]






ggplot(first, aes(x=dir)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 359)) + theme_bw()


ggplot(first, aes(x=bear_next360)) + 
  geom_histogram(position="identity", alpha=0.5, fill = "royalblue1") + 
  coord_polar()+
  scale_x_continuous(limits = c(0, 359)) + theme_bw()




###################################################################################################################################Now heatmaps


head(gps_all)

gps_all$year<-year(gps_all$dateTime_gps)
table(gps_all$year)
gps_all$date<-ymd(str_sub(gps_all$dateTime_gps, 1,10))

gps_all_2015<-subset(gps_all, year == 2015)
gps_all_2016<-subset(gps_all, year == 2016)
gps_all_2017<-subset(gps_all, year == 2017)
gps_all_2018<-subset(gps_all, year == 2018)
gps_all_2019<-subset(gps_all, year == 2019)
gps_all_2020<-subset(gps_all, year == 2020)
gps_all_2021<-subset(gps_all, year == 2021)


library(oce)
# place <- c(-15.7886, 27.8444)  ## (long, lat)
start_date <- as.character(min(gps_all$dateTime_gps))
end_date <- as.character(max(gps_all$dateTime_gps))
seq_date <- seq(from = as.POSIXct(start_date, tz="UTC"), 
                to = as.POSIXct(end_date, tz="UTC"), by="day")
moon_data <- moonAngle(t = seq_date, longitude = -23.777352, latitude = 15.85740)$illuminatedFraction
moon <- data.frame(date = as.Date(seq_date), moonill = moon_data)

plot(moon$date, moon$moonill, type="l")

#### Join with activity data 
gps_all <-
  gps_all %>% 
  left_join(moon, by = "date")




sunrise<- getSunlightTimes(data = gps_all, keep = c("sunrise"), tz = "UTC")
sunset<- getSunlightTimes(data = gps_all, keep = c("sunset"), tz = "UTC")


gps_all$sunrise<-sunrise$sunrise
gps_all$sunset<-sunset$sunset





####need to summarize proportion of time in ext foraging for all birds per hour per day. 

gps_all_2017$hour<-as.numeric(hour(gps_all_2017$dateTime_gps))
gps_all_2017$doy<- as.numeric(strftime(gps_all_2017$dateTime_gps, format = "%j"))

avez<-gps_all_2017 %>%
  group_by(hour, date, behaviour)%>%
  summarize(rows = n(), 
            nind = length(unique(ring)))
            
library(reshape2)
            
avez2<-avez %>%
group_by(hour, date)%>%
summarize( totalrows = sum(rows))
            
avez<-merge(avez, avez2, by= c("hour", "date"))
head(avez)
            
avez_for<-subset(avez, behaviour == "3")
            
avez_for$prop<-avez_for$rows/avez_for$totalrows
            
head(avez_for)
avez_for$ind_alpha<- ((avez_for$nind)-min(avez_for$nind))/(max(avez_for$nind)-min(avez_for$nind))
  

####with moon          
plot_heatmap <- 
ggplot(data=avez_for)+
geom_tile(aes(x=date, y=hour, fill=prop), colour = NA)+
scale_fill_gradient(low="cyan", high="red", name = "foraging")+
theme_base +
  geom_line(data=gps_all_2017, aes(x=date, y=moonill*24), size = 1.2) # colour= "white"
          


#####with sunrise
plot_heatmap +
  geom_line(data = track_ind, aes(x=date, y=sunrise_trj_hour_dec), colour = "black")+
  geom_line(data = track_ind, aes(x=date, y=sunset_trj_hour_dec), colour = "black")




  
            
###############################################################################################################################################################
####need to summarize proportion of time in ext foraging for all birds per hour per day. 

gps_all$hour<-as.numeric(hour(gps_all$dateTime_gps))
gps_all$doy<- as.numeric(strftime(gps_all$dateTime_gps, format = "%j"))

 gps_all<-gps_all%>%
   dplyr::group_by(doy)%>%
   dplyr::mutate(mean_sunrise =  mean(as.numeric(hour(sunrise)) + as.numeric(minute(sunrise))/60 + as.numeric(second(sunrise))/3600 ), 
                 mean_sunset = mean(as.numeric(hour(sunset)) + as.numeric(minute(sunset))/60 + as.numeric(second(sunset))/3600))
                                                 




library(suncalc)





avez<-gps_all %>%
  group_by(hour, doy, behaviour)%>%
  summarize(rows = n(), 
            nind = length(unique(ring)))

library(reshape2)

avez2<-avez %>%
  group_by(hour, doy)%>%
  summarize( totalrows = sum(rows))

avez<-merge(avez, avez2, by= c("hour", "doy"))
head(avez)

avez_for<-subset(avez, behaviour == "3")

avez_for$prop<-avez_for$rows/avez_for$totalrows

head(avez_for)
avez_for$ind_alpha<- ((avez_for$nind)-min(avez_for$nind))/(max(avez_for$nind)-min(avez_for$nind))



plot_heatmap <- 
  ggplot(data=avez_for)+
  geom_tile(aes(x=doy, y=hour, fill=prop), colour = NA)+
  scale_fill_gradient(low="cyan", high="red", name = "foraging")+
  theme_base +  
  geom_line(data = gps_all, aes(x=doy, y=mean_sunrise), colour = "black")+
  geom_line(data = gps_all, aes(x=doy, y=mean_sunset), colour = "black")

#############################################
            























head(gps_all)

library("lme4")


M1_SPEED <- lme(speed ~ behaviour,
                 data=gps_all, method="REML",
                random = ~ 1 | ring, 
             control=list(maxIter=10000, niterEM=10000))






M1_fsle_7days <- lme(fsle_7days ~ behaviour,
                data=gps_all_flse, method="REML",
                random = ~ 1 | ring, 
                control=list(maxIter=10000, niterEM=10000))

summary(M1_SPEED)
plot(M1_SPEED)
 
library(emmeans)

emmeans(M1_SPEED, list(pairwise ~ behaviour), adjust = "tukey")
