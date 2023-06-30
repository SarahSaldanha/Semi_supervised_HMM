library(dplyr)

library(tidyverse)

##install.packages("irr")
library(irr)

library(agreement)
library(reshape2)

op <- options(digits.secs=6)
#options(expressions = 20000)
#memory.limit(size=99999999)



###MAKE FUNCTION THAT OPENS EACH FILE, EXTRACTS THE CONFUSION MATRIX
all_iterations<-c("iteration1", "iteration2", "iteration3", "iteration4", "iteration5", "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")




for (iteration in all_iterations){
  #for(ii in seq(0.00,0.463, by = 0.04)){
    for(ii in c(0,0.09)){
  #iteration<-all_iterations[1]
  #ii = 0
  
 data<-readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/", iteration, "/Final_birdPrep_states_extracted_AWS_3states_", ii, ".rds"))
 
 test_states<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds")
 
 test_states<-select(test_states,true_state_act_inactive, ID = ring_dep_trip, time =dateTime_gps, x =lon, y = lat)
 data<-merge(data, test_states, by = c("ID", "time", "x", "y"), all.x = T)
 
 #data<-subset(data, is.na(true_state))
 
 
 
 
 st<-select( data, state, true_state_act_inactive)
 
 saveRDS(st, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/All_states_for_consistency_analysis/P_", ii, "/", iteration, "_state.rds"))
 }
}


#kp <- vector()




# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###now merge all the datasets within a folder into one. 

for(ii in c(0,0.09)){
  #ii<-0
  
  p <-paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/All_states_for_consistency_analysis/P_", ii, "/")
  setwd(p)
  
df <- list.files(path = p) %>%
  map(readRDS) %>% 
  bind_cols()

df<-select(df,state...1, state...3,  state...5, state...7, state...9, state...11, state...13, state...15, state...17, state...19, true_state_act_inactive...2)

names(df) <- c("iteration1", "iteration2","iteration3", "iteration4","iteration5","iteration6","iteration7","iteration8","iteration9","iteration10", "true_state_act_inactive")  


data<-readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/", iteration, "/Final_birdPrep_states_extracted_AWS_3states_", ii, ".rds"))

test_states<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds")

test_states<-select(test_states,true_state_act_inactive, ID = ring_dep_trip, time =dateTime_gps, x =lon, y = lat)
data<-merge(data, test_states, by = c("ID", "time", "x", "y"), all.x = T)

#data<-subset(data, is.na(true_state))
data<-select(data, ID,  time, x, y, true_state_act_inactive)

df<-cbind(data, df)

df$point<-seq_along(1: nrow(df))


df<- melt(df, id.vars = c("point", "ID", "time", "x", "y", "true_state_act_inactive"))

names(df)<- c("point", "ID", "time", "x", "y", "true_state_act_inactive", "itt", "state")
df$dummy<-1

df<-df %>%
  dplyr::group_by(point, ID, time, x, y,true_state_act_inactive)%>%
  dplyr::summarize(most_common_behaviour = getmode(state),
         p_consistency = sum(dummy [state == most_common_behaviour])/10)%>%
  ungroup()



head(df)
  

df$prop_ks<-ii


# 
# 
# 
# ##for (ii in )
# 
# df <- as.matrix(df)
# a<-kripp.alpha(df, method = "nominal")
# 
# Proportion<-c(ii)
# kripp<-c(a$value)
# 
# x<-data.frame( Proportion,  kripp)

saveRDS(df, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/Percent_consistency/Proportion_",ii ,".rds"))

}

####now group these back together and plot?? 
setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/Percent_consistency")


Merged_krippendorff <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/Percent_consistency/"), readRDS))

# 
# library(mgcv)
# 
# my_theme <-
#   theme_light()+
#   theme(panel.grid.minor = element_blank(),
#         axis.title = element_text(size = rel(1.15)),
#         axis.text = element_text(size = rel(1.1)))
# 
# 
# ###Accuracy
# gam_kripp <- gam(p_consistency ~ 
#                       s(prop_ks), 
#                     method="REML", 
#                     data = Merged_krippendorff)
# 
# pp <- plot(gam_kripp, shift=mean(Merged_krippendorff$p_consistency))
# 
# p.krip <- data.frame(x=pp[[1]]$x, 
#                          y=as.numeric(pp[[1]]$fit)+mean(Merged_krippendorff$p_consistency), 
#                          se=pp[[1]]$se)
# 
# #### PLOTS
# gam_Accuracy_plot<- 
#   ggplot()+
#   ## phenology lines
#   ## points
#   geom_point(data=Merged_krippendorff, aes(x=prop_ks, y=p_consistency), alpha=0.3, colour = "darkblue")+
#   ## gam
#   geom_ribbon(data=p.krip, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "darkblue", size=0.2)+
#   geom_line(data=p.krip, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
#   
#   #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
#   guides(colour = guide_legend(override.aes = list(alpha = 1)))+
#   my_theme+
#   theme(axis.text = element_text(size=12),
#         panel.grid.minor.x = element_blank())
# 
# 
# gam_Accuracy_plot
# 
# 
# 
# ####ok this doenst look great so lets now plot diferent gams based on the state? 
# 
# 
# 
# 
# ###Accuracy
# gam_rest <- gam(p_consistency ~ 
#                    s(prop_ks), 
#                  method="REML", 
#                  data = subset(Merged_krippendorff, most_common_behaviour == 1))
# 
# pp <- plot(gam_rest, shift=mean(subset(Merged_krippendorff, most_common_behaviour == 1)$p_consistency))
# 
# p.rest <- data.frame(x=pp[[1]]$x, 
#                      y=as.numeric(pp[[1]]$fit)+mean(subset(Merged_krippendorff, most_common_behaviour == 1)$p_consistency), 
#                      se=pp[[1]]$se)
# 
# 
# 
# 
# 
# 
# gam_forage <- gam(p_consistency ~ 
#                   s(prop_ks), 
#                 method="REML", 
#                 data = subset(Merged_krippendorff, most_common_behaviour == 2))
# 
# pp <- plot(gam_forage, shift=mean(subset(Merged_krippendorff, most_common_behaviour == 2)$p_consistency))
# 
# p.forage <- data.frame(x=pp[[1]]$x, 
#                      y=as.numeric(pp[[1]]$fit)+mean(subset(Merged_krippendorff, most_common_behaviour == 2)$p_consistency), 
#                      se=pp[[1]]$se)
# 
# 
# 
# 
# 
# 
# 
# gam_travel <- gam(p_consistency ~ 
#                     s(prop_ks), 
#                   method="REML", 
#                   data = subset(Merged_krippendorff, most_common_behaviour == 3))
# 
# pp <- plot(gam_travel, shift=mean(subset(Merged_krippendorff, most_common_behaviour == 3)$p_consistency))
# 
# p.travel <- data.frame(x=pp[[1]]$x, 
#                      y=as.numeric(pp[[1]]$fit)+mean(subset(Merged_krippendorff, most_common_behaviour == 3)$p_consistency), 
#                      se=pp[[1]]$se)
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
# #### PLOTS
# gam__plot<- 
#   ggplot()+
#   ## phenology lines
#   ## points
#   geom_point(data=Merged_krippendorff, aes(x=prop_ks, y=p_consistency), alpha=0.3, colour = "gold")+
#   geom_point(data=Merged_krippendorff, aes(x=prop_ks, y=p_consistency), alpha=0.3, colour = "red")+
#   geom_point(data=Merged_krippendorff, aes(x=prop_ks, y=p_consistency), alpha=0.3, colour = "cyan")+
#   ## gam
#   geom_ribbon(data=p.travel, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "cyan", size=0.2)+
#   geom_line(data=p.travel, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
#   
#   geom_ribbon(data=p.rest, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "gold", size=0.2)+
#   geom_line(data=p.rest, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
#   
#   geom_ribbon(data=p.forage, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "red", size=0.2)+
#   geom_line(data=p.forage, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
#   
#   #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
#   guides(colour = guide_legend(override.aes = list(alpha = 1)))+
#   my_theme+
#   theme(axis.text = element_text(size=12),
#         panel.grid.minor.x = element_blank())
# 
# 
# gam__plot
# 
# 
# ####ok so resting is quite consistent but foraging and travelling are very intertwined .Although usually 8/10 of the models 
# 
# ####maybe make some violin plots per behaviour? 
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- m-sd(x)
#   ymax <- m+sd(x)
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# ggplot(Merged_krippendorff, aes(x =as.factor(most_common_behaviour), y = p_consistency ))+
#   geom_violin() +
#   stat_summary(fun.data=data_summary)
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





