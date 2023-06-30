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








