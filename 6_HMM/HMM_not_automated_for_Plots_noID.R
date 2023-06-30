
op <- options(digits.secs=6)
options(expressions = 20000)

memory.limit(size=99999999)


# parallel computing
library(doParallel)
library(foreach)
library(dplyr)          ## data manipulation
library(tidyr)          ## data manipulation
library(lubridate)      ## data manipulation date & time
library(ggplot2)        ## visualization
library(data.table)
library(momentuHMM)
library(reshape2)
library(stringr)
# milliseconds




setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     fit the data to a hmm
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined_TRUE_STATES_SET_trimmed.rds")
head(all)

####add in ring and dep numbers as seperate columns 

first.word <- function(my.string){
  unlist(strsplit(my.string, "_"))[3]
}

all$ring<-unlist(sapply(all$ring_dep_trip,  first.word))
all$dep<-unlist(sapply(all$ring_dep_trip,  first.word))

length(unique(all$ring)) #####386
length(unique(all$dep)) ####477





allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/allks_for_cm_NOID_622PM.rds" )

  

allks<-allks%>% arrange (contBlock, dateTime_gps)


birdPrep <- data.frame(ID = allks$contBlock, time = allks$dateTime_gps, lon = allks$lon, lat = allks$lat)

#####Lets do a full 3 state

birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))


# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## replace block ID to have the trip IDs rather than the continuous block IDs.
table(birdPrep$ID == allks$contBlock)
table(birdPrep$time == allks$dateTime_gps)

birdPrep$ID <- allks$ring_dep_trip



# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## try k-means clustering to get starting values for steps.
set.seed(3790)



clusterBird_step3 <- kmeans(na.omit(data.frame(birdPrep$step)), 3)


muS3_1 <- sort(clusterBird_step3$centers)[1] #RESTING
muS3_2 <- sort(clusterBird_step3$centers)[2] #Preening/foraging
muS3_3 <- sort(clusterBird_step3$centers)[3] #travelling


###priors for std of steps


sdS3_1 <- sd(na.omit(birdPrep$step)[clusterBird_step3[[1]] == which(clusterBird_step3$centers == sort(clusterBird_step3$centers)[1])])
sdS3_2 <- sd(na.omit(birdPrep$step)[clusterBird_step3[[1]] == which(clusterBird_step3$centers == sort(clusterBird_step3$centers)[2])])
sdS3_3 <- sd(na.omit(birdPrep$step)[clusterBird_step3[[1]] == which(clusterBird_step3$centers == sort(clusterBird_step3$centers)[3])])



#####Set up angle priors


rhoA3_1 <- 10 # resting
rhoA3_2 <- 1 #foraging
rhoA3_3 <- 8 # Travel




sum(birdPrep$step == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation


stepPar3_0 <- c(muS3_1, muS3_2, muS3_3, sdS3_1, sdS3_2, sdS3_3)

anglePar3_0 <- c(rhoA3_1, rhoA3_2, rhoA3_3)

#####FIT MOST COMPLETE MODEL 3 STATE, 4 STATE, 5 STATE
dist = list(step ="gamma", angle = "vm") ### lets start with a 3 state 


nbStates <- 3




library(CircStats)


table(birdPrep$ID == allks$ring_dep_trip)
table(birdPrep$time == allks$dateTime_gps)


# iteration written with two ts.....
# 
i<-1
   ii<-0
     fitTrack_3_STATES_ks_full_0_1<- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))

   
     i<-10
     ii<-0
     fitTrack_3_STATES_ks_full_0_10<- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
     
     
     
    i<-1
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_1 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    
    i<-2
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_2 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-3
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_3 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-4
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_4 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-5
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_5 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-6
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_6 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-7
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_7 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-8
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_8 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-9
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_9 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    i<-10
    ii<-0.75
    fitTrack_3_STATES_ks_full_0.75_10 <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("itteration", i, "_90")]))$row_ID, floor(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA))
    
    
birdPrep2<-birdPrep
  




#save.image("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/hmm_workspace_0.75_sam.RData")
load ("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/hmm_workspace_0.75_sam.RData")

####extract some to  check confusion matrices 
birdPrep2$fitTrack_3_STATES_ks_full_0.75_1 <- viterbi(fitTrack_3_STATES_ks_full_0.75_1)
p1_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_1)[1]/nrow(birdPrep2)
p1_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_1)[2]/nrow(birdPrep2)
p1_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_1)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_2 <- viterbi(fitTrack_3_STATES_ks_full_0.75_2)
p2_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_2)[1]/nrow(birdPrep2)
p2_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_2)[2]/nrow(birdPrep2)
p2_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_2)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_3 <- viterbi(fitTrack_3_STATES_ks_full_0.75_3)
p3_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_3)[1]/nrow(birdPrep2)
p3_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_3)[2]/nrow(birdPrep2)
p3_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_3)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_4 <- viterbi(fitTrack_3_STATES_ks_full_0.75_4)
p4_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_4)[1]/nrow(birdPrep2)
p4_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_4)[2]/nrow(birdPrep2)
p4_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_4)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_5 <- viterbi(fitTrack_3_STATES_ks_full_0.75_5)
p5_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_5)[1]/nrow(birdPrep2)
p5_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_5)[2]/nrow(birdPrep2)
p5_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_5)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_6 <- viterbi(fitTrack_3_STATES_ks_full_0.75_6)
p6_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_6)[1]/nrow(birdPrep2)
p6_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_6)[2]/nrow(birdPrep2)
p6_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_6)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_7 <- viterbi(fitTrack_3_STATES_ks_full_0.75_7)
p7_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_7)[1]/nrow(birdPrep2)
p7_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_7)[2]/nrow(birdPrep2)
p7_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_7)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_8 <- viterbi(fitTrack_3_STATES_ks_full_0.75_8)
p8_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_8)[1]/nrow(birdPrep2)
p8_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_8)[2]/nrow(birdPrep2)
p8_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_8)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_9 <- viterbi(fitTrack_3_STATES_ks_full_0.75_9)
p9_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_9)[1]/nrow(birdPrep2)
p9_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_9)[2]/nrow(birdPrep2)
p9_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_9)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0.75_10 <- viterbi(fitTrack_3_STATES_ks_full_0.75_10)
p10_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_10)[1]/nrow(birdPrep2)
p10_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_10)[2]/nrow(birdPrep2)
p10_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0.75_10)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0_1 <- viterbi(fitTrack_3_STATES_ks_full_0_1)
p0_0_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_1)[1]/nrow(birdPrep2)
p0_0_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_1)[2]/nrow(birdPrep2)
p0_0_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_1)[3]/nrow(birdPrep2)

birdPrep2$fitTrack_3_STATES_ks_full_0_10 <- viterbi(fitTrack_3_STATES_ks_full_0_10)
p0_10_1<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_10)[1]/nrow(birdPrep2)
p0_10_2<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_10)[2]/nrow(birdPrep2)
p0_10_3<-table(birdPrep2$fitTrack_3_STATES_ks_full_0_10)[3]/nrow(birdPrep2)


    
what<-merge(birdPrep2, allks, by.y = c("ring_dep_trip", "dateTime_gps"), by.x = c("ID", "time")) 
table(what$true_state_act_inactive, what$fitTrack_3_STATES_ks_full_0.75_2)
    
    
    
    
    table(what$true_state_act_inactive, what$fitTrack_3_STATES_ks_full_0.75_1)
    table(what$true_state_act_inactive, what$fitTrack_3_STATES_ks_full_0_1)
    
    
    
   
    
####distributions with known states
   #  plot(fitTrack_3_STATES_ks_full_0.75_1,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_2,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_3,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_4,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_5,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_6,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_7,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_8,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_9,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0.75_10,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   #  plot(fitTrack_3_STATES_ks_full_0_10,breaks=200,plotCI=TRUE,  col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   # plot(fitTrack_3_STATES_ks_full_0_1,breaks=200,plotCI=TRUE, col =c("goldenrod", "red", "cyan"), plotTracks = F, ask = F)
   
   
   TRIAL<-plot(
     fitTrack_3_STATES_ks_full_0.75_10,
     animals = NULL,
     covs = NULL,
     ask = FALSE,
     breaks = 100,
     sepAnimals = FALSE,
     sepStates = FALSE,
     col = c("goldenrod","red", "cyan"), 
   cumul = TRUE,
   plotTracks = FALSE,
   plotCI = FALSE,
   alpha = 0.95,
   plotStationary = TRUE)
   
   
   
   
####distribution without known states
   #plot(fitTrack_3_STATES_ks_full_0_1,breaks=100,plotCI=TRUE)
    
   #### for function gammaParamsConvert
   library(ConnMatTools)
   
   
   ############plot the distributions 
   
   ###Step
   ####create plot 
   birdPrep2<-birdPrep
   xSeq_step <- hist(birdPrep2$step, breaks = 200)$breaks
   xSeq_step[1] <- 0.001 # zero returns infinty so we replace this.
   
  
   rest <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[1],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[2])
 
   forage <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[3],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[4])
   
   travel <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[5],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[6])
    
  # 
  #  
  # D<-density(birdPrep2$step, na.rm = T) 
  # plot(D,  ylim = c(0, 6),main=NULL, xlab = "step", ylab = "density")
  # polygon(D, col="grey", border="black")
  # lines(xSeq_step, dgamma(xSeq_step, rest$shape,  scale=rest$scale),  col = "darkgoldenrod")
  # lines(xSeq_step, dgamma(xSeq_step, forage$shape,  scale=forage$scale),  col = "red")
  # lines(xSeq_step, dgamma(xSeq_step, travel$shape,  scale=travel$scale),  col = "cyan")

  ###try for 0.75
  
  rest_1 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_1$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_1$mle$step[2])
  forage_1 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_1$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_1$mle$step[4])
  travel_1 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_1$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_1$mle$step[6])
  
  rest_2 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_2$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_2$mle$step[2])
  forage_2 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_2$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_2$mle$step[4])
  travel_2 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_2$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_2$mle$step[6])
  
  rest_3 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_3$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_3$mle$step[2])
  forage_3 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_3$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_3$mle$step[4])
  travel_3 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_3$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_3$mle$step[6])
  
  rest_4 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_4$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_4$mle$step[2])
  forage_4 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_4$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_4$mle$step[4])
  travel_4 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_4$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_4$mle$step[6])
  
  rest_5 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_5$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_5$mle$step[2])
  forage_5 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_5$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_5$mle$step[4])
  travel_5 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_5$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_5$mle$step[6])
  
  rest_6 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_6$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_6$mle$step[2])
  forage_6 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_6$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_6$mle$step[4])
  travel_6 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_6$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_6$mle$step[6])
  
  rest_7 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_7$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_7$mle$step[2])
  forage_7 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_7$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_7$mle$step[4])
  travel_7 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_7$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_7$mle$step[6])
  
  rest_8 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_8$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_8$mle$step[2])
  forage_8 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_8$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_8$mle$step[4])
  travel_8 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_8$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_8$mle$step[6])
  
  rest_9 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_9$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_9$mle$step[2])
  forage_9 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_9$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_9$mle$step[4])
  travel_9 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_9$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_9$mle$step[6])

  rest_10 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_10$mle$step[1],sd=fitTrack_3_STATES_ks_full_0.75_10$mle$step[2])
  forage_10 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_10$mle$step[3],sd=fitTrack_3_STATES_ks_full_0.75_10$mle$step[4])
  travel_10 <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0.75_10$mle$step[5],sd=fitTrack_3_STATES_ks_full_0.75_10$mle$step[6])
  
  
  rest<- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[1],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[2])
  forage <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[3],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[4])
  travel <- gammaParamsConvert(mean=fitTrack_3_STATES_ks_full_0_1$mle$step[5],sd=fitTrack_3_STATES_ks_full_0_1$mle$step[6])
  library(scales)
  
  #D<-density(birdPrep2$step, na.rm = T, bw = 0.01) 
  #plot(D,  ylim = c(0, 6),main="", xlab = "step", ylab = "density"
  hist(birdPrep$step, ylab = "freq. density", breaks = seq(from = 0, to = 8.3, by = 0.083), xlab = "step length", main = "", freq = FALSE, cex.lab = 1.2, xlim = c(0, 5), col = "white")   
       
  #polygon(D, col="grey", border="black")
  lines(xSeq_step, (dgamma(xSeq_step, rest_1$shape,  scale=rest_1$scale)*p1_1),  col = alpha("goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_1$shape,  scale=forage_1$scale)*p1_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_1$shape,  scale=travel_1$scale)*p1_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_2$shape,  scale=rest_2$scale)*p2_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_2$shape,  scale=forage_2$scale)*p2_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_2$shape,  scale=travel_2$scale)*p2_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_3$shape,  scale=rest_3$scale)*p3_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_3$shape,  scale=forage_3$scale)*p3_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_3$shape,  scale=travel_3$scale)*p3_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_4$shape,  scale=rest_4$scale)*p4_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_4$shape,  scale=forage_4$scale)*p4_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_4$shape,  scale=travel_4$scale)*p4_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_5$shape,  scale=rest_5$scale)*p5_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_5$shape,  scale=forage_5$scale)*p5_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_5$shape,  scale=travel_5$scale)*p5_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_6$shape,  scale=rest_6$scale)*p6_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_6$shape,  scale=forage_6$scale)*p6_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_6$shape,  scale=travel_6$scale)*p6_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_7$shape,  scale=rest_7$scale)*p7_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_7$shape,  scale=forage_7$scale)*p7_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_7$shape,  scale=travel_7$scale)*p7_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_8$shape,  scale=rest_8$scale)*p8_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_8$shape,  scale=forage_8$scale)*p8_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_8$shape,  scale=travel_8$scale)*p8_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_9$shape,  scale=rest_9$scale)*p9_1),  col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_9$shape,  scale=forage_9$scale)*p9_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_9$shape,  scale=travel_9$scale)*p9_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest_10$shape,  scale=rest_10$scale)*p10_1), col = alpha( "goldenrod", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, forage_10$shape,  scale=forage_10$scale)*p10_2),  col = alpha( "red", 0.7))
  lines(xSeq_step, (dgamma(xSeq_step, travel_10$shape,  scale=travel_10$scale)*p10_3),  col = alpha( "cyan", 0.7))
  
  lines(xSeq_step, (dgamma(xSeq_step, rest$shape,  scale=rest$scale)*p0_0_1),  col = "darkgoldenrod", lty=2, lwd=2)
  lines(xSeq_step, (dgamma(xSeq_step, forage$shape,  scale=forage$scale)*p0_0_2),  col = "darkred", lty=2, lwd=2)
  lines(xSeq_step, (dgamma(xSeq_step, travel$shape,  scale=travel$scale)*p0_0_3),  col = "#006666", lty=2, lwd=2)
  
  legend(2, 4, legend=c("semi-supervised rest", "semi-supervised forage", "semi-supervised travel", "unsupervised rest","unsupervised forage","unsupervised travel"),col=c("goldenrod", "red", "cyan", "darkgoldenrod", "darkred", "#006666"), lty=c(1, 1, 1, 2, 2, 2), cex=0.8,lwd=c(1, 1, 1, 2, 2, 2),box.lty=0)
  
  
  
  
  ##########Now 
  par(ask = F)
  
  
  
  breaksPlot_angle <- hist(birdPrep$angle, breaks = seq(from = -3.2, to =3.2, by = 0.16))$breaks 
  enter
  
  
  
  hist(birdPrep$angle, ylab = "freq. density", breaks = seq(from = -3.2, to =3.2, by = 0.16), xlab = "turning angle", main = "", freq = FALSE, ylim = c(0, 1.4), cex.lab = 1.2, col = "white")
  
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                   mu = circular(0),
                   kappa = kappaA_1))*0.7, col = "goldenrod", lwd = 1)
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                   mu=circular(0),
                   kappa = kappaA_2))*0.06, col = "red", lwd = 1)
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                   mu=circular(0),
                   kappa = kappaA_3))*0.24, col = "cyan", lwd = 1)
  
  
  library(scales)
  
  a_rest_1 <- (dvonmises(breaksPlot_angle,
                        mu=circular(0),
                        kappa=fitTrack_3_STATES_ks_full_0.75_1$mle$angle[2]))*p1_1
  a_forage_1 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_1$mle$angle[4]))*p1_2
  a_travel_1 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_1$mle$angle[6]))*p1_3
  
  a_rest_2 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_2$mle$angle[2]))*p2_1
  a_forage_2 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_2$mle$angle[2]))*p2_2
  a_travel_2 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_2$mle$angle[2]))*p2_3
  
  
  a_rest_3 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_3$mle$angle[2]))*p3_1
  a_forage_3 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_3$mle$angle[2]))*p3_2
  a_travel_3 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_3$mle$angle[2]))*p3_3
  
  a_rest_4 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_4$mle$angle[2]))*p4_1
  a_forage_4 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_4$mle$angle[2]))*p4_2
  a_travel_4 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_4$mle$angle[2]))*p4_3
  
  a_rest_5 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_5$mle$angle[2]))*p5_1
  a_forage_5 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_5$mle$angle[2]))*p5_2
  a_travel_5 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_5$mle$angle[2]))*p5_3
  
  a_rest_6 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_6$mle$angle[2]))*p6_1
  a_forage_6 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_6$mle$angle[2]))*p6_2
  a_travel_6 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_6$mle$angle[2]))*p6_3
  
  a_rest_7 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_7$mle$angle[2]))*p7_1
  a_forage_7 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_7$mle$angle[2]))*p7_2
  a_travel_7 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_7$mle$angle[2]))*p7_3
  
  a_rest_8 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_8$mle$angle[2]))*p8_1
  a_forage_8 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_8$mle$angle[2]))*p8_2
  a_travel_8 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_8$mle$angle[2]))*p8_3
  
  a_rest_9 <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0.75_9$mle$angle[2]))*p9_1
  a_forage_9 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_9$mle$angle[2]))*p9_2
  a_travel_9 <- (dvonmises(breaksPlot_angle,
                           mu=circular(0),
                           kappa=fitTrack_3_STATES_ks_full_0.75_9$mle$angle[2]))*p9_3
  
  a_rest_10 <- (dvonmises(breaksPlot_angle,
                          mu=circular(0),
                          kappa=fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2]))*p10_1
  a_forage_10 <- (dvonmises(breaksPlot_angle,
                             mu=circular(0),
                             kappa=fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2]))*p10_2
  a_travel_10 <- (dvonmises(breaksPlot_angle,
                            mu=circular(0),
                            kappa=fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2]))*p10_3
  
  
  a_rest<- (dvonmises(breaksPlot_angle,
                      mu=circular(0),
                      kappa=fitTrack_3_STATES_ks_full_0_1$mle$angle[2]))*p0_10_1
  a_forage <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0_1$mle$angle[2]))*p0_10_2
  a_travel <- (dvonmises(breaksPlot_angle,
                         mu=circular(0),
                         kappa=fitTrack_3_STATES_ks_full_0_1$mle$angle[2]))*p0_10_3

  
  
  
  

  hist(birdPrep$angle, ylab = "freq. density", breaks = seq(from = -3.2, to =3.2, by = 0.16), xlab = "turning angle", main = "", freq = FALSE, ylim = c(0, 1.7), cex.lab = 1.2, col = "white")

  
  #polygon(D, col="grey", border="black")
  lines(breaksPlot_angle, a_rest_1,  col = alpha("goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_1,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_1,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_2,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_2,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_2,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_3,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_3,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_3,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_4,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_4,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_4,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_5,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_5,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_5,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_6,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_6,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_6,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_7,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_7,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_7,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_8,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_8,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_8,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_9,  col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_9,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_9,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest_10, col = alpha( "goldenrod", 0.7))
  lines(breaksPlot_angle, a_forage_10,  col = alpha( "red", 0.7))
  lines(breaksPlot_angle, a_travel_10,  col = alpha( "cyan", 0.7))
  
  lines(breaksPlot_angle, a_rest,  col = "darkgoldenrod", lty=2, lwd=2)
  lines(breaksPlot_angle, a_forage,  col = "darkred", lty=2, lwd=2)
  lines(breaksPlot_angle, a_travel,  col = "#006666", lty=2, lwd=2)
  
  # legend(0.2, 1.6, legend=c("semi-supervised rest", "semi-supervised forage", "semi-supervised travel", "unsupervised rest","unsupervised forage","unsupervised travel"),col=c("goldenrod", "red", "cyan", "darkgoldenrod", "darkred", "#006666"), lty=c(1, 1, 1, 2, 2, 2), cex=0.8,lwd=c(1, 1, 1, 2, 2, 2),box.lty=0)
  # 
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #TRY WITH SAMS SCRIPTS 
  
  load ("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/hmm_workspace_0.75_sam.RData")
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## make some plots of the model outputs
  library(circular)
  
  theme_base <- theme(axis.title = element_text(size=12),
                      axis.text = element_text(size=10),
                      legend.title = element_text(size=12),
                      legend.text= element_text(size=10),
                      legend.key.size = unit(0.75, "cm"),
                      panel.grid = element_blank(), panel.border = element_blank())
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## state distributions
  breaksPlot_step <- hist(fitTrack_3_STATES_ks_full_0.75_10$data$step, breaks = seq(from = 0, to = 8.3, by = 0.083))$breaks
  breaksPlot_angle <- hist(fitTrack_3_STATES_ks_full_0.75_10$data$angle, breaks = seq(from = -3.2, to =3.2, by = 0.064))$breaks
  
  ##%% step
  muS_1 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[1, 1]
  sdS_1 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[2, 1]
  muS_2 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[1, 2]
  sdS_2 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[2, 2]
  muS_3 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[1, 3]
  sdS_3 <- fitTrack_3_STATES_ks_full_0.75_10$mle$step[2, 3]
  kappaA_1 <- fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2, 1]
  kappaA_2 <- fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2, 2]
  kappaA_3 <- fitTrack_3_STATES_ks_full_0.75_10$mle$angle[2, 3]

  #png(filename = paste("./dataPlots/fittedStepLengthsAngles_histogramStates.png", sep=""), width = 25, height = 20, res = 150, units = "cm")
  
  
  
  
  par(mfrow = c(2, 1), mar = c(5, 4, 2, 4), oma = c(2, 2, 1, 1))
  hist(birdPrep$step, ylab = "freq. density", breaks = seq(from = 0, to = 8.3, by = 0.083), xlab = "step length", main = "", freq = FALSE, cex.lab = 1.2)
  
  lines(breaksPlot_step,
        (dgamma(breaksPlot_step,
               shape = muS_1^2/sdS_1^2,
               rate = muS_1/sdS_1^2))*0.7, col = "gold", lwd = 1)
  lines(breaksPlot_step,
        (dgamma(breaksPlot_step,
               shape = muS_2^2/sdS_2^2,
               rate = muS_2/sdS_2^2))*0.06, col = "red", lwd = 1)
  lines(breaksPlot_step,
        (dgamma(breaksPlot_step,
               shape = muS_3^2/sdS_3^2,
               rate = muS_3/sdS_3^2))*0.24, col = "cyan", lwd = 1)
  hist(birdPrep$angle, ylab = "freq. density", breaks = seq(from = -3.2, to =3.2, by = 0.1), xlab = "turning angle", main = "", freq = FALSE, ylim = c(0, 1.4), cex.lab = 1.2)
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                  mu = circular(0),
                  kappa = kappaA_1))*0.7, col = "gold", lwd = 1)
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                  mu=circular(0),
                  kappa = kappaA_2))*0.06, col = "red", lwd = 1)
  lines(breaksPlot_angle,
        (dvonmises(breaksPlot_angle,
                  mu=circular(0),
                  kappa = kappaA_3))*0.24, col = "cyan", lwd = 1)
 # dev.off()
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  plot(xSeq_step, dgamma(xSeq_step, rest$shape,  rate= fitTrack_3_STATES_ks_full_0$mle$step[1]/(fitTrack_3_STATES_ks_full_0$mle$step[2]^2)))
  
  
  shape = muS_3^2/sdS_3^2,
  rate = muS_3/sdS_3^2
  
  
  
  lines(xSeq_step,
        dgamma(xSeq_step, l$shape,  scale=l$scale),
               col = "red", lwd = 1)
  
  plot(xSeq_step, dgamma(xSeq_step,
         l$shape,  
         l$scale))
  

  
  ##%%%%%%%%%%%%%%%
  ## gamma
  ## https://en.wikipedia.org/wiki/Gamma_distribution
  
  # png(filename = paste("./dataPlots/stepLengths_shapeScale_GAMMA.png", sep=""), width = 25, height = 20, res = 150, units = "cm")
  # hist(birdPrep$step, freq = FALSE, ylab = "density", breaks = 60, xlab = "step length", main = "GAMMA")
  
  for(II in 1:length(muS_1seq)){
    
    lines(xSeq_step,
          dgamma(xSeq_step,
                 shape = muS_1seq[II]^2/sdS_1seq[II]^2,
                 rate = muS_1seq[II]/sdS_1seq[II]^2), col = "red", lwd = 1)
    lines(xSeq_step,
          dgamma(xSeq_step,
                 shape = muS_2seq[II]^2/sdS_2seq[II]^2,
                 rate = muS_2seq[II]/sdS_2seq[II]^2), col = "royalblue", lwd = 1)
    lines(xSeq_step,
          dgamma(xSeq_step,
                 shape = muS_3seq[II]^2/sdS_3seq[II]^2,
                 rate = muS_3seq[II]/sdS_3seq[II]^2), col = "darkgreen", lwd = 1)
    
  }
  rm(II)
  
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_1^2/sdS_1^2,
               rate = muS_1/sdS_1^2), col = "red4", lwd = 3)
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_2^2/sdS_2^2,
               rate = muS_2/sdS_2^2), col = "royalblue4", lwd = 3)
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_3^2/sdS_3^2,
               rate = muS_3/sdS_3^2), col = "darkgreen", lwd = 3)
  
  dev.off()
   
   
   
   
   
   