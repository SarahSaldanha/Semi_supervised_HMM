
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



####remove some repeated rows 

all<-all %>% arrange(ring_dep_trip, dateTime_gps)

all<-all%>%
  dplyr::group_by(ring_dep_trip, dateTime_gps,lat, lon, contBlock) %>%
  dplyr::arrange(desc(prop_known_axy), desc(sum_seg_length)) %>%
  dplyr::filter(row_number()==1)


####check for zero step lengths(that the bird is not moving because it is on land) ###  removing these just makes running the models easier 

all<-all %>% arrange(ring_dep_trip, dateTime_gps)

all$time_diff<-lead(all$dateTime_gps)-all$dateTime_gps
all$time_diff<-ifelse(lead(all$contBlock) == all$contBlock, all$time_diff, NA) ###all good! 

###check finally how many ks 

table(all$true_state_act_inactive, useNA = "always") ###check how many are classified in each state #9.19% rest, 0.27% forage, 2.56% travel
table(is.na(all$true_state_act_inactive))  ###10% of data with known state



####make informed dataset 

all<-all%>% arrange (contBlock, dateTime_gps)

all$row_ID<-seq_along(1:nrow(all))


birdPrep <- data.frame(ID = all$contBlock, time = all$dateTime_gps, lon = all$lon, lat = all$lat)






#####Lets do a full 3 state HMM!


birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))


# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## replace block ID to have the trip IDs rather than the continuous block IDs.
table(birdPrep$ID == all$contBlock)
table(birdPrep$time == all$dateTime_gps)
birdPrep$ID <- all$ring_dep_trip



# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## replace block ID to have the trip IDs rather than the continuous block IDs.
table(birdPrep$ID == allks$contBlock)
table(birdPrep$time == allks$dateTime_gps)
birdPrep$ID <- allks$ring_dep_trip


#######create true state dataframe (with each of the different sets to be used for the models)



ks_randomz<-filter(all, !is.na(true_state_act_inactive))$row_ID




set.seed(371990)


###shuffled
rows <- sample(ks_randomz)


####split into 10 chunks
chunk_length <-  floor(length(rows)*0.1)    # Define number of elements in chunks
#Now, we can use the split, ceiling, and seq_along functions to divide our vector into chunks:

list_subsets<-split(rows,             # Applying split() function
                    ceiling(seq_along(rows) / chunk_length))

random_samples_10 <- data.frame(sample1 = list_subsets[1], sample2 = list_subsets[2], sample3 = list_subsets[3],  sample4 = list_subsets[4], sample5 = list_subsets[5], sample6 = list_subsets[6], sample7 = list_subsets[7], sample8 = list_subsets[8], sample9 = list_subsets[9], sample10 = list_subsets[10])


row_samples<-melt(random_samples_10)
names(row_samples)<-c("sample_10","row_ID")

all<-merge(all, row_samples, by = "row_ID", all.x = T)




head(all)

all$iteration1_90<-ifelse(!(all$sample_10 == "X1"), all$true_state_act_inactive , NA)
all$iteration2_90<-ifelse(!(all$sample_10 == "X2"), all$true_state_act_inactive , NA)
all$iteration3_90<-ifelse(!(all$sample_10 == "X3"), all$true_state_act_inactive , NA)
all$iteration4_90<-ifelse(!(all$sample_10 == "X4"), all$true_state_act_inactive , NA)
all$iteration5_90<-ifelse(!(all$sample_10 == "X5"), all$true_state_act_inactive , NA)
all$iteration6_90<-ifelse(!(all$sample_10 == "X6"), all$true_state_act_inactive , NA)
all$iteration7_90<-ifelse(!(all$sample_10 == "X7"), all$true_state_act_inactive , NA)
all$iteration8_90<-ifelse(!(all$sample_10 == "X8"), all$true_state_act_inactive , NA)
all$iteration9_90<-ifelse(!(all$sample_10 == "X9"), all$true_state_act_inactive , NA)
all$iteration10_90<-ifelse(!(all$sample_10 == "X10"), all$true_state_act_inactive , NA)


head(all)
#saveRDS(all, "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds" )
all<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds" )














#######create true state dataframe (with each of the different sets to be used for the models)






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

####need to add zeromass for tese trips



## zero-mass step parameters - required when there are step lengths of zero - take this as proportion of zeros in dataset

sum(birdPrep$step == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
sum(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == max(clusterBird_step3$centers))] == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
sum(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == min(clusterBird_step3$centers))] == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## zero-mass step parameters - required when there are step lengths of zero - take this as proportion of zeros in dataset
length(which(birdPrep$step == 0))/nrow(birdPrep)
length(which(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == max(clusterBird_step3$centers))] == 0))/nrow(birdPrep[clusterBird_step3[[1]] == which(clusterBird_step3$centers == max(clusterBird_step3$centers)), ])
length(which(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == min(clusterBird_step3$centers))] == 0))/nrow(birdPrep[clusterBird_step3[[1]] == which(clusterBird_step3$centers == min(clusterBird_step3$centers)), ])

zeroMass_1 <- length(which(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[1])] == 0))/nrow(birdPrep[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[1]), ])

zeroMass_2 <- length(which(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[2])] == 0))/nrow(birdPrep[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[2]), ])

zeroMass_3 <- length(which(birdPrep$step[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[3])] == 0))/nrow(birdPrep[clusterBird_step3[[1]] == which(clusterBird_step3$centers == clusterBird_step3$centers[3]), ])
# 
stepPar3_0 <- c(muS_1, muS_2, muS_3, sdS_1, sdS_2, sdS_3, zeroMass_1, zeroMass_2, zeroMass_3)

anglePar3_0 <- c(rhoA3_1, rhoA3_2, rhoA3_3)

#####FIT MOST COMPLETE MODEL 3 STATE, 4 STATE, 5 STATE
dist = list(step ="gamma", angle = "vm") ### lets start with a 3 state 

nbStates <- 3





#---------------------------------------------------------------
#Prepare cluster
#---------------------------------------------------------------

cl <- makeCluster(cores)####creates a socket cluster
registerDoParallel(cl)




all<-all %>% arrange (contBlock, dateTime_gps)


table(birdPrep$ID == all$ring_dep_trip)




l<- foreach(i = 1:10) %:% foreach(ii = c(0, 0.09), .packages=c("tidyverse", "lubridate", "data.table", "zoo", "dplyr", "momentuHMM", "ggsn", "stringr", "reshape2")) %dopar% {
#i = 1
    #ii<-0

    fitTrack_3_STATES_ks_full <- fitHMM(data = birdPrep, nbStates = 3, dist = dist,  Par0 = list(step = stepPar3_0, angle =anglePar3_0), formula = ~ 1, optMethod="Nelder-Mead", knownStates = ifelse(all$row_ID  %in% sample(filter(all, !is.na(all[,paste0("iteration", i, "_90")]))$row_ID, round(nrow(all)*ii), replace = F), all$true_state_act_inactive, NA))
    
    
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ## refit model using varying starting values that are based on those from the first candidate model (to check convergence of this model to global rather than local maxima)
    muS_1 <-  fitTrack_3_STATES_ks_full$mle$step[1, 1]
    muS_2 <-  fitTrack_3_STATES_ks_full$mle$step[1, 2]
    muS_3 <-  fitTrack_3_STATES_ks_full$mle$step[1, 3]
    
    
    
    sdS_1 <-  fitTrack_3_STATES_ks_full$mle$step[2, 1]
    sdS_2 <-  fitTrack_3_STATES_ks_full$mle$step[2, 2]
    sdS_3 <-  fitTrack_3_STATES_ks_full$mle$step[2, 3]
    
    # 
    kappaA_1 <-  fitTrack_3_STATES_ks_full$mle$angle[2, 1]
    kappaA_2 <-  fitTrack_3_STATES_ks_full$mle$angle[2, 2]
    kappaA_3 <-  fitTrack_3_STATES_ks_full$mle$angle[2, 3]
    
    # 
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ## sequence of values to try - step
    muS_1seq <- abs(rnorm(10, muS_1, muS_1))#####need to change this back to 10!!!!!!!
    muS_2seq <- abs(rnorm(10, muS_2, muS_2))
    muS_3seq <- abs(rnorm(10, muS_3, muS_3))
    
    # 
    sdS_1seq <- abs(rnorm(10, sdS_1, sdS_1))
    sdS_2seq <- abs(rnorm(10, sdS_2, sdS_2))
    sdS_3seq <- abs(rnorm(10, sdS_3, sdS_3))
    
    # 
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ## sequence of values to try - angle
    kappaA_1seq <- abs(rnorm(10, kappaA_1, kappaA_1))
    kappaA_2seq <- abs(rnorm(10, kappaA_2, kappaA_2))
    kappaA_3seq <- abs(rnorm(10, kappaA_3, kappaA_3))
    
    # 
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ## compile sequence values
    stepPar0_seq <- Map(function(u, v, w, x, y, z) c(u, v, w, x, y, z), u = muS_1seq, v = muS_2seq, w = muS_3seq, x = sdS_1seq, y = sdS_2seq, z = sdS_3seq)
    
    anglePar0_seq <- Map(function(x, y, z) c(x, y, z), x = kappaA_1seq, y = kappaA_2seq, z = kappaA_3seq)
    # 
    
    #
    
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ## fit sequence models to test for local versus global maxima

    
    ks<-ifelse(allks$row_ID  %in% sample(filter(allks, !is.na(allks[,paste0("iteration", i, "_90")]))$row_ID, round(nrow(allks)*ii), replace = F), allks$true_state_act_inactive, NA)
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ## sequence model to test for local versus global maxima
    .f <- function(x, y, z){
      fitHMM(data = z, nbStates = 3,
             dist = list(step ="gamma", angle = "vm"), #"weibull" for weibull, "vm" for von mises, "wrpcauchy" for wrapped cauchy
             Par0 = list(step = x, angle = y),
             optMethod="Nelder-Mead",
             knownStates = ks, 
             formula = ~1)   
    }
    
    
    modelIter_startPar0 <- Map(purrr::possibly(.f, otherwise = NA), x = stepPar0_seq, y = anglePar0_seq, z = list(birdPrep))
    
    modelIter_startPar0_no_na<-Filter(function(a) any(!is.na(a)), modelIter_startPar0)
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #unlist(unname(lapply(modelIter_startPar0_no_na, function(x) x$mod$minimum)))## look at negative log likelihoods and AIC to pick the smallest ones 
    
    X<-max(unlist(unname(lapply(modelIter_startPar0_no_na, function(x) x$mod$minimum))), fitTrack_3_STATES_ks_full$mod$minimum) # is the same as the original model - good :)
    
    BestModel<- ifelse(fitTrack_3_STATES_ks_full$mod$minimum == X, 1, ifelse( modelIter_startPar0_no_na[[1]]$mod$minimum == X, 2, ifelse(modelIter_startPar0_no_na[[2]]$mod$minimum == X, 3, ifelse(modelIter_startPar0_no_na[[3]]$mod$minimum == X, 4, ifelse(modelIter_startPar0_no_na[[4]]$mod$minimum == X, 4, ifelse(modelIter_startPar0_no_na[[5]]$mod$minimum == X, 6, ifelse(modelIter_startPar0_no_na[[6]]$mod$minimum == X, 7, ifelse(modelIter_startPar0_no_na[[7]]$mod$minimum == X, 7, ifelse(modelIter_startPar0_no_na[[8]]$mod$minimum == X, 9, ifelse(modelIter_startPar0_no_na[[9]]$mod$minimum == X, 10, 11))))))))))
    
    ####################################################################
    
    ##extract behaviours of first model since its equal to the others!!
    
    modelIter_startPar0_no_na<- append(list(fitTrack_3_STATES_ks_full), modelIter_startPar0_no_na)
    
    
    
    birdPrep2<-birdPrep
    best_model<-modelIter_startPar0_no_na[[BestModel]]
    birdPrep2$state <- viterbi(best_model)
    probs<-round(stateProbs(best_model, hierarchical = FALSE), digits = 3)
    probs<-as.data.frame(probs)
    names(probs)<-c("prob1", "probp2", "prob3")
    birdPrep2<-cbind(birdPrep2, probs)
    birdPrep2$ks_prop<-ii
    birdPrep2
    return(list(birdPrep2, modelIter_startPar0_no_na))
    
    
  }
#}




stopCluster(cl)

elem<-(c(L))
elem[[1]][[1]][1] ####THIS IS THE EXTRACTED DATA FOR IT 1 PROP 1
elem[[10]][[4]][1]
elem[[1]][[2]][1]
elem[[1]][[12]][2]#####THESE ARE THE MODELS TESTED FOR THIS

#SO WE NEED THE FIRST ELEMENT (iterationS) TO GO FROM 1 TO 10
# WE NEED THE SECOND ELEMENT (PROPORTIONS) TO GO FROM 1 TO 13
#WE NEED THE THIRD ELEMENT TO GO FROM 1 TO 2



saveRDS(l, file ="D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/FitTrack_all_gps.rds")




#L<-readRDS("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/HMMS/Final_birdPrep_states_extracted_AWS_3states_CORRECT/full_list.rds")


for (i in 1:10){
  for(j in 1:2){
    #i = 1
    #j = 1
    output<-as.data.frame(l[[i]][[j]][1])
    #saveRDS(output, paste0("D:/Dropbox/PhD/Methods_Paper/HMMS/Final_birdPrep_states_extracted_AWS_3states/iteration", i, "/Final_birdPrep_states_extracted_AWS_3states_",  unique(output$ks_prop), ".rds"))
    saveRDS(output, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/iteration", i, "/Final_birdPrep_states_extracted_AWS_3states_",  unique(output$ks_prop), ".rds"))
    model<-l[[i]][[j]][2]
    #saveRDS(model, paste0("D:/Dropbox/PhD/Methods_Paper/HMMS/Final_birdPrep_states_extracted_AWS_3states/iteration", i, "/model_selection_",  unique(output$ks_prop), ".rds"))
    saveRDS(model, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/FitTrack_Full/iteration", i, "/model_selection_",  unique(output$ks_prop), ".rds"))
    
  }
}






# lapply(seq_along(l), function(ii){
#   saveRDS(l[[ii]][[1]], paste0("D:/Dropbox/PhD/Methods_Paper/HMMS/Final_birdPrep_states_extracted_AWS_3states/iteration", i, "/Final_birdPrep_states_extracted_AWS_3states_",  l[[ii]][[1]]$ks_prop, ".rds"))
# })
# 
# str(l)
# 
# 


