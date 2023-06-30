

library(dplyr)          ## data manipulation
library(tidyr)          ## data manipulation
library(lubridate)      ## data manipulation date & time

library(zoo)
library(data.table)
library(plotly)
library(diveMove)
library(ggplot2)


memory.limit(size=5000000000)

###############TDR data analysis

birdTrack <- readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Data_no_ID_GPS/2_trackInterpolations/interpolatedTRACKS_linear300sec_noID.rds")
head(birdTrack)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}




####bring in axy data and take only TDR lines
library(tidyverse)
setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Axy_trips")


PHAAET_files <- list.files( pattern = '.csv')
print(PHAAET_files)





axy_PHAAET_files_TDR <- function(file_name){
  #file_name <-PHAAET_files[26]
  
  # First import the data from the file
  
  axy_data<-fread( file_name, header = T)
  
  
  ###some of my trips have Pressure data throughout while others only at first second. 
  
  if(length(table(is.na(axy_data$Pressure)))== 1){
    axy_data<-subset(axy_data,str_sub(Timestamp_mod_axy, -2,-1) == "00") 
  }else{axy_data<-subset(axy_data, !is.na(Pressure))}
  

  ####now subset data to the 1sec with Pressure data 
  axy_data$Timestamp_mod_axy_TDR<-floor_date(ymd_hms(axy_data$Timestamp_mod_axy), "sec")



     ####Calculate depth and spread pressure
  axy_data$Approx_depth_negative_mode<-0.01*(getmode(round(axy_data$Pressure))-axy_data$Pressure)
  axy_data$Approx_depth_negative_mean<-0.01*(mean(axy_data$Pressure)-axy_data$Pressure)
  
  
  
  # ggplot(data = axy_data[1000:1500], aes(x =Timestamp_mod_axy_TDR, y= Pressure)) +
  #   geom_line()+
  #   geom_point()+
  #   geom_hline(yintercept = mean(axy_data$Pressure), colour = "red") +
  #   geom_hline(yintercept = getmode(round(axy_data$Pressure)), colour = "green") 
  # 
  # 
  # ggplot(data = axy_data[1000:1500], aes(x =Timestamp_mod_axy_TDR, y= Approx_depth_negative_mode)) +
  #   geom_line()+
  #   geom_point()
  

fwrite(axy_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_data/", unique(axy_data$ring_dep_trip), "_TDR.csv"), row.names = FALSE)
gc()
rm(axy_data)  
}



lapply(PHAAET_files, axy_PHAAET_files_TDR)



####### Now bring in all the TDR data files, manipulate with moveDive and then merge with complete gps datasets. 



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_data/")



PHAAET_files <- list.files(pattern = '.csv')
print(PHAAET_files)


##Some links to understand the filters better
#https://mran.microsoft.com/snapshot/2017-12-24/web/packages/diveMove/vignettes/diveMove.pdf
#https://cran.r-project.org/web/packages/diveMove/vignettes/diveMove.html


#Not working
##### ###6, 7, 15, 21, 22, 36,  NO DIVES. if there are no dives in your trip it will fail!!! 

#file_name <-PHAAET_files[1]
TDR_ZERO_CORRECT <- function(file_name){
  #file_name <-PHAAET_files[36]
  
  # First import the data from the file
  
  tdr_data<-fread(file_name, header = T)
  
  
  tdr_data<-subset(tdr_data, !duplicated(Timestamp_mod_axy))


  tdr_data$Timestamp_mod_axy_TDR<-ymd_hms( tdr_data$Timestamp_mod_axy_TDR)
  tdr_data$depth<-(tdr_data$Approx_depth_negative_mode*-1)####needs to be negative 
  
tdr_data <- tdr_data[order(tdr_data$Timestamp_mod_axy_TDR)]
 


####create the Time-Depth recorded data  (dtime = 1 since the data was recorded every second, in concurent data we are keeping all the additional columns)
  tdrX <- createTDR(time=tdr_data$Timestamp_mod_axy_TDR,
                    depth=tdr_data$depth,
                    concurrentData=dplyr::select(tdr_data, Timestamp_mod_axy_TDR, -depth),
                    dtime=1, file = file_name,
                    speed=F)
  
  #plotTDR(tdrX)
  
  
  # 
  # to callibrate the depth, we will applya smoothing/filtering mechanism where running quantiles can be applied to depth measurements sequentially, using .depth.filter.
  
  # We will use two filters: the first one being a running median with a narrow window to remove noise from the time series, followed by a running low quantile using a wide time window. The integer vector given as argument k specifies the width of the moving window(s), where ki is the width for the ith filter in units of the sampling interval of the TDR object. Similarly, the integer vector given as argument probs specifies the quantile for each filter, where probsi is the quantile for the ith filter. Smoothing/filtering can be performed within specified minimum and maximum depth bounds using argument depth.bounds2, in cases where surface durations are relatively brief separated by long periods of deep diving. These cases usually require large windows, and using depth bounds helps to stabilize the surface signal. Further details on this method are provided by Luque and Fried (2011).
  
  

  db <- c(-0.25,0.25) ####interval of where we thing the zero is
  dt <- 0.2 #dive threshold (we think that dives should be at minimum 0.2m depth, this will change by species)
  k<-c(3, 60)##length of the two running windows size in seconds
  P <- c(0.5, 0.05)####probs
  
  
  d.filter <- diveMove:::.depthFilter(depth = getDepth(tdrX),
                                      k = k, probs = P,
                                      depth.bounds = db, na.rm = TRUE)
  #plotTDR(tdrX)
  

  png(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_plots/", file_name, "_zero_corrected.png"))
  
  # Creating a plot
  
  plotZOC(tdrX, d.filter)
  # Closing the graphical device
  dev.off() 
  
  dcalib <- calibrateDepth(tdrX, dive.thr = dt, zoc.method = "filter",
                           k = k, probs = P, dive.model="smooth.spline",
                           depth.bounds = db, na.rm = TRUE)
  
  tdrXSumm1 <- diveStats(dcalib)

  fwrite(tdrXSumm1, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_summary/", unique(tdr_data$ring_dep_trip), "_TDR_summary.csv"), row.names = FALSE)
  
  rm(tdrXSumm1)
  
}

lapply(PHAAET_files, TDR_ZERO_CORRECT)



######



####this should be the output of the bringing everything together file. 
all<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined.rds")




#bring in all summary tables and then merge this to the gps data. Then check whether the tdr and axy data are similar or not. also compare the tdr and corrected tdr.. 

#######

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_summary")

FILES <- list.files(pattern = '*.csv')
print(FILES)



####merging files and adding in the tripID
Summary_tag <- function(FILES){
  SUM<-fread(FILES, header = T)
  SUM$ring_dep_trip<-str_sub(FILES, 1, -17)
  
  write.csv(SUM, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_summary/", unique(SUM$ring_dep_trip), "_TDR_summary" ,".csv"), row.names = FALSE)
  
}

lapply(FILES, Summary_tag)

 
MergedSummary <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/TDR_summary/", pattern = '*.csv'), read.csv))

head(MergedSummary)



table(is.na(MergedSummary$begdesc))

MergedSummary$begdesc<-ymd_hms(MergedSummary$begdesc)
MergedSummary$enddesc<-ymd_hms(MergedSummary$enddesc)
MergedSummary$begasc<-ymd_hms(MergedSummary$begasc)
MergedSummary$end_dive<-MergedSummary$begdesc + MergedSummary$divetim


msum<-MergedSummary
hist(msum$divetim, breaks = 100)
subset(msum, divetim>100)
msum<-subset(msum, divetim<100)####SOME trips with a super long dive... Ideally you could go back and play with the divemove parameters for these, but since there are few I just removed them. 

mean(msum$divetim) ######1.49  ###1.66
sd(msum$divetim)#####2.3   ###3.95


mean(msum$maxdep)#####0.7847  ###0.7768404
sd(msum$maxdep)####N0.3612 ### 0.3648426
table(msum$ring_dep_trip)

xxx<-msum %>% 
  dplyr::group_by(ring_dep_trip) %>%
  dplyr::summarize(n())
  
mean(xxx$'n()') ####26.275   #########25.22222
sd(xxx$'n()') ###27.693 ######27.3397





####Now bring this together with the all dataset based on the intervals! 

####so we want to loop this, first by matching with the trip ID and the iterval between one position and the next 

head(all)
head(msum)

####for now simplify dataset... 

msum<-select(msum, ring_dep_trip,begdesc, divetim, maxdep, end_dive)

####add in closest lowest gps Timestamp_mod_axy to the begdesc. then summarize the number of dives and merge with to the all dataset


###merged with gps
###match with datatable. 

  msum2<-select(msum, begdesc, ring_dep_trip)
  msum2$join_time <-msum2$begdesc
  
  all2<-select(all, dateTime_gps, ring_dep_trip)
  all2$join_time <-all2$dateTime_gps
  
  setDT(msum2)
  setDT(all2)
  
  setkey(msum2,ring_dep_trip, join_time)
  setkey(all2,ring_dep_trip, join_time)
  
 matched<- all2[msum2, roll = T]
 ###SO THIS MATCHES EACH OF THE DIVES TO ONE GPS POSITION. 
  
  matched<-select(matched, -join_time)
  
  sum_matched<-matched %>%
    group_by(dateTime_gps, ring_dep_trip)%>%
    summarise(TDR_dives = n()) %>%
    ungroup()
  
  unique(sum_matched$ring_dep_trip)
  
  sum_matched<-sum_matched[with(sum_matched, order(ring_dep_trip, dateTime_gps)),]
  
    

  
  ####now merge this with all


  
  head(all)
  
  ######not merging properly??? issues  with ring_dep_trips OR DATEIMES
  all<-full_join(all, sum_matched, by=c("ring_dep_trip", "dateTime_gps"))
  

  

  ####now lets set the 0 dives to when there were no dives detected and see how well these dives match up with those detected with axy! 
  
  ##
  all<-all%>% ungroup()
    

  ####some all dont have a tag id

no_tag_id<-subset(all, is.na(Tag_type))

a<-select(all, ring_dep_trip, Tag_type)
a<-a%>% distinct()
a<-subset(a, !is.na(Tag_type))

no_tag_id<-select(no_tag_id, -Tag_type)
no_tag_id<-merge(a, no_tag_id, by = "ring_dep_trip")


tag_id<-subset(all, !(is.na(Tag_type)))

all<-rbind(tag_id,no_tag_id )


####seperate into axy and non-axy data

TDR_ONLY<-subset(all, Tag_type == "wd_axy" |Tag_type == "axy")
other<-subset(all, !(Tag_type == "wd_axy" |Tag_type == "axy"))
  
  
TDR_ONLY$TDRdive01<-ifelse(TDR_ONLY$TDR_dives>0, 1, 0)
  
  
TDR_ONLY$TDR_dives<-ifelse(is.na(TDR_ONLY$TDR_dives) , 0, TDR_ONLY$TDR_dives)
TDR_ONLY$TDRdive01<-ifelse(is.na(TDR_ONLY$TDRdive01) , 0, TDR_ONLY$TDRdive01)
  
  
  ####how much did this change 
  table(TDR_ONLY$TDRdive01, TDR_ONLY$dive_tdr_01)
  table(TDR_ONLY$TDRdive01, TDR_ONLY$dive_01_acc)#### looks like the axy is detecting much more dives then the tdr
  
  TDR_ONLY$TDRdive01<-as.numeric(TDR_ONLY$TDRdive01)
  
  
  ###bring these together and save? 
  
  other$TDRdive01<-NA
  no_tag_id$TDRdive01<-NA
  
 all3<- rbind(TDR_ONLY, other)
  
 table(all3$TDRdive01, all3$dive_tdr_01)
 table(all3$TDRdive01, all3$dive_01_acc)#

 
 saveRDS(all3,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/combined_datasets/final_datal_wetdry_axy_combined_TDR.rds" )
 
  
  #####based on this lets redefine the true-states. 
  ####Then run the mdoels 
  

 
  