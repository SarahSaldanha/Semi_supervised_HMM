options(digits = 5, OutDec = ".")
options(scipen=999) 

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
library(geosphere)
library(RStoolbox)
library(data.table)

wdir <- "D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos"


#wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos"

#wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos"
theme_set(theme_light())

my_theme <-
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1.15)),
        axis.text = element_text(size = rel(1.1)))

setwd("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos")
#setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos")
#setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos")

####LOAD IN R FUNCTIONS

source(file = file.path(wdir, "/CURSO_R_ACTIVITY_ANALYSIS (1)/CODE/Functions_Activity_analysis.R"))


########bring in the .deg files

data_wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos/all"
#data_wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos/ALL"
#data_wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos/ALL"

migtfiles <- list.files(data_wdir, pattern = ".deg")
print(migtfiles)

migtfilesAdj <- list.files(data_wdir, pattern = "driftadj.deg")
print(migtfilesAdj)

## load files using function "importActMT", which transforms the timestampa to biotrack style

#### Load data to one dataframe
setwd(data_wdir)

#actfile<-"BG930_PHAAET_2021_ICimadriftadj.deg"

library(lubridate)
importActMT <- function (actfile, ... ) {
  
  z <- read.delim(actfile, header = TRUE, skip = 19) 
  colnames(z) <- c("datetime", "duration", "act")
  message(actfile)
  
  message("Importing wet-dry data as durations ...")
  
  ## transform act to Biotrack style (MT loggers give the state and its duration before the timestamp)
  z$datetime_orig <- dmy_hms(z$datetime)
  z$duration<-as.numeric(z$duration)
  z$datetime <- z$datetime_orig - as.numeric(z$duration)
  z$date <- as.Date(z$datetime, tz = "GMT")
  z$dec_hour <- hour(z$datetime) + minute(z$datetime)/60 + second(z$datetime)/3600
  z$hour <- floor(z$dec_hour)
  z$origin_act <- "WD"
  z$producer <- "MigTech"
  # z$geo <- unlist(strsplit(actfile, "_"))[1]
  z$actfile <- actfile
  #z$datetime_orig <- NULL
  # if (!is.null(ID)) {z$geo <- ID}
  # z$timeadj <- "no"
  # print(head(z))
  
  return(z)
}

setwd(data_wdir)
actMigT <- 
  purrr::map_dfr(migtfilesAdj, importActMT)

library(stringr)

actMigT<-actMigT%>% 
  mutate(geo = str_split(actfile, "_", simplify = TRUE)[, 1])

head(actMigT)


    
####bring in geo metadata and match it up


met <- read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/BOU_matrix_for_all.csv", header = T, row.names = NULL)

head(met)

met<-dplyr::select(met,Species = ï..Species , Island, Colony,anella, Nest, geo_alfanum, sex, deployed,  recovered)


met$deployed<-dmy(met$deployed)
met$recovered<-dmy(met$recovered)

actMigT2<-merge(actMigT, met, by.y="geo_alfanum", by.x ="geo",  all= F) ####loosing "S960" BECAUSE THIS GEO DOES NOT HAVE AN AXY TRIP

unique(actMigT$geo)

unique(actMigT2$geo)

act<-actMigT2 %>%
  subset(date >= deployed & date <= recovered)

unique(act$geo)


#####bring in GPS data ####here doing it with the interpolated track since we will need this for the HMM. I might reconsider for the future. 

birdTrack <- readRDS("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper/Final_Data/3D_All_interpolations/interpolatedTRACKS_linear300sec_Methods_paper_2021.rds")

class(birdTrack$dateTime)

birdTrack<-birdTrack[with(birdTrack, order(contBlock, dateTime)),]

birdTrack$tripunique<-birdTrack$refID*100 + birdTrack$tripID2

#bring in nest coordinates. 



#birdinfo<-read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Most_recent_GPS_info/birdInfo_tropicbirds_ALL.csv")

#birdinfo<-dplyr::select(birdinfo, nestLoc_lat, refID1, nestLoc_lon, sex, lifeCycleStatus, Island, Colony, date_deployment, date_recovery, ring)

#birdTrack<-merge(birdTrack, birdinfo, by.x= "refID", by.y="refID1")

#####Now we need to drop geo data when rings are not the same as those in the GPS data... I did this match manually, so I can bring in the data for this. 


gps_geo<-read.csv("D:/Dropbox/sarah_fenha/sarah_geo_analysis/matched_gps_geos_exended_dates.csv", sep=',')

gps_geo$GPS_DATE_DEP<-dmy(gps_geo$GPS_DATE_DEP)
gps_geo$GPS_DATE_REC<-dmy(gps_geo$GPS_DATE_REC)


g<-unique(gps_geo$geo_alfanum)



act2<-subset(act, geo %in% g)


act3<-merge(act2, gps_geo, by.x = "geo", by.y="geo_alfanum")


act3<-subset(act3, datetime_orig >= (GPS_DATE_DEP)-1  & datetime <= (GPS_DATE_REC)+1)


unique(act3$geo)






#actdata<-subset(act3, geo == "BG930")


##########
###OR go straight to expanding 


#act2<-act2[1:10,]

#too big 
#res <- lapply( 1:nrow(act2) , function(x){ data.frame(
#  datetime = strptime( act2[ x , 1 ] , format = "%d/%m/%Y %H:%M:%S" ) +  ( seq_len( act2[ x , 2 ] ) - 1 ) ,
#  duration = rep( 1 , act2[ x , 2 ] ) ,
#  mean = rep( act2[ x , 3 ] ,  act2[ x , 2 ] ) ) } )

#do.call( rbind , res )



transf_Actmin<- function (actdata, ... ) {    
  
  message("Be sure data used come from Biotrack/BAS geolocators, this function is not prepared for Migrate Tech geos.
          For MigTech geos you need to adjuts the timestamp to be the start of the state!!!\n")
  
  library(tidyr)      ## {fill}
  library(doBy)
  library(lubridate)
  library(reshape2)   ## {dcast}
  library(plyr)
  
  
  #### for wet-dry type  
  #####################
  
  zz <- actdata  
  
  #### TRANSFORM "WET/DRY" TO NUMERIC VALUES (1-200)
  
  #### THE ALGORITHM TO TRANSFORM TO CONTINUOUS 
  ## 1) Create timesequence 
  
  zz <- arrange(zz, datetime)
  start <- seq.POSIXt(from = zz$datetime[1],
                      to = zz$datetime[length(zz$datetime)] + zz$duration[length(zz$datetime)], "6 sec", 
                       tz = "GMT")  ## 1hour = 6 * 10 minutes intervals (10*60sec) = 3600 (*60)
  end <- seq.POSIXt(from = start[2], to = start[length(start)] + 6*1, by = 6*1, tz = "GMT") #(3600), (*60) 
  timeseq <- data.frame(datetime=start, DateTimeend=end)
  timeseq$intchar <- paste(timeseq$datetime, timeseq$DateTimeend, sep="--")  
  
  ## 2) Merge with original activity data
  actm <- merge(timeseq, zz, all=T)
  actm <- arrange(actm, datetime)
  actm$DateTimeend <- c(as.character(actm$datetime[-1]), as.character(tail(actm$datetime, 1) + 6*1)) #(3600)
  actm$DateTimeend <- ymd_hms(actm$DateTimeend, tz="GMT")
  ## calculate duration of each phase in seconds
  actm$duract_sec <- as.numeric(difftime(actm$DateTimeend, actm$datetime, units = "secs"))
  
  ## 3) Assign act to missing values
  
  actm<-fill(actm, everything())
  
  actm$hour<-hour(actm$datetime)
  actm$dec_hour<-hour(actm$datetime) + minute(actm$datetime)/60 + second(actm$datetime)/3600
  
  #####create 01 code for act
  actm$act01<-ifelse(actm$act == "wet", 0, 1)
  
  colnames(actm)[colnames(actm) == 'anella'] <- 'ring'
  
  #### EXTRACT THE SEQUENCE OF ACT
  
  return(actm)
}


table(act3$geo)


BG930<-subset(act3, geo == "BG930")
BG930_6s<-transf_Actmin(BG930)
BG930_6s$geo<-"BG930"

BR070<-subset(act3, geo == "BR070")
BR070_6s<-transf_Actmin(BR070)
BR070_6s$geo<-"BR070"


BR084<-subset(act3, geo == "BR084")
BR084_6s<-transf_Actmin(BR084)
BR084_6s$geo<-"BR084"


BR098<-subset(act3, geo == "BR098")
BR098_6s<-transf_Actmin(BR098)
BR098_6s$geo<-"BR098"

S943<-subset(act3, geo == "S943")
S943_6s<-transf_Actmin(S943)
S943_6s$geo<-"S943"


S985<-subset(act3, geo == "S985")
S985_6s<-transf_Actmin(S985)
S985_6s$geo<-"S985"

BM321001<-subset(act3, geo == "BM321001")
BM321001_6s<-transf_Actmin(BM321001)
BM321001_6s$geo<-"BM321001"

BH184<-subset(act3, geo == "BH184")
BH184_6s<-transf_Actmin(BH184)
BH184_6s$geo<-"BH184"



table(act3$geo)




all_6s<-rbind (BG930_6s, BH184_6s, BM321001_6s, BR070_6s, BR084_6s, BR098_6s, S943_6s, S985_6s)






#write.table(all_6s, "gps_gls_all_6s_april_29.csv", row.names = F, sep=";")

# all_6s<-read.csv("gps_gls_all_6s_april_29.csv", sep=";")

str(all_6s)

all_6s$datetime<-ymd_hms(all_6s$datetime)
all_6s$DateTimeend<-ymd_hms(all_6s$DateTimeend)
all_6s$datetime_orig<-ymd_hms(all_6s$datetime_orig)
all_6s$GPS_DATE_DEP<-ymd(all_6s$GPS_DATE_DEP)
all_6s$GPS_DATE_REC<-ymd(all_6s$GPS_DATE_REC)
all_6s$date<-ymd(all_6s$date)




#############################
############################################
###############################################
#################################################
######################################################
######################################################
######################################################



################################Now I want to match this with the axy and gps data. Make a function for this because it will take a long time or maybe match with gps data first and then with axy? 







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

op <- options(digits.secs=6)

memory.limit(size=5000000000)


setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files")


###bring in bird data
bird<-read.csv("bird_info_axys.csv")
bird<-dplyr::select(bird, ring = ï..BirdId, Sex, Colony, date_deployment,GMT_hour_dep= GMT_Hour, date_recovery, GMT_Hour_recover, Breed.Stage, HZ)
bird$dep_dt<-dmy_hms(paste0(bird$date_deployment, " ", bird$GMT_hour_dep))
bird$rec_dt<-dmy_hms(paste0(bird$date_recovery, " ", bird$GMT_Hour_recover))
bird<-dplyr::select(bird, ring, Sex, dep_dt, rec_dt, Breed.Stage, HZ)
#bird<-subset(bird, HZ ==25 )
bird$ring<-as.character(bird$ring)
bird<-dplyr::select(bird, ring, dep_dt, rec_dt)
####bring in GPS cut data. 



#Set the require segment length
freq <- 25# The Frequency of accelerometry data (Hz)
secs <- 2 # The number of seconds over which to calculate the desired metrics.
# The manuscript says to use 1 second intervals, but Phil said "to capture 
# gliding flight as well I've found that a longer period is needed."
numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 

head(birdTrack)

birdTrack2<-select(birdTrack, lat, lon, alt, Satellites, tripID2, refID_corrected, contBlock, ring, date_recovery, complete, tripunique, Year, dateTime)

unique(birdTrack2$ring)

birdTrack2<-birdTrack2 %>%
  ungroup() %>%
  group_by(tripunique)%>%
  dplyr::mutate(min_trip_dt = min(dateTime, na.rm = T),
         max_trip_dt = max(dateTime, na.rm =T)) %>%
  ungroup()



                    
#####bullshit 
birdTrack2<-birdTrack2 %>%
dplyr::mutate(velocity  = distHaversine(cbind(lon, lat), cbind(lag(lon), lag(lat))) / as.numeric(difftime(dateTime, lag(dateTime))))


birdTrack2$velocity<-ifelse(as.numeric(difftime(birdTrack2$dateTime, lag(birdTrack2$dateTime))) == 300,birdTrack2$velocity, NA )

birdTrack2$velocity<-ifelse(!(lag(birdTrack$tripunique) == birdTrack$tripunique), NA, birdTrack2$velocity)

summary(birdTrack2$velocity)


tail(birdTrack2$dateTime)#####rounded to the seconds because of the interpolaiton ---> round geo to the minute? 


all_6s$datetime_round<-floor_date(all_6s$datetime, unit = "min")




all_geo_gps<-merge(birdTrack2, all_6s, by.x = c("ring", "dateTime"), by.y =c("ring", "datetime_round"), all.y = T)

unique(all_geo_gps$ring)



all_geo_gps<- fill(all_geo_gps,c(3:16), .direction = "down")



all_geo_gps$trip_file <- paste0(all_geo_gps$ID, "_", all_geo_gps$tripID2)

all_geo_gps<-subset(all_geo_gps, dateTime >= min_trip_dt & dateTime <= max_trip_dt )

unique(all_geo_gps$ID)
unique(all_geo_gps$trip_file)

all_geo_gps<-subset(all_geo_gps, ring == RING)
all_geo_gps$dateTime


table(all_geo_gps$trip_file)

names(all_geo_gps)[names(all_geo_gps) == "dateTime"] <- "dateTime_round"


#####bring in the axy data in turn and merge to this dataset 



setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/trips/cut_trips_final")
data_wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/trips/cut_trips_final"

PHAAET_files <- list.files(pattern = '*.csv')
print(PHAAET_files)


unique(all_geo_gps$trip_file) #### 8 tracks, from 7 individuals, 17 trips 


#PHAAET_files<- PHAAET_files[c(8,9,10,11,12,16,17,18,26,28,29,30,31,32,33,36,37)]




geo_gps_axy_together <- function(file_name){
  
  #file_name <-PHAAET_files[17]
  axy_data<-fread(file_name, header = T)
  

  #merge with the all_geo_act_file
  axy_data<-dplyr::select(axy_data, -lat, -lon, -refID, -date)
  head(axy_data$Timestamp)
  axy_data$Timestamp<-ymd_hms(axy_data$Timestamp)
  axy_data$ring<-as.character(axy_data$ring)

  #modify datetime of gps geo so that it matches... 
  
  #all_geo_gps$datetime_mod<-all_geo_gps$datetime-33

  
  axy_data2<-merge(axy_data, all_geo_gps, by.x = c("ring", "Timestamp"), by.y =c("ring", "datetime"), all.x = T, all.y =F)

  axy_data2<- fill(axy_data2,c(23:64), .direction = "down")
  
  
  

  axy_data2<-subset(axy_data2, Timestamp >= min_trip_dt & Timestamp <= max_trip_dt )
  
  

  write.csv(axy_data2,paste0("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/TRIPS_WITH_WET_DRY/", unique(axy_data2$tripID), "_WET_DRY.csv"),row.names=F)
  
  
}

lapply(PHAAET_files,geo_gps_axy_together)


















#####bring in and manually correct time of geolocator data before the merge 

#8201674_1 remove 42 seconds ####last column is the file name ########36
#7502351_3 remove 6 seconds ##########11
#8201666_5 ok
#7502351_4 remove 12 seconds  #############12
#8201666_2 ok
#7502351_1 20/04 remove 12 seconds ##############9
#8201666_3 ok
#8200501_2 remove 6 sec ###############18
#8201666_4 add 12 seconds #########31wrong! 

#7502351_2 remove 6 seconds
#8200501_1 remove 6 seconds
#8201666_6 add 12 seconds
#7502351 remove 30 seconds
#8201666_4 go with original... corrected is messed up
#8201666_1 good
#8201658 remove 24 seconds








print(PHAAET_files)

######8200501_2
  file_name <-PHAAET_files[26]
axy_data<-fread(file_name, header = T)
  
  
  #merge with the all_geo_act_file
  axy_data<-dplyr::select(axy_data, -lat, -lon, -refID, -date)
  head(axy_data$Timestamp)
  axy_data$Timestamp<-ymd_hms(axy_data$Timestamp)
  axy_data$ring<-as.character(axy_data$ring)
  
  #modify datetime of gps geo so that it matches... 
  
  all_geo_gps$datetime_mod<-all_geo_gps$datetime - 24
  
  
  axy_data2<-merge(axy_data, all_geo_gps, by.x = c("ring", "Timestamp"), by.y =c("ring", "datetime_mod"), all.x = T, all.y =F)
  
  axy_data2<- fill(axy_data2,c(23:65), .direction = "down")
  
  tail(axy_data2)
  
  
  axy_data2<-subset(axy_data2, Timestamp >= min_trip_dt & Timestamp <= max_trip_dt )
  
  
  
  write.csv(axy_data2,paste0("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/TRIPS_WITH_WET_DRY/", unique(axy_data2$tripID), "_WET_DRY_corrected.csv"),row.names=F)
  
  




















































































birdTrack$velocity<-birdTrack$lat, birdTrack$lon, lag(birdTrack$lat), lag(birdTrack$lon)

head(all_6s)

merge(all_6s, birdTrack, by = c("datetime", "ring", )

















all_6s<-read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/All/gps_gls_all_6s.csv", sep=";")
all_6s<-read.csv("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper/Final_Data/3D_All_interpolations/gps_gls_all_6s_april_29.csv", sep=";" )
str(all_6s)


all_6s$datetime<- ymd_hms(substring(all_6s$intchar, 1, 19))   

all_6s$date<- ymd(substring(all_6s$intchar, 1, 10))  


all_6s<-unique(all_6s)



#####bring in GPS data ####here D:/Dropbox/sarah_fenha/sarah_geo_analysis/interpolatedTRACKS_linear300sec_Final_Judoing it with the interpolated track since we will need this for the HMM. I might reconsider for the future. 

birdTrack <- readRDS("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper/Final_Data/3D_All_interpolations/interpolatedTRACKS_linear300sec_Methods_paper_2021.rds")

class(birdTrack$dateTime)

birdTrack<-birdTrack[with(birdTrack, order(contBlock, dateTime)),]

birdTrack$tripunique<-birdTrack$refID*100 + birdTrack$tripID2

#bring in nest coordinates. 

#birdinfo<-read.csv("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Most_recent_GPS_info/birdInfo_tropicbirds_ALL.csv")

tail(birdinfo)


#birdinfo<-dplyr::select(birdinfo, nestLoc_lat, nestLoc_lon, sex, lifeCycleStatus, Island, Colony, date_deployment, date_recovery, ring)

#birdTrack<-merge(birdTrack, birdinfo, by.x= "refID", by.y="refID1")

#####Now we need to drop geo data when rings are not the same as those in the GPS data... I did this match manually, so I can bring in the data for this. 


gps_geo<-read.csv("D:/Dropbox/sarah_fenha/sarah_geo_analysis/matched_gps_geos_exended_dates.csv", sep=',')

gps_geo$GPS_DATE_DEP<-dmy(gps_geo$GPS_DATE_DEP)
gps_geo$GPS_DATE_REC<-dmy(gps_geo$GPS_DATE_REC)



trial<-merge(all_6s, gps_geo, by.x= "geo", by.y = "geo_alfanum")


####subset the GPS data to this time.

head(birdTrack)

birdTrack$date_recovery<- ymd(birdTrack$date_recovery)
birdTrack$date_deployment<- ymd(birdTrack$date_deployment)



str(gps_geo)

gps_geo$RING<-as.factor(gps_geo$RING)

str(birdTrack)

birdTrack$ring <-as.factor(birdTrack$ring)

str(gps_geo)


track<- merge(birdTrack, gps_geo, by.x= c("ring",  "date_recovery", "ID"), by.y = c("RING",  "GPS_DATE_REC", "ID"))

track$date_deployment<-track$GPS_DATE_DEP



track$dt<-track$dateTime

deployment_times<-track %>%
  group_by(geo_alfanum)%>%
  dplyr::summarize (min.dt = min(dt), 
             max.dt = max(dt))


  
trial<- merge(trial, deployment_times, by.x = "geo", by.y = "geo_alfanum")
  


##################

table(trial$datetime)



trial<- trial[with(trial, order(geo, datetime)), ]

trial$date<-  as.Date(format(trial$datetime, "%Y-%m-%d"))


trial<-subset(trial, datetime >= min.dt & datetime <= max.dt+ 5*60)



str(track)


all<- merge(trial, track, by.x = c("geo",  "datetime", "ID", "GPS_DATE_DEP", "type"), by.y = c("geo_alfanum", "dateTime", "ID", "GPS_DATE_DEP", "type"),  all = T )


head(all)

all$sex<-NA


trial<-all %>%
  fill (dt, .direction = "down")%>%
  fill (lon, .direction = "down") %>%
  fill (lat, .direction = "down") %>%
  fill (nestLoc_lat, .direction = "down") %>%
  fill (nestLoc_lon, .direction = "down") %>%
  fill (sex, .direction = "down") %>%
  fill (island, .direction = "down") %>%
  fill (colony, .direction = "down") %>%
  fill (ID, .direction = "down") %>%
  fill (contBlock, .direction = "down") %>%
  fill (region, .direction = "down") %>%
  #fill (refID, .direction = "down") %>%
  fill (tripunique, .direction = "down") #%>%
  #fill (lifeCycleStatus, .direction = "down")


all<-trial %>%
  ungroup() %>%
  dplyr::group_by(dt, lat, lon, sex, lifeCycleStatus, nestLoc_lon, nestLoc_lat,Island, Colony) %>%
  dplyr::summarize(PropWet = sum(DurWet, na.rm = T)/(sum(DurWet, na.rm = T) + sum(DurDry, na.rm = T)), 
                   Nchanges = sum(Nchanges, na.rm = T),
                   Nlanding = sum(Nlanding, na.rm = T), 
                   DurWet = sum(DurWet, na.rm = T), 
                   DurDry = sum(DurDry, na.rm = T),
                   geo = unique(geo), 
                   refID = unique (refID),
                   contBlock = unique(contBlock), 
                   tripunique = unique(tripunique))



all<-trial %>%
  ungroup() %>%
  dplyr::group_by(dt, lat, lon, sex, nestLoc_lon, nestLoc_lat,island, colony) %>%
  dplyr::summarize(PropWet = sum(DurWet, na.rm = T)/(sum(DurWet, na.rm = T) + sum(DurDry, na.rm = T)), 
                   Nchanges = sum(Nchanges, na.rm = T),
                   Nlanding = sum(Nlanding, na.rm = T), 
                   DurWet = sum(DurWet, na.rm = T), 
                   DurDry = sum(DurDry, na.rm = T),
                   geo = unique(geo), 
                   #refID = unique (refID),
                   contBlock = unique(contBlock), 
                   tripunique = unique(tripunique))


table(all$PropWet)



saveRDS(all, "D:/Dropbox/sarah_fenha/gps_geo_6s_organized_propwet_April_29_2021.rds")


###make #plot with points indicating the prop wet 


## make #plots

theme_base <- theme(axis.title = element_text(size=12), 
                    axis.text = element_text(size=10),
                    legend.title = element_text(size=12),
                    legend.text= element_text(size=10),
                    legend.key.size = unit(0.75, "cm"),
                    panel.grid = element_blank(), panel.border = element_blank(),
                    panel.background = element_rect(fill = "white", colour = "black"))



coastLine_toplot <- readRDS("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/capeVerde_Coast.rds")



mapExtendLon <- abs(diff(range(trial$lon)))/5
mapExtendLat <- abs(diff(range(trial$lat)))/5

mapLimitsY <-  c(10, 22)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-28, -18) #c(min(trial$lat) -mapExtendLat, max(trial$lat) +mapExtendLat)

rm(mapExtendLon, mapExtendLat)


all<-subset(all, !geo == "J464001" & 	!geo == "BD818001" 	& 	!geo == "BJ486001" ) #####FIGURE OUT WHY THIS DIDNT WORK LATER

all$totDur<-all$DurDry+all$DurWet
all$PropWet<-ifelse(all$totDur > 300, NA,all$PropWet )
all$Nlanding<-ifelse(all$totDur > 300, NA,all$Nlanding )
all$Nchanges<-ifelse(all$totDur > 300, NA,all$Nchanges )




### there are 146 points like this where the time wet or dry is huge... probably because of gaps in the tracking data. Turn these to NA

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all<-all[with(all, order(geo,contBlock, dt)),]

##geo == "T030001" | geo == "J459001"| geo ==  "J487001"

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
table(all$Colony, all$geo)
mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/PropWet.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = PropWet), size = 1, alpha = 0.8) +
  
  # general formatting
  scale_colour_gradient2( midpoint = 0.5, name = "PropWet", low = ("cyan"), mid = ("darkred"),  high = ("gold"), limits = c(0, 1), na.value = NA) +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base
dev.off()



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/Nlandings.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = Nlanding, size = Nlanding), alpha = 1) +
  
  scale_size(range = c(0,3)) +
  
  # general formatting
  scale_colour_gradient2( midpoint = 2.5, name = "Nlanding", low = "cyan",  mid = "red", high = "darkred", limits = c(0, 5), na.value = NA) +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base
dev.off()



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/Nchanges.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = Nchanges, size = Nchanges), alpha = 1) +
  
  scale_size(range = c(0,3)) +
  
  # general formatting
  scale_colour_gradient2( midpoint = 5, name = "Nchanges", low = "cyan",  mid = "red", high = "darkred", limits = c(0, 10), na.value = NA) +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base
dev.off()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     fit the data to a hmm
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library("momentuHMM")
library("gg#plot2")
library("ggsn")
library("circular")
library("parallel")
options(expressions = 20000)




setwd("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/ALL")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## folders out
if (file.exists(paste("./", "fitHMM_dataOut", sep = "")) == FALSE){
  dir.create(paste("./", "fitHMM_dataOut", sep = ""))
}

if (file.exists(paste("./fitHMM_dataOut/", "fitHMM_fitHMM_trackStates", sep = "")) == FALSE){
  dir.create(paste("./fitHMM_dataOut/", "fitHMM_fitHMM_trackStates", sep = ""))
}

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## functions
logit <- function(x) log(x/(1-x))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all<-all[with(all, order(geo,contBlock, dt)),]




birdPrep <- data.frame(ID = all$contBlock, time = all$dt, lon = all$lon, lat = all$lat)

birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))
class(birdPrep)
head(birdPrep)



birdPrep$ID <- all$tripunique
class(birdPrep)
head(birdPrep)

######lets take a look at the data 

hist(birdPrep$step, ylab = "frequency", breaks = 60, xlab = "step length", main = "")
hist(birdPrep$angle, ylab = "frequency", breaks = 60, xlab = "turning angle", main = "")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## when we fit the hmm, we must give some ideas to what we think the different states will look like.  Using this as a starting point, the HMM finds the best definitions.
## so we need to decide on number of states we expect, their distributions and the initial starting values for their definitions
## these are important, and small variations may impact how the model performs


#####assign K means

clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 3)



clusterBird_step$centers

muS_1 <-sort(clusterBird_step$centers)[1] #resting
muS_2 <-sort(clusterBird_step$centers)[2] #foraging
muS_3 <-sort(clusterBird_step$centers)[3] #travelling

sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])
sdS_3 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[3])])


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## next we will create a sequence of potential starting values - so we can run the model with variation around what we expect to see if it perfroms similarly (local versu global maxima)
## we will randomly draw these numbers from a normal distribution around the values taken above



set.seed(1234)
muS_1seq <- abs(rnorm(10, muS_1, muS_1))
muS_2seq <- abs(rnorm(10, muS_2, muS_2))
muS_3seq <- abs(rnorm(10, muS_3, muS_3))

set.seed(1234)
sdS_1seq <- abs(rnorm(10, sdS_1, sdS_1))
sdS_2seq <- abs(rnorm(10, sdS_2, sdS_2))
sdS_3seq <- abs(rnorm(10, sdS_3, sdS_3))
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xSeq_step <- hist(birdPrep$step, breaks = 120)$breaks
xSeq_step[1] <- 0.001 # zero returns infinty so we replace this.


hist(birdPrep$step, freq = FALSE, ylab = "density", breaks = 60, xlab = "step length", main = "GAMMA")

for(II in 1:length(muS_1seq)){
  
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_1seq[II]^2/sdS_1seq[II]^2,
               rate = muS_1seq[II]/sdS_1seq[II]^2), col = "gold", lwd = 1)
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_2seq[II]^2/sdS_2seq[II]^2,
               rate = muS_2seq[II]/sdS_2seq[II]^2), col = "red", lwd = 1)
  lines(xSeq_step,
        dgamma(xSeq_step,
               shape = muS_3seq[II]^2/sdS_3seq[II]^2,
               rate = muS_3seq[II]/sdS_3seq[II]^2), col = "cyan", lwd = 1)
  
}
rm(II)

lines(xSeq_step,
      dgamma(xSeq_step,
             shape = muS_1^2/sdS_1^2,
             rate = muS_1/sdS_1^2), col = "gold", lwd = 3)
lines(xSeq_step,
      dgamma(xSeq_step,
             shape = muS_2^2/sdS_2^2,
             rate = muS_2/sdS_2^2), col = "red", lwd = 3)
lines(xSeq_step,
      dgamma(xSeq_step,
             shape = muS_3^2/sdS_3^2,
             rate = muS_3/sdS_3^2), col = "cyan", lwd = 3)

dev.off()


library("circular") #angles are circular so we need to look from this library

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
## NOTE -angleA (below) is only necessary if we are estimating angle means in HMM (we assume these to be zero normally) - see estAngleMean within HMM info.
## intial values for angle concentrations need to be decided
## large concentration means narrow distribution (small angles)
## small concetration means wide distribution (larger angles more likely)

## for von mises
kappaA_1 <- 5 #resting
kappaA_2 <- 1 #foraging
kappaA_3 <- 25 #travelling

## for wrap cauchy (must be between 0 and 1)
rhoA_1 <- 0.2 #resting
rhoA_2 <- 0.05  #foraging
rhoA_3 <- 0.8  #travelling
#angleA_1 <- 0 # if we dont assume means to be 0, we have to set intial values for this parameter in the model - estAngleMean = list(angle = TRUE)
#angleA_2 <- 0

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## create a sequence of values - so we can run the model with variation around expected start values to see if the model perfroms similarly (local versu global maxima)
## also check if the chosen concentrations represent well what we see in the data (and specifiy (1) a state with small angles more likely and (2) a state with larger angles likely)
## check which of the two distributions better suits the data

set.seed(471355)
kappaA_1seq <- abs(rnorm(10, kappaA_1, kappaA_1)) #for Von Mises - between 0 and inf
kappaA_2seq <- abs(rnorm(10, kappaA_2, kappaA_2)) #for Von Mises - between 0 and inf
kappaA_3seq <- abs(rnorm(10, kappaA_3, kappaA_3)) #for Von Mises - between 0 and inf
set.seed(471355)
rhoA_1seq <- rbeta(10, 5, 1) #for Wrapped Cauchy (must be between 0 and 1)
rhoA_2seq <- rbeta(10, 2, 5) #for Wrapped Cauchy (must be between 0 and 1)
rhoA_3seq <- rbeta(10, 3, 5) #for Wrapped Cauchy (must be between 0 and 1)
#angleA_1seq <- abs(rnorm(10, angleA_1, 1.5))
#angleA_2seq <- abs(rnorm(10, angleA_2, 1.5))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at potential state distributions etc - lets plot some distributions and see what they look like

## create indices to plot against (the range of data we have)
xSeq_angle <- hist(birdPrep$angle, breaks = 120)$breaks

##%%%%%%%%%%%%%%%
## von mises
## https://en.wikipedia.org/wiki/Von_Mises_distribution

png(filename = paste("./dataPlots/angles_shapeScale_VONMISES.png", sep=""), width = 25, height = 20, res = 150, units = "cm")
hist(birdPrep$angle, freq = FALSE, ylab = "density", breaks = 60, xlab = "angle", main = "VON MISES")

for(II in 1:length(kappaA_1seq)){
  
  lines(xSeq_angle,
        dvonmises(xSeq_angle,
                  mu = circular(0),
                  kappa = kappaA_1seq[II]), col = "gold", lwd = 1)
  lines(xSeq_angle,
        dvonmises(xSeq_angle,
                  mu=circular(0),
                  kappa = kappaA_2seq[II]), col = "red", lwd = 1)
  lines(xSeq_angle,
        dvonmises(xSeq_angle,
                  mu=circular(0),
                  kappa = kappaA_3seq[II]), col = "cyan", lwd = 1)
  
  
}
rm(II)

lines(xSeq_angle,
      dvonmises(xSeq_angle,
                mu = circular(0),
                kappa = kappaA_1), col = "gold", lwd = 3)
lines(xSeq_angle,
      dvonmises(xSeq_angle,
                mu = circular(0),
                kappa = kappaA_2), col = "red", lwd = 3)
lines(xSeq_angle,
      dvonmises(xSeq_angle,
                mu = circular(0),
                kappa = kappaA_3), col = "cyan", lwd = 3)

dev.off()

##%%%%%%%%%%%%%%%
## wrapped cauchy
## https://en.wikipedia.org/wiki/Wrapped_Cauchy_distribution

png(filename = paste("./dataPlots/angles_shapeScale_WRAPCAUCHY.png", sep=""), width = 25, height = 20, res = 150, units = "cm")
hist(birdPrep$angle, freq = FALSE, ylab = "density", breaks = 60, xlab = "angle", main = "WRAPPED CAUCHY")

for(II in 1:length(rhoA_1seq)){
  
  lines(xSeq_angle,
        dwrappedcauchy(xSeq_angle,
                       mu = circular(0),
                       rho = rhoA_1seq[II]), col = "gold", lwd = 1)
  lines(xSeq_angle,
        dwrappedcauchy(xSeq_angle,
                       mu=circular(0),
                       rho = rhoA_2seq[II]), col = "red", lwd = 1)
  lines(xSeq_angle,
        dwrappedcauchy(xSeq_angle,
                       mu=circular(0),
                       rho = rhoA_3seq[II]), col = "cyan", lwd = 1)
  
}
rm(II)

lines(xSeq_angle,
      dwrappedcauchy(xSeq_angle,
                     mu = circular(0),
                     rho = rhoA_1), col = "gold", lwd = 3)
lines(xSeq_angle,
      dwrappedcauchy(xSeq_angle,
                     mu=circular(0),
                     rho = rhoA_2), col = "red", lwd = 3)
lines(xSeq_angle,
      dwrappedcauchy(xSeq_angle,
                     mu=circular(0),
                     rho = rhoA_3), col = "cyan", lwd = 3)


dev.off()


stepPar0 <- c(muS_1, muS_2, muS_3, sdS_1, sdS_2, sdS_3)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2, rhoA_3) # for von mises
anglePar0

rm(muS_1, muS_2, muS_3, sdS_1, sdS_2, sdS_3)
rm(rhoA_1, rhoA_2, rhoA_3)

fitTrack_behavOnly_3state<- fitHMM(data = birdPrep, nbStates = 3,
                             dist = list(step ="gamma", angle = "wrpcauchy"), #"weibull" for weibull, "vm" for von mises, "wrpcauchy" for wrapped cauchy
                             Par0 = list(step = stepPar0, angle = anglePar0))



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## have a quick look at model outputs
#plot(fitTrack_behavOnly_3state, plotCI = TRUE)
dev.off()

fitTrack_behavOnly_3state$mle$gamma

fitTrack_behavOnly_3state$mle$step
fitTrack_behavOnly_3state$mle$angle

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## extract behavioural states from model
birdStates <- viterbi(fitTrack_behavOnly_3state)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at probabilities of being in each state
birdProbs <- stateProbs(fitTrack_behavOnly_3state)
head(birdProbs)

#plotStates(fitTrack_behavOnly_3state)
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## look at state definitions and confident intervals
## lets check step states are discrete and not overlapping. Angles will overlap as both are centred on 0 (but the width of the distributions should be different)
birdCI <- CIreal(fitTrack_behavOnly_3state)

fitTrack_behavOnly_3state$mle$step
birdCI$step$lower
birdCI$step$upper

fitTrack_behavOnly_3state$mle$gamma
birdCI$gamma$lower
birdCI$gamma$upper

fitTrack_behavOnly_3state$mle$angle
birdCI$angle$lower
birdCI$angle$upper

all$fitTrack_behavOnly_3state<- birdStates

all$fitTrack_behavOnly_3state<- ifelse(all$fitTrack_behavOnly_3state==1, "resting", ifelse(all$fitTrack_behavOnly_3state ==2, "foraging", "travelling"))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_behavOnly_3state.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_behavOnly_3state), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()


##############################################################################################################################################################################################################HMM with 4 states 
################################################################################################################################################################################################################################################################################


birdPrep <- data.frame(ID = all$contBlock, time = all$dt, lon = all$lon, lat = all$lat)
birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))
birdPrep$ID <- all$tripunique

#####assign K means

clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 4)



clusterBird_step$centers

muS_1 <-sort(clusterBird_step$centers)[1] #resting
muS_2 <-sort(clusterBird_step$centers)[2] # int foraging
muS_3 <-sort(clusterBird_step$centers)[3]# ext foraging
muS_4 <-sort(clusterBird_step$centers)[4] #travelling

sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])
sdS_3 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[3])])
sdS_4 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[4])])


## assign initial values for angle
## look at concentrations (high is small angels and low is larger variable angles)
## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
## descide distributions - here go with von mises
rhoA_1 <- 0.2 # resting
rhoA_2 <- 0.05 # int foraging
rhoA_3 <- 0.1 # ext foraging
rhoA_4 <- 0.8 # travelling


stepPar0 <- c(muS_1, muS_2, muS_3, muS_4, sdS_1, sdS_2, sdS_3, sdS_4)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2, rhoA_3, rhoA_4) 
anglePar0

rm(muS_1, muS_2, muS_3,muS_4, sdS_1, sdS_2, sdS_3, sdS_4)
rm(rhoA_1, rhoA_2, rhoA_3, rhoA_4)

fitTrack_behavOnly_4states <- fitHMM(data = birdPrep, nbStates = 4,
                                     dist = list(step ="gamma", angle = "wrpcauchy"), #"weibull" for weibull, "vm" for von mises, "wrpcauchy" for wrapped cauchy
                                     Par0 = list(step = stepPar0, angle = anglePar0))



#plot(fitTrack_behavOnly_4states, plotCI = TRUE)
dev.off()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## extract behavioural states from model
birdStates <- viterbi(fitTrack_behavOnly_4states)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!


#plotStates(fitTrack_behavOnly_4state)
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## look at state definitions and confident intervals
## lets check step states are discrete and not overlapping. Angles will overlap as both are centred on 0 (but the width of the distributions should be different)
birdCI <- CIreal(fitTrack_behavOnly_4states)

fitTrack_behavOnly_4state$mle$step
birdCI$step$lower
birdCI$step$upper

fitTrack_behavOnly_4state$mle$gamma
birdCI$gamma$lower
birdCI$gamma$upper

fitTrack_behavOnly_4state$mle$angle
birdCI$angle$lower
birdCI$angle$upper

all$birdStates_4_state_behaviour_only<- birdStates


all$birdStates_4_state_behaviour_only<- ifelse(all$birdStates_4_state_behaviour_only==1, "resting", ifelse(all$birdStates_4_state_behaviour_only ==2, "intensive foraging",ifelse(all$birdStates_4_state_behaviour_only ==3,"extensive foraging",  "travelling")))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/birdStates_4_state_behaviour_only.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = birdStates_4_state_behaviour_only), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("darkred","red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()



##############################################################################################################################################################################################################HMM with 2 states 
################################################################################################################################################################################################################################################################################


birdPrep <- data.frame(ID = all$contBlock, time = all$dt, lon = all$lon, lat = all$lat)
birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))
birdPrep$ID <- all$tripunique

#####assign K means

clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 2)



clusterBird_step$centers

muS_1 <-sort(clusterBird_step$centers)[1] #resting
muS_2 <-sort(clusterBird_step$centers)[2] # travelling


sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])


## assign initial values for angle
## look at concentrations (high is small angels and low is larger variable angles)
## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
## descide distributions - here go with von mises
rhoA_1 <- 0.2 # resting
rhoA_2 <- 0.8 # travelling


stepPar0 <- c(muS_1, muS_2,  sdS_1, sdS_2)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2) # for von mises
anglePar0

rm(muS_1, muS_2, sdS_1, sdS_2)
rm(rhoA_1, rhoA_2)

fitTrack_behavOnly_2states <- fitHMM(data = birdPrep, nbStates = 2,
                                     dist = list(step ="gamma", angle = "wrpcauchy"), #"weibull" for weibull, "vm" for von mises, "wrpcauchy" for wrapped cauchy
                                     Par0 = list(step = stepPar0, angle = anglePar0))





#plot(fitTrack_behavOnly_2states, plotCI = TRUE)
dev.off()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## extract behavioural states from model
birdStates <- viterbi(fitTrack_behavOnly_2states)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at probabilities of being in each state
birdProbs <- stateProbs(fitTrack_behavOnly_2states)
head(birdProbs)

#plotStates(fitTrack_behavOnly_2states)
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## look at state definitions and confident intervals
## lets check step states are discrete and not overlapping. Angles will overlap as both are centred on 0 (but the width of the distributions should be different)
birdCI <- CIreal(fitTrack_behavOnly_2states)

fitTrack_behavOnly_2states$mle$step
birdCI$step$lower
birdCI$step$upper

fitTrack_behavOnly_2states$mle$gamma
birdCI$gamma$lower
birdCI$gamma$upper

fitTrack_behavOnly_2states$mle$angle
birdCI$angle$lower
birdCI$angle$upper

all$fitTrack_behavOnly_2states<- birdStates

all$fitTrack_behavOnly_2states<- ifelse(all$fitTrack_behavOnly_2states==1, "resting","travelling")

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_behavOnly_2states.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_behavOnly_2states), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("goldenrod", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()





##############################################################################################################################################################################################################HMM with 5states 
################################################################################################################################################################################################################################################################################


birdPrep <- data.frame(ID = all$contBlock, time = all$dt, lon = all$lon, lat = all$lat)
birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))
birdPrep$ID <- all$tripunique

#####assign K means

clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 5)


clusterBird_step$centers

muS_1 <-sort(clusterBird_step$centers)[1] #resting
muS_2 <-sort(clusterBird_step$centers)[2] # int foraging
muS_3 <-sort(clusterBird_step$centers)[3]# ext foraging
muS_4 <-sort(clusterBird_step$centers)[4] #travelling
muS_5 <-sort(clusterBird_step$centers)[5] #fast travelling

sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])
sdS_3 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[3])])
sdS_4 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[4])])
sdS_5 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[5])])


## assign initial values for angle
## look at concentrations (high is small angels and low is larger variable angles)
## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
## descide distributions - here go with von mises
rhoA_1 <- 0.2 # resting
rhoA_2 <- 0.05 # int foraging
rhoA_3 <- 0.1 # ext foraging
rhoA_4 <- 0.75 # travelling
rhoA_5 <- 0.95 # travelling


stepPar0 <- c(muS_1, muS_2, muS_3, muS_4, muS_5, sdS_1, sdS_2, sdS_3, sdS_4, sdS_5)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2, rhoA_3, rhoA_4, rhoA_5) # for von mises
anglePar0

rm(muS_1, muS_2, muS_3,muS_4 ,muS_5,  sdS_1, sdS_2, sdS_3, sdS_4, sdS_5)
rm(rhoA_1, rhoA_2, rhoA_3, rhoA_4, rhoA_5)

fitTrack_behavOnly_5states <- fitHMM(data = birdPrep, nbStates = 5,
                                     dist = list(step ="gamma", angle = "wrpcauchy"), #"weibull" for weibull, "vm" for von mises, "wrpcauchy" for wrapped cauchy
                                     Par0 = list(step = stepPar0, angle = anglePar0))




#plot(fitTrack_behavOnly_5states, #plotCI = TRUE)
dev.off()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## extract behavioural states from model
birdStates <- viterbi(fitTrack_behavOnly_5states)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at probabilities of being in each state
birdProbs <- stateProbs(fitTrack_behavOnly_5states)
head(birdProbs)

#plotStates(fitTrack_behavOnly_5states)
dev.off()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## look at state definitions and confident intervals
## lets check step states are discrete and not overlapping. Angles will overlap as both are centred on 0 (but the width of the distributions should be different)
birdCI <- CIreal(fitTrack_behavOnly_5states)


all$fitTrack_behavOnly_5states<- birdStates

all$fitTrack_behavOnly_5states<- ifelse(all$fitTrack_behavOnly_5states==1, "resting", ifelse(all$fitTrack_behavOnly_5states ==2, "intensive foraging",ifelse(all$fitTrack_behavOnly_5states == 3, "extensive foraging", ifelse(all$fitTrack_behavOnly_5states == 4, "slow travel","fast travel"))))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_behavOnly_5states.tiff", width = 7, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_behavOnly_5states), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("darkred","cyan", "red", "gold", "navy"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()


################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


#####Now add the prop wet into the data. 

################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################




################################################################################################################################################################################################################################################################################
###################Now HMM with propr wet and day as a variable ################################################################################################################################################################################################################################################################################

#install.packages ("suncalc")
library(suncalc)
library(lubridate)
library(dplyr)

all$date <-date(all$dt)

sun<-getSunlightTimes(data = all, 
                      keep = c("dusk", "dawn", "sunrise", "sunset", "night", "nightEnd"), tz = "GMT")
all<- merge(all, sun, by = c("date", "lat", "lon"))

all$date <-all$dt
sun2<-getSunlightPosition(data = all)

all<-merge(all, sun2, by = c("date", "lat", "lon"))



all$propWetCAT<-ifelse(all$PropWet == 1, "wet", ifelse( all$PropWet == 0, "dry", "mix"))

all$propWetno01<- ifelse(all$PropWet ==0, 0.0000000001, ifelse(all$PropWet == 1, 0.9999999999, all$PropWet))



#all$diff_dawn<- all$dt-all$dawn
#all$diff_dusk <- all$dt-all$dusk

#all$min_diff<-ifelse(all$diff_dawn < all$diff_dusk, all$diff_dawn, all$diff_dusk)

all$day01<-ifelse(all$dt > all$dawn & all$dt < all$dusk, 1, 0) ####better use this??

all$day<-ifelse(all$day01 ==1, "day", "night")




PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
table(all$Colony, all$geo)
mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



############plot with sun elevation 

tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/sun_altitude.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = altitude), size = 1, alpha = 0.8) +
  
  # general formatting
  scale_colour_gradient2( midpoint = 0, name = "Sun Altitude", mid = ("red"), low = ("black"), high = ("yellow"), limits = c(-pi/2, pi/2), na.value = NA) +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base
dev.off()




tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/day_night.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = day), size = 1, alpha = 0.8) +
  
  # general formatting
  scale_color_manual(values=c("orange", "darkblue"),  name = "") + 
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  theme(legend.position="top") +
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base

dev.off()


#plot propwet vs sun altitude 


all$time<- strftime(all$dt, format="%H:%M:%S")
all$time <- as.POSIXct(all$time, format="%H:%M:%S") ###bullshit date


require(lubridate)
clockS = function(t){hour(t)*3600+minute(t)*60+second(t)}
all$time<-clockS(all$time) #seconds since midnight
all$time<-all$time/3600 #hour since midnight


all$time_round<- round(all$time)


mean_sun_altitude <- all %>% 
  group_by(time_round)%>% 
  summarize (mean_sun_altitude =mean(altitude))

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


library(mgcv)
gam.propWet <- gam(PropWet~ s(time) + sex + lifeCycleStatus, data=all)
summary(gam.propWet)

plot(gam.propWet, se=TRUE,col="blue")

library(voxel)




all_moon<-getMoonIllumination(date = all$date, keep = c("fraction", "phase","angle"))

all<-merge(all, all_moon, by= "date")

night_all<-subset(all, day01 == 0)

gam.moon <- gam(PropWet~ s(fraction) + s(time), data=night_all)


summary(gam.moon)

plot(gam.moon, se=TRUE,col="blue")




ggplot(night_all, aes(x=fraction, y=PropWet)) + 
  geom_smooth() +
  theme_base + 
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22), labels = c( "00:00", "2:00", "4:00","6:00", "8:00","10:00", "12:00","14:00", "16:00","18:00", "20:00", "22:00"))


#####try HGAM here to have differences in the effect of s(moon) for day and night. Can also do this for sex and breeding phase


all<-unique(all)


#####add this to the 4 state with prop wet?



all<-all[with(all, order(tripunique, dt)),]




write.table(all, "D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/All/all_before_currents.csv", row.names = FALSE, sep=";")




birdPrep <- data.frame(ID = all$contBlock, time = all$dt, lon = all$lon, lat = all$lat, propWet = all$PropWet,Nlanding = all$Nlanding, day= all$day,day01 = all$day01,  sun_altitude =  all$altitude, propWetno01 = all$propWetno01, propWetCAT = all$propWetCAT, fraction = all$fraction)


birdPrep<- unique(birdPrep)


table(is.na(birdPrep$propWet))
table(is.na(birdPrep$day))
table(is.na(birdPrep$propWetCAT))



birdPrep$propWet[is.na(birdPrep$propWet)] <- 0
birdPrep$propWetno01[is.na(birdPrep$propWetno01)] <- 0.0000000001
#birdPrep$propWetCAT[is.na(birdPrep$propWetCAT)] <- "unknown"



head(birdPrep)


#####ok so instead of adding the variables in as covariates, I am going to add them in as data streams by removing the covNames function. 

birdPrep <- prepData(birdPrep, type = "LL", coordNames =c("lon", "lat"))
birdPrep$ID <- all$tripunique


############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################# 3 state with prop wet



###Now we have to set the distribution of the new data streams just as what we do for step and angle. propWet appears to have a Beta distribution with a peak at 0 and 1, and Nlanding has a peak at 0. We have to set these parameters for a seperate model for each state. 



set.seed(4713475)
clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 3)


# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for STEP - look at shape and scale parametres alongside distribution types (gamma versus weibull)
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## intial values for step length - from kmeans
clusterBird_step$centers

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## distribution - gamma

#####Create priors for stem means
####You can also play around with different priors by putting other numbers here to see how this affects where your models converge    
muS_1 <- sort(clusterBird_step$centers)[1] #resting
muS_2 <- sort(clusterBird_step$centers)[2] #foraging 
muS_3 <- sort(clusterBird_step$centers)[3] #traveling

###priors for std of steps
sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])
sdS_3 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[3])])




# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for angle
# ## look at concentrations (high is small angels and low is larger variable angles)
# ## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
# ## descide distributions - here go with von mises
kappaA_1 <- 25
kappaA_2 <- 1
kappaA_3 <- 15



rhoA_1 <-0.2
rhoA_2 <-0.05
rhoA_3 <-0.8



# 



# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## check for step lengths of zero - we need to deal with this in the model if we have step lengths of zero


sum(birdPrep$step == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
#sum(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == max(clusterBird_step$centers))] == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
#sum(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == min(clusterBird_step$centers))] == 0, na.rm = TRUE) #if more than zero need to define zero-mass parameter for zero inflation
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## zero-mass step parameters - required when there are step lengths of zero - take this as proportion of zeros in dataset
#length(which(birdPrep$step == 0))/nrow(birdPrep)
#length(which(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == max(clusterBird_step$centers))] == 0))/nrow(birdPrep[clusterBird_step[[1]] == which(clusterBird_step$centers == max(clusterBird_step$centers)), ])
#length(which(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == min(clusterBird_step$centers))] == 0))/nrow(birdPrep[clusterBird_step[[1]] == which(clusterBird_step$centers == min(clusterBird_step$centers)), ])

#zeroMass_1 <- length(which(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[1])] == 0))/nrow(birdPrep[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[1]), ])

#zeroMass_2 <- length(which(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[2])] == 0))/nrow(birdPrep[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[2]), ])

#zeroMass_3 <- length(which(birdPrep$step[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[3])] == 0))/nrow(birdPrep[clusterBird_step[[1]] == which(clusterBird_step$centers == clusterBird_step$centers[3]), ])
# 
rm(clusterBird_step)

####lets try gamma distribution for the number of landings 

hist(birdPrep$Nlanding, breaks = 100)
set.seed(4713475)
clusterBird_lan <- kmeans(na.omit(data.frame(birdPrep$Nlanding)), 3)








# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for STEP - look at shape and scale parametres alongside distribution types (gamma versus weibull)
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## intial values for step length - from kmeans
clusterBird_lan$centers

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## distribution - gamma

#####Create priors for stem means
####You can also play around with different priors by putting other numbers here to see how this affects where your models converge    ####the step centers dont really make too much sense here.... we can maybe try to play with them later.
muS_1_lan <- sort(clusterBird_lan$centers)[2] #resting
muS_2_lan <- sort(clusterBird_lan$centers)[3] #foraging 
muS_3_lan <-  sort(clusterBird_lan$centers)[1] #traveling

###priors for std of landings 
sdS_1_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[2])])
sdS_2_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[3])])
sdS_3_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[1])])



# ## zero-mass step parameters - required when there are step lengths of zero - take this as proportion of zeros in dataset

zeroMass_1_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[2])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[2]), ])

zeroMass_2_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[3])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[3]), ])

zeroMass_3_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[1])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[1]), ])


######now beta distribution for the propWet

#####here these numbers come from playing around with beta distribution simulations

betaa_1 <- 50 #resting
betaa_2 <- 1.1 #foraging
betaa_3 <- 1 #travelling

betab_1 <- 0.5 #resting
betab_2 <- 1.1#foraging
betab_3 <- 50 #travelling


clusterBird_propWet <- kmeans(na.omit(data.frame(birdPrep$propWet)), 3)
clusterBird_propWet$centers



zeroMass_1_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3]), ])

zeroMass_2_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2]), ])

zeroMass_3_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1]), ])





oneMass_1_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3]), ])

oneMass_2_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2]), ])

oneMass_3_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1]), ])


######do the same thing with the beta distribution now with the data that artificially has no 0s and 1s

######now beta distribution for the propWet

#####here these numbers come from playing around with beta distribution simulations



betaa_1 <- 50 #resting
betaa_2 <- 1.1 #foraging
betaa_3 <- 1 #travelling

betab_1 <- 0.5 #resting
betab_2 <- 1.1#foraging
betab_3 <- 50 #travelling

#####for the wet/mix/dry variable. We have to put start proportions for each state. So night higher in Day, travel higher in Day and rest higher in night.


table(birdPrep$propWetCAT)


#dry
Prop_dry_1<- 0.05 #resting 
Prop_dry_2<- 0.10 #foraging
Prop_dry_3<-0.90 #travelling
  
#mix
Prop_mix_1<- 0.05
Prop_mix_2<- 0.80
Prop_mix_3<- 0.05



 

######for the day/night variable. We have to put start proportions for each state. So night higher in Day, travel higher in Day and rest higher in night. 


Prop_day_1<- 0.05 #resting
Prop_day_2<- 0.80 #foraging
Prop_day_3<-0.85 #travelling
















#####################for sun altitude--- 


set.seed(4713475)
clusterBird_alt <- kmeans(na.omit(data.frame(birdPrep$sun_altitude)), 3)


# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for STEP - look at shape and scale parametres alongside distribution types (normal)
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## intial values for step length - from kmeans
clusterBird_alt$centers

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## distribution - gamma

#####Create priors for sun alt
####You can also play around with different priors by putting other numbers here to see how this affects where your models converge    ####the step centers dont really make too much sense here.... we can maybe try to play with them later.
muS_1_alt <- sort(clusterBird_alt$centers)[2] #resting
muS_2_alt <- sort(clusterBird_alt$centers)[3] #foraging 
muS_3_alt <-  sort(clusterBird_alt$centers)[1] #traveling

###priors for std of sun alt
sdS_1_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[2])])
sdS_2_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[3])])
sdS_3_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[1])])



######now that we have all of these starting parameters, lets try to set them up to run the different models
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## compile initial starting values - put all starting values together to input into the model
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## expected values
#stepPar0 <- c(muS_1, muS_2, muS_3, sdS_1, sdS_2, sdS_3, zeroMass_1, zeroMass_2, zeroMass_3)
stepPar0 <- c(muS_1, muS_2, muS_3, sdS_1, sdS_2, sdS_3)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2, rhoA_3) 
anglePar0


###


landingPar <-  c(muS_1_lan, muS_2_lan, muS_3_lan, sdS_1_lan, sdS_2_lan, sdS_3_lan, zeroMass_1_lan, zeroMass_2_lan, zeroMass_3_lan)

betaPar0 <-c(betaa_1, betaa_2, betaa_3, betab_1, betab_2, betab_3, zeroMass_1_wet, zeroMass_2_wet, zeroMass_3_wet, oneMass_1_wet, oneMass_2_wet , oneMass_3_wet)

betapar0_no01<-c(betaa_1, betaa_2, betaa_3, betab_1, betab_2, betab_3)

PropPar0_day<-c(Prop_day_1, Prop_day_2,Prop_day_3)

PropPar0_dry<-c(Prop_dry_1, Prop_dry_2,Prop_dry_3, Prop_mix_1, Prop_mix_2, Prop_mix_3 )


atlPar0<- c(muS_1_alt, muS_2_alt, muS_3_alt, sdS_1_alt, sdS_2_alt, sdS_3_alt)












##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Proportion wet and sun altitude

fitTrack_PrWet_sunatl_3state <- fitHMM(data = birdPrep, nbStates = 3,
                                        dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm", propWet= "beta"), 
                                        Par0 = list(step = stepPar0, angle = anglePar0, propWet = betaPar0, sun_altitude = atlPar0, formula = ~1))   



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## have a quick look at model outputs
#plot(fitTrack_PrWet_sunatl, plotCI = TRUE)
dev.off()

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWet_sunatl)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWet_sunatl<- birdStates

all$fitTrack_PrWet_sunatl<- ifelse(all$fitTrack_PrWet_sunatl==1, "resting", ifelse(all$fitTrack_PrWet_sunatl ==2, "foraging", "travelling"))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWet_sunatl.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWet_sunatl), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




##  sun altitude

fitTrack_sunatl_3state <- fitHMM(data = birdPrep, nbStates = 3,
                                dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm"), 
                                Par0 = list(step = stepPar0, angle = anglePar0, sun_altitude = atlPar0, formula = ~1))   


####this is shit















## Proportion wet wihout 0s and 1s

fitTrack_PrWetno01<- fitHMM(data = birdPrep, nbStates = 3,
                                dist = list(step ="gamma", angle = "wrpcauchy", propWetno01= "beta"), 
                                Par0 = list(step = stepPar0, angle = anglePar0, propWetno01 = betapar0_no01, formula = ~1)) 



## have a quick look at model outputs
#plot(fitTrack_PrWetno01, plotCI = TRUE)
dev.off()

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWetno01)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWetno01<- birdStates

all$fitTrack_PrWetno01<- ifelse(all$fitTrack_PrWetno01==1, "resting", ifelse(all$fitTrack_PrWetno01 ==2, "foraging", "travelling"))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWetno01.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWetno01), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




















##Proportion wet without 0s and 1s and sun altitude

fitTrack_PrWetno01_sunatl <- fitHMM(data = birdPrep, nbStates = 3,
                                    dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm", propWetno01= "beta"), 
                                    Par0 = list(step = stepPar0, angle = anglePar0, propWetno01 = betapar0_no01, sun_altitude = atlPar0, formula = ~1)) 





## have a quick look at model outputs
#plot(fitTrack_PrWetno01_sunatl, plotCI = TRUE)
dev.off()

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWetno01_sunatl)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWetno01_sunatl<- birdStates

all$fitTrack_PrWetno01_sunatl<- ifelse(all$fitTrack_PrWetno01_sunatl==1, "resting", ifelse(all$fitTrack_PrWetno01_sunatl ==2, "foraging", "travelling"))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWetno01_sunatl.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWetno01_sunatl), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




#proportion wet as a category

fitTrack_propWetCat <- fitHMM(data = birdPrep, nbStates = 3,
                          dist = list(step ="gamma", angle = "wrpcauchy", propWetCAT = "cat3"),   Par0 = list(step = stepPar0, angle = anglePar0, propWetCAT = PropPar0_dry, formula = ~1))   

#Nlandings


#day as a category

fitTrack_DayCat <- fitHMM(data = birdPrep, nbStates = 3,
                       dist = list(step ="gamma", angle = "wrpcauchy", day = "cat2"),   Par0 = list(step = stepPar0, angle = anglePar0, day = PropPar0_day, formula = ~1))   
























########################################################################################################################################################################################################################################################################################################################################################################################################################################################################

######do this all with a 4 state propWet

################################################################################################################################################################################################################################################################################################################################################################################################################

clusterBird_step <- kmeans(na.omit(data.frame(birdPrep$step)), 4)



clusterBird_step$centers

muS_1 <-sort(clusterBird_step$centers)[1] #resting
muS_2 <-sort(clusterBird_step$centers)[2] # int foraging
muS_3 <-sort(clusterBird_step$centers)[3]# ext foraging
muS_4 <-sort(clusterBird_step$centers)[4] #travelling

sdS_1 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[1])])
sdS_2 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[2])])
sdS_3 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[3])])
sdS_4 <- sd(na.omit(birdPrep$step)[clusterBird_step[[1]] == which(clusterBird_step$centers == sort(clusterBird_step$centers)[4])])


## assign initial values for angle
## look at concentrations (high is small angels and low is larger variable angles)
## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
## descide distributions - here go with von mises
## assume mean to be zero (see estAngleMean within HMM info to estimate this as well)
## descide distributions - here go with von mises
rhoA_1 <- 0.2 # resting
rhoA_2 <- 0.05 # int foraging
rhoA_3 <- 0.5 # ext foraging
rhoA_4 <- 0.8 # travelling


stepPar0 <- c(muS_1, muS_2, muS_3, muS_4, sdS_1, sdS_2, sdS_3, sdS_4)
stepPar0
anglePar0 <- c(rhoA_1, rhoA_2, rhoA_3, rhoA_4) # for von mises
anglePar0


####lets try gamma distribution for the number of landings 

hist(birdPrep$Nlanding, breaks = 100)

clusterBird_lan <- kmeans(na.omit(data.frame(birdPrep$Nlanding)), 4)

# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for STEP - look at shape and scale parametres alongside distribution types (gamma versus weibull)
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## intial values for step length - from kmeans
clusterBird_lan$centers

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## distribution - gamma

#####Create priors for stem means
####You can also play around with different priors by putting other numbers here to see how this affects where your models converge    ####the step centers dont really make too much sense here.... we can maybe try to play with them later.


muS_1_lan <-sort(clusterBird_lan$centers)[2] #resting
muS_2_lan <-sort(clusterBird_lan$centers)[4] # int foraging
muS_3_lan <-sort(clusterBird_lan$centers)[3]# ext foraging
muS_4_lan <-sort(clusterBird_lan$centers)[1] #travelling

sdS_1_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[2])])
sdS_2_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[4])])
sdS_3_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[3])])
sdS_4_lan <- sd(na.omit(birdPrep$Nlanding)[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[1])])


# ## zero-mass Nlanding parameters - required when there are Nlanding lengths of zero - take this as proportion of zeros in dataset

zeroMass_1_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[2])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[2]), ])

zeroMass_2_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[4])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[4]), ])

zeroMass_3_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[3])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[3]), ])

zeroMass_4_lan <- length(which(birdPrep$Nlanding[clusterBird_lan[[1]] == which(clusterBird_lan$centers ==  sort(clusterBird_lan$centers)[1])] == 0))/nrow(birdPrep[clusterBird_lan[[1]] == which(clusterBird_lan$centers == sort(clusterBird_lan$centers)[1]), ])


######now beta distribution for the propWet
hist(birdPrep$propWet, breaks = 100)


#####here these numbers come from playing around with beta distribution simulations

betaa_1 <- 25 #resting
betaa_2 <- 9 # int foraging
betaa_3 <- 1.5 #ext foraging
betaa_4 <- 1 #travelling

betab_1 <- 0.5 #resting
betab_2 <- 5#int foraging
betab_3 <- 10#ext foraging
betab_4 <- 50 #travelling




clusterBird_propWet <- kmeans(na.omit(data.frame(birdPrep$propWet)), 4)
clusterBird_propWet$centers



zeroMass_1_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[4])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[4]), ])

zeroMass_2_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3]), ])

zeroMass_3_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2]), ])

zeroMass_4_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1])] == 0))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1]), ])





oneMass_1_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[4])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[4]), ])

oneMass_2_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[3]), ])

oneMass_3_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[2]), ])

oneMass_4_wet <- length(which(birdPrep$propWet[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1])] == 1))/nrow(birdPrep[clusterBird_propWet[[1]] == which(clusterBird_propWet$centers == sort(clusterBird_propWet$centers)[1]), ])



######do the same thing with the beta distribution now with the data that artificially has no 0s and 1s so its only the first parts 





#####for the wet/mix/dry variable. We have to put start proportions for each state. So night higher in Day, travel higher in Day and rest higher in night.


#table(birdPrep$propWetCAT)


#dry
#Prop_dry_1<- 0.05 #resting 
#Prop_dry_2<- 0.10 #int foraging
#Prop_dry_3<- 0.10 #ext foraging
#Prop_dry_4<-0.90 #travelling

#mix
#Prop_mix_1<- 0.05
#Prop_mix_2<- 0.80
#Prop_mix_3<- 0.80
#Prop_mix_4<- 0.05





######for the day/night variable. We have to put start proportions for each state. So night higher in Day, travel higher in Day and rest higher in night. bernoulli distribution


Prop_day_1<- 0.10 #resting
Prop_day_2<- 0.90 # int foraging
Prop_day_3<-0.80 # ext foraging
Prop_day_4<-0.70 #travelling



#####################for sun altitude--- 

clusterBird_alt <- kmeans(na.omit(data.frame(birdPrep$sun_altitude)), 4)


# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## assign initial values for STEP - look at shape and scale parametres alongside distribution types (normal)
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## intial values for step length - from kmeans
clusterBird_alt$centers

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## distribution - gamma

#####Create priors for sun alt
####You can also play around with different priors by putting other numbers here to see how this affects where your models converge    ####the step centers dont really make too much sense here.... we can maybe try to play with them later.
muS_1_alt <- sort(clusterBird_alt$centers)[1] #resting
muS_2_alt <- sort(clusterBird_alt$centers)[4] # int foraging 
muS_3_alt <- sort(clusterBird_alt$centers)[3] #ext foraging 
muS_4_alt <-  sort(clusterBird_alt$centers)[2] #traveling

###priors for std of sun alt
sdS_1_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[1])])
sdS_2_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[4])])
sdS_3_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[3])])
sdS_4_alt <- sd(na.omit(birdPrep$sun_altitude)[clusterBird_alt[[1]] == which(clusterBird_alt$centers == sort(clusterBird_alt$centers)[2])])



######now that we have all of these starting parameters, lets try to set them up to run the different models
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## compile initial starting values - put all starting values together to input into the model
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## expected values

landingPar <-  c(muS_1_lan, muS_2_lan, muS_3_lan,muS_4_lan, sdS_1_lan, sdS_2_lan, sdS_3_lan,sdS_4_lan, zeroMass_1_lan, zeroMass_2_lan, zeroMass_3_lan, zeroMass_4_lan)

betaPar0 <-c(betaa_1, betaa_2, betaa_3, betaa_4, betab_1, betab_2, betab_3, betab_4, zeroMass_1_wet, zeroMass_2_wet, zeroMass_3_wet, zeroMass_4_wet, oneMass_1_wet, oneMass_2_wet , oneMass_3_wet , oneMass_4_wet)

betapar0_no01<-c(betaa_1, betaa_2, betaa_3, betaa_4, betab_1, betab_2, betab_3, betab_4)

PropPar0_day<-c(Prop_day_1, Prop_day_2,Prop_day_3, Prop_day_4)




atlPar0<- c(muS_1_alt, muS_2_alt, muS_3_alt, muS_4_alt, sdS_1_alt, sdS_2_alt, sdS_3_alt, sdS_4_alt)



# V4<V3<V2<V1, S1<S2<S3<S4, ZERO MASS 

#####need to work one this

omegaDM <- matrix(c(1,0,0,0,0,0,0,0, 
                    0,0,1,1,0,0,0,0,
                    0,0,1,1,0,0,0,0,
                    1,1,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,
                    0,0,1,0,0,0,0,0,
                    0,0,0,0,1,0,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,1,0,0,
                    0,0,0,0,0,1,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,
                    
                    ),nrow=nbStates*4,byrow=TRUE,
                  dimnames=list(c(paste0("shape1_",1:nbStates),
                                  paste0("shape2_",1:nbStates),
                                  paste0("zeromass_",1:nbStates), 
                                  paste0("ONEmass_",1:nbStates)),
                                c("shape_1:(Intercept)","shape2_1",
                                  "shape_2:(Intercept)","shape1_2",
                                  
                                  
                                  
                                  
                                  "zeromass_1:(Intercept)",
                                  "zeromass_23:(Intercept)")))


nbStates<-2

omegaDM <- matrix(c(1,0,0,0,0,0,
                    0,0,1,1,0,0,
                    0,0,1,1,0,0,
                    1,1,0,0,0,0,
                    0,0,1,0,0,0,
                    0,0,1,0,0,0,
                    0,0,0,0,1,0,
                    0,0,0,0,0,1,
                    0,0,0,0,0,1),nrow=nbStates*3,byrow=TRUE,
                  dimnames=list(c(paste0("shape1_",1:nbStates),
                                  paste0("shape2_",1:nbStates),
                                  paste0("zeromass_",1:nbStates)),
                                c("shape_1:(Intercept)","shape2_1",
                                  "shape_2:(Intercept)","shape1_2",
                                  "zeromass_1:(Intercept)",
                                  "zeromass_23:(Intercept)")))






omegaworkBounds <- matrix(c(-Inf,0,-Inf,0,-Inf,-Inf,
                            rep(Inf,ncol(omegaDM))),ncol(omegaDM),2,
                          dimnames=list(colnames(omegaDM),c("lower","upper")))
omegaBounds <- matrix(c(1,10,
                        1,10,
                        1,10,
                        1,10,
                        1,10,
                        1,10,
                        0,1,
                        0,1,
                        0,1),nrow=nbStates*3,byrow=TRUE,
                      dimnames=list(rownames(omegaDM),c("lower","upper")))








##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## normal 

fitTrack_4state <- fitHMM(data = birdPrep, nbStates = 4,
                                       dist = list(step ="gamma", angle = "wrpcauchy"), 
                                       Par0 = list(step = stepPar0, angle = anglePar0))   


#plot(fitTrack_4state, plotCI = TRUE)





##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Proportion wet and day01


fitTrack_PrWet_sunatl_4state <- fitHMM(data = birdPrep, nbStates = 4,
                                       dist = list(step ="gamma", angle = "wrpcauchy", day01 = "bern", propWet= "beta"), 
                                       Par0 = list(step = stepPar0, angle = anglePar0, propWet = betaPar0, day01 = PropPar0_day, formula = ~1))   




#plot(fitTrack_PrWet_sunatl_4state, plotCI = TRUE) ####bullshit


####add constraints














##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Proportion wet and sun altitude

fitTrack_PrWet_sunatl_4state <- fitHMM(data = birdPrep, nbStates = 4,
                                dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm", propWet= "beta"), 
                                Par0 = list(step = stepPar0, angle = anglePar0, propWet = betaPar0, sun_altitude = atlPar0, formula = ~1))   



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## have a quick look at model outputs
#plot(fitTrack_PrWet_sunatl_4state, plotCI = TRUE)
dev.off()

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWet_sunatl_4state)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWet_sunatl_4state<- birdStates

all$fitTrack_PrWet_sunatl_4state<- ifelse(all$fitTrack_PrWet_sunatl_4state==1, "resting", ifelse(all$fitTrack_PrWet_sunatl_4state ==2, "intensive foraging", ifelse(all$fitTrack_PrWet_sunatl_4state ==3, "extensive foraging","travelling")))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWet_sunatl_4state.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWet_sunatl_4state), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




##  sun altitude

fitTrack_sunatl_4state <- fitHMM(data = birdPrep, nbStates = 4,
                          dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm"), 
                          Par0 = list(step = stepPar0, angle = anglePar0, sun_altitude = atlPar0, formula = ~1))   


####this is shit

#plot(fitTrack_sunatl_4state, plotCI = TRUE)



##################################################################################################################################################################################################################################################################################################################################################### 4state with restricted covariates. 




#####bring in current data and match it. 














































#####HMM with constraints

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nbStates<-4
stateNames <- c("resting", "intensive foraging",  "extensive foraging", "transit")




#####without ZeroMass

# step DM - saying that state 1 (resting) mean and sd are smaller than state 2 (foraging) which are smaller than state 3 (transit)
stepDM <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 
                   1, 1, 0, 0, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 0,
                   1, 1, 1, 1, 0, 0, 0, 0,
                   1, 1, 1, 1, 1, 0, 0, 0,
                   0, 0, 0, 0, 1, 1, 0, 0,
                   0, 0, 0, 0, 1, 1, 1, 0,
                   0, 0, 0, 0, 1, 1, 1, 1), nrow = 2*nbStates, byrow = TRUE,
                 dimnames=list(c(paste0("mean_",1:nbStates),
                                 paste0("sd_",1:nbStates)),
                               c(paste0("mean_",1:nbStates,":(Intercept)"), "sd_1:(Intercept)","sd_2:(Intercept)","sd_3:(Intercept)", "sd_4:(Intercept)")))


####Here you can put numerical bounds. So here the mean of the resting state mean is between 0 and 0.3 (6s/s), the mean of the foraging state   is between 0 and 2.9 amd the travelling state is between 2.9 and the max step length of your data. SD are not constrained ###0.3, 2.9, 7.87
stepBounds <- matrix(c(0, Inf,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, Inf), nrow = 2*nbStates, byrow=TRUE,
                     dimnames=list(c(paste0("mean_",1:nbStates),
                                     paste0("sd_",1:nbStates))))





angleDM <- matrix(c(1,0,0,0,
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,1),nrow=nbStates,byrow=TRUE,
                  dimnames=list(paste0("concentration_",1:nbStates),
                                c("concentration_1:(Intercept)",
                                  "concentration_2:(Intercept)",
                                  "concentration_3:(Intercept)",
                                  "concentration_4:(Intercept)")))

userBounds <- list(step = stepBounds)





DM<-list(step =stepDM, angle = angleDM) #, angle = angleDM


fitTrack_4state_constraints <- fitHMM(data = birdPrep, nbStates = 4, dist = list(step ="gamma", angle = "wrpcauchy"),  Par0 = list(step = stepPar0, angle = anglePar0), stateNames = stateNames, DM = DM, formula = ~1)

#userBounds = userBounds


fitTrack_4state_constraints_test_prop_wet <- fitHMM(data = birdPrep, nbStates = 4, dist = list(step ="gamma", angle = "wrpcauchy", propWet= "beta"),  Par0 = list(step = stepPar0, angle = anglePar0, propWet =betaPar0), stateNames = stateNames ,userBounds = userBounds, DM = DM, formula = ~1)






plot(fitTrack_4state_constraints, plotCI = TRUE)



nbStates<-4


#####WithZeroMass



# step DM - saying that state 1 (resting) mean and sd are smaller than state 2 (foraging) which are smaller than state 3 (transit), Zero inflati
stepDM <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0,
                   1, 1, 0, 0, 0, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 1, 0, 0, 0, 0, 0,
                   0, 0, 0, 1, 1, 0, 0, 0, 0,
                   0, 0, 0, 1, 1, 1, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 0, 0, 0, 0, 0, 0, 1, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 3*nbStates, byrow = TRUE,
                 dimnames=list(c(paste0("mean_",1:nbStates),
                                 paste0("sd_",1:nbStates), 
                                 paste0("Zero_", 1:nbStates)),
                               c(paste0("mean_",1:nbStates,":(Intercept)"), "sd_1:(Intercept)","sd_2:(Intercept)","sd_3:(Intercept)", paste0("zeromass_",1:nbStates,":(Intercept)"))))



stepBounds <- matrix(c(0, 0.3,
                       0.3, 2.9,
                       2.9, 7.65,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, 1,
                       0, 1,
                       0, 1), nrow = 3*nbStates, byrow=TRUE,
                     dimnames=list(c(paste0("mean_",1:nbStates),
                                     paste0("sd_",1:nbStates), 
                                     paste("zero_", 1:nbStates))))

userBounds <- list(step = stepBounds)

DM<-list(step =stepDM)


fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only <- fitHMM(data = birdPrep, nbStates = 3, dist = list(step ="gamma", angle = "vm"),  Par0 = list(step = stepPar0, angle = anglePar0), stateNames = stateNames,userBounds = userBounds, DM = DM, formula = ~1)



save.image("./dataOut/ fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only.Rdata")
# 
load("./dataOut/fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only.Rdata")

# 
load("./dataOut/backUp_modelFitting_CaboVerde_0.35_5_constraints.Rdata") 

load("./dataOut/backUp_modelFitting_CaboVerde_0.35_5_constraints.Rdata") 



plot(fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only, plotCI = TRUE)















## Proportion wet wihout 0s and 1s

fitTrack_PrWetno01_4state<- fitHMM(data = birdPrep, nbStates = 4,
                            dist = list(step ="gamma", angle = "wrpcauchy", propWetno01= "beta"), 
                            Par0 = list(step = stepPar0, angle = anglePar0, propWetno01 = betapar0_no01, formula = ~1)) 



## have a quick look at model outputs
#
dev.off()
plot(fitTrack_PrWetno01_4state, plotCI = TRUE)

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWetno01_4state)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWetno01_4state<- birdStates
table(all$fitTrack_PrWetno01_4state)

all$fitTrack_PrWetno01_4state<- ifelse(all$fitTrack_PrWetno01_4state==1, "resting", ifelse(all$fitTrack_PrWetno01_4state ==2, "intensive foraging", ifelse(all$fitTrack_PrWetno01_4state ==3, "extensive foraging","travelling")))


PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWetno01_4state.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWetno01_4state), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "darkred", "gold",  "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




















##Proportion wet without 0s and 1s and sun altitude

fitTrack_PrWetno01_sunatl_4state <- fitHMM(data = birdPrep, nbStates = 4,
                                    dist = list(step ="gamma", angle = "wrpcauchy", sun_altitude = "norm", propWetno01= "beta"), 
                                    Par0 = list(step = stepPar0, angle = anglePar0, propWetno01 = betapar0_no01, sun_altitude = atlPar0, formula = ~1)) 





## have a quick look at model outputs
#plot(fitTrack_PrWetno01_sunatl_4state, plotCI = TRUE)
dev.off()

## extract behavioural states from model
birdStates <- viterbi(fitTrack_PrWetno01_sunatl)
head(birdStates)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## look at number of observations in each state
table(birdStates) #seems our birds spend alot of time in foraging state!

all$fitTrack_PrWetno01_sunatl<- birdStates

all$fitTrack_PrWetno01_sunatl<- ifelse(all$fitTrack_PrWetno01_sunatl==1, "resting", ifelse(all$fitTrack_PrWetno01_sunatl ==2, "foraging", "travelling"))

PLOT_SUB<-subset(all, geo == "T030001" | geo == "J459001")
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

mapLimitsY <-  c(14.65, 17)#c(min(trial$lon) -mapExtendLon, max(trial$lon) +mapExtendLon)
mapLimitsX <-  c(-25.4, -23.8)



tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/fitTrack_PrWetno01_sunatl.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, aes(lon, lat, color = fitTrack_PrWetno01_sunatl), size = 1, alpha = 0.8) +
  
  # genral formatting
  scale_color_manual(values = c("red", "gold", "cyan"), na.value = NA, name = "") +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme(legend.key=element_blank()) +
  theme(legend.position = "top")+
  theme_base
dev.off()




#proportion wet as a category

fitTrack_propWetCat <- fitHMM(data = birdPrep, nbStates = 3,
                              dist = list(step ="gamma", angle = "wrpcauchy", propWetCAT = "cat3"),   Par0 = list(step = stepPar0, angle = anglePar0, propWetCAT = PropPar0_dry, formula = ~1))   

#Nlandings


#day as a category

fitTrack_DayCat <- fitHMM(data = birdPrep, nbStates = 3,
                          dist = list(step ="gamma", angle = "wrpcauchy", day = "cat2"),   Par0 = list(step = stepPar0, angle = anglePar0, day = PropPar0_day, formula = ~1))   







save.image(file='HMMs.RData'



##################################################################################################################################################################################################################################################################################################################################################### 4state with restricted covariates. 


#####HMM with constraints

# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nbStates<-3
stateNames <- c("resting", "foraging", "transit")




#####without ZeroMass

# step DM - saying that state 1 (resting) mean and sd are smaller than state 2 (foraging) which are smaller than state 3 (transit)
stepDM <- matrix(c(1, 0, 0, 0, 0, 0, 
                   1, 1, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0,
                   0, 0, 0, 1, 0, 0, 
                   0, 0, 0, 1, 1, 0, 
                   0, 0, 0, 1, 1, 1), nrow = 2*nbStates, byrow = TRUE,
                 dimnames=list(c(paste0("mean_",1:nbStates),
                                 paste0("sd_",1:nbStates)),
                               c(paste0("mean_",1:nbStates,":(Intercept)"), "sd_1:(Intercept)","sd_2:(Intercept)","sd_3:(Intercept)")))


####Here you can put numerical bounds. So here the mean of the resting state mean is between 0 and 0.3 (6s/s), the mean of the foraging state   is between 0 and 2.9 amd the travelling state is between 2.9 and the max step length of your data. SD are not constrained 
stepBounds <- matrix(c(0, 0.35,
                       0, 2.9,
                       2.9, 7.65,
                       0, Inf,
                       0, Inf,
                       0, Inf), nrow = 2*nbStates, byrow=TRUE,
                     dimnames=list(c(paste0("mean_",1:nbStates),
                                     paste0("sd_",1:nbStates))))

userBounds <- list(step = stepBounds)

DM<-list(step =stepDM)



fitTrack_behavOnlyall_3state_constraints_test <- fitHMM(data = birdPrep, nbStates = 3, dist = list(step ="gamma", angle = "vm"),  Par0 = list(step = stepPar0, angle = anglePar0), stateNames = stateNames,userBounds = userBounds, DM = DM, formula = ~1)


plot(fitTrack_behavOnlyall_3state_constraints_test, plotCI = TRUE)






#####WithZeroMass



# step DM - saying that state 1 (resting) mean and sd are smaller than state 2 (foraging) which are smaller than state 3 (transit), Zero inflati
stepDM <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0,
                   1, 1, 0, 0, 0, 0, 0, 0, 0,
                   1, 1, 1, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 1, 0, 0, 0, 0, 0,
                   0, 0, 0, 1, 1, 0, 0, 0, 0,
                   0, 0, 0, 1, 1, 1, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 1, 0, 0, 
                   0, 0, 0, 0, 0, 0, 0, 1, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 1), nrow = 3*nbStates, byrow = TRUE,
                 dimnames=list(c(paste0("mean_",1:nbStates),
                                 paste0("sd_",1:nbStates), 
                                 paste0("Zero_", 1:nbStates)),
                               c(paste0("mean_",1:nbStates,":(Intercept)"), "sd_1:(Intercept)","sd_2:(Intercept)","sd_3:(Intercept)", paste0("zeromass_",1:nbStates,":(Intercept)"))))



stepBounds <- matrix(c(0, 0.3,
                       0.3, 2.9,
                       2.9, 7.65,
                       0, Inf,
                       0, Inf,
                       0, Inf,
                       0, 1,
                       0, 1,
                       0, 1), nrow = 3*nbStates, byrow=TRUE,
                     dimnames=list(c(paste0("mean_",1:nbStates),
                                     paste0("sd_",1:nbStates), 
                                     paste("zero_", 1:nbStates))))

userBounds <- list(step = stepBounds)

DM<-list(step =stepDM)


fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only <- fitHMM(data = birdPrep, nbStates = 3, dist = list(step ="gamma", angle = "vm"),  Par0 = list(step = stepPar0, angle = anglePar0), stateNames = stateNames,userBounds = userBounds, DM = DM, formula = ~1)



save.image("./dataOut/ fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only.Rdata")
# 
load("./dataOut/fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only.Rdata")

# 
load("./dataOut/backUp_modelFitting_CaboVerde_0.35_5_constraints.Rdata") 

load("./dataOut/backUp_modelFitting_CaboVerde_0.35_5_constraints.Rdata") 



plot(fitTrack_behavOnlyall_3state_constraints_full_complete_trips_only, plotCI = TRUE)
































