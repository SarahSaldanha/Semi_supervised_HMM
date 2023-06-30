####--------------------------------------------------------------------------~ 
#### FUNCTIONS TO WORK WITH ACTIVITY DATA                                  ####
#### Zuzana Zajkova
#### zuzulaz@gmail.com
####--------------------------------------------------------------------------~


####--------------------------------------------------------------------------~
#### IMPORT ACTIVITY DATA (0-200 FORMAT)                                   ####
####--------------------------------------------------------------------------~

#data_wdir <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/Geos"

importAct <- function(actfile, work.dir = data_wdir) {
  
  library(lubridate)
  
  ind_act <- read.csv(file.path(work.dir, actfile), header = FALSE, skip = 0) 
  message(actfile)
  
  #### for 0-200 type  
  #####################
  colnames(ind_act) <- c("valid", "datetime", "julian", "act")
  ind_act <- subset(ind_act, valid == "ok" | valid == "SUSPECT")   ## removing temperature values, if there are some
  ind_act$valid <- factor(ind_act$valid)
  ind_act$datetime <- as.POSIXct(strptime(ind_act$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
  temp <- unlist(strsplit(actfile, "_"))
  ind_act$geo <- temp[1]
  ind_act$species <- temp[2]
  ind_act$recovered_year <- temp[3]
  ind_act$actfile <- actfile
  ind_act$date <- as.Date(ind_act$datetime, tz = "GMT")
  ind_act$doy <- strftime(ind_act$date, format = "%j")
  ind_act$year <- year(ind_act$datetime)
  ind_act$month_num <- month(ind_act$datetime, label = FALSE)
  ind_act$month_char <- as.character(month(ind_act$datetime, label = TRUE, abbr = FALSE))
  ind_act$day <- day(ind_act$datetime)
  ind_act$dec_hour <- hour(ind_act$datetime) + minute(ind_act$datetime)/60 + second(ind_act$datetime)/3600
  ind_act$hour <- hour(ind_act$datetime)
  ind_act$Origin <- "0_200"
  return(ind_act)
}
####--------------------------------------------------------------------------~


####--------------------------------------------------------------------------~
#### IMPORT TRAJECTORY                                                     ####
####--------------------------------------------------------------------------~

# data_wdir <- "/Users/wreciak/zz/Dropbox/CURSOS_zz/CURSO_R_ACTIVITY_ANALYSIS/DATA"

importTrj <- function(trjfile, work.dir = data_wdir) {
  
  ind_trj <- read.csv(file.path(work.dir, trjfile), header = FALSE, skip = 1) 
  message(trjfile)
  
  names(ind_trj) <- c("fix", "date", "time", "julian", "sunrise_trj", "sunset_trj", 
                      "lat", "complat", "long", 
                      "distance_bas", "heading_bas", "velocity_bas", "confidence")
  ind_trj$date <- as.Date(ind_trj$date, format = "%d/%m/%Y")
  ind_trj$trjfile <- trjfile
  ind_trj$geo  <- unlist(strsplit(trjfile, "_"))[1]
  ind_trj$species  <- unlist(strsplit(trjfile, "_"))[2]
  ind_trj$long <- ind_trj$long * -1
  
  return(ind_trj)
}

####--------------------------------------------------------------------------~



####--------------------------------------------------------------------------~
#### IMPORT ACTIVITY FUNCTION                                              ####
####--------------------------------------------------------------------------~

# import 0-200

### NOT WORKING, NEED TO REWRITE TO NEW R FUNCTIONS

importAct_2 <- function (actfile, toNumeric = TRUE, basPlot = FALSE, ID = NULL, ... ) {       ## based on readAct and readAct2 {BAStag}
  
  library(tidyr)      ## {fill}
  library(doBy)
  library(lubridate) 
  library(plyr)
  library(reshape2)
  
  z <- read.csv(actfile, header = FALSE, skip = 0) 
  message(actfile)
  
  #### for 0-200 type  
  #####################
  if (ncol(z) == 4) {      ##  & min(z$V4, na.rm=T) == 0 & max(z$V4, na.rm=T) == 200) {
    
    message("Importing 0-200 activity data ...")
    
    colnames(z) <- c("valid", "datetime", "Julian", "Act")
    zz <- subset(z, valid == "ok" | valid == "SUSPECT")   ## removing temperature values
    # zz$valid <- factor(zz$valid)
    zz$julian <- NULL
    zz$valid <- NULL
    zz$datetime <- as.POSIXct(strptime(zz$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
    zz$date <- as.Date(zz$datetime, tz = "GMT")
    zz$dec_hour <- hour(zz$datetime) + minute(zz$datetime)/60 + second(zz$datetime)/3600
    zz$hour <- floor(zz$dec_hour)
    zz$origin_act <- "0_200"
    zz$producer <- "Biotrack_BAS"
    # if (!is.null(ID)) {zz$ID <- ID}
    # if (julian.rm) {zz$Julian <- NULL}
    final <- zz
    retun(final)
  }
  
  
  #### for wet-dry type  
  #####################
  if (ncol(z) == 5) { ## & min(z$V4, na.rm=T) == 6) {    
    
    #### TRANSFORM "WET/DRY" TO NUMERIC VALUES (1-200)
    
    colnames(z) <- c("valid", "datetime", "julian", "duration", "act")
    ##  >> exclude records with "ERROR"
    zz <- subset(z, valid == "ok" | valid == "SUSPECT")
    zz$valid <- factor(zz$valid)
    zz$act <- factor(zz$act)
    
    if (toNumeric){
      
      message("Importing and transforming wet-dry data to 0-200 ...")
      
      #### THE ALGORITHM TO TRANSFORM TO CONTINUOUS 
      ## 1) Create timesequence of 10 min. (600 sec) intervals 
      zz$datetime <- as.POSIXct(strptime(zz$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
      start <- seq.POSIXt(from = zz$datetime[1], to = zz$datetime[length(zz$datetime)] + zz$duration[length(zz$datetime)], 
                          by = 10*60, tz = "GMT")  ## 10 minutes intervals (10*60sec)
      end <- seq.POSIXt(from = start[2], to = start[length(start)] + 600, by = 10*60, tz = "GMT") 
      timeseq <- data.frame(datetime=start, datetimeend=end)
      timeseq$intchar <- paste(timeseq$datetime, timeseq$datetimeend, sep="--")  
      
      ## 2) Merge with original data
      actm <- merge(timeseq, zz, all=T)
      actm <- arrange(actm, datetime)
      actm$datetimeend <- c(as.character(actm$datetime[-1]), as.character(tail(actm$datetime, 1) + 600))
      actm$datetimeend <- ymd_hms(actm$datetimeend, tz="GMT" )
      ## calculate duration of each phase in seconds
      actm$duract_sec <- as.numeric(difftime(actm$datetimeend, actm$datetime, units = "secs"))
      
      ## 3) Assign act to missing values
      actm <- actm %>% fill(act)
      actm <- actm %>% fill(intchar)
      actm <- actm %>% fill(valid)
      
      ## 4) Aggregate per intervals
      actnew <- summaryBy(duract_sec ~ intchar + act + valid, data=actm, FUN=c(sum), na.rm=TRUE)
      actnew$act <- round(actnew$duract_sec.sum / 3, digits=0)  ## need to round, as in some cases decimal numbers occur
      
      actcast <- reshape2::dcast(intchar ~ valid + act, value="act", data=actnew)
      
      if (length(unique(zz$valid)) == 1) {     ## only "ok"
        actcast$ok_dry[is.na(actcast$ok_dry)==TRUE] <- 0
        actcast$ok_wet[is.na(actcast$ok_wet)==TRUE] <- 0
        actcast$valid <- "ok"
        actcast$act <- actcast$ok_wet
        actcast$ok_dry <- actcast$ok_wet <- NULL
      } else {                                   ## "ok" & "SUSPECT"
        actcast$ok_dry[is.na(actcast$ok_dry)==TRUE] <- 0
        actcast$ok_wet[is.na(actcast$ok_wet)==TRUE] <- 0
        actcast$SUSPECT_dry[is.na(actcast$SUSPECT_dry)==TRUE] <- 0
        actcast$SUSPECT_wet[is.na(actcast$SUSPECT_wet)==TRUE] <- 0
        actcast$valid <- ifelse((actcast$ok_dry + actcast$ok_wet) == 200 & (actcast$SUSPECT_dry+ actcast$SUSPECT_wet) == 0, 
                                "ok", "SUSPECT")
        actcast$act <- ifelse(actcast$valid == "ok", 
                              actcast$ok_wet, 
                              actcast$SUSPECT_wet)
        actcast$ok_dry <- actcast$ok_wet <- actcast$SUSPECT_dry <- actcast$SUSPECT_wet <- NULL
      }
      
      ## 5) Final arrangement
      actcast$datetime <- ymd_hms(substr(actcast$intchar, 1, 19), tz="GMT")
      actcast$intchar <- NULL
      actcast <- actcast[c("valid", "datetime", "act")]     ## reorder columns
      actcast$date <- as.Date(actcast$datetime, tz = "GMT")
      actcast$dec_hour <- hour(actcast$datetime) + minute(actcast$datetime)/60 + second(actcast$datetime)/3600
      actcast$hour <- floor(actcast$dec_hour)
      actcast$origin_act <- "WD"
      actcast$producer <- "Biotrack_BAS"
      actcast$julian <- NULL
      actcast$valid <- NULL
      
      if (!is.null(ID)) {actcast$ID <- ID}
      
      final <- arrange(actcast, Date)
      
      ## remove last interval (not complete)
      final <- final[-nrow(final),]
      
    } else{      ## toNumeric = FALSE
      
      message("Importing wet-dry data as durations ...")
      
      zz$datetime <- as.POSIXct(strptime(zz$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
      zz$date <- as.Date(zz$datetime, tz = "GMT")
      zz$dec_hour <- hour(zz$datetime) + minute(zz$datetime)/60 + second(zz$datetime)/3600
      zz$hour <- floor(zz$dec_hour)
      zz$origin_act <- "WD"
      zz$producer <- "Biotrack_BAS"
      if (!is.null(ID)) {zz$ID <- ID}
      final <- zz
    }
    final
  }
  
  ##  Plots
  ## a) basic plot
  if (basPlot) {
    plot(final$datetime, final$act, xlab="datetime", ylab="act", 
         col=factor(final$valid), main=paste0("Activity ID: ", ID), cex.main=.8)
    legend("topleft", col=as.factor(unique(final$valid)), 
           legend=unique(final$valid), pch=1, cex=.8)
    mtext(side=3, unique(final$origin_act), cex=.8)  
  }
  
  print(tail(final))
  final
}

####--------------------------------------------------------------------------~
#### importActWD                                                           ####
####--------------------------------------------------------------------------~

#### Function to import activity data in wetdry format

importActWD <- function (actfile, ... ) {
  
  z <- read.csv(actfile, header = FALSE, skip = 0) 
  message(actfile)
  
  #if (ncol(z) == 4) {  
   #stop("The file is probably in 0-200 act format!") 
   ## note: should be more convinient implement  tryCatch here
   ## to not stop, but carry on and skip failed ones
  #}
  
  message("Importing wet-dry data as durations ...")
  
  colnames(z) <- c("date", "time", "duration", "act")
  ##  >> exclude records with "ERROR"
 # zz <- subset(z, valid == "ok" | valid == "SUSPECT")
  zz$julian <- NULL
  zz$valid <- NULL
  zz$datetime <- as.POSIXct(strptime(zz$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
  zz$date <- as.Date(zz$datetime, tz = "GMT")
  zz$act <- factor(zz$act)
  zz$date <- as.Date(zz$datetime, tz = "GMT")
  zz$dec_hour <- hour(zz$datetime) + minute(zz$datetime)/60 + second(zz$datetime)/3600
  zz$hour <- floor(zz$dec_hour)
  zz$origin_act <- "WD"
  zz$producer <- "Biotrack_BAS"
  # zz$geo <- unlist(strsplit(actfile, "_"))[1]
  zz$actfile <- actfile
  # if (!is.null(ID)) {zz$geo <- ID}
  # print(head(zz))
  
  return(zz)
}



####--------------------------------------------------------------------------~
#### importActMT                                                           ####
####--------------------------------------------------------------------------~

#### Function to import activity data in wetdry format
#### Transforms to Biotrack style of timestamp!

importActMT <- function (actfile, ... ) {
  
  z <- read.delim(actfile, header = TRUE, skip = 19) 
  colnames(z) <- c("datetime", "duration", "act")
  message(actfile)
  
  message("Importing wet-dry data as durations ...")
  
  ## transform act to Biotrack style (MT loggers give the state and its duration before the timestamp)
  z$datetime_orig <- as.POSIXct(strptime(z$datetime, "%d/%m/%Y %H:%M:%S", tz = "GMT"))
  z$datetime <- z$datetime_orig - z$duration
  z$date <- as.Date(z$datetime, tz = "GMT")
  z$dec_hour <- hour(z$datetime) + minute(z$datetime)/60 + second(z$datetime)/3600
  z$hour <- floor(z$dec_hour)
  z$origin_act <- "WD"
  z$producer <- "MigTech"
  # z$geo <- unlist(strsplit(actfile, "_"))[1]
  z$actfile <- actfile
  z$datetime_orig <- NULL
  # if (!is.null(ID)) {z$geo <- ID}
  # z$timeadj <- "no"
  # print(head(z))
  
  return(z)
}


####--------------------------------------------------------------------------~
#### import and transformt to hourly intervals                             ####
####--------------------------------------------------------------------------~

################################################
####  FUNCTION TO IMPORT AND                ####
####  TRANSFORM ACTIVITY DATA               ####              
####  FROM WET/DRY TO 0-200                 ####
################################################     

#### Zuzana Zajkova
#### SEPT 2016
#### zuzulaz@gmail.com

# library(tidyr)      ## {fill}
# library(doBy)
# library(lubridate) 
# library(plyr)

################################################

#### testing examples
## short gps track
# actfile <- "~/zz/Dropbox/CURSOS_zz/CURSO_R_ACTIVITY_ANALYSIS/DATA_2/19084_CALBOR_2011_Veneguera_6198154_Hembra GPS 6_000.act"
## year round
# actfile <- "~/zz/Dropbox/MARTA_Giant Petrels/ACT/19901001_MACHAL_2012_MARION_000.act"
# id <-  actfile
# ttt <- importActHour_Biotrack(actfile, ID = id)

importActHour_Biotrack <- function (actfile, julian.rm = TRUE, basPlot = TRUE, ID = NULL, ... ) {    
  
  message("Be sure data used come from Biotrack/BAS geolocators, this function is not prepared for Migrate Tech geos.
          For MigTech geos you need to adjuts the timestamp to be the start of the state!!!\n")
  
  library(tidyr)      ## {fill}
  library(doBy)
  library(lubridate)
  library(reshape2)   ## {dcast}
  library(plyr)
  
  z <- read.csv(actfile, header = FALSE, skip = 0) 
  message(paste("actfile:" ,actfile))
  
  #### for wet-dry type  
  #####################
  if (ncol(z) == 5) {      ## & min(z$V4, na.rm=T) == 6) {    
    
    #### TRANSFORM "WET/DRY" TO NUMERIC VALUES (1-200)
    
    colnames(z) <- c("Valid", "datetime", "Julian", "Duration", "Act")
    ##  "error message" solution here (>> see ACT_TRANSF_function_README.txt) >> exclude records with "ERROR"
    zz <- subset(z, Valid == "ok" | Valid == "SUSPECT")
    # zz$Valid <- factor(zz$Valid)
    zz$Valid <- NULL  ## sept 2016 ignore this 
    zz$Act <- factor(zz$Act)
    
    #### THE ALGORITHM TO TRANSFORM TO CONTINUOUS 
    ## 1) Create timesequence 
    zz$datetime <- as.POSIXct(strptime(zz$datetime, "%d/%m/%y %H:%M:%S", tz = "GMT"))
    zz <- arrange(zz, datetime)
    start <- seq.POSIXt(from = ceiling_date(zz$datetime[1], "hour"), 
                        to = ceiling_date(zz$datetime[length(zz$datetime)] + zz$Duration[length(zz$datetime)], "hour"), 
                        by = 60*60, tz = "GMT")  ## 1hour = 6 * 10 minutes intervals (10*60sec) = 3600
    end <- seq.POSIXt(from = start[2], to = start[length(start)] + 3600, by = 60*60, tz = "GMT") 
    timeseq <- data.frame(datetime=start, DateTimeend=end)
    timeseq$intchar <- paste(timeseq$datetime, timeseq$DateTimeend, sep="--")  
    
    ## 2) Merge with original activity data
    actm <- merge(timeseq, zz, all=T)
    actm <- arrange(actm, datetime)
    actm$DateTimeend <- c(as.character(actm$datetime[-1]), as.character(tail(actm$datetime, 1) + 3600))
    actm$DateTimeend <- ymd_hms(actm$DateTimeend, tz="GMT" )
    ## calculate duration of each phase in seconds
    actm$duract_sec <- as.numeric(difftime(actm$DateTimeend, actm$datetime, units = "secs"))
    
    ## 3) Assign act to missing values
    actm <- actm %>% fill(Act)
    actm <- actm %>% fill(intchar)
    # actm <- actm %>% fill(Valid)
    
    #### EXTRACT THE SEQUENCE OF ACT
    ####################################
    
    #### create temp column of Act
    actm$Actcode <- ifelse(actm$Act == "wet", "W", "D")
    
    #### remove the first row
    actm2 <- actm[-which(is.na(actm$intchar)), ]
    
    final <- data.frame()
    
    intervals <- unique(actm2$intchar)
    message("Extracting activity variables for hourly intervals ... ")
    
    # create progress bar 
    total <- length(intervals)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    for (i in seq_along(intervals)) {

      intchar <- intervals[i]
      
      # update progress bar
      setTxtProgressBar(pb, i)
      
      
      intsub <- subset(actm, intchar== intervals[i])
      intsub
      
      ActSeq <- paste(as.character(intsub$Actcode), collapse="")        # sequence of states within the interval
      DurSeq <- paste(as.character(intsub$duract_sec), collapse="-")    # sequence of states duration within the interval
      ActDurSeq <- paste(paste0(as.character(intsub$Actcode), as.character(intsub$duract_sec)), collapse="-")
      Nchanges <- length(intsub$Act) - 1                                # number of states changes within the interval
      Nwet <- length(which(as.character(intsub$Act)== "wet"))           # number of "wet" states   (Nwet + Ndry = Nchanges + 1)
      Ndry <- length(which(as.character(intsub$Act) == "dry"))          # number of "dry" states   (Nwet + Ndry = Nchanges + 1)
      Ntakeoff <- str_count(ActSeq, pattern = "WD")
      Nlanding <- str_count(ActSeq, pattern = "DW")
      
      DurWet <- sum(intsub$duract_sec[intsub$Act=="wet"], na.rm=T)      # total duration of "wet" states (in seconds? #minutes? proportion?)
      # MinWet <- ifelse("wet" %in% intsub$Act, min(intsub$duract_sec[intsub$Act=="wet"], na.rm=T), 0)
      # MaxWet <- ifelse("wet" %in% intsub$Act, max(intsub$duract_sec[intsub$Act=="wet"], na.rm=T), 0)
      # MeanWet <- ifelse("wet" %in% intsub$Act, mean(intsub$duract_sec[intsub$Act=="wet"], na.rm=T), 0)
      
      DurDry <- sum(intsub$duract_sec[intsub$Act=="dry"], na.rm=T)      # total duration of "dry" states (in seconds? #minutes? proportion?)
      # MinDry <- ifelse("dry" %in% intsub$Act, min(intsub$duract_sec[intsub$Act=="dry"], na.rm=T), 0)
      # MaxDry <- ifelse("dry" %in% intsub$Act, max(intsub$duract_sec[intsub$Act=="dry"], na.rm=T), 0)
      # MeanDry <- ifelse("dry" %in% intsub$Act, mean(intsub$duract_sec[intsub$Act=="dry"], na.rm=T), 0)
      
      PropWet <- DurWet/sum(intsub$duract_sec)
      PropDry <- 1 - PropWet
      
      variables <- data.frame(intchar, ActSeq, DurSeq, ActDurSeq, Nchanges, Nwet, Ndry, 
                              Ntakeoff, Nlanding, PropWet, DurWet, PropDry, DurDry)
      
      final <- rbind(final, variables)
    }
    close(pb)
    
    
    final$ActCat <- ifelse(final$ActSeq == "D", "dry", 
                           ifelse(final$ ActSeq== "W", "wet", "mix"))  
    
    
    #### FINAL ARRANGEMENT
    final$datetime <- ymd_hms(substr(final$intchar, 1, 19), tz="GMT")  ## assign the activity to start of interval 
    final$date <- as.Date(final$datetime, tz = "GMT")
    final$hour <- hour(final$datetime)
    final$dec_hour <- hour(final$datetime) + minute(final$datetime)/60 + second(final$datetime)/3600
    if (!is.null(ID)) {final$ID <- ID}
    
    ## remove last interval (may not be complete)
    final <- final[-nrow(final),]
    
    #### plot
    if (basPlot) {
      plot(final$datetime, final$PropWet, xlab="datetime", ylab="PropWet", main=paste0("Activity ID: ", ID), cex.main=.8)
    }
    
    return(final)
  }
}

#### END FUNCTION
################################################
################################################


####--------------------------------------------------------------------------~
#### Transform activity data to hourly intervals                           ####
####--------------------------------------------------------------------------~

## to use for data already in dataframe !! for

transf_ActHour <- function (actdata, ... ) {    
  
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
  start <- seq.POSIXt(from = ceiling_date(zz$datetime[1], "hour"), 
                      to = ceiling_date(zz$datetime[length(zz$datetime)] + zz$duration[length(zz$datetime)], "hour"), 
                      by = 60*60, tz = "GMT")  ## 1hour = 6 * 10 minutes intervals (10*60sec) = 3600
  end <- seq.POSIXt(from = start[2], to = start[length(start)] + 3600, by = 60*60, tz = "GMT") 
  timeseq <- data.frame(datetime=start, DateTimeend=end)
  timeseq$intchar <- paste(timeseq$datetime, timeseq$DateTimeend, sep="--")  
  
  ## 2) Merge with original activity data
  actm <- merge(timeseq, zz, all=T)
  actm <- arrange(actm, datetime)
  actm$DateTimeend <- c(as.character(actm$datetime[-1]), as.character(tail(actm$datetime, 1) + 3600))
  actm$DateTimeend <- ymd_hms(actm$DateTimeend, tz="GMT")
  ## calculate duration of each phase in seconds
  actm$duract_sec <- as.numeric(difftime(actm$DateTimeend, actm$datetime, units = "secs"))
  
  ## 3) Assign act to missing values
  actm <- actm %>% fill(act)
  actm <- actm %>% fill(intchar)
  
  #### EXTRACT THE SEQUENCE OF ACT
  ####################################
  
  #### create temp column of Act
  actm$Actcode <- ifelse(actm$act == "wet", "W", "D")
  
  #### remove the first row if needed
  frow <- which(is.na(actm$intchar))
  if(length(frow) == 0) actm2 <- actm
  if(length(frow) != 0) actm2 <- actm[-frow, ]
  
  final <- data.frame()
  
  intervals <- unique(actm2$intchar)
  message("Extracting activity variables for hourly intervals ... ")
  
  # create progress bar 
  total <- length(intervals)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (i in seq_along(intervals)) {
    
    intchar <- intervals[i]
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
    
    intsub <- subset(actm, intchar== intervals[i])
    intsub
    
    ActSeq <- paste(as.character(intsub$Actcode), collapse="")        # sequence of states within the interval
    DurSeq <- paste(as.character(intsub$duract_sec), collapse="-")    # sequence of states duration within the interval
    ActDurSeq <- paste(paste0(as.character(intsub$Actcode), as.character(intsub$duract_sec)), collapse="-")
    Nchanges <- length(intsub$act) - 1                                # number of states changes within the interval
    Nwet <- length(which(as.character(intsub$act)== "wet"))           # number of "wet" states   (Nwet + Ndry = Nchanges + 1)
    Ndry <- length(which(as.character(intsub$act) == "dry"))          # number of "dry" states   (Nwet + Ndry = Nchanges + 1)
    Ntakeoff <- str_count(ActSeq, pattern = "WD")
    Nlanding <- str_count(ActSeq, pattern = "DW")
    
    DurWet <- sum(intsub$duract_sec[intsub$act=="wet"], na.rm=T)      # total duration of "wet" states (in seconds? #minutes? proportion?)
    # MinWet <- ifelse("wet" %in% intsub$act, min(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    # MaxWet <- ifelse("wet" %in% intsub$act, max(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    # MeanWet <- ifelse("wet" %in% intsub$act, mean(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    
    DurDry <- sum(intsub$duract_sec[intsub$act=="dry"], na.rm=T)      # total duration of "dry" states (in seconds? #minutes? proportion?)
    # MinDry <- ifelse("dry" %in% intsub$act, min(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    # MaxDry <- ifelse("dry" %in% intsub$act, max(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    # MeanDry <- ifelse("dry" %in% intsub$act, mean(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    
    PropWet <- DurWet/sum(intsub$duract_sec)
    PropDry <- 1 - PropWet
    
    variables <- data.frame(intchar, ActSeq, DurSeq, ActDurSeq, Nchanges, Nwet, Ndry, 
                            Ntakeoff, Nlanding, PropWet, DurWet, PropDry, DurDry)
    
    final <- rbind(final, variables)
  }
  close(pb)
  
  
  final$ActCat <- ifelse(final$ActSeq == "D", "dry", 
                         ifelse(final$ActSeq== "W", "wet", "mix"))  
  
  
  #### FINAL ARRANGEMENT
  final$datetime <- ymd_hms(substr(final$intchar, 1, 19), tz="GMT")  ## assign the activity to start of interval 
  final$date <- as.Date(final$datetime, tz = "GMT")
  final$hour <- hour(final$datetime)
  final$dec_hour <- hour(final$datetime) + minute(final$datetime)/60 + second(final$datetime)/3600
  # if (!is.null(ID)) {final$ID <- ID}
  
  ## remove last interval (may not be complete)
  final <- final[-nrow(final),]
  
  return(final)
}

#### END FUNCTION





####--------------------------------------------------------------------------~
#### Transform activity data to whatever intervals                         ####
####--------------------------------------------------------------------------~

## to use for data already in dataframe !! 

# *** You need to specify the free_sequence vector as timestamp, in POSIXct format and timezone GMT ***

#### testing
# actdata = filter(actWD, ring == "6198156")
# timeSequence = filter(gps_embc_all, ring == "6198156") %>% dplyr::select(datetime) %>% pull
#### testing

transf_ActFree <- function (actdata, timeSequence = NULL, timeInterval = NULL, ... ) {    
  
  message("Be sure data used come from Biotrack/BAS geolocators, this function is not prepared for Migrate Tech geos.
          For MigTech geos you need to adjuts the timestamp to be the start of the state!!!\n")
  
  #### STOP FUNCTION IF NO PARMETER IS SET
  if (is.null(timeSequence) & is.null(timeInterval)) {
    stop("You need to set one of parameters (timeSequence or timeInterval)!")
  }
  
  if(!is.null(timeSequence) & !is.POSIXct(timeSequence)) {
    stop("*** You need to specify the timeSequence vector as a timestamp, in POSIXct format and timezone GMT ***")
  }
  
  ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Sys.setenv(TZ='GMT') ### !!! IMPORTANT: IF NOT SET LIKE THIS, MAKE PROBLEMS TO CREATE seqdate
  message("******** Time zone has been set to GMT/UTC !!! ********")
  ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  library(tidyr)      ## {fill}
  library(doBy)
  library(lubridate)
  library(reshape2)   ## {dcast}
  library(plyr)
  
  
  #### for wet-dry type  
  #####################
  
  zz <- actdata  
  zz <- arrange(zz, datetime)
  
  #### TRANSFORM "WET/DRY" TO NUMERIC VALUES (1-200)
  
  #### THE ALGORITHM TO TRANSFORM TO CONTINUOUS 
  ## 1) Create timesequence 
  
  
  
  #### A) CREATING REGULAR x MINUTES TIMESTAMP                ####
  ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(timeInterval) & is.null(timeSequence)) {
    
    start <- seq.POSIXt(from = zz$datetime[1], to = zz$datetime[length(zz$datetime)] + zz$duration[length(zz$datetime)], 
                        by = timeInterval, tz = "GMT")  
    
    end <- seq.POSIXt(from = start[2], to = start[length(start)] + timeInterval, by = timeInterval, tz = "GMT") 
    timeseq <- data.frame(datetime=start, DateTimeend=end)
    timeseq$intchar <- paste(timeseq$datetime, timeseq$DateTimeend, sep="--")  
    timeseq$diff <- as.numeric(timeseq$DateTimeend - timeseq$datetime)
    # head(timeseq)
    message(paste0("Using timesequence of regular ", timeInterval, " secs intervals. From ", timeseq$datetime[1], " until ", timeseq$datetime[length(timeseq$datetime)], 
                   ", totally ", as.numeric(round (difftime(timeseq$datetime[length(timeseq$datetime)], timeseq$datetime[1], unit="days"), 1)),  " days." ))
  }
  
  #### B) USING IR/REGULAR TIMESEQUENCE FROM timeSequence VECTOR   ####  (FOR GPS FIXES)
  ####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(timeSequence) & is.null(timeInterval)) {
    
    # if(is.null(trackStart) & is.null(trackEnd)){
    #   start <- sort(timeSequence)
    #   end <- as.POSIXct(c(start[-1], start[length(start)] + 600), tz = "GMT")
    # }  
    # if(!is.null(trackStart) & !is.null(trackEnd)){
    #   start <- as.POSIXct(c(trackStart, sort(timeSequence)), tz = "GMT")
    #   end <- as.POSIXct(c(start[-1], trackEnd), tz = "GMT")
    # }  
    
    start <- sort(timeSequence)
    end <- as.POSIXct(c(start[-1], start[length(start)] + 600), tz = "GMT")
    
    timeseq <- data.frame(datetime=start, DateTimeend=end)
    timeseq$intchar <- paste(timeseq$datetime, timeseq$DateTimeend, sep="--")
    timeseq$diff <- as.numeric(difftime(timeseq$DateTimeend, timeseq$datetime, units="mins"))
    ## check if there are 0 diffs and remove
    timeseq <- dplyr::filter(timeseq, diff !=0)
    
    message(paste0("Using timesequence from timeSequence (mean = ", round(mean(timeseq$diff), 2), 
                   " minutes, range ", round(range(timeseq$diff), 2)[1], " - ",
                   round(range(timeseq$diff), 2)[2], " intervals. ", 
                   "From ", timeseq$datetime[1], " until ", timeseq$datetime[length(timeseq$datetime)], 
                   ", totally ", as.numeric(round (difftime(timeseq$datetime[length(timeseq$datetime)], timeseq$datetime[1], unit="days"), 1)),  " days." ))
    rbind(head(timeseq), tail(timeseq))
  }  
  
  
  ## 2) Merge with original activity data
  actm <- merge(timeseq, zz, all=T)
  actm <- arrange(actm, datetime)
  actm$DateTimeend <- c(as.character(actm$datetime[-1]), as.character(tail(actm$datetime, 1) + 3600))
  actm$DateTimeend <- ymd_hms(actm$DateTimeend, tz="GMT")
  ## calculate duration of each phase in seconds
  actm$duract_sec <- as.numeric(difftime(actm$DateTimeend, actm$datetime, units = "secs"))
  
  ## 3) Assign act to missing values
  actm <- actm %>% fill(act)
  actm <- actm %>% fill(intchar)
  
  #### EXTRACT THE SEQUENCE OF ACT
  ####################################
  
  #### create temp column of Act
  actm$Actcode <- ifelse(actm$act == "wet", "W", "D")
  
  #### remove the first row if needed
  frow <- which(is.na(actm$intchar))
  if(length(frow) == 0) actm2 <- actm
  if(length(frow) != 0) actm2 <- actm[-frow, ]
  
  final <- data.frame()
  
  intervals <- unique(actm2$intchar)
  message("Extracting activity variables  ... ")
  
  # create progress bar 
  total <- length(intervals)
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  
  for (i in seq_along(intervals)) {
    
    intchar <- intervals[i]
    
    # update progress bar
    setTxtProgressBar(pb, i)
    
    
    intsub <- subset(actm, intchar== intervals[i])
    intsub
    
    ActSeq <- paste(as.character(intsub$Actcode), collapse="")        # sequence of states within the interval
    DurSeq <- paste(as.character(intsub$duract_sec), collapse="-")    # sequence of states duration within the interval
    ActDurSeq <- paste(paste0(as.character(intsub$Actcode), as.character(intsub$duract_sec)), collapse="-")
    Nchanges <- length(intsub$act) - 1                                # number of states changes within the interval
    Nwet <- length(which(as.character(intsub$act)== "wet"))           # number of "wet" states   (Nwet + Ndry = Nchanges + 1)
    Ndry <- length(which(as.character(intsub$act) == "dry"))          # number of "dry" states   (Nwet + Ndry = Nchanges + 1)
    Ntakeoff <- str_count(ActSeq, pattern = "WD")
    Nlanding <- str_count(ActSeq, pattern = "DW")
    
    DurWet <- sum(intsub$duract_sec[intsub$act=="wet"], na.rm=T)      # total duration of "wet" states (in seconds? #minutes? proportion?)
    # MinWet <- ifelse("wet" %in% intsub$act, min(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    # MaxWet <- ifelse("wet" %in% intsub$act, max(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    # MeanWet <- ifelse("wet" %in% intsub$act, mean(intsub$duract_sec[intsub$act=="wet"], na.rm=T), 0)
    
    DurDry <- sum(intsub$duract_sec[intsub$act=="dry"], na.rm=T)      # total duration of "dry" states (in seconds? #minutes? proportion?)
    # MinDry <- ifelse("dry" %in% intsub$act, min(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    # MaxDry <- ifelse("dry" %in% intsub$act, max(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    # MeanDry <- ifelse("dry" %in% intsub$act, mean(intsub$duract_sec[intsub$act=="dry"], na.rm=T), 0)
    
    PropWet <- DurWet/sum(intsub$duract_sec)
    PropDry <- 1 - PropWet
    
    variables <- data.frame(intchar, ActSeq, DurSeq, ActDurSeq, Nchanges, Nwet, Ndry, 
                            Ntakeoff, Nlanding, PropWet, DurWet, MinWet, MaxWet, MeanWet, PropDry, DurDry, MinDry, MaxDry, MeanDry)
    
    final <- rbind(final, variables)
  }
  close(pb)
  
  
  final$ActCat <- ifelse(final$ActSeq == "D", "dry", 
                         ifelse(final$ActSeq== "W", "wet", "mix"))  
  
  
  #### FINAL ARRANGEMENT
  final$datetime <- ymd_hms(substr(final$intchar, 1, 19), tz="GMT")  ## assign the activity to start of interval 
  final$date <- as.Date(final$datetime, tz = "GMT")
  final$hour <- hour(final$datetime)
  final$dec_hour <- hour(final$datetime) + minute(final$datetime)/60 + second(final$datetime)/3600
  # if (!is.null(ID)) {final$ID <- ID}
  
  ## remove last interval (may not be complete)
  final <- final[-nrow(final),]
  
  return(final)
}

#### END FUNCTION
