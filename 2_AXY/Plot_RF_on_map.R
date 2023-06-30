####
rm(list=ls())

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



####read in the data coming out of the Ranfom Forest Model 
setwd("D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/TRIPS_WITH_WET_DRY")

data<-readRDS("./accdata3_wet_dry_cmp.rds")

memory.limit(size=9999999999)

filepath <- "D:/Dropbox/Sarah Saldanha (1)/All.GPSandAXYS/axys_all_files/Trips/"

#####now merge with the gps positions



####accdata4 have original lat and lon but we need the interpolated lat and lon... 

birdTrack <- readRDS("D:/Dropbox/Sarah Saldanha (1)/Methods_Paper/Final_Data/GPS_Methods_paper_2021.rds")
table(birdTrack$region)

birdTrack<-subset(birdTrack, region =="CaboVerde")


#####add nest distance

birdTrack$nestLoc_lon<-as.numeric(birdTrack$nestLoc_lon)
birdTrack$nestLoc_lat<-as.numeric(birdTrack$nestLoc_lat)

library(geosphere)

birdTrack$dist_colony<-distHaversine(cbind(birdTrack$lon, birdTrack$lat), cbind(birdTrack$nestLoc_lon, birdTrack$nestLoc_lat))



###select only complete trips
#comtrip<-subset(birdTrack,complete == "c")
#comtrip$complete<-as.factor(as.character(comtrip$complete))


birdTrack<-birdTrack[with(birdTrack, order(contBlock, dateTime)),]





####create tripunique variable

length(unique(birdTrack$refID_corrected))
length(unique(birdTrack$tripID2))
length(unique(birdTrack$contBlock))
birdTrack$tripunique2<-birdTrack$refID_corrected*100 + birdTrack$tripID2


####create tripID variable in bird Track  

birdTrack$tripID<-paste0(birdTrack$ID, "_", birdTrack$tripID2)

gps_merge<-select(birdTrack, lon_int =lon, lat_int= lat, alt_int = alt, dateTime_int =dateTime,refID_corrected, tripID2, contBlock, tripID, nestLoc_lat, nestLoc_lon, dist_colony, tripunique2)



gps_merge$dateTime<-gps_merge$dateTime_int

#######Now join this with accdata4 based on dateTime and tripID

data$dateTime_minute<-floor_date(data$Timestamp3, unit = "minutes")
                                 



accdata5<-left_join(data, gps_merge, by = c("tripID" = "tripID", "dateTime_minute" = "dateTime", "tripID2" = "tripID2", "refID_corrected" = "refID_corrected", "contBlock" = "contBlock"  ))


#plot(accdata5$lon_int, accdata5$lat_int)


accdata5 <- accdata5 %>%
  group_by(tripID)%>%
   fill(refID_corrected)%>% fill(tripID2)%>% fill(contBlock) %>% fill(ring)  %>% fill(nestLoc_lat)%>% fill(nestLoc_lon)%>% fill(dist_colony)%>% fill(tripunique2) %>% fill(complete)  %>% fill(Satellites)%>%
  ungroup()

accdata5<-accdata5[with(accdata5, order( tripID2, Timestamp3)),]

accdata5<-accdata5 %>%
  group_by(tripID2)%>%
  mutate(spline_lat = na.spline(c(lat_int)), 
         spline_lon = na.spline(c(lon_int))) %>%
  ungroup()





accdata5$spline_lon

#utils::View(accdata5)





  ####SUBSET one track to plot behaviours. 

## make #plots

theme_base <- theme(axis.title = element_text(size=12), 
                    axis.text = element_text(size=10),
                    legend.title = element_text(size=12),
                    legend.text= element_text(size=10),
                    legend.key.size = unit(0.75, "cm"),
                    panel.grid = element_blank(), panel.border = element_blank(),
                    panel.background = element_rect(fill = "white", colour = "black"))


coastLine_toplot <- readRDS("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/capeVerde_Coast.rds")
#mapExtendLon <- abs(diff(range(trial$lon)))/5
#mapExtendLat <- abs(diff(range(trial$lat)))/5




##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


##geo == "T030001" | geo == "J459001"| geo ==  "J487001"

table(accdata5$tripID)

PLOT_SUB<-subset(accdata5, tripID== "8201689_PHAAET_rec09042021_ICima_ninho_27_21_S1_1")

plot(PLOT_SUB$lon_int, PLOT_SUB$lat_int)







PLOT_SUB$spline_lat

####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 

library(RColorBrewer)
myColors <- brewer.pal(4,"Set1")
names(myColors) <- levels(PLOT_SUB$Labels4)
colScale <- scale_colour_manual(name = "grp",values = myColors)



######this plot is fucked.... looks like the behaviours are not classified well at all.Not dealing with it now... but need to look into this!  

#"tiff("D:/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/PropWet.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
ggplot(data = PLOT_SUB, aes(x = spline_lon, y = spline_lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(spline_lon, spline_lat), size = 0.5, alpha = 0.8) +  
  geom_point(data = PLOT_SUB, 
             aes(spline_lon, spline_lat, color = Labels4), size = 1, alpha = 0.8) + 
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values=c("red", "cyan", "goldenrod", "white"))+

  theme_base





  # general formatting
  scale_colour_gradient2( midpoint = 0.5, name = "PropWet", low = ("cyan"), mid = ("darkred"),  high = ("gold"), limits = c(0, 1), na.value = NA) +

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
  scale_colour_gradient2( midpoint = 10.5, name = "Nlanding", low = "cyan",  mid = "gold", high = "darkred", limits = c(0, 21), na.value = NA) +
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base
dev.off()




accdata5








table(tail(accdata5, 750000)$lat_int)####looks good. 


####interpolate between gps positions

# map wet/dry

#map accelerometry behaviour marked from the moels with, 0.8 and 0.9




