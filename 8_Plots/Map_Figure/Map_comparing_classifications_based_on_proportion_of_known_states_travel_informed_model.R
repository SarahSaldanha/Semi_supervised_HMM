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
library(RStoolbox)
library(stringr)


#####lets do this with iteration 1... First we need to make a map of the knownstates, indicating which were used for the confusion matrix. 



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")


allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks$  iteration2_90),allks$true_state_act_inactive, NA)

table(allks$true_state_act_inactive)




Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/fitTrack_0.75/iteration2"), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
iteration<-"iteration2"
  
file_name <-Original_files[15] ###take iteration 10 to match with previous code
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1, 5)
    
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/fitTrack_0.75/iteration2/",  file_name))
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"), by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    
    joined<-dplyr::select(joined, ring_dep_trip = ID , dateTime_gps=time, lon = x, lat=y, state, prob1, probp2, prob3,   iteration2_90, test_state_full_act_inactive, true_state_act_inactive, true_state, Tag_type)
   
 

head(joined)

###make #plot with points indicating the prop wet 


## make #plots

theme_base <- theme(axis.title = element_text(size=12), 
                    axis.text = element_text(size=10),
                    legend.title = element_text(size=12),
                    legend.text= element_text(size=10),
                    legend.key.size = unit(0.75, "cm"),
                    panel.grid = element_blank(), panel.border = element_blank(),
                    panel.background = element_rect(fill = "white", colour = "black"))



coastLine_toplot <- readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/Data_no_ID_GPS/capeVerde_Coast.rds")



#rm(mapExtendLon, mapExtendLat)


joined<-joined[with(joined, order(ring_dep_trip, dateTime_gps)),]

joined$state_factor<-ifelse(joined$state == 1, "resting", ifelse(joined$state == 2, "foraging", "travelling"))

joined$true_state_marker<-ifelse(!is.na(joined$true_state_act_inactive), 1, 0.5)
joined$test_state_marker<-ifelse(!is.na(joined$test_state_full_act_inactive), 1, 0.5)


joined<-joined %>% 
    group_by(ring_dep_trip) %>%
     arrange(ring_dep_trip, dateTime_gps) %>%
  ungroup()

all_trips<-unique(joined$ring_dep_trip)

#ID_197_250_595

# joined %>%
#   group_by(ring_dep_trip) %>%
#   arrange(ring_dep_trip, dateTime_gps)%>%
#   group_map(
#     .f = ~ ggplot(.x, aes(x = lon, y = lat)) +
#       geom_point())


###select your trip here. I have this set up as a loop to plot all the trips but 
#29 is the example trip that was inclued in the paper 

ii<-29
plot(subset(joined, ring_dep_trip == all_trips[ii])$lon, subset(joined, ring_dep_trip == all_trips[ii])$lat)
enter


#for (ii in 1:length(all_trips)){
  

  
#trips<-c( "7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_2")

PLOT_SUB<-subset(joined, ring_dep_trip == all_trips[ii])
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))

mapExtendLon <- abs(diff(range(PLOT_SUB$lon)))/3
mapExtendLat <- abs(diff(range(PLOT_SUB$lat)))/3

mapLimitsY <- c(min(PLOT_SUB$lat) -mapExtendLat, max(PLOT_SUB$lat) +mapExtendLat) 
mapLimitsX <- c(min(PLOT_SUB$lon) -mapExtendLon, max(PLOT_SUB$lon) +mapExtendLon)


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
###WHAT DO i WANT.... ONE MAP SHOWING THE TEST STATES OF THIS iteration 

plot(PLOT_SUB$lon, PLOT_SUB$lat)
enter



####fULL 

PLOT_SUB$true_state_factor<-as.factor(ifelse(!is.na(PLOT_SUB$true_state_act_inactive), PLOT_SUB$true_state_act_inactive, 6))
PLOT_SUB$true_state_factor<-ifelse(PLOT_SUB$true_state_factor == 1, "resting", ifelse(PLOT_SUB$true_state_factor == 2, "foraging",ifelse(PLOT_SUB$true_state_factor == 3, "travelling",  "unknown")))
PLOT_SUB$test_state_full_act_inactive_plot<-ifelse(!is.na(PLOT_SUB$test_state_full_act_inactive),1, 0)

#####Probability
PLOT_SUB$highest_prob<- pmax(PLOT_SUB$prob1, PLOT_SUB$probp2,PLOT_SUB$prob3)
PLOT_SUB$highest_prob_over0.90<-ifelse(PLOT_SUB$highest_prob > 0.9, 10, 0)


PLOT_SUB$correct_classification<-ifelse(PLOT_SUB$test_state_full_act_inactive == PLOT_SUB$state, 1, ifelse(is.na(PLOT_SUB$test_state_full_act_inactive), NA, 0))

PLOT_SUB$behaviour_of_highest_prob<-ifelse(PLOT_SUB$highest_prob == PLOT_SUB$prob1, "resting", ifelse(PLOT_SUB$highest_prob == PLOT_SUB$probp2, "foraging", "travelling" ))



##for the plot... force points 



PLOT_SUB_prob<-dplyr::select(PLOT_SUB, lon, lat, COLOR = highest_prob_over0.90)
PLOT_SUB_prob$SIZE <- 0

PLOT_SUB_prob$prop_ks<-"Probability"








PLOT_SUB_TEST<-dplyr::select(PLOT_SUB, lon, lat, COLOR = true_state_factor, SIZE =test_state_full_act_inactive_plot )
PLOT_SUB_TEST$prop_ks<-"test_states"

PLOT_SUB_FULL<-dplyr::select(PLOT_SUB, lon, lat, COLOR = state_factor, SIZE =test_state_full_act_inactive_plot )
PLOT_SUB_FULL$prop_ks<-"0.75"




####now do with the other proportions of known states


file_name2 <-Original_files[16]###0 ####will need to change this eventually to have 0
it2<-str_split(file_name2, "_")
it2<-it2[[1]]
it2<-it2[7]
it2<-str_sub(it2,1,-5)

Model<-it2
#Read in the HMM output
output2<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/fitTrack_0.75/iteration2", "/", file_name2))
####Merge with the known states 
joined2<- merge(output2, allks, by.x = c("ID", "time", "x", "y"), by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))

joined2<-dplyr::select(joined2, ring_dep_trip = ID , dateTime_gps=time, lon = x, lat=y, state, prob1, probp2, prob3,   iteration2_90, test_state_full_act_inactive, true_state_act_inactive, true_state, Tag_type)

table(joined2$Tag_type, joined2$ring_dep_trip)

joined2<-joined2[with(joined2, order(ring_dep_trip, dateTime_gps)),]

joined2$state_factor<-ifelse(joined2$state == 1, "resting", ifelse(joined2$state == 2, "foraging", "travelling"))

joined2$true_state_marker<-ifelse(!is.na(joined2$true_state_act_inactive), 1, 0.5)
joined2$test_state_marker<-ifelse(!is.na(joined2$test_state_full), 1, 0.5)



PLOT_SUB2<-subset(joined2, ring_dep_trip == all_trips[ii])
PLOT_SUB2<-subset(PLOT_SUB2, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
###WHAT DO i WANT.... ONE MAP SHOWING THE TEST STATES OF THIS iteration 


####


PLOT_SUB2$true_state_factor<-as.factor(ifelse(!is.na(PLOT_SUB2$true_state), PLOT_SUB2$true_state, 6))
PLOT_SUB2$test_state_full_plot<-ifelse(!is.na(PLOT_SUB2$test_state_full),1, 0)

PLOT_SUB_0<-dplyr::select(PLOT_SUB2, lon, lat, COLOR = state_factor, SIZE =test_state_full_plot )
PLOT_SUB_0$prop_ks<-"0.00"

plot_sub_all<-rbind(PLOT_SUB_TEST, PLOT_SUB_0, PLOT_SUB_FULL, PLOT_SUB_prob)
plot_sub_all$prop_ks <- ordered(plot_sub_all$prop_ks, levels = c("test_states", "0.00", "0.75", "Probability"))

#pdf(file = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS//Plots_of_classifications/",all_trips[ii], ".pdf"),width = 6, height = 6) # The height of the plot in inches


a<-ggplot(data = plot_sub_all, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = plot_sub_all, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = plot_sub_all, aes(lon, lat, color = COLOR, size = SIZE), alpha = 0.5) +
  geom_point(data = plot_sub_all, aes(lon, lat, size = SIZE, color = COLOR),  shape = 1)+
  scale_size_continuous(range = c(0.5, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=c("black", "grey", "red","#FFCC33", "cyan", "grey45"))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  facet_wrap(~ prop_ks)+
  annotate("segment", x = plot_sub_all$lon[5], xend = plot_sub_all$lon[6], y = plot_sub_all$lat[5], yend =  plot_sub_all$lat[6], colour = "black", size=1, arrow=arrow())+
  
  
  #annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  #annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = plot_sub_all$lon[1], y = plot_sub_all$lat[1], colour = "black", shape = 17, size = 2)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")

#print(a)

#dev.off()

####lets do this for a bunch of trips?? 





#}


































##############Make one nice Map for the papers... 




####0000 breeding
Test_states<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat,  fill = true_state_factor, size = test_state_full_act_inactive_plot, color = true_state_factor),  pch=21,  stroke = 0.1) +

  scale_size_continuous(range = c(1, 3))+
  scale_colour_manual(values=alpha(c("red","#FFCC33", "cyan", "grey45"), 0.8))+
  scale_fill_manual(values =alpha(c( "red","#FFCC33", "cyan", "grey45"),0.6))+

  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+

  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") +
  
  annotate("text", x=-24.3, y=15.5, label= "A",size = 8) + 
  labs(x = "Longitude", y = "Latitude")


Test_states

####ZOOM 1

zoom_1<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat,  fill = true_state_factor, size = test_state_full_act_inactive_plot, color = true_state_factor),  pch=21,  stroke = 0.1) +
  
  scale_size_continuous(range = c(1, 4))+
  scale_colour_manual(values=alpha(c("red","#FFCC33", "cyan", "grey45"), 0.8))+
  scale_fill_manual(values =alpha(c( "red","#FFCC33", "cyan", "grey45"),0.6))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.68, -25.45))+
  ylim(c (14.22, 14.46))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") +
  annotate("text", x=-24.3, y=15.5, label= "A",size = 8) + 
  labs(x = "Longitude", y = "Latitude")


zoom_1

zoom_2<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat,  fill = true_state_factor, size = test_state_full_act_inactive_plot, color = true_state_factor),  pch=21,  stroke = 0.1) +
  
  scale_size_continuous(range = c(1, 4))+
  scale_colour_manual(values=alpha(c("red","#FFCC33", "cyan", "grey45"), 0.8))+
  scale_fill_manual(values =alpha(c( "red","#FFCC33", "cyan", "grey45"),0.6))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.2, -24.88))+
  ylim(c (14.78, 14.98))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red")  +
  annotate("text", x=-24.3, y=15.5, label= "C",size = 8) + 
  labs(x = "Longitude", y = "Latitude")


zoom_2
###################now behavioural classification




####0000 breeding
Prop0.75<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") +
  annotate("text", x=-24.3, y=15.5, label= "C",size = 8) +
  labs(x = "Longitude", y = "Latitude")


Prop0.75

####ZOOM 1

zoom_1_0.75<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.68, -25.45))+
  ylim(c (14.22, 14.46))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.97 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red")  +
  annotate("text", x=-24.3, y=15.5, label= "C",size = 8) +
  labs(x = "Longitude", y = "Latitude")


zoom_1_0.75

zoom_2_0.75<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.2, -24.88))+
  ylim(c (14.78, 14.98))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red")  +
  annotate("text", x=-24.3, y=15.5, label= "C",size = 8) + 
  labs(x = "Longitude", y = "Latitude")



zoom_2_0.75
###########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################



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

####0000 breeding
Prob0.9<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(highest_prob_over0.90), fill =as.factor(highest_prob_over0.90) , size = test_state_full_act_inactive_plot), pch = 21, stroke =0.1 ) +
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red", "grey")), 0.9)+
  scale_fill_manual(values=alpha(c("red", "grey")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+

  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red")  +
  annotate("text", x=-24.3, y=15.5, label= "D",size = 8) +
  labs(x = "Longitude", y = "Latitude")


Prob0.9

####ZOOM 1


zoom_1_prob0.9<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(highest_prob_over0.90), fill =as.factor(highest_prob_over0.90) , size = test_state_full_act_inactive_plot), pch = 21, stroke =0.1 ) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red", "grey")), 0.9)+
  scale_fill_manual(values=alpha(c("red", "grey")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.68, -25.45))+
  ylim(c (14.22, 14.46))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.97 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") 

zoom_1_prob0.9

zoom_2_prob0.9<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = as.factor(highest_prob_over0.90), fill =as.factor(highest_prob_over0.90) , size = test_state_full_act_inactive_plot), pch = 21, stroke =0.1 ) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red", "grey")), 0.9)+
  scale_fill_manual(values=alpha(c("red", "grey")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.2, -24.88))+
  ylim(c (14.78, 14.98))+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") +
  annotate("text", x=-24.3, y=15.5, label= "A",size = 8) + 
  labs(x = "Longitude", y = "Latitude")



zoom_2_prob0.9


#################################################################################################################################################################################################################################################################################################################################################################################################################################################


####now do with the other proportions of known states


file_name2 <-Original_files[16]###0 ####will need to change this eventually to have 0
it2<-str_split(file_name2, "_")
it2<-it2[[1]]
it2<-it2[7]
it2<-str_sub(it2,1,-5)

Model<-it2
#Read in the HMM output
output2<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/fitTrack_0.75/iteration2/", file_name2))
####Merge with the known states 
joined2<- merge(output2, allks, by.x = c("ID", "time", "x", "y"), by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))

joined2<-dplyr::select(joined2, ring_dep_trip = ID , dateTime_gps=time, lon = x, lat=y, state, prob1, probp2, prob3,   iteration2_90, test_state_full_act_inactive, true_state_act_inactive, true_state, Tag_type)

table(joined2$Tag_type, joined2$ring_dep_trip)

joined2<-joined2[with(joined2, order(ring_dep_trip, dateTime_gps)),]

joined2$state_factor<-ifelse(joined2$state == 1, "resting", ifelse(joined2$state == 2, "foraging", "travelling"))

joined2$true_state_marker<-ifelse(!is.na(joined2$true_state_act_inactive), 1, 0.5)
joined2$test_state_marker<-ifelse(!is.na(joined2$test_state_full), 1, 0.5)



PLOT_SUB2<-subset(joined2, ring_dep_trip == all_trips[ii])
PLOT_SUB2<-subset(PLOT_SUB2, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
###WHAT DO i WANT.... ONE MAP SHOWING THE TEST STATES OF THIS iteration 


####


PLOT_SUB2$true_state_factor<-as.factor(ifelse(!is.na(PLOT_SUB2$true_state), PLOT_SUB2$true_state, 6))
PLOT_SUB2$test_state_full_plot<-ifelse(!is.na(PLOT_SUB2$test_state_full),1, 0)



#####Probability
PLOT_SUB2$highest_prob<- pmax(PLOT_SUB2$prob1, PLOT_SUB2$probp2,PLOT_SUB2$prob3)
PLOT_SUB2$highest_prob_over0.90<-ifelse(PLOT_SUB2$highest_prob > 0.9, 10, 0)


PLOT_SUB2$correct_classification<-ifelse(PLOT_SUB2$test_state_full_act_inactive == PLOT_SUB2$state, 1, ifelse(is.na(PLOT_SUB2$test_state_full_act_inactive), NA, 0))

PLOT_SUB2$test_state_full_act_inactive_plot<-ifelse(!is.na(PLOT_SUB2$test_state_full_act_inactive),1, 0)


####0000 breeding
Prop0<-ggplot(data = PLOT_SUB2, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB2, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB2, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  # annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  # annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red")+
  annotate("text", x=-24.3, y=15.5, label= "B",size = 8) + 
  labs(x = "Longitude", y = "Latitude") 


Prop0
####ZOOM 1

zoom_1_0<-ggplot(data = PLOT_SUB2, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB2, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB2, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  #scale_size_discrete()+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.68, -25.45))+
  ylim(c (14.22, 14.46))+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.97 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") 

zoom_1_0

zoom_2_0<-ggplot(data = PLOT_SUB2, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB2, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB2, 
             aes(lon, lat, color = as.factor(correct_classification), fill = as.factor(correct_classification), size = test_state_full_act_inactive_plot ), pch=21, stroke = 0.1, alpha =0.8) +
  scale_size_continuous(range = c(1, 4))+
  #scale_size_discrete()+
  scale_colour_manual(values=alpha(c( "red","green", "grey45")), 0.8)+
  scale_fill_manual(values=alpha(c( "red","green", "grey45")), 0.6)+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(c (-25.2, -24.88))+
  ylim(c (14.78, 14.98))+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")+
  
  geom_rect(mapping=aes(xmin=-25.2, xmax=-24.88, ymin = 14.78, ymax=14.98 ), fill = NA,  color="darkgreen") +
  
  geom_rect(mapping=aes(xmin=-25.68, xmax=-25.45, ymin = 14.22, ymax=14.46 ), fill = NA,  color="red") +
  annotate("text", x=-24.3, y=15.5, label= "A",size = 8) + 
  labs(x = "Longitude", y = "Latitude")



zoom_2_0








###try to arrange these.... 
library(gridExtra)
library(grid)
library(lattice)


grid.arrange( Test_states, Prop0, Prop0.75, Prob0.9,  nrow = 2)














###make a facetted plot



probability<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = highest_prob ), alpha = 0.4, size = 2) +
  geom_point(data = PLOT_SUB, aes(lon, lat, size = test_state_full_act_inactive_plot , color = state_factor),  shape = 1)+
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=c( "red","#FFCC33", "cyan"))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")



###make a facetted plot




















####3*4.5 in pdf and then 152 percent in view 

#tiff("C:/Users/Sarah Saldanha/Dropbox/sarah_fenha/All.GPSandAXYS/Geos/PropWet.tiff", width = 6, height = 8, units = 'in', res = 600, compression = 'none')
test<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.2) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = true_state_factor, size = test_state_full_plot), alpha = 0.4) +
  geom_point(data = PLOT_SUB, aes(lon, lat, size = test_state_full_plot, color = true_state_factor),  shape = 1)+
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=c( "#FFCC33","red", "grey45"))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
theme(legend.position = "none")
#dev.off()





full<-ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = state_factor, size = test_state_full_plot), alpha = 0.4) +
  geom_point(data = PLOT_SUB, aes(lon, lat, size = test_state_full_plot, color = state_factor),  shape = 1)+
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=c( "red","#FFCC33", "cyan"))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")
























 










library(gridExtra)
grid.arrange(
  test,
  no_ks,
  full,
  nrow = 1)











#######
file_name <-Original_files[6]###0
it<-str_split(file_name, "_")
it<-it[[1]]
it<-it[6]
it<-str_sub(it,1,-5)

Model<-it
#Read in the HMM output
output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMS/fitTrack_0.75/  iteration2/", file_name))
####Merge with the known states 
joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"), by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))

joined<-dplyr::select(joined, ring_dep_trip = ID , dateTime_gps=time, lon = x, lat=y, state, prob1, probp2, prob3,   iteration2_90, test_state_full_act_inactive, true_state_act_inactive, true_state, Tag_type)

table(joined$Tag_type, joined$ring_dep_trip)


head(joined)


joined<-joined[with(joined, order(ring_dep_trip, dateTime_gps)),]

joined$state_factor<-ifelse(joined$state == 1, "resting", ifelse(joined$state == 2, "foraging", "travelling"))

joined$true_state_marker<-ifelse(!is.na(joined$true_state), 1, 0.5)
joined$test_state_marker<-ifelse(!is.na(joined$test_state_full), 1, 0.5)

head(joined)

####"7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_3", SHORT
####"7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_2", A BIT TO THE SIDE BUT MAYBE

###&"8201653_PHAAET_rec21012021_ICima_ninho_39_36_S1_1",
#######"8201653_PHAAET_rec21012021_ICima_ninho_39_36_S1_2",
####"8201701_PHAAET_rec21022021_ICima_ninho_62_21_S1_2",
####"8201653_PHAAET_rec21012021_ICima_ninho_39_36_S1_1",
####"8201666_PHAAET_rec04022021_ICima_ninho_39_37_S1_1",

####"8201653_PHAAET_rec21012021_ICima_ninho_39_36_S1_1"
####"8201653_PHAAET_rec21012021_ICima_ninho_39_36_S1_3",
trips<-c("7502351_PHAAET_rec20042021_ICima_ninho_3_37_S1_2")

PLOT_SUB<-subset(joined, ring_dep_trip %in%trips)
PLOT_SUB<-subset(PLOT_SUB, !is.na(lon))


####LETS CHOOSE SOME TRACKS FOR COMPARISON PLOTS 
###WHAT DO i WANT.... ONE MAP SHOWING THE TEST STATES OF THIS iteration 


####

PLOT_SUB$true_state_factor<-as.factor(ifelse(!is.na(PLOT_SUB$true_state), PLOT_SUB$true_state, 6))
PLOT_SUB$test_state_full_plot<-ifelse(!is.na(PLOT_SUB$test_state_full),1, 0)
####3*4.5 in pdf and then 152 percent in view 

ggplot(data = PLOT_SUB, aes(x = lon, y = lat)) + 
  geom_polygon(data = coastLine_toplot, aes(x = long, y = lat, group = group), color = "black", fill = "grey80") +
  
  # add in birds
  geom_path(data = PLOT_SUB, aes(lon, lat), size = 0.5, alpha = 0.5) +  
  geom_point(data = PLOT_SUB, 
             aes(lon, lat, color = state_factor, size = test_state_full_plot), alpha = 0.4) +
  geom_point(data = PLOT_SUB, aes(lon, lat, size = test_state_full_plot, color = state_factor),  shape = 1)+
  scale_size_continuous(range = c(1, 3))+
  #scale_size_discrete()+
  scale_colour_manual(values=c( "red","#FFCC33", "cyan"))+
  xlab("lon") +
  ylab("lat") +
  coord_fixed(ratio = 1) +
  xlim(mapLimitsX)+
  ylim(mapLimitsY)+
  annotate("segment", x = -25.15, xend = -25.25, y = 15.2, yend = 15.05, colour = "black", size=1, arrow=arrow())+
  annotate("segment", x = -25.35, xend = -25.15, y = 14.28, yend = 14.27, colour = "black", size=1, arrow=arrow())+
  annotate("point", x = -24.636917, y = 14.971024, colour = "black", shape = 17, size = 4)+
  #scale_x_continuous(limits = mapLimitsX ) +
  #scale_y_continuous(limits = mapLimitsY) +
  theme_base +
  theme(legend.position = "none")












