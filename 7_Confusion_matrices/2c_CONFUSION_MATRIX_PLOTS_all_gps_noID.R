
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
library(geosphere)
library(data.table)
library("momentuHMM")
library("ggplot2")
library("ggsn")
library("circular")
library("parallel")
library(cvms)
library(broom)    # tidy()
library(tibble)
library(qwraps2)
library(caret)
library(data.table)
library(pROC)
library(VUROCS)

op <- options(digits.secs=6)
options(expressions = 20000)

memory.limit(size=99999999)





setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds")


allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks$iteration10_90),allks$true_state, NA)

allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks$iteration10_90),allks$true_state_act_inactive, NA)

table(allks$true_state_act_inactive)





####READ in the random iterations so that I have the known states associated with each. 


###check how often predicted behaviour == behaviour. 



###MAKE FUNCTION THAT OPENS EACH FILE, EXTRACTS THE CONFUSION MATRIX


all_iterations<-c("iteration1", "iteration2", "iteration3", "iteration4", "iteration5", "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")



for (iteration in all_iterations){
  #iteration<-all_iterations[2]
  #file_name<-Original_files[1]

  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  
  
  for (file_name in Original_files){
  
  #file_name <-Original_files[1]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1,-5)

  Model<-it
  #Read in the HMM output
  output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
  
  table(output$state)
  ####Merge with the known states 
 joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
 
 table(joined$test_state_full_act_inactive)
  
 conf_mat <- confusionMatrix(as.factor(joined$state), as.factor(joined$test_state_full_act_inactive))
 
 Accuracy<- conf_mat$overall[1]
 Sensitivity_1<-conf_mat$byClass[1]
 Sensitivity_2<-conf_mat$byClass[2]
 Sensitivity_3<-conf_mat$byClass[3]
 
 Specificity_1<-conf_mat$byClass[4]
 Specificity_2<-conf_mat$byClass[5]
 Specificity_3<-conf_mat$byClass[6]
 
 PosPredValue_1<-conf_mat$byClass[7]
 PosPredValue_2<-conf_mat$byClass[8]
 PosPredValue_3<-conf_mat$byClass[9]
 
 NegPredValue_1<-conf_mat$byClass[10]
 NegPredValue_2<-conf_mat$byClass[11]
 NegPredValue_3<-conf_mat$byClass[12]
 
 Precision_1<-conf_mat$byClass[13]
 Precision_2<-conf_mat$byClass[14]
 Precision_3<-conf_mat$byClass[15]
 
 Recall_1<-conf_mat$byClass[16]
 Recall_2<-conf_mat$byClass[17]
 Recall_3<-conf_mat$byClass[18]
 
 F1_1<-conf_mat$byClass[19]
 F1_2<-conf_mat$byClass[20]
 F1_3<-conf_mat$byClass[21]
 
 Prevalence_1<-conf_mat$byClass[22]
 Prevalence_2<-conf_mat$byClass[23]
 Prevalence_3<-conf_mat$byClass[24]
 
 DetectionRate_1<-conf_mat$byClass[25]
 DetectionRate_2<-conf_mat$byClass[26]
 DetectionRate_3<-conf_mat$byClass[27]
 
 DetectionPrevalence_1<-conf_mat$byClass[28]
 DetectionPrevalence_2<-conf_mat$byClass[29]
 DetectionPrevalence_3<-conf_mat$byClass[30]
 
 BalancedAccuracy_1<-conf_mat$byClass[31]
 BalancedAccuracy_2<-conf_mat$byClass[32]
 BalancedAccuracy_3<-conf_mat$byClass[33]
 
 df_cm<-data.frame(iteration, Model, Accuracy, Sensitivity_1,Specificity_1, PosPredValue_1, NegPredValue_1, Precision_1,Recall_1, F1_1,Prevalence_1,  DetectionRate_1, DetectionPrevalence_1, BalancedAccuracy_1, Sensitivity_2,Specificity_2, PosPredValue_2, NegPredValue_2, Precision_2,Recall_2, F1_2,Prevalence_2,  DetectionRate_2, DetectionPrevalence_2, BalancedAccuracy_2, Sensitivity_3,Specificity_3, PosPredValue_3, NegPredValue_3, Precision_3,Recall_3, F1_3,Prevalence_3,  DetectionRate_3, DetectionPrevalence_3, BalancedAccuracy_3
 )
 
 
 
 trial<-subset(joined, !is.na(test_state_full_act_inactive) & ! is.na(state))
 
 vus_data<-VUS(as.factor(trial$test_state_full_act_inactive), as.factor(trial$state))
 
 
 df_cm$VUS<-vus_data[[1]]
 df_cm$weighed_F1<-((vus_data[[2]][1]*df_cm$F1_1) + (vus_data[[2]][2]*df_cm$F1_2) +(vus_data[[2]][3]*df_cm$F1_3)) / sum(vus_data[[2]])
 
 df_cm$n_1<-vus_data[[2]][1]
 df_cm$n_2<-vus_data[[2]][2]
 df_cm$n_3<-vus_data[[2]][3]
 
 df_cm$averaged_F1<-mean(df_cm$F1_1, df_cm$F1_2, df_cm$F1_3)
 
 
 
 #GET PROBABILITY MATRIX of classifications
 
 pm<- cbind(joined$prob1, joined$probp2, joined$prob3)
 
 colnames(pm)<-c("1", "2","3")
 
 
 A<-multiclass.roc( as.factor(joined$test_state_full_act_inactive), pm)
 
 df_cm$multiclass_AUC<-A$auc [1]
 
 
 ###get pairwise AUC 
 
 pairwise_AUC_12<-multiclass.roc( as.factor(joined$test_state_full_act_inactive), pm,  levels = c("1", "2"))
 pairwise_AUC_13<-multiclass.roc( as.factor(joined$test_state_full_act_inactive), pm,  levels = c("1", "3"))
 pairwise_AUC_23<-multiclass.roc( as.factor(joined$test_state_full_act_inactive), pm,  levels = c("2", "3"))
 
 
 df_cm$pairwise_AUC_12<-pairwise_AUC_12$auc [1]
 df_cm$pairwise_AUC_13<-pairwise_AUC_13$auc [1]
 df_cm$pairwise_AUC_23<-pairwise_AUC_23$auc [1]
 
 
 
 joined$rest01_test<-ifelse(joined$test_state_full_act_inactive == 1, 1, 0)
 joined$foraging01_test<-ifelse(joined$test_state_full_act_inactive == 2, 1, 0)
 joined$travel01_test<-ifelse(joined$test_state_full_act_inactive == 3, 1, 0)
 
 
 joined$rest01_predicted<-ifelse(joined$state == 1, 1, 0)
 joined$foraging01_predicted<-ifelse(joined$state == 2, 1, 0)
 joined$travel01_predicted<-ifelse(joined$state == 3, 1, 0)
 
 
 
 df_cm$auc_1_all<-auc(joined$rest01_test, joined$rest01_predicted)[1]
 df_cm$auc_2_all<-auc(joined$foraging01_test, joined$foraging01_predicted)[1]
 df_cm$auc_3_all<-auc(joined$travel01_test, joined$travel01_predicted)[1]
 
 
 
 
 
 
 saveRDS(df_cm, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/confusio_matrix_df_full_gps/", iteration,"_", Model,"_", "df_cm.rds"))



  
  }
  
}         
  


####Now read them all in and parte as one dataframe. 

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/confusio_matrix_df_full_gps")

Merged_cm <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/confusio_matrix_df_full_gps/"), readRDS))
Merged_cm$Model<-as.numeric(Merged_cm$Model)


Merged_cm2<-dplyr::select(Merged_cm, -iteration)

data_long <- melt(Merged_cm2, id.vars = c("Model"))

table<-data_long%>% group_by(Model, variable)%>%
  dplyr::summarise(mean= mean(value), sd= sd(value))%>%
  ungroup()%>% 
  mutate_if(is.numeric, round, digits=2) %>%
  mutate_if(is.numeric, format, nsmall = 2) 
  
table$mean_sd<-paste0( table$mean, "±", table$sd)
table_final<-dcast(table, Model ~ variable, value.var ="mean_sd")

write.csv(table_final,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/summary_0_0.09_raw.csv", row.names = FALSE)




mean(subset(Merged_cm, Model == "0.09")$Accuracy, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Accuracy, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Accuracy, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Accuracy, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$Sensitivity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Sensitivity_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$Sensitivity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Sensitivity_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.09")$Sensitivity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Sensitivity_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_3, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$Specificity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Specificity_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$Specificity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Specificity_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.09")$Specificity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Specificity_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_3, na.rm = T)



mean(subset(Merged_cm, Model == "0.09")$Precision_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Precision_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$Precision_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Precision_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.09")$Precision_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$Precision_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_3, na.rm = T)


mean(subset(Merged_cm, Model == "0.09")$multiclass_AUC, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$multiclass_AUC, na.rm = T)
mean(subset(Merged_cm, Model == "0")$multiclass_AUC, na.rm = T)
sd(subset(Merged_cm, Model == "0")$multiclass_AUC, na.rm = T)




mean(subset(Merged_cm, Model == "0.09")$averaged_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$averaged_F1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$averaged_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$averaged_F1, na.rm = T)



mean(subset(Merged_cm, Model == "0.09")$weighed_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$weighed_F1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$weighed_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$weighed_F1, na.rm = T)


mean(subset(Merged_cm, Model == "0.09")$pairwise_AUC_12, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$pairwise_AUC_12, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_12, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_12, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$pairwise_AUC_13, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$pairwise_AUC_13, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_13, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_13, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$pairwise_AUC_23, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$pairwise_AUC_23, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_23, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_23, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$auc_1_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$auc_1_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_1_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_1_all, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$auc_2_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$auc_2_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_2_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_2_all, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$auc_3_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$auc_3_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_3_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_3_all, na.rm = T)

mean(subset(Merged_cm, Model == "0.09")$auc_3_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.09")$auc_3_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_3_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_3_all, na.rm = T)


####create a gamm plot! 





## GAM calculations
library(mgcv)

theme_set(theme_light())

my_theme <-
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1.15)),
        axis.text = element_text(size = rel(1.1)))


summary_merged<-Merged_cm %>%
  dplyr::group_by(Model)%>%
  dplyr::summarize(meanAccuracy = mean (Accuracy),
            meanSensitivity_1 = mean (Sensitivity_1), 
            meanSpecificity_1 = mean (Specificity_1), 
            meanSensitivity_2 = mean (Sensitivity_2), 
            meanSpecificity_2 = mean (Specificity_2), 
            meanSensitivity_3 = mean (Sensitivity_3),
            meanSpecificity_3 = mean (Specificity_3), 
            sdAccuracy = sd (Accuracy),
            sdSensitivity_1 = sd (Sensitivity_1), 
            sdSpecificity_1 = sd (Specificity_1), 
            sdSensitivity_2 = sd (Sensitivity_2), 
            sdSpecificity_2 = sd (Specificity_2), 
            sdSensitivity_3 = sd (Sensitivity_3),
            sdSpecificity_3 = sd (Specificity_3), 
            
            meanmulticlass_AUC = mean (multiclass_AUC),
            meanPrecision_1 = mean (Precision_1), 
            meanauc_1_all = mean (auc_1_all), 
            meanPrecision_2 = mean (Precision_2), 
            meanauc_2_all = mean (auc_2_all), 
            meanPrecision_3 = mean (Precision_3),
            meanauc_3_all = mean (auc_3_all), 
            sdmulticlass_AUC = sd (multiclass_AUC),
            sdPrecision_1 = sd (Precision_1), 
            sdauc_1_all = sd (auc_1_all), 
            sdPrecision_2 = sd (Precision_2), 
            sdauc_2_all = sd (auc_2_all), 
            sdPrecision_3 = sd (Precision_3),
            sdauc_3_all = sd (auc_3_all) 

            
            )%>%
  ungroup()

###boxplot and violin plot??? 


ggplot(Merged_cm, aes(as.factor(Model), Accuracy)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
             fun.args = list(
               mult = 1), colour="red", alpha=0.7) +
  my_theme+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Accuracy")+
  theme(axis.title.x = element_blank()) 
  





ggplot(Merged_cm, aes(as.factor(Model), multiclass_AUC)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Multiclass AUC")+
  theme(axis.title.x = element_blank()) 









####group by sensitivity and specificity so that I can make two plots instead of 6

ss<-dplyr::dplyr::select(Merged_cm,Model, Sensitivity_1, Sensitivity_2, Sensitivity_3)

Sensitivity_plot <- melt(ss, id.vars = c("Model"))
Sensitivity_plot<-dplyr::select(Sensitivity_plot, Model, Behaviour=variable, Sensitivity =value)
Sensitivity_plot
levels(Sensitivity_plot$Behaviour)[levels(Sensitivity_plot$Behaviour)=="Sensitivity_1"] <- "resting"
levels(Sensitivity_plot$Behaviour)[levels(Sensitivity_plot$Behaviour)=="Sensitivity_2"] <- "foraging"
levels(Sensitivity_plot$Behaviour)[levels(Sensitivity_plot$Behaviour)=="Sensitivity_3"] <- "traveling"




ggplot(Sensitivity_plot, aes(x =as.factor(Model), y =Sensitivity, group = Behaviour, colour = Behaviour)) +
  geom_point(alpha=0.2, position=position_jitter(height=0, width=0.1)) +
  scale_color_manual(values = c("resting" = "goldenrod", "foraging" = "darkred", "traveling" = "cyan"))+
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Sensitivity")+
  my_theme+
  theme(axis.title.x = element_blank()) 
  



ss<-dplyr::dplyr::(Merged_cm,Model, Specificity_1, Specificity_2, Specificity_3)

Specificity_plot <- melt(ss, id.vars = c("Model"))
Specificity_plot<-select(Specificity_plot, Model, Behaviour=variable, Specificity =value)
Specificity_plot
levels(Specificity_plot$Behaviour)[levels(Specificity_plot$Behaviour)=="Specificity_1"] <- "resting"
levels(Specificity_plot$Behaviour)[levels(Specificity_plot$Behaviour)=="Specificity_2"] <- "foraging"
levels(Specificity_plot$Behaviour)[levels(Specificity_plot$Behaviour)=="Specificity_3"] <- "traveling"




ggplot(Specificity_plot, aes(x =as.factor(Model), y =Specificity, group = Behaviour, colour = Behaviour)) +
  geom_point(alpha=0.2, position=position_jitter(height=0, width=0.1)) +
  scale_color_manual(values = c("resting" = "goldenrod", "foraging" = "darkred", "traveling" = "cyan"))+
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Specificity")+
  my_theme+
  theme(axis.title.x = element_blank()) 




####group by Precision and specificity so that I can make two plots instead of 6

ss<-dplyr::select(Merged_cm,Model, Precision_1, Precision_2, Precision_3)

Precision_plot <- melt(ss, id.vars = c("Model"))
Precision_plot<-dplyr::select(Precision_plot, Model, Behaviour=variable, Precision =value)
Precision_plot
levels(Precision_plot$Behaviour)[levels(Precision_plot$Behaviour)=="Precision_1"] <- "resting"
levels(Precision_plot$Behaviour)[levels(Precision_plot$Behaviour)=="Precision_2"] <- "foraging"
levels(Precision_plot$Behaviour)[levels(Precision_plot$Behaviour)=="Precision_3"] <- "traveling"




ggplot(Precision_plot, aes(x =as.factor(Model), y =Precision, group = Behaviour, colour = Behaviour)) +
  geom_point(alpha=0.2, position=position_jitter(height=0, width=0.1)) +
  scale_color_manual(values = c("resting" = "goldenrod", "foraging" = "darkred", "traveling" = "cyan"))+
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name= "Precision")+
  my_theme+
  theme(axis.title.x = element_blank()) 













  

ggplot(Merged_cm, aes(as.factor(Model), Sensitivity_2)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  theme(axis.title.x = element_blank()) 



ggplot(Merged_cm, aes(as.factor(Model), Sensitivity_3)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  theme(axis.title.x = element_blank()) 


ggplot(Merged_cm, aes(as.factor(Model), Specificity_1)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  theme(axis.title.x = element_blank()) 




ggplot(Merged_cm, aes(as.factor(Model), Specificity_2)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  theme(axis.title.x = element_blank()) 




ggplot(Merged_cm, aes(as.factor(Model), Specificity_3)) +
  geom_point(alpha=0.3, position=position_jitter(height=0, width=0.1)) +
  stat_summary(fun.data = "mean_sdl",
               fun.args = list(
                 mult = 1), colour="red", alpha=0.7) +
  my_theme+
  theme(axis.title.x = element_blank()) 


#####Now make tile plots with mean confusion matrix predictions for the number of points with 0 and with all known states








for (iteration in all_iterations){
  #iteration<-all_iterations[10]
  #file_name<-Original_files[1]
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds")
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    # file_name<-Original_files[2]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1,-5)
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
    
    allks<-dplyr::select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    table(joined$state)
    
    joined$point<-seq_along(1:nrow(joined))
    
    conf_mat <- confusionMatrix(as.factor(joined$state), as.factor(joined$test_state_full_act_inactive))
    plt <- as.data.frame(conf_mat$table)
    
    plt$iteration<- iteration
    plt$propKS<-it
    
    ###save out the conf_matrix table
    
    saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_raw/", iteration,"_", Model,"_", "df_cm.rds"))
    
    ####now create dataset of probabilities 
    
    joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
    
    joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
    
    head(joined)
    
    
    datalist = list()
    
    for (i in 1:length(seq(0.33:1, by=0.01))){
      #i<-14
      aa<-seq(0.33:1, by=0.01)[i]
      j<- subset(joined, highest_prob >= aa)
      df<-as.data.frame(table(j$state))
      names(df)<-c('state', 'nb_points')
      df$max_prob <-aa
      
      datalist[[i]] <- df # add it to your list
    }
    
    big_data <- do.call(rbind, datalist)
    
    big_data$iteration<-iteration
    big_data$propks<-Model
    
    ####save this out for each iteration and each proportion.
    saveRDS(big_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/HMM_PROBABILITY_GRAPH/", iteration,"_", Model,"_", "Probability.rds"))
    
    setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency")
    
    
    Merged_krippendorff <- 
      do.call(rbind,
              lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency/"), readRDS))
    
    #######now merge this consistency 
    
    consistency<-subset(Merged_krippendorff, prop_ks == Model)
    
    
    joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y"))
    saveRDS(joined2, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/HMM_CONSISTENCY_GRAPH/", iteration,"_", Model,"_", "consistency.rds"))  
    
  }
  
}         









































# 
# 
# 
# 
# 
# 
# #####
# 
# 
# 
# setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/HMM_PROBABILITY_GRAPH/travel_full")
# 
# library(purrr)
# 
# df <- list.files(pattern = ".rds") %>%
#   map_dfr(readRDS)
# 
# 
# head(df)
# 
# 
# 
# 
# ####lets plot this out as a gamm
# 
# 
# 
# ## GAM calculations
# library(mgcv)
# 
# theme_set(theme_light())
# 
# my_theme <-
#   theme_light()+
#   theme(panel.grid.minor = element_blank(),
#         axis.title = element_text(size = rel(1.15)),
#         axis.text = element_text(size = rel(1.1)))
# 
# 
# 
# ####split this by behaviour
# 
# df<-df%>%
#   group_by(state, propks, iteration) %>%
#   dplyr::mutate(max = max(nb_points))%>%
#   ungroup()
# 
# 
# 
# df<-df%>%
#   group_by(state, propks, iteration) %>%
#   dplyr::mutate(prob_points = nb_points/max*100)%>%
#   ungroup()
# 
# 
# 
# rest<-subset(df, state == 1)
# 
# forage<-subset(df, state == 2)
# 
# travel<-subset(df, state == 3)
# 
# 
# 
# 
# 
# ##############################################################################
# gam_prob_rest <- gam(nb_points ~ 
#                        s(max_prob, k = 6), 
#                      method="REML", 
#                      data = rest)
# 
# pp <- plot(gam_prob_rest, shift=mean(rest$nb_points))
# 
# p.prop_rest <- data.frame(x=pp[[1]]$x, 
#                           y=as.numeric(pp[[1]]$fit)+mean(rest$nb_points), 
#                           se=pp[[1]]$se)
# ##############################################################################
# 
# gam_prob_forage <- gam(nb_points ~ 
#                          s(max_prob,  k = 6), 
#                        method="REML", 
#                        data = forage)
# 
# pp <- plot(gam_prob_forage, shift=mean(forage$nb_points))
# 
# p.prop_forage <- data.frame(x=pp[[1]]$x, 
#                             y=as.numeric(pp[[1]]$fit)+mean(forage$nb_points), 
#                             se=pp[[1]]$se)
# 
# ##############################################################################
# 
# gam_prob_travel <- gam(nb_points ~ 
#                          s(max_prob,  k = 6), 
#                        method="REML", 
#                        data = travel)
# 
# pp <- plot(gam_prob_travel, shift=mean(travel$nb_points))
# 
# p.prop_travel <- data.frame(x=pp[[1]]$x, 
#                             y=as.numeric(pp[[1]]$fit)+mean(travel$nb_points), 
#                             se=pp[[1]]$se)
# 
# ##############################################################################
# 
# #####do as percentage of number of total points
# 
# 
# ##############################################################################
# gam_percent_rest <- gam(prob_points ~ 
#                           s(max_prob,  k = 6), 
#                         method="REML", 
#                         data = rest)
# 
# pp <- plot(gam_percent_rest, shift=mean(rest$prob_points))
# 
# p.prop_rest <- data.frame(x=pp[[1]]$x, 
#                           y=as.numeric(pp[[1]]$fit)+mean(rest$prob_points), 
#                           se=pp[[1]]$se)
# ##############################################################################
# 
# gam_percent_forage <- gam(prob_points ~ 
#                             s(max_prob,  k = 6), 
#                           method="REML", 
#                           data = forage)
# 
# pp <- plot(gam_percent_forage, shift=mean(forage$prob_points))
# 
# p.prop_forage <- data.frame(x=pp[[1]]$x, 
#                             y=as.numeric(pp[[1]]$fit)+mean(forage$prob_points), 
#                             se=pp[[1]]$se)
# 
# ##############################################################################
# 
# gam_percent_travel <- gam(prob_points ~ 
#                             s(max_prob,  k = 6), 
#                           method="REML", 
#                           data = travel)
# 
# pp <- plot(gam_percent_travel, shift=mean(travel$prob_points))
# 
# p.prop_travel <- data.frame(x=pp[[1]]$x, 
#                             y=as.numeric(pp[[1]]$fit)+mean(travel$prob_points), 
#                             se=pp[[1]]$se)
# 
# ##############################################################################
# 
# 
# #### PLOTS
# gam_percentage_point_vs_probability_included <- 
#   ggplot()+
#   ## phenology lines
#   ## points
#   geom_point(data=df, aes(x=max_prob, y=prob_points, colour = state), alpha=0.1)+
#   scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
#   
#   
#   ## gam
#   geom_ribbon(data=p.prop_rest, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
#   geom_line(data=p.prop_rest, aes(x=x, y=y), colour="grey20", linetype="solid")+
#   
#   
#   ## gam
#   geom_ribbon(data=p.prop_forage, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
#   geom_line(data=p.prop_forage, aes(x=x, y=y), colour="grey20", linetype="solid")+
#   
#   
#   
#   ## gam
#   geom_ribbon(data=p.prop_travel, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
#   geom_line(data=p.prop_travel, aes(x=x, y=y), colour="grey20", linetype="solid")+
#   
#   
#   
#   scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability ")+
#   scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, 10), name= "Proportion of points")+
#   #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
#   guides(colour = guide_legend(override.aes = list(alpha = 1)))+
#   my_theme+
#   theme(axis.text = element_text(size=12),
#         panel.grid.minor.x = element_blank())
# 
# gam_percentage_point_vs_probability_included
# 
# 
# 
# #####now lets do with with the accuracy! 
# 
# 
# 
# 
# 





for (iteration in all_iterations){
  #iteration<-all_iterations[9]
  #file_name<-Original_files[1]
  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/all_for_cm_NOID_FINAL_full_model.rds")
  
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    # file_name<-Original_files[1]
    
    #file_name <-Original_files[13]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1,-5)
    
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
    
    allks<-dplyr::select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    table(joined$state)
    
    
    ####make loop to extract accuracy, sensitivity and sensibility at each prob drop
    ####now create dataset of probabilities 
    
    joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
    
    joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
    
    head(joined)
    
    
    datalist = list()
    
    for (i in 1:length(seq(0.33:1, by=0.01))){
      
      tryCatch({
        
        #i<-68
        aa<-seq(0.33:1, by=0.01)[i]
        j<- subset(joined, highest_prob >= aa)
        
        conf_mat <- confusionMatrix(as.factor(j$test_state_full_act_inactive), as.factor(j$state))
        
        Accuracy<- conf_mat$overall[1]
        Sensitivity_1<-conf_mat$byClass[1]
        Sensitivity_2<-conf_mat$byClass[2]
        Sensitivity_3<-conf_mat$byClass[3]
        
        Specificity_1<-conf_mat$byClass[4]
        Specificity_2<-conf_mat$byClass[5]
        Specificity_3<-conf_mat$byClass[6]
        
        
        
        df<-data.frame(iteration, Model, Accuracy, Sensitivity_1, Specificity_1, Sensitivity_2,Specificity_2, Sensitivity_3,Specificity_3)
        
        
        df$max_prob <-aa
        
        datalist[[i]] <- df # add it to your list
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    
    big_data <- do.call(rbind, datalist)
    
    ###save out the conf_matrix table
    
    saveRDS(big_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/HMM_PROBABILITY_CM/", iteration,"_", Model,"_", "df_cm_probability.rds"))
    
  }
}

####now lets merge 


####now group these back together and plot?? 
setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/HMM_PROBABILITY_CM/")


Probability_HMM <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/HMM_PROBABILITY_CM/"), readRDS))


Probability_HMM<-subset(Probability_HMM, Model == 0.09)
Probability_HMM<-subset(Probability_HMM, max_prob < 1)

###NOW PLOT THESE OUT...

##############################################################################

#####do as percentage of number of total points
library(mgcv)

##############################################################################
gam_Acc <- gam(Accuracy ~  s(max_prob,  k = 6), 
               method="REML", 
               data = Probability_HMM)

pp <- plot(gam_Acc, shift=mean(Probability_HMM$Accuracy))

p.gam_Acc <- data.frame(x=pp[[1]]$x, 
                        y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM$Accuracy), 
                        se=pp[[1]]$se)
##############################################################################

gam_Sensitivity_1 <- gam(Sensitivity_1 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = subset(Probability_HMM, !is.na(Sensitivity_1)))

pp <- plot(gam_Sensitivity_1, shift=mean(subset(Probability_HMM, !is.na(Sensitivity_1))$Sensitivity_1))

p.Sensitivity_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(subset(Probability_HMM, !is.na(Sensitivity_1))$Sensitivity_1), 
                              se=pp[[1]]$se)

##############################################################################


gam_Sensitivity_2 <- gam(Sensitivity_2 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = subset(Probability_HMM, !is.na(Sensitivity_2)))

pp <- plot(gam_Sensitivity_2, shift=mean(subset(Probability_HMM, !is.na(Sensitivity_2))$Sensitivity_2))

p.Sensitivity_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(subset(Probability_HMM, !is.na(Sensitivity_2))$Sensitivity_2), 
                              se=pp[[1]]$se)

##############################################################################


gam_Sensitivity_3 <- gam(Sensitivity_3 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = subset(Probability_HMM, !is.na(Sensitivity_3)))

pp <- plot(gam_Sensitivity_3, shift=mean(subset(Probability_HMM, !is.na(Sensitivity_3))$Sensitivity_3))

p.Sensitivity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(subset(Probability_HMM, !is.na(Sensitivity_3))$Sensitivity_3), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_1 <- gam(Specificity_1 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = subset(Probability_HMM, !is.na(Specificity_1)))

pp <- plot(gam_Specificity_1, shift=mean(subset(Probability_HMM, !is.na(Specificity_1))$Specificity_1))

p.Specificity_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(subset(Probability_HMM, !is.na(Specificity_1))$Specificity_1), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_2 <- gam(Specificity_2 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM)

pp <- plot(gam_Specificity_2, shift=mean(Probability_HMM$Specificity_2))

p.Specificity_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM$Specificity_2), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_3 <- gam(Specificity_3 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM)

pp <- plot(gam_Specificity_3, shift=mean(Probability_HMM$Specificity_3))

p.Specificity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM$Specificity_3), 
                              se=pp[[1]]$se)

##############################################################################
Probability_HMM$Model<-as.numeric(Probability_HMM$Model)

#### PLOTS
gam_probability_Specificity <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Specificity_1),colour = "goldenrod", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Specificity_2), colour ="darkred", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Specificity_3), colour ="cyan", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  geom_ribbon(data=p.Specificity_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Specificity_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Specificity_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Specificity_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Specificity_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Specificity_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability included")+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.1), name= "Specificity")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())+
  guides(colour=guide_legend(title="Prop KS"))

gam_probability_Specificity


gam_probability_Sensitivity <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Sensitivity_1), alpha = 0.1, colour = "goldenrod")+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Sensitivity_2) , alpha = 0.1, colour ="darkred")+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Sensitivity_3), alpha = 0.1, colour ="cyan")+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  geom_ribbon(data=p.Sensitivity_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Sensitivity_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Sensitivity_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Sensitivity_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Sensitivity_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Sensitivity_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability included")+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.1), name= "Sensitivity")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())+
  guides(colour=guide_legend(title="Prop KS"))

gam_probability_Sensitivity


############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


#confusion matrix plots


#bring in all data from confusion matrices of 0 and 0.09


setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_raw")


df <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS)

p0<-subset(df, propKS == 0)

p0<- p0%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0$Percentage<-round(p0$Freq/p0$sum*100)


p0_sum<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0_sum$Mean<-paste0(p0_sum$mean_Freq, " \u00b1 ", p0_sum$sd_Freq)

p0_perc<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0_perc$Mean<-paste0(p0_perc$mean_Prencetage, " \u00b1 ", p0_perc$sd_Percentage)

####now lets create a geom:tile
ggplot(p0_sum, aes(Reference, Prediction, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194", limits = c(0,3500)) +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0_perc, aes(Reference,Prediction , fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

####################


p0.09<-subset(df, propKS == 0.09)

p0.09<- p0.09%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.09$Percentage<-round(p0.09$Freq/p0.09$sum*100)


p0.09_sum<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.09_sum$Mean<-paste0(p0.09_sum$mean_Freq, " \u00b1 ", p0.09_sum$sd_Freq)

p0.09_perc<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.09_perc$Mean<-paste0(p0.09_perc$mean_Prencetage, " \u00b1 ", p0.09_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0.09_sum, aes(Reference, Prediction, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194", limits = c(0,3500)) +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0.09_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")




#####Now do the same but with consistency and with probability over 100 


for (iteration in all_iterations){
  #iteration<-all_iterations[1]
  #file_name<-Original_files[1]
  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/allks_for_cm_3_All_gps_travel.rds")
  

  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    # file_name<-Original_files[1]
    
    #file_name <-Original_files[13]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[8]
    it<-str_sub(it,7,-5)
    
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
    
    allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    table(joined$state)
    
    joined$point<-seq_along(1:nrow(joined))
    
    joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
    
    joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
    
    joined<-subset(joined, highest_prob > 0.9)
    
    conf_mat <- confusionMatrix(as.factor(joined$test_state_full_act_inactive), as.factor(joined$state))
    plt <- as.data.frame(conf_mat$table)
    
    plt$iteration<- iteration
    plt$propKS<-it
    
    ###save out the conf_matrix table
    
    saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_prop_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
    
    
  }
}



for (iteration in all_iterations){
  #iteration<-all_iterations[1]
  #file_name<-Original_files[1]
  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/allks_for_cm_3_All_gps_travel.rds")
  
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    
    tryCatch({
      # file_name<-Original_files[1]
      
      #file_name <-Original_files[13]
      it<-str_split(file_name, "_")
      it<-it[[1]]
      it<-it[8]
      it<-str_sub(it,7,-5)
      
      Model<-it
      #Read in the HMM output
      output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
      
      allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
      
      
      ####Merge with the known states 
      joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
      table(joined$state)
      
      joined$point<-seq_along(1:nrow(joined))
      
      joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
      
      joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
      
      setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency")
      
      Merged_krippendorff <- 
        do.call(rbind,
                lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency/"), readRDS))
      
      #######now merge this consistency 
      
      consistency<-subset(Merged_krippendorff, prop_ks == Model)
      
      
      joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y"))
      
      
      
      joined2<-subset(joined2, p_consistency > 0.8)
      
      
      
      
      conf_mat <- confusionMatrix(as.factor(joined2$test_state_full_act_inactive), as.factor(joined2$state))
      plt <- as.data.frame(conf_mat$table)
      
      plt$iteration<- iteration
      plt$propKS<-it
      
      ###save out the conf_matrix table
      
      saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_consistency_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    
  }
}  






for (iteration in all_iterations){
  #iteration<-all_iterations[1]
  #file_name<-Original_files[1]
  
  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/allks_for_cm_3_All_gps_travel.rds")
  
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/",iteration), pattern = 'Final_birdPrep_states_extracted_AWS_3states')
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    
    tryCatch({
      # file_name<-Original_files[1]
      
      #file_name <-Original_files[13]
      it<-str_split(file_name, "_")
      it<-it[[1]]
      it<-it[8]
      it<-str_sub(it,7,-5)
      
      Model<-it
      #Read in the HMM output
      output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/", iteration, "/", file_name))
      
      allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
      
      
      ####Merge with the known states 
      joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
      table(joined$state)
      
      joined$point<-seq_along(1:nrow(joined))
      
      joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
      
      joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
      
      setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency")
      
      Merged_krippendorff <- 
        do.call(rbind,
                lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/FitTrack_Full/Percent_consistency/"), readRDS))
      
      #######now merge this consistency 
      
      consistency<-subset(Merged_krippendorff, prop_ks == Model)
      
      
      joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y", "point"))
      
      
      
      joined2<-subset(joined2, p_consistency > 0.8 & highest_prob > 0.9)
      
      
      
      
      conf_mat <- confusionMatrix(as.factor(joined2$test_state_full_act_inactive), as.factor(joined2$state))
      plt <- as.data.frame(conf_mat$table)
      
      plt$iteration<- iteration
      plt$propKS<-it
      
      ###save out the conf_matrix table
      
      saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_consistency_and_prop_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    
  }
}  



#####create the confusion matrix plot for prob >  0.9



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_prop_0.9")


df <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS)

p0<-subset(df, propKS == 0)

p0<- p0%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0$Percentage<-round(p0$Freq/p0$sum*100)


p0_sum<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0_sum$Mean<-paste0(p0_sum$mean_Freq, " \u00b1 ", p0_sum$sd_Freq)

p0_perc<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0_perc$Mean<-paste0(p0_perc$mean_Prencetage, " \u00b1 ", p0_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

####################


p0.09<-subset(df, propKS == 0.09)

p0.09<- p0.09%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.09$Percentage<-round(p0.09$Freq/p0.09$sum*100)


p0.09_sum<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.09_sum$Mean<-paste0(p0.09_sum$mean_Freq, " \u00b1 ", p0.09_sum$sd_Freq)

p0.09_perc<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.09_perc$Mean<-paste0(p0.09_perc$mean_Prencetage, " \u00b1 ", p0.09_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0.09_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0.09_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")






setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_consistency_0.9")


df <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS)

p0<-subset(df, propKS == 0)

p0<- p0%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0$Percentage<-round(p0$Freq/p0$sum*100)


p0_sum<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0_sum$Mean<-paste0(p0_sum$mean_Freq, " \u00b1 ", p0_sum$sd_Freq)

p0_perc<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0_perc$Mean<-paste0(p0_perc$mean_Prencetage, " \u00b1 ", p0_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

####################


p0.09<-subset(df, propKS == 0.09)

p0.09<- p0.09%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.09$Percentage<-round(p0.09$Freq/p0.09$sum*100)


p0.09_sum<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.09_sum$Mean<-paste0(p0.09_sum$mean_Freq, " \u00b1 ", p0.09_sum$sd_Freq)

p0.09_perc<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.09_perc$Mean<-paste0(p0.09_perc$mean_Prencetage, " \u00b1 ", p0.09_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0.09_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0.09_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")







setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_full_consistency_and_prop_0.9")


df <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS)

p0<-subset(df, propKS == 0)

p0<- p0%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0$Percentage<-round(p0$Freq/p0$sum*100)


p0_sum<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0_sum$Mean<-paste0(p0_sum$mean_Freq, " \u00b1 ", p0_sum$sd_Freq)

p0_perc<- p0 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0_perc$Mean<-paste0(p0_perc$mean_Prencetage, " \u00b1 ", p0_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

####################


p0.09<-subset(df, propKS == 0.09)

p0.09<- p0.09%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.09$Percentage<-round(p0.09$Freq/p0.09$sum*100)


p0.09_sum<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.09_sum$Mean<-paste0(p0.09_sum$mean_Freq, " \u00b1 ", p0.09_sum$sd_Freq)

p0.09_perc<- p0.09 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.09_perc$Mean<-paste0(p0.09_perc$mean_Prencetage, " \u00b1 ", p0.09_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0.09_sum, aes(Prediction, Reference, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0.09_perc, aes(Prediction, Reference, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
















##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################





