

library(dplyr)       
library(tidyr)         
library(lubridate)      
library(ggplot2)   
library(stringr)
library(caret)
library(VUROCS)
library(pROC)
library(mgcv)
library(qwraps2)
library(caret)
library(data.table)
library(purrr)

op <- options(digits.secs=6)
options(expressions = 20000)

memory.limit(size=99999999)




setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")




allks$test_state_full<-as.numeric(ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks$iteration10_90),allks$true_state, NA))

allks$test_state_full_act_inactive<-as.numeric(ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks$iteration10_90),allks$true_state_act_inactive, NA))

table(allks$true_state_act_inactive)





####READ in the random iterations so that I have the known states associated with each. 


###check how often predicted behaviour == behaviour. 



###MAKE FUNCTION THAT OPENS EACH FILE, EXTRACTS THE CONFUSION MATRIX


all_iterations<-c("iteration1", "iteration2", "iteration3", "iteration4", "iteration5", "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")


all_iterations<-c( "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")

for (iteration in all_iterations){
  #iteration<-all_iterations[5]
  

  #names(allks) <- gsub(x = names(allks), pattern = "itteration", replacement = "iteration")  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  

  
  Original_files <- list.files(path = paste0("./HMMS/fitTrack_0.75/",iteration), pattern = c('Final_birdPrep_states_extracted_AWS_3states', '.rds'))
  #file_name<-Original_files[10]

  for (file_name in Original_files){
    
  #file_name <-Original_files[15]
  it<-str_split(file_name, "_")
  it<-it[[1]]
  it<-it[7]
  it<-str_sub(it,1,-5)

  Model<-it
  #Read in the HMM output
  output<- readRDS(paste0("./HMMS/fitTrack_0.75/", iteration, "/", file_name))
  ####Merge with the known states 
  
  output$max_prob<-ifelse(output$prob1 >= output$probp2 & output$prob1 >=  output$prob3, output$prob1,ifelse(output$prob2 >= output$probp1 & output$prob2 >=  output$prob3, output$prob2, output$prob3))
  
  
 joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
 table(joined$state, joined$test_state_full_act_inactive)

 joined<-subset(joined, max_prob >= 0.9)
 
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


trial<-subset(joined, !is.na(test_state_full_act_inactive) & ! is.na(state))

vus_data<-VUS(as.factor(trial$test_state_full_act_inactive), as.factor(trial$state))


df_cm<-data.frame(iteration, Model, Accuracy, Sensitivity_1,Specificity_1, PosPredValue_1, NegPredValue_1, Precision_1,Recall_1, F1_1,Prevalence_1,  DetectionRate_1, DetectionPrevalence_1, BalancedAccuracy_1, Sensitivity_2,Specificity_2, PosPredValue_2, NegPredValue_2, Precision_2,Recall_2, F1_2,Prevalence_2,  DetectionRate_2, DetectionPrevalence_2, BalancedAccuracy_2, Sensitivity_3,Specificity_3, PosPredValue_3, NegPredValue_3, Precision_3,Recall_3, F1_3,Prevalence_3,  DetectionRate_3, DetectionPrevalence_3, BalancedAccuracy_3)



df_cm$VUS<-vus_data[[1]]
df_cm$weighed_F1<-((vus_data[[2]][1]*df_cm$F1_1) + (vus_data[[2]][2]*df_cm$F1_2) +(vus_data[[2]][3]*df_cm$F1_3)) / sum(vus_data[[2]])

df_cm$n_1<-vus_data[[2]][1]
df_cm$n_2<-vus_data[[2]][2]
df_cm$n_3<-vus_data[[2]][3]

df_cm$F1_2<-ifelse(!is.na(df_cm$F1_2), df_cm$F1_2, 0)
df_cm$F1_3<-ifelse(!is.na(df_cm$F1_3), df_cm$F1_3, 0)

df_cm$averaged_F1<-mean(df_cm$F1_1, df_cm$F1_2, df_cm$F1_3)

###

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




saveRDS(df_cm, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_CM/", iteration,"_", Model,"_", "df_cm.rds"))


# 
# # 
#   # Model<-it
#   # # 
#  tab<-table(joined$state,joined$test_state_full_act_inactive)
# tp_1<-tab[1] 
# tp_2<-tab[5] 
# tp_3<-tab[9] 
#   # # 
# fn_1<-tab[2]+tab[3]
# fn_2<-tab[4] +tab[6]
# fn_3<-tab[7] + tab[8]
#   # 
#   # # 
# fp_1<-tab[4] + tab[7]
# fp_2<-tab[2] + tab[8]
# fp_3<-tab[3]+ tab[6]
#   # # 
# tn_1<-tab[5]+ tab[6]+ tab[8]+ tab[9]
# tn_2<-tab[1] + tab[3] + tab[7]+ tab[9] 
# tn_3<-tab[1]+tab[2]+ tab[4]+ tab[5]
#   # 
# TP<- tp_1+tp_2+tp_3
# FN<- fn_1+fn_2+fn_3
# TN<- tn_1+tn_2+tn_3
# FP<- fp_1+fp_2+fp_3
#   # 
  # Accuracy<-(TP+TN)/ (TP+FP+FN+TN)
  # Specificity_1<-tn_1/ (tn_1+fp_1) 
  # Sensitivity_1<- tp_1/(tp_1+fn_1)
  # prevalence_1<-tp_1/ (tp_1+fp_1+fn_1+tn_1)
  # balanced_1<-(Sensitivity_1+Specificity_1)/2
  # Precision_1<- tp_1/(tp_1+fp_1)
  # False_positive_rate_1<-fp_1/(tn_1+fp_1)
  # 
  # 
  # Specificity_2<-tn_2/ (tn_2+fp_2) 
  # Sensitivity_2<- tp_2/(tp_2+fn_2)
  # prevalence_2<-tp_2/ (tp_2+fp_2+fn_2+tn_2)
  # balanced_2<-(Sensitivity_2+Specificity_2)/2
  # Precision_2<- tp_2/(tp_2+fp_2)
  # False_positive_rate_2<-fp_2/(tn_2+fp_2)
  # 
  # 
  # Specificity_3<-tn_3/ (tn_3+fp_3) 
  # Sensitivity_3<- tp_3/(tp_3+fn_3)
  # prevalence_3<-tp_3/ (tp_3+fp_3+fn_3+tn_3)
  # balanced_3<-(Sensitivity_3+Specificity_3)/2
  # Precision_3<- tp_3/(tp_3+fp_3)
  # False_positive_rate_3<-fp_3/(tn_3+fp_3)
  # 
  # df_cm<-data.frame(Model, Accuracy,Specificity_1, Sensitivity_1, prevalence_1, balanced_1, Precision_1,False_positive_rate_1, Specificity_2, Sensitivity_2, prevalence_2, balanced_2, Precision_2, False_positive_rate_2, Specificity_3, Sensitivity_3, prevalence_3, balanced_3,Precision_3, False_positive_rate_3, iteration)
  
#  saveRDS(df_cm, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/confusion_matrix_df_4states/", iteration,"_", Model,"_", "df_cm.rds"))
  
  }
  
} 
  


####Now read them all in and paste as one dataframe. 

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_CM")

Merged_cm <- 
  do.call(rbind,
          lapply(list.files(), readRDS))
Merged_cm$Model<-as.numeric(Merged_cm$Model)
Merged_cm2<-select(Merged_cm, -iteration)

Merged_cm2<-subset(Merged_cm2, Model == 0 | Model == 0.75)

data_long <- melt(Merged_cm2, id.vars = c("Model"))

table<-data_long%>% group_by(Model, variable)%>%
  dplyr::summarise(mean= mean(value), sd= sd(value))%>%
  ungroup()%>% 
  mutate_if(is.numeric, round, digits=2) %>%
  mutate_if(is.numeric, format, nsmall = 2) 

table$mean_sd<-paste0( table$mean, "±", table$sd)
table_final<-dcast(table, Model ~ variable, value.var ="mean_sd")

write.csv(table_final,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/summary_0_0.75_raw_0.90.csv", row.names = FALSE)





mean(subset(Merged_cm, Model == "0.75")$Accuracy, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Accuracy, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Accuracy, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Accuracy, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$Sensitivity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Sensitivity_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$Sensitivity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Sensitivity_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$Sensitivity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Sensitivity_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Sensitivity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Sensitivity_3, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$Specificity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Specificity_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$Specificity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Specificity_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$Specificity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Specificity_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Specificity_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Specificity_3, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$Precision_1, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Precision_1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_1, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$Precision_2, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Precision_2, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_2, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_2, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$Precision_3, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$Precision_3, na.rm = T)
mean(subset(Merged_cm, Model == "0")$Precision_3, na.rm = T)
sd(subset(Merged_cm, Model == "0")$Precision_3, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$multiclass_AUC, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$multiclass_AUC, na.rm = T)
mean(subset(Merged_cm, Model == "0")$multiclass_AUC, na.rm = T)
sd(subset(Merged_cm, Model == "0")$multiclass_AUC, na.rm = T)




mean(subset(Merged_cm, Model == "0.75")$averaged_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$averaged_F1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$averaged_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$averaged_F1, na.rm = T)



mean(subset(Merged_cm, Model == "0.75")$weighed_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$weighed_F1, na.rm = T)
mean(subset(Merged_cm, Model == "0")$weighed_F1, na.rm = T)
sd(subset(Merged_cm, Model == "0")$weighed_F1, na.rm = T)


mean(subset(Merged_cm, Model == "0.75")$pairwise_AUC_12, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$pairwise_AUC_12, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_12, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_12, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$pairwise_AUC_13, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$pairwise_AUC_13, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_13, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_13, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$pairwise_AUC_23, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$pairwise_AUC_23, na.rm = T)
mean(subset(Merged_cm, Model == "0")$pairwise_AUC_23, na.rm = T)
sd(subset(Merged_cm, Model == "0")$pairwise_AUC_23, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$auc_1_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$auc_1_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_1_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_1_all, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$auc_2_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$auc_2_all, na.rm = T)
mean(subset(Merged_cm, Model == "0")$auc_2_all, na.rm = T)
sd(subset(Merged_cm, Model == "0")$auc_2_all, na.rm = T)

mean(subset(Merged_cm, Model == "0.75")$auc_3_all, na.rm = T)
sd(subset(Merged_cm, Model == "0.75")$auc_3_all, na.rm = T)
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


###Accuracy
gam_Accuracy <- gam(Accuracy ~ 
                    s(Model, k=12), 
                  method="REML", 
                  data = Merged_cm)

pp <- plot(gam_Accuracy, shift=mean(Merged_cm$Accuracy))

p.Accuracy <- data.frame(x=pp[[1]]$x, 
                    y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Accuracy), 
                    se=pp[[1]]$se)

#### PLOTS
gam_Accuracy_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Accuracy), alpha=0.3, colour = "darkblue")+
  ## gam
  geom_ribbon(data=p.Accuracy, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "darkblue", size=0.2)+
  geom_line(data=p.Accuracy, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), name= "Accuracy")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())


gam_Accuracy_plot







###auc
gam_AUC <- gam(multiclass_AUC ~ 
                      s(Model, k=12), 
                    method="REML", 
                    data = Merged_cm)

pp <- plot(gam_AUC, shift=mean(Merged_cm$multiclass_AUC))

p.AUC <- data.frame(x=pp[[1]]$x, 
                         y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$multiclass_AUC), 
                         se=pp[[1]]$se)

#### PLOTS
gam_AUC_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=multiclass_AUC), alpha=0.3, colour = "darkblue")+
  ## gam
  geom_ribbon(data=p.AUC, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "darkblue", size=0.2)+
  geom_line(data=p.AUC, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), name= "AUC")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())


gam_AUC_plot



###weighed_F1
gam_weighed_F1 <- gam(weighed_F1 ~ 
                 s(Model, k=12), 
               method="REML", 
               data = Merged_cm)

pp <- plot(gam_weighed_F1, shift=mean(Merged_cm$weighed_F1))

p.weighed_F1 <- data.frame(x=pp[[1]]$x, 
                    y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$weighed_F1), 
                    se=pp[[1]]$se)

#### PLOTS
gam_weighed_F1_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=weighed_F1), alpha=0.3, colour = "darkblue")+
  ## gam
  geom_ribbon(data=p.weighed_F1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "darkblue", size=0.2)+
  geom_line(data=p.weighed_F1, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), name= "weighed F1")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())


gam_weighed_F1_plot




###VUS
gam_VUS <- gam(VUS ~ 
                        s(Model, k=12), 
                      method="REML", 
                      data = Merged_cm)

pp <- plot(gam_VUS, shift=mean(Merged_cm$VUS))

p.VUS <- data.frame(x=pp[[1]]$x, 
                           y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$VUS), 
                           se=pp[[1]]$se)

#### PLOTS
gam_VUS_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=VUS), alpha=0.3, colour = "darkblue")+
  ## gam
  geom_ribbon(data=p.VUS, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.7, fill = "grey", colour = "darkblue", size=0.2)+
  geom_line(data=p.VUS, aes(x=x, y=y), colour="black", linetype="solid", size = 0.5)+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.1), name= "VUS")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())


gam_VUS_plot





###pairwise_AUC_23
gam_pairwise_AUC_23 <- gam(pairwise_AUC_23 ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_pairwise_AUC_23, shift=mean(Merged_cm$pairwise_AUC_23))

p.pairwise_AUC_23 <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$pairwise_AUC_23), 
                            se=pp[[1]]$se)

gam_pairwise_AUC_23_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=pairwise_AUC_23), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.pairwise_AUC_23, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.pairwise_AUC_23, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Pairwise AUC of foraging and travel")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_pairwise_AUC_23_plot

###pairwise_AUC_13
gam_pairwise_AUC_13 <- gam(pairwise_AUC_13 ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_pairwise_AUC_13, shift=mean(Merged_cm$pairwise_AUC_13))

p.pairwise_AUC_13 <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$pairwise_AUC_13), 
                            se=pp[[1]]$se)

gam_pairwise_AUC_13_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=pairwise_AUC_13), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.pairwise_AUC_13, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.pairwise_AUC_13, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Pairwise AUC of rest and travel")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_pairwise_AUC_13_plot

###pairwise_AUC_12
gam_pairwise_AUC_12 <- gam(pairwise_AUC_12 ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_pairwise_AUC_12, shift=mean(Merged_cm$pairwise_AUC_12))

p.pairwise_AUC_12 <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$pairwise_AUC_12), 
                            se=pp[[1]]$se)

gam_pairwise_AUC_12_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=pairwise_AUC_12), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.pairwise_AUC_12, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.pairwise_AUC_12, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Precision of rest and foraging")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_pairwise_AUC_12_plot




###auc_1_all
gam_auc_1_all <- gam(auc_1_all ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_auc_1_all, shift=mean(Merged_cm$auc_1_all))

p.auc_1_all <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$auc_1_all), 
                            se=pp[[1]]$se)

gam_auc_1_all_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=auc_1_all), alpha=0.2, colour = "goldenrod")+
  ## gam
  geom_ribbon(data=p.auc_1_all, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.auc_1_all, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "AUC resting vs all")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_auc_1_all_plot





###auc_2_all
gam_auc_2_all <- gam(auc_2_all ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_auc_2_all, shift=mean(Merged_cm$auc_2_all))

p.auc_2_all <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$auc_2_all), 
                            se=pp[[1]]$se)

gam_auc_2_all_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=auc_2_all), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.auc_2_all, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.auc_2_all, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "AUC foraging vs all")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_auc_2_all_plot





###auc_3_all
gam_auc_3_all <- gam(auc_3_all ~ 
                         s(Model, k=12), 
                       method="REML", 
                       data = Merged_cm)

pp <- plot(gam_auc_3_all, shift=mean(Merged_cm$auc_3_all))

p.auc_3_all <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$auc_3_all), 
                            se=pp[[1]]$se)

gam_auc_3_all_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=auc_3_all), alpha=0.2, colour = "cyan4")+
  ## gam
  geom_ribbon(data=p.auc_3_all, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.auc_3_all, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "AUC travel vs all")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_auc_3_all_plot









###Precision_2
gam_Precision_2 <- gam(Precision_2 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Precision_2, shift=mean(Merged_cm$Precision_2))

p.Precision_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Precision_2), 
                              se=pp[[1]]$se)

gam_Precision_2_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Precision_2), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.Precision_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Precision_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Precision of foraging")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Precision_2_plot



###Specificity_2
gam_Specificity_2 <- gam(Specificity_2 ~ 
                      s(Model, k=12), 
                    method="REML", 
                    data = Merged_cm)

pp <- plot(gam_Specificity_2, shift=mean(Merged_cm$Specificity_2))

p.Specificity_2 <- data.frame(x=pp[[1]]$x, 
                         y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Specificity_2), 
                         se=pp[[1]]$se)

gam_Specificity_2_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Specificity_2), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.Specificity_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Specificity_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Specificity of foraging")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Specificity_2_plot

###Sensitivity_2
gam_Sensitivity_2 <- gam(Sensitivity_2 ~ 
                      s(Model, k=12), 
                    method="REML", 
                    data = Merged_cm)

pp <- plot(gam_Sensitivity_2, shift=mean(Merged_cm$Sensitivity_2))

p.Sensitivity_2 <- data.frame(x=pp[[1]]$x, 
                         y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Sensitivity_2), 
                         se=pp[[1]]$se)
#### PLOTS
gam_Sensitivity_2_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Sensitivity_2), alpha=0.2, colour = "darkred")+
  ## gam
  geom_ribbon(data=p.Sensitivity_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Sensitivity_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Sensitivity of foraging")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Sensitivity_2_plot

###Sensitivity_1
gam_Sensitivity_1 <- gam(Sensitivity_1 ~ 
                      s(Model, k=12), 
                    method="REML", 
                    data = Merged_cm)

pp <- plot(gam_Sensitivity_1, shift=mean(Merged_cm$Sensitivity_1))

p.Sensitivity_1 <- data.frame(x=pp[[1]]$x, 
                         y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Sensitivity_1), 
                         se=pp[[1]]$se)
#### PLOTS
gam_Sensitivity_1_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Sensitivity_1), alpha=0.2, colour = "goldenrod")+
  ## gam
  geom_ribbon(data=p.Sensitivity_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Sensitivity_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Sensitivity of resting")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Sensitivity_1_plot

###Precision_1
gam_Precision_1 <- gam(Precision_1 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Precision_1, shift=mean(Merged_cm$Precision_1))

p.Precision_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Precision_1), 
                              se=pp[[1]]$se)
#### PLOTS
gam_Precision_1_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Precision_1), alpha=0.2, colour = "goldenrod")+
  ## gam
  geom_ribbon(data=p.Precision_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Precision_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Precision of resting")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Precision_1_plot



###Specificity_1
gam_Specificity_1 <- gam(Specificity_1 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Specificity_1, shift=mean(Merged_cm$Specificity_1))

p.Specificity_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Specificity_1), 
                              se=pp[[1]]$se)
#### PLOTS
gam_Specificity_1_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Specificity_1), alpha=0.2, colour = "goldenrod")+
  ## gam
  geom_ribbon(data=p.Specificity_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Specificity_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Specificity of resting")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Specificity_1_plot


###############
##############
#############


###Sensitivity_3
gam_Sensitivity_3 <- gam(Sensitivity_3 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Sensitivity_3, shift=mean(Merged_cm$Sensitivity_3))

p.Sensitivity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Sensitivity_3), 
                              se=pp[[1]]$se)
#### PLOTS
gam_Sensitivity_3_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Sensitivity_3), alpha=0.2, colour = "cyan4")+
  ## gam
  geom_ribbon(data=p.Sensitivity_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Sensitivity_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Sensitivity of travelling")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Sensitivity_3_plot





###Precision_3
gam_Precision_3 <- gam(Precision_3 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Precision_3, shift=mean(Merged_cm$Precision_3))

p.Precision_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Precision_3), 
                              se=pp[[1]]$se)
#### PLOTS
gam_Precision_3_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Precision_3), alpha=0.2, colour = "cyan4")+
  ## gam
  geom_ribbon(data=p.Precision_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Precision_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.01), breaks = seq(0, 1, 0.10), name= "Precision of travelling")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Precision_3_plot




###Specificity_3
gam_Specificity_3 <- gam(Specificity_3 ~ 
                           s(Model, k=12), 
                         method="REML", 
                         data = Merged_cm)

pp <- plot(gam_Specificity_3, shift=mean(Merged_cm$Specificity_3))

p.Specificity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Merged_cm$Specificity_3), 
                              se=pp[[1]]$se)
#### PLOTS
gam_Specificity_3_plot<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Merged_cm, aes(x=Model, y=Specificity_3), alpha=0.2, colour = "cyan4")+
  ## gam
  geom_ribbon(data=p.Specificity_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Specificity_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  scale_x_continuous(limits = c(0, 0.75), breaks = seq(0, 0.75, 0.15),name="Proportion of known states")+
  scale_y_continuous(limits = c(-0.01, 1.03), breaks = seq(0, 1, 0.10), name= "Specificity of travelling")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_Specificity_3_plot




###try to arrange these.... 
library(gridExtra)
library(grid)
library(lattice)


grid.arrange( gam_Sensitivity_1_plot,gam_Sensitivity_2_plot, gam_Sensitivity_3_plot, gam_Specificity_1_plot, gam_Specificity_2_plot, gam_Specificity_3_plot, gam_Precision_1_plot, gam_Precision_2_plot, gam_Precision_3_plot,  nrow = 3)


grid.arrange( gam_auc_1_all_plot,gam_auc_2_all_plot, gam_auc_3_all_plot, nrow = 1) 
grid.arrange( gam_pairwise_AUC_12_plot, gam_pairwise_AUC_13_plot, gam_pairwise_AUC_23_plot, nrow = 1) 
              
              
      







#####Now make tile plots with mean confusion matrix predictions for the number of points with 0 and with all known states




for (iteration in all_iterations){
  #iteration<-all_iterations[10]
  #file_name<-Original_files[1]  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern = ('Final_birdPrep_states_extracted_AWS_3states'))

  
  allks<-readRDS("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/allks_for_cm_NOID_FINAL.rds")
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
   # file_name<-Original_files[4]
    
    #file_name <-Original_files[13]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1,-5)
    
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
    
    allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    table(joined$state)
    
    joined$point<-seq_along(1:nrow(joined))
    
    conf_mat <- confusionMatrix( as.factor(joined$state), as.factor(joined$test_state_full_act_inactive))
    plt <- as.data.frame(conf_mat$table)
    
    plt$iteration<- iteration
    plt$propKS<-it
    
    ###save out the conf_matrix table
    
    saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75/", iteration,"_", Model,"_", "df_cm.rds"))
    
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
    saveRDS(big_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75/", iteration,"_", Model,"_", "Probability.rds"))
    
    setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency")
    
    
    Merged_krippendorff <- 
      do.call(rbind,
              lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency/"), readRDS))
    
    #######now merge this consistency 
    
    consistency<-subset(Merged_krippendorff, prop_ks == Model)
    
    
    joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y", "point"))
    saveRDS(joined2, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_CONSISTENCY_GRAPH/", iteration,"_", Model,"_", "consistency.rds"))  
  
  }
  
}         





#####
setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")


for (iteration in all_iterations){
  #iteration<-all_iterations[10]
  #file_name<-Original_files[1]
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern =  'Final_birdPrep_states_extracted_AWS_3stat')
  
  allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")
  
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
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
    
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
    
    saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75/", iteration,"_", Model,"_", "df_cm.rds"))
    
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
    saveRDS(big_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_GRAPH/", iteration,"_", Model,"_", "Probability.rds"))
    # 
    # setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency")
    # 
    # 
    # Merged_krippendorff <- 
    #   do.call(rbind,
    #           lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency/"), readRDS))
    # 
    # #######now merge this consistency 
    # 
    # consistency<-subset(Merged_krippendorff, prop_ks == Model)
    # 
    # 
    # joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y"))
    # saveRDS(joined2, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/HMM_CONSISTENCY_GRAPH/", iteration,"_", Model,"_", "consistency.rds"))  
    
  }
  
}         







































setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_GRAPH/")


df <- list.files(pattern = ".rds") %>%
  map_dfr(readRDS)


head(df)




####split this by behaviour

df<-df%>%
  group_by(state, propks, iteration) %>%
  dplyr::mutate(max = max(nb_points))%>%
  ungroup()


find_cut<-df%>%
  group_by(propks, iteration, max_prob) %>%
  dplyr::mutate(prop_points = sum(nb_points)/sum(max)*100)%>%
  ungroup()


find_cut<-subset(find_cut, prop_points>79)

table(find_cut$max_prob)###cutting the data to a probability of 0.9 keeps 80% of the points while also limiting error

####lets plot this out as a gamm



## GAM calculations
library(mgcv)

theme_set(theme_light())

my_theme <-
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = rel(1.15)),
        axis.text = element_text(size = rel(1.1)))










df<-df%>%
  group_by(state, propks, iteration) %>%
  dplyr::mutate(prob_points = nb_points/max*100)%>%
  ungroup()


rest<-subset(df, state == 1)

forage<-subset(df, state == 2)

travel<-subset(df, state == 3)





##############################################################################
gam_prob_rest <- gam(nb_points ~ 
                      s(max_prob, k = 5), 
                    method="REML", 
                    data = rest)

pp <- plot(gam_prob_rest, shift=mean(rest$nb_points))

p.prop_rest <- data.frame(x=pp[[1]]$x, 
                         y=as.numeric(pp[[1]]$fit)+mean(rest$nb_points), 
                         se=pp[[1]]$se)
 ##############################################################################
 
gam_prob_forage <- gam(nb_points ~ 
                       s(max_prob,  k = 50), 
                     method="REML", 
                     data = forage)

pp <- plot(gam_prob_forage, shift=mean(forage$nb_points))

p.prop_forage <- data.frame(x=pp[[1]]$x, 
                          y=as.numeric(pp[[1]]$fit)+mean(forage$nb_points), 
                          se=pp[[1]]$se)

##############################################################################

gam_prob_travel <- gam(nb_points ~ 
                         s(max_prob,  k = 50), 
                       method="REML", 
                       data = travel)

pp <- plot(gam_prob_travel, shift=mean(travel$nb_points))

p.prop_travel <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(travel$nb_points), 
                            se=pp[[1]]$se)

##############################################################################

#####do as percentage of number of total points


##############################################################################
gam_percent_rest <- gam(prob_points ~ 
                       s(max_prob,  k = 68), 
                     method="REML", 
                     data = rest)

pp <- plot(gam_percent_rest, shift=mean(rest$prob_points))

p.prop_rest <- data.frame(x=pp[[1]]$x, 
                          y=as.numeric(pp[[1]]$fit)+mean(rest$prob_points), 
                          se=pp[[1]]$se)
##############################################################################

gam_percent_forage <- gam(prob_points ~ 
                         s(max_prob,  k = 50), 
                       method="REML", 
                       data = forage)

pp <- plot(gam_percent_forage, shift=mean(forage$prob_points))

p.prop_forage <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(forage$prob_points), 
                            se=pp[[1]]$se)

##############################################################################

gam_percent_travel <- gam(prob_points ~ 
                         s(max_prob,  k = 50), 
                       method="REML", 
                       data = travel)

pp <- plot(gam_percent_travel, shift=mean(travel$prob_points))

p.prop_travel <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(travel$prob_points), 
                            se=pp[[1]]$se)

##############################################################################

df$State<-ifelse(df$state == "1", "rest", ifelse(df$state == "2", "forage", "travel"))
df$State <- factor(df$State, levels=c('rest', 'forage', 'travel'))


#### PLOTS
gam_percentage_point_vs_probability_included <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=df, aes(x=max_prob, y=prob_points, colour = State), alpha=0.1)+
  scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  ## gam
  geom_ribbon(data=p.prop_rest, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.prop_rest, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  
  ## gam
  geom_ribbon(data=p.prop_forage, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.prop_forage, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  
  
  ## gam
  geom_ribbon(data=p.prop_travel, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.prop_travel, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability ")+
  scale_y_continuous(limits = c(0, 104), breaks = seq(0, 100, 10), name= "Percentage of points")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_percentage_point_vs_probability_included



#####now lets do with with the accuracy! 




for (iteration in all_iterations){
  #iteration<-all_iterations[1]
  #file_name<-Original_files[1]
  
  allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern = 'NEW.rds')
  
  #Original_files<- c(Original_files[15], Original_files[14])
  
  for (file_name in Original_files){
    # file_name<-Original_files[15]
    
    #file_name <-Original_files[13]
    it<-str_split(file_name, "_")
    it<-it[[1]]
    it<-it[7]
    it<-str_sub(it,1,-5)
    
    Model<-it
    #Read in the HMM output
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
    
    allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
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
          
          #i<-25
          aa<-seq(0.33:1, by=0.01)[i]
          j<- subset(joined, highest_prob >= aa)
          
          conf_mat <- confusionMatrix(as.factor(j$state), as.factor(j$test_state_full_act_inactive))
          
          Accuracy<- conf_mat$overall[1]
          Sensitivity_1<-conf_mat$byClass[1]
          Sensitivity_2<-conf_mat$byClass[2]
          Sensitivity_3<-conf_mat$byClass[3]
          
          Specificity_1<-conf_mat$byClass[4]
          Specificity_2<-conf_mat$byClass[5]
          Specificity_3<-conf_mat$byClass[6]
          
          Precision_1<-conf_mat$byClass[13]
          Precision_2<-conf_mat$byClass[14]
          Precision_3<-conf_mat$byClass[15]
          
          
          
          
          df<-data.frame(iteration, Model, Accuracy, Sensitivity_1, Specificity_1, Sensitivity_2,Specificity_2, Sensitivity_3,Specificity_3,Precision_1, Precision_2, Precision_3 )
          
          
          df$max_prob <-aa
          
          datalist[[i]] <- df # add it to your list
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      }
        
        big_data <- do.call(rbind, datalist)
        
        ###save out the conf_matrix table
        
        saveRDS(big_data, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_CM/", iteration,"_", Model,"_", "df_cm_probability.rds"))
          
  }
}
    
####now lets merge 


####now group these back together and plot?? 
setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_CM")


Probability_HMM <- 
  do.call(rbind,
          lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/HMM_PROBABILITY_CM/"), readRDS))

Probability_HMM$Model<-as.numeric(Probability_HMM$Model)
Probability_HMM_0.75<-subset(Probability_HMM, Model == 0.75)
Probability_HMM_0.75<-subset(Probability_HMM_0.75, max_prob<1)
Probability_HMM_0.75<-subset(Probability_HMM_0.75, !is.na(Sensitivity_3))





Merged_cm2<-select(Probability_HMM, -iteration)
Merged_cm2<-subset(Merged_cm2, max_prob >= 0.9)

Merged_cm2<-subset(Merged_cm2, Model == 0 | Model == 0.75)

data_long <- melt(Merged_cm2, id.vars = c("Model"))

table<-data_long%>% group_by(Model, variable)%>%
  dplyr::summarise(mean= mean(value), sd= sd(value))%>%
  ungroup()%>% 
  mutate_if(is.numeric, round, digits=2) %>%
  mutate_if(is.numeric, format, nsmall = 2) 

table$mean_sd<-paste0( table$mean, "±", table$sd)
table_final<-dcast(table, Model ~ variable, value.var ="mean_sd")

write.csv(table_final,"D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/summary_0_0.75_over0.9.csv", row.names = FALSE)


###NOW PLOT THESE OUT...

##############################################################################






#####do as percentage of number of total points


##############################################################################
gam_Acc_0.75 <- gam(Accuracy ~  s(max_prob,  k = 6), 
                        method="REML", 
                        data = Probability_HMM_0.75)

pp_0.75 <- plot(gam_Acc_0.75, shift=mean(Probability_HMM_0.75$Accuracy))

p.gam_Acc_0.75 <- data.frame(x=pp_0.75[[1]]$x, 
                          y=as.numeric(pp_0.75[[1]]$fit)+mean(Probability_HMM_0.75$Accuracy), 
                          se=pp_0.75[[1]]$se)
##############################################################################

gam_Sensitivity_1 <- gam(Sensitivity_1 ~ 
                            s(max_prob,  k = 6), 
                          method="REML", 
                          data = Probability_HMM_0.75)

pp <- plot(gam_Sensitivity_1, shift=mean(Probability_HMM_0.75$Sensitivity_1))

p.Sensitivity_1 <- data.frame(x=pp[[1]]$x, 
                            y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Sensitivity_1), 
                            se=pp[[1]]$se)

##############################################################################


gam_Sensitivity_2 <- gam(Sensitivity_2 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Sensitivity_2, shift=mean(Probability_HMM_0.75$Sensitivity_2))

p.Sensitivity_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Sensitivity_2), 
                              se=pp[[1]]$se)

##############################################################################


gam_Sensitivity_3 <- gam(Sensitivity_3 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Sensitivity_3, shift=mean(Probability_HMM_0.75$Sensitivity_3))

p.Sensitivity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Sensitivity_3), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_1 <- gam(Specificity_1 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Specificity_1, shift=mean(Probability_HMM_0.75$Specificity_1))

p.Specificity_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Specificity_1), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_2 <- gam(Specificity_2 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Specificity_2, shift=mean(Probability_HMM_0.75$Specificity_2))

p.Specificity_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Specificity_2), 
                              se=pp[[1]]$se)

##############################################################################


gam_Specificity_3 <- gam(Specificity_3 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Specificity_3, shift=mean(Probability_HMM_0.75$Specificity_3))

p.Specificity_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Specificity_3), 
                              se=pp[[1]]$se)

##############################################################################
##############################################################################


gam_Precision_1 <- gam(Precision_1 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Precision_1, shift=mean(Probability_HMM_0.75$Precision_1))

p.Precision_1 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Precision_1), 
                              se=pp[[1]]$se)

##############################################################################


gam_Precision_2 <- gam(Precision_2 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Precision_2, shift=mean(Probability_HMM_0.75$Precision_2))

p.Precision_2 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Precision_2), 
                              se=pp[[1]]$se)

##############################################################################


gam_Precision_3 <- gam(Precision_3 ~ 
                           s(max_prob,  k = 6), 
                         method="REML", 
                         data = Probability_HMM_0.75)

pp <- plot(gam_Precision_3, shift=mean(Probability_HMM_0.75$Precision_3))

p.Precision_3 <- data.frame(x=pp[[1]]$x, 
                              y=as.numeric(pp[[1]]$fit)+mean(Probability_HMM_0.75$Specificity_3), 
                              se=pp[[1]]$se)

##############################################################################





Probability_HMM_0<-subset(Probability_HMM, Model == 0)
Probability_HMM_0<-subset(Probability_HMM_0, max_prob<1)
Probability_HMM_0<-subset(Probability_HMM_0, !is.na(Sensitivity_3))



#####do as percentage of number of total points


##############################################################################
gam_Acc_0 <- gam(Accuracy ~  s(max_prob,  k = 15), 
               method="REML", 
               data = Probability_HMM_0)

pp_0 <- plot(gam_Acc_0, shift=mean(Probability_HMM_0$Accuracy))

p.gam_Acc_0 <- data.frame(x=pp_0[[1]]$x, 
                        y=as.numeric(pp_0[[1]]$fit)+mean(Probability_HMM_0$Accuracy), 
                        se=pp_0[[1]]$se)

summary(gam_Acc_0)













#### PLOTS
gam_probability_Precision <- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Precision_1),colour = "goldenrod", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Precision_2), colour ="darkred", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Precision_3), colour ="cyan", alpha = 0.1)+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+
  
  geom_ribbon(data=p.Precision_1, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "gold", colour = "goldenrod", size=0.2)+
  geom_line(data=p.Precision_1, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Precision_2, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.Precision_2, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.Precision_3, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "cyan", colour = "cyan4", size=0.2)+
  geom_line(data=p.Precision_3, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability included")+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.1), name= "Precision")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())+
  guides(colour=guide_legend(title="Prop KS"))

gam_probability_Precision

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


gam_probability_ACC<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=Probability_HMM, aes(x=max_prob, y=Accuracy), alpha = 0.1, colour = "darkblue")+
  #scale_color_manual(values = c("goldenrod", "darkred", "cyan4"))+

  geom_ribbon(data=p.gam_Acc_0.75, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkblue", colour = "darkblue", size=0.2)+
  geom_line(data=p.gam_Acc_0.75, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability included")+
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1, 0.1), name= "Accuracy")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())+
  guides(colour=guide_legend(title="Prop KS"))

gam_probability_ACC



all.points<- rbind(Probability_HMM_0, Probability_HMM_0.75)

all.points$Model <- as.factor (all.points$Model)



gam_probability_ACC<- 
  ggplot()+
  ## phenology lines
  ## points
  geom_point(data=all.points, aes(x=max_prob, y=Accuracy, colour = Model), alpha = 0.1)+

  scale_color_manual(values = c("darkred", "darkblue"))+
  
  geom_ribbon(data=p.gam_Acc_0.75, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkblue", colour = "darkblue", size=0.2)+
  geom_line(data=p.gam_Acc_0.75, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  geom_ribbon(data=p.gam_Acc_0, aes(x=x, ymin=y-se, ymax=y+se), alpha=0.6, fill = "darkred", colour = "darkred", size=0.2)+
  geom_line(data=p.gam_Acc_0, aes(x=x, y=y), colour="grey20", linetype="solid")+
  
  
  scale_x_continuous(limits = c(0.33, 1), breaks = seq(0.3, 1, 0.1),name="Minimum probability included")+
  scale_y_continuous(limits = c(0.6, 1), breaks = seq(0.6, 1, 0.05), name= "Accuracy")+
  #coord_cartesian(xlim=c(0, 366), ylim=c(0, 24))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  my_theme+
  theme(axis.text = element_text(size=12),
        panel.grid.minor.x = element_blank())

gam_probability_ACC














############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


#confusion matrix plots


#bring in all data from confusion matrices of 0 and 0.75
  

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75")


df <- list.files(pattern = "df_cm") %>%
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
rng = range(c((0), (4000))) #a range to have the same min and max for both plots
ggplot(p0_sum, aes(Reference, Prediction,  fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194",limits=c(floor(rng[1]), ceiling(rng[2]))) +
  labs(y = "Prediction",x = "Reference") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")







# 
# ggplot(data = melt(x)) + geom_tile(aes(x=X1,y=X2,fill = value)) +
#   scale_fill_gradient2(low="blue", mid="cyan", high="purple", #colors in the scale
#                        midpoint=mean(rng),    #same midpoint for plots (mean of the range)
#                        breaks=seq(-100,100,4), #breaks in the scale bar
#                        limits=c(floor(rng[1]), ceiling(rng[2]))) #same limits for plots
# 




####################


p0.75<-subset(df, propKS == 0.75)

p0.75<- p0.75%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.75$Percentage<-round(p0.75$Freq/p0.75$sum*100)


p0.75_sum<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.75_sum$Mean<-paste0(p0.75_sum$mean_Freq, " \u00b1 ", p0.75_sum$sd_Freq)

p0.75_perc<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.75_perc$Mean<-paste0(p0.75_perc$mean_Prencetage, " \u00b1 ", p0.75_perc$sd_Percentage)

####now lets create a geom:tile 

ggplot(p0.75_sum, aes(Reference, Prediction,  fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194", limits=c(floor(rng[1]), ceiling(rng[2]))) +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")



ggplot(p0.75_perc, aes(Reference, Prediction,  fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
  



#####Now do the same but with consistency and with probability over 90 

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

all_iterations<-c("iteration1", "iteration2", "iteration3", "iteration4", "iteration5", "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")



for (iteration in all_iterations){
  #iteration<-all_iterations[10]
  #file_name<-Original_files[5]
  
  allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")
  
  allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
  
  allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
  
  table(allks$true_state_act_inactive)
  
  Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern =  'Final_birdPrep_states_extracted_AWS_3states')
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
    output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
    
    allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
    
    
    ####Merge with the known states 
    joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
    table(joined$state)
    
    joined$point<-seq_along(1:nrow(joined))
    
    joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
    
    joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
    
    joined<-subset(joined, highest_prob > 0.9)
    
    conf_mat <- confusionMatrix(as.factor(joined$state),as.factor(joined$test_state_full_act_inactive))
    plt <- as.data.frame(conf_mat$table)
    
    plt$iteration<- iteration
    plt$propKS<-it
    
    ###save out the conf_matrix table
    
    saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75_prop_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
    
    
  }
}
    
#     
# 
# for (iteration in all_iterations){
#   #iteration<-all_iterations[1]
#   #file_name<-Original_files[1]
#   
#   allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")
#   
#   
#   allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
#   
#   allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
#   
#   table(allks$true_state_act_inactive)
#   Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern =  'Final_birdPrep_states_extracted_AWS_3states')
#   
#   #Original_files<- c(Original_files[15], Original_files[14])
#   
#   for (file_name in Original_files){
#     
#     tryCatch({
#     # file_name<-Original_files[1]
#     
#     #file_name <-Original_files[13]
#     it<-str_split(file_name, "_")
#     it<-it[[1]]
#     it<-it[7]
#     it<-str_sub(it,1,-5)
#     
#     Model<-it
#     #Read in the HMM output
#     output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
#     
#     allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
#     
#     
#     ####Merge with the known states 
#     joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
#     table(joined$state)
#     
#     joined$point<-seq_along(1:nrow(joined))
#     
#     joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
#     
#     joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
#     
#     setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency")
#     
#     Merged_krippendorff <- 
#       do.call(rbind,
#               lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency/"), readRDS))
#     
#     #######now merge this consistency 
#     
#     consistency<-subset(Merged_krippendorff, prop_ks == Model)
#     
#     
#     joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y", "point"))
#     
#     
#     
#     joined2<-subset(joined2, p_consistency > 0.8)
#     
#    
#       
#     
#     conf_mat <- confusionMatrix(as.factor( as.factor(joined2$state), joined2$test_state_full_act_inactive))
#     plt <- as.data.frame(conf_mat$table)
#     
#     plt$iteration<- iteration
#     plt$propKS<-it
#     
#     ###save out the conf_matrix table
#     
#     saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_consistency_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     
#     
#   }
# }  
#     
#     
#     
# 
# 
# 
# for (iteration in all_iterations){
#   #iteration<-all_iterations[1]
#   #file_name<-Original_files[1]
#   
#   
#   allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")
#   
#   
#   allks$test_state_full<-ifelse(!is.na(allks$true_state) & is.na(allks[paste0(iteration, "_90")]),allks$true_state, NA)
#   
#   allks$test_state_full_act_inactive<-ifelse(!is.na(allks$true_state_act_inactive) & is.na(allks[paste0(iteration, "_90")]),allks$true_state_act_inactive, NA)
#   
#   
#   
#   Original_files <- list.files(path = paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/",iteration), pattern =  'Final_birdPrep_states_extracted_AWS_3states')
#   
#   #Original_files<- c(Original_files[15], Original_files[14])
#   
#   for (file_name in Original_files){
#     
#     tryCatch({
#       # file_name<-Original_files[1]
#       
#       #file_name <-Original_files[13]
#       it<-str_split(file_name, "_")
#       it<-it[[1]]
#       it<-it[7]
#       it<-str_sub(it,1,-5)
#       
#       Model<-it
#       #Read in the HMM output
#       output<- readRDS(paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/", iteration, "/", file_name))
#       
#       allks<-select(allks,ring_dep_trip,dateTime_gps, lon, lat, test_state_full_act_inactive )
#       
#       
#       ####Merge with the known states 
#       joined<- merge(output, allks, by.x = c("ID", "time", "x", "y"),by.y = c("ring_dep_trip", "dateTime_gps", "lon", "lat"))
#       table(joined$state)
#       
#       joined$point<-seq_along(1:nrow(joined))
#       
#       joined$highest_prob<- pmax(joined$prob1, joined$probp2,joined$prob3)
#       
#       joined$behaviour_of_highest_prob<-ifelse(joined$highest_prob == joined$prob1, "resting", ifelse(joined$highest_prob == joined$probp2, "foraging", "travelling" ))
#       
#       setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency")
#       
#       Merged_krippendorff <- 
#         do.call(rbind,
#                 lapply(list.files(path = "D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/fitTrack_0.75/Percent_consistency/"), readRDS))
#       
#       #######now merge this consistency 
#       
#       consistency<-subset(Merged_krippendorff, prop_ks == Model)
#       
#       
#       joined2<-merge(consistency, joined, by = c("ID", "time", "x", "y", "point"))
#       
#       
#       
#       joined2<-subset(joined2, p_consistency > 0.8 & highest_prob > 0.9)
#       
#       
#       
#       
#       conf_mat <- confusionMatrix(as.factor(joined2$state), as.factor(joined2$test_state_full_act_inactive))
#       plt <- as.data.frame(conf_mat$table)
#       
#       plt$iteration<- iteration
#       plt$propKS<-it
#       
#       ###save out the conf_matrix table
#       
#       saveRDS(plt, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_consistency_and_prop_0.9/", iteration,"_", Model,"_", "df_cm.rds"))
#     }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     
#     
#   }
# }  



#####create the confusion matrix plot for prob >  0.9



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/HMM_0.75_prop_0.9")

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
dev.off()
pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_sum_90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size




ggplot(p0_sum, aes(x = Reference, y = Prediction, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194",limits=c(floor(rng[1]), ceiling(rng[2]))) +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off() 



pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_perc_90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size



ggplot(p0_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
dev.off() 


####################


p0.75<-subset(df, propKS == 0.75)

p0.75<- p0.75%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.75$Percentage<-round(p0.75$Freq/p0.75$sum*100)


p0.75_sum<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.75_sum$Mean<-paste0(p0.75_sum$mean_Freq, " \u00b1 ", p0.75_sum$sd_Freq)

p0.75_perc<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.75_perc$Mean<-paste0(p0.75_perc$mean_Prencetage, " \u00b1 ", p0.75_perc$sd_Percentage)

####now lets create a geom:tile 

pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size


ggplot(p0.75_sum, aes(Reference, Prediction , fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194",limits=c(floor(rng[1]), ceiling(rng[2]))) +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off()


pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_perc_90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size



ggplot(p0.75_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
dev.off()



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

#####create the confusion matrix plot for raw data



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_raw")


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
dev.off()
pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_sum_raw.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size




ggplot(p0_sum, aes(x = Reference, y = Prediction, fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off() 



pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_perc_raw.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size



ggplot(p0_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
dev.off() 


####################


p0.75<-subset(df, propKS == 0.75)

p0.75<- p0.75%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.75$Percentage<-round(p0.75$Freq/p0.75$sum*100)


p0.75_sum<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.75_sum$Mean<-paste0(p0.75_sum$mean_Freq, " \u00b1 ", p0.75_sum$sd_Freq)

p0.75_perc<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.75_perc$Mean<-paste0(p0.75_perc$mean_Prencetage, " \u00b1 ", p0.75_perc$sd_Percentage)

####now lets create a geom:tile 

pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_raw.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size


ggplot(p0.75_sum, aes(Reference, Prediction , fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off()


pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_perc_raw.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size



ggplot(p0.75_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
dev.off()




















































# 
# setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_consistency_0.9")
# 
# 
# df <- list.files(pattern = ".rds") %>%
#   map_dfr(readRDS)
# 
# p0<-subset(df, propKS == 0)
# 
# 
# p0<- p0%>%
#   dplyr::group_by (Reference, iteration)%>%
#   dplyr::mutate(sum = sum(Freq))%>%
#   ungroup()
# 
# p0$Percentage<-round(p0$Freq/p0$sum*100)
# 
# 
# p0_sum<- p0 %>%
#   dplyr::group_by(Prediction, Reference) %>%
#   dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
#                    sd_Freq = round(sd(Freq), digits = 0))%>%
#   ungroup()
# 
# 
# p0_sum$Mean<-paste0(p0_sum$mean_Freq, " \u00b1 ", p0_sum$sd_Freq)
# 
# p0_perc<- p0 %>%
#   dplyr::group_by(Prediction, Reference) %>%
#   dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
#                    sd_Percentage = round(sd(Percentage), digits = 0))%>%
#   ungroup()
# 
# 
# p0_perc$Mean<-paste0(p0_perc$mean_Prencetage, " \u00b1 ", p0_perc$sd_Percentage)
# 
# ####now lets create a geom:tile 
# 
# pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_sum_cons90.pdf",         # File name
#     width = 4, height = 3, # Width and height in inches
#     bg = "white")          # Paper size
# 
# 
# ggplot(p0_sum, aes(Reference,Prediction , fill= mean_Freq)) +
#   geom_tile() + geom_text(aes(label = Mean)) +
#   scale_fill_gradient(low="white", high="#009194") +
#   labs(x = "Reference",y = "Prediction") +
#   scale_x_discrete(labels=c("rest", "forage", "travel")) +
#   scale_y_discrete(labels=c("rest", "forage", "travel")) +
#   labs(fill = "Mean ")
# dev.off()
# 
# pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_perc_cons90.pdf",         # File name
#     width = 4, height = 3, # Width and height in inches
#     bg = "white")          # Paper size
# 
# 
# ggplot(p0_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
#   geom_tile() + geom_text(aes(label = Mean)) +
#   scale_fill_gradient(low="white", high="#009194") +
#   labs(x = "Reference",y = "Prediction") +
#   scale_x_discrete(labels=c("rest", "forage", "travel")) +
#   scale_y_discrete(labels=c("rest", "forage", "travel")) + 
#   labs(fill = "Mean %")
# dev.off()
# ####################


p0.75<-subset(df, propKS == 0.75)

p0.75<- p0.75%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.75$Percentage<-round(p0.75$Freq/p0.75$sum*100)


p0.75_sum<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.75_sum$Mean<-paste0(p0.75_sum$mean_Freq, " \u00b1 ", p0.75_sum$sd_Freq)

p0.75_perc<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.75_perc$Mean<-paste0(p0.75_perc$mean_Prencetage, " \u00b1 ", p0.75_perc$sd_Percentage)

####now lets create a geom:tile 
pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_sum_cons90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size

ggplot(p0.75_sum, aes(Reference,Prediction , fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off()

pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_perc_cons90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")          # Paper size

ggplot(p0.75_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")
dev.off()






setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/HMMs/tabular_confusion_matrix/travel_consistency_and_prop_0.9")


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
pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_perc_propcons90..pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")   

ggplot(p0_sum, aes(Reference,Prediction , fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off()

pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0_perc_propcons90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")   

ggplot(p0_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

dev.off()
####################


p0.75<-subset(df, propKS == 0.75)

p0.75<- p0.75%>%
  dplyr::group_by (Reference, iteration)%>%
  dplyr::mutate(sum = sum(Freq))%>%
  ungroup()

p0.75$Percentage<-round(p0.75$Freq/p0.75$sum*100)


p0.75_sum<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Freq = round(mean (Freq), digits = 0), 
                   sd_Freq = round(sd(Freq), digits = 0))%>%
  ungroup()


p0.75_sum$Mean<-paste0(p0.75_sum$mean_Freq, " \u00b1 ", p0.75_sum$sd_Freq)

p0.75_perc<- p0.75 %>%
  dplyr::group_by(Prediction, Reference) %>%
  dplyr::summarize(mean_Prencetage = round(mean (Percentage), digits = 0), 
                   sd_Percentage = round(sd(Percentage), digits = 0))%>%
  ungroup()


p0.75_perc$Mean<-paste0(p0.75_perc$mean_Prencetage, " \u00b1 ", p0.75_perc$sd_Percentage)

####now lets create a geom:tile 

pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_sum_propcons90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")   


ggplot(p0.75_sum, aes(Reference,Prediction , fill= mean_Freq)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) +
  labs(fill = "Mean ")
dev.off()



pdf("C:/Users/Sarah Saldanha/Dropbox/PhD/Methods_Paper/Figures and Tables/Confusion_Matrices/travel/p0.75_perc_propcons90.pdf",         # File name
    width = 4, height = 3, # Width and height in inches
    bg = "white")   

ggplot(p0.75_perc, aes(Reference, Prediction, fill= mean_Prencetage)) +
  geom_tile() + geom_text(aes(label = Mean)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("rest", "forage", "travel")) +
  scale_y_discrete(labels=c("rest", "forage", "travel")) + 
  labs(fill = "Mean %")

dev.off()


                                                 
