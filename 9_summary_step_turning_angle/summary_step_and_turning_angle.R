####averaging transition probabilities between 0 and 0.75 10 model itterations



setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID")

allks<-readRDS("./HMMs/allks_for_cm_NOID_FINAL.rds")



  
    output<- readRDS(paste0("./HMMS/fitTrack_0.75/", iteration, "/", "model_selection_0.75.rds"))
    X<-min(unlist(unname(lapply(output[[1]], function(x) x$mod$minimum))))


all_iterations<-c("iteration1", "iteration2", "iteration3", "iteration4", "iteration5", "iteration6", "iteration7", "iteration8", "iteration9", "iteration10")

library(momentuHMM)

for (iteration in all_iterations){
  #iteration<-all_iterations [1]
  
    #Read in the HMM output
    output<- readRDS(paste0("./HMMS/fitTrack_0.75/", iteration, "/", "model_selection_0.rds"))

    
    X<-min(unlist(unname(lapply(output[[1]], function(x) x$mod$minimum))))
      
    BestModel<- ifelse(output[[1]][[1]]$mod$minimum == X, 1, ifelse( output[[1]][[2]]$mod$minimum == X, 2, ifelse(output[[1]][[3]]$mod$minimum == X, 3, ifelse(output[[1]][[4]]$mod$minimum == X, 4, ifelse(output[[1]][[5]]$mod$minimum == X, 4, ifelse(output[[1]][[6]]$mod$minimum == X, 6, ifelse(output[[1]][[7]]$mod$minimum == X, 7, ifelse(output[[1]][[8]]$mod$minimum == X, 7, ifelse(output[[1]][[9]]$mod$minimum == X, 9, ifelse(output[[1]][[10]]$mod$minimum == X, 10, 11))))))))))
    
    
    m_step<-melt(output[[1]][[BestModel]]$mle$step)
    m_step$model<-0
    m_angle<-melt(output[[1]][[BestModel]]$mle$angle)
    m_angle$model<-0
    
    write.csv(m_step, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_step/", iteration, "_", 0, ".csv"))
      
    write.csv(m_angle, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_angle/", iteration, "_", 0, ".csv"))
    
    
    BestModel<- ifelse(output[[1]][[1]]$mod$minimum == X, 1, ifelse( output[[1]][[2]]$mod$minimum == X, 2, ifelse(output[[1]][[3]]$mod$minimum == X, 3, ifelse(output[[1]][[4]]$mod$minimum == X, 4, ifelse(output[[1]][[5]]$mod$minimum == X, 4, ifelse(output[[1]][[6]]$mod$minimum == X, 6, ifelse(output[[1]][[7]]$mod$minimum == X, 7, ifelse(output[[1]][[8]]$mod$minimum == X, 7, ifelse(output[[1]][[9]]$mod$minimum == X, 9, ifelse(output[[1]][[10]]$mod$minimum == X, 10, 11))))))))))
    
    
    m_step<-melt(output[[1]][[BestModel]]$mle$step)
    m_step$model<-0.75
    m_angle<-melt(output[[1]][[BestModel]]$mle$angle)
    m_angle$model<-0.75
    
    write.csv(m_step, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_step/", iteration, "_", 0.75, ".csv"))
    
    write.csv(m_angle, paste0("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_angle/", iteration, "_", 0.75, ".csv"))
    
}
          


setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_angle/")

Merged_angle <- 
  do.call(rbind,
          lapply(list.files(), read.csv))%>%
  group_by(model, Var1, Var2) %>%
  dplyr::summarize(mean = mean(value), 
                   sd = sd(value))

setwd("D:/Dropbox/PhD/All.GPSandAXYS/AXY_DATA_METHODS_PAPER_NO_ID/summary_step_angle/summary_step/")

Merged_step <- 
  do.call(rbind,
          lapply(list.files(), read.csv))%>%
  group_by(model, Var1, Var2) %>%
  dplyr::summarize(mean = mean(value), 
                   sd = sd(value))
    
    
    
    

   
   
   
   
      
