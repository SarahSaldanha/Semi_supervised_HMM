# Semi_supervised_HMM
Using TDR, accelerometry and wet-dry data to semi-supervise Hidden Markov Models applied to tropicbird tracking data

The following is the code for the article "Animal behaviour on the move: the use of auxiliary information and semi-supervision to improve behavioural inferences from hidden Markov Models applied to GPS tracking data-sets". 

1_Scripts_pre_processing_cutting_gps: 
Scripts to interpolate GPS tracks to 5 minutes to prepare the data for the HMMs

2_AXY: Adapted script  from  PM Collins (2015), later modified by B Clark (2018) for the Grupo Aves Marinas Course, Dec 2020 to calculate metrics from the accelerometry data and run a random forest model to classify accelerometry and TDR data into flapping flight, resting and diving

3_WD: Adapted script from Z. Zajkova to extract 6s migrate GLS wet-dry data and match this data to GPS positions.  

4_TDR: Clean TDR data using the package diveMove to extract number and duration of dives. 

5_Bring_Everything_together: Combined GPS, WD, TDR and accelerometry data create known states dataset 

6_HMM run HMMs on the informed dataset and on the full GPS dataset 

7_Confusion_matrices: create and plot confusion matrices for the results of the HMM models 

8_Plots: plot the results of the HMM supervision onto a map

9_summary_step_turning_angle: summarize step and turning angle of the HMM models to visualize how they are affected by semi-supervision. 


