##//Data managmenet for KSAT 
##//This is the stable version for lab measurements

###///This script is brute force but allows for more efficient troubleshooting
##//Package management

library(readxl)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tidyselect)
library(stringr)

##//ggplot theme 
theme_set(theme_minimal())

##//custom color ramp 
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")

##//Where is the data
setwd("D:/Dropbox/Projects/Indiana/SoilLab/Data/KSat/RawData/MMSF_20220405")
dir()

##//List of files in the directory
Flist <- dir(pattern = "KSAT")

##//Complied dataframe
DOut <- data.frame()
Fdat <- data.frame();   #View(Fdat) ##// uncomment to see what data table looks like, hint its currently empty

##//Reading in the files by lines so that we can rip apart the irregular vetor formats
for(k in 1:length(Flist)){
  text <- readLines(Flist[k])
  
  ##//Pulling meta data and results
  meta <-  text %>% 
    enframe(name = NULL) %>%
    filter(str_detect(value, "^.")) %>%   ##// Regular expression grabbing information for each line with two  vectors 
    separate(value, sep = "\t", into = c("Parameter", "Value", "V1", "V2", "V3", "V4", "V5" ))
  #View(maybe) ##//Uncomment to check on tables
  ##// V1-V5 serve as place holders as file structure has data on the bottom with more parameters (See Data below)
  ##//Data and Meta data
  KSat_dat <- data.frame(
    ##//KSat
    File = meta$Value[meta$Parameter=="File name"],
    CulturInfo = meta$Value[meta$Parameter=="CulturInfo"],
    SoftwareVersion = meta$Value[meta$Parameter=="Software Version"],
    FirmwareVersion = meta$Value[meta$Parameter=="Firmware Version"],
    Last_zero = meta$Value[meta$Parameter=="Last setting of the zero point"],
    SerialNumber = meta$Value[meta$Parameter=="Serial Number"],
    AdditionalComment = meta$Value[meta$Parameter=="Additional Comment"],
    
    ##//Measurement
    Mode = meta$Value[meta$Parameter=="Mode"],
    SampleRate_s = meta$Value[meta$Parameter=="Sample rate [s]"],
    CrownType = meta$Value[meta$Parameter=="Crown type"],
    SyntheticData = meta$Value[meta$Parameter=="Use Synthetic Data"],
    
    ##//Parameters
    ##//Operation Parameters 
    H_end_abs_cm = meta$Value[meta$Parameter=="H_end_abs [cm]"],
    H_end_rel = meta$Value[meta$Parameter=="H_end_rel [-]"],
    dH_min_cm  = meta$Value[meta$Parameter=="dH_min [cm]"],
    dH_ini_cm = meta$Value[meta$Parameter=="dH_ini [cm]"],
    
    ##//Geometery Parameters 
    A_bur_cm_2 = meta$Value[meta$Parameter=="A_bur [cmÂ²]"],
    A_cap_in_cm_2 = meta$Value[meta$Parameter=="A_cap_in [cmÂ²]"],                      
    A_sample_cm_2 = meta$Value[meta$Parameter=="A_sample  [cmÂ²]"],
    L_bur_cm = meta$Value[meta$Parameter=="L_bur [cm]"],
    L_sample_cm = meta$Value[meta$Parameter=="L_sample [cm]"],
    L_plate_Bottom_cm = meta$Value[meta$Parameter=="L_plate_Bottom [cm]"],
    L_plate_Top_cm = meta$Value[meta$Parameter=="L_plate_Top [cm]"],
    
    ##//Evaluation  Parameters                
    T_ref_C = meta$Value[meta$Parameter=="T_ref [Â°C]"],
    K_plate_cm_d = meta$Value[meta$Parameter=="K_plate [cm/d]"],
    Auto_Offset = meta$Value[meta$Parameter=="Use Auto-Offset"],
    Max_Auto_Offset_cm = meta$Value[meta$Parameter=="Max Auto-Offset [cm]"],              
    TriggerAuto_Offset_cm = meta$Value[meta$Parameter=="Trigger Auto-Offset [cm]"],
    StartMeasurement = meta$Value[meta$Parameter=="Start of measurement"],                
    TestDuration = meta$Value[meta$Parameter=="Test duration"],
    
    ##//Results
    UseAutoOffsetAdjustment = meta$Value[meta$Parameter=="Use auto offset adjustment"],
    MaxOffsetAdjustment_cm = meta$Value[meta$Parameter=="Max offset adjustment [cm]"],
    FittingParameter_a_cm = meta$Value[meta$Parameter=="Fitting Parameter a [cm]"],
    FittingParameter_b_s = meta$Value[meta$Parameter=="Fitting Parameter b [s-1]"],
    FittingParameter_c_cm = meta$Value[meta$Parameter=="Fitting Parameter c [cm]"],
    FittingParameter_r2 = meta$Value[meta$Parameter=="Fitting Parameter r2 [-]"],
    KsTotal_cm_d = meta$Value[meta$Parameter=="Ks Total [cm/d]"],
    KsSoil_cm_d = meta$Value[meta$Parameter=="Ks Soil [cm/d]"],
    KsSoil_m_s = meta$Value[meta$Parameter=="Ks Soil [m/s]"],
    Norm_KsSoil_cm_d = meta$Value[meta$Parameter=="Ks Soil normalized at 10.0 Â°C [cm/d]"],
    Norm_KsSoil_m_s = meta$Value[meta$Parameter=="Ks Soil normalized at 10.0 Â°C [m/s]"])
  
  
  
  ###//Raw fluxes to long format dataframe
  KData <-  text %>% 
    enframe(name = NULL) %>%
    filter(str_detect(value, "^\\d")) %>%  ##// Regular expression searching for start of line begining with a number 
    separate(value, sep = "\t", into = c("Date", "Chaos" ,"Runtime_s",	"PressureHead_cm", "Fitting_cm", "Temp_C"))
  
  ##//The chaos parameter is currently an unknown entity that does not correlate with any other parameters in the 
  #    KSAT data table
  
  ##//Updating the classes
  KData$Date_ct <- as.POSIXct(strptime(KData$Date, format = "%m/%d/%Y %H:%M:%S %p"), tz="EST")
  KData$Runtime_s <- as.numeric(KData$Runtime_s)
  KData$PressureHead_cm <- as.numeric(KData$PressureHead_cm)
  KData$Fitting_cm <- as.numeric(KData$Fitting_cm)
  KData$Chaos <- as.numeric(KData$Chaos)
  KData$Temp_C <- as.numeric(KData$Temp_C)
  
  ##//Reconstructing data to be in long format 
  tests <- KData
  
  KData$File <- KSat_dat$File[1]
  KData$CulturInfo <- KSat_dat$CulturInfo[1]
  KData$SoftwareVersion <- KSat_dat$SoftwareVersion[1]
  KData$FirmwareVersion <- KSat_dat$FirmwareVersion[1]
  KData$Last_zero <- KSat_dat$Last_zero[1]
  KData$SerialNumber <- KSat_dat$SerialNumber[1]
  KData$AdditionalComment <- KSat_dat$AdditionalComment[1]
  KData$Mode <- KSat_dat$Mode[1]
  KData$SampleRate_s <- KSat_dat$SampleRate_s[1]
  KData$CrownType <- KSat_dat$CrownType[1]
  KData$SyntheticData <- KSat_dat$SyntheticData[1]
  KData$H_end_abs_cm <- KSat_dat$H_end_abs_cm[1]
  KData$H_end_rel <- KSat_dat$H_end_rel[1]
  KData$dH_min_cm <- KSat_dat$dH_min_cm[1]
  KData$dH_ini_cm <- KSat_dat$dH_ini_cm[1]
  KData$A_bur_cm_2 <- KSat_dat$A_bur_cm_2[1]
  KData$A_cap_in_cm_2 <- KSat_dat$A_cap_in_cm_2[1]
  KData$A_sample_cm_2 <- KSat_dat$A_sample_cm_2[1]
  KData$L_bur_cm <- KSat_dat$L_bur_cm[1]
  KData$L_sample_cm <- KSat_dat$L_sample_cm[1]
  KData$L_plate_Bottom_cm <- KSat_dat$L_plate_Bottom_cm[1]
  KData$L_plate_Top_cm <- KSat_dat$L_plate_Top_cm[1]
  KData$T_ref_C <- KSat_dat$T_ref_C[1]
  KData$K_plate_cm_d <- KSat_dat$K_plate_cm_d[1]
  KData$Auto_Offset <- KSat_dat$Auto_Offset[1]
  KData$Max_Auto_Offset_cm <- KSat_dat$Max_Auto_Offset_cm[1]
  KData$TriggerAuto_Offset_cm <- KSat_dat$TriggerAuto_Offset_cm[1]
  KData$StartMeasurement <- KSat_dat$StartMeasurement[1]
  KData$TestDuration <- KSat_dat$TestDuration[1]
  KData$UseAutoOffsetAdjustment <- KSat_dat$UseAutoOffsetAdjustment[1]
  KData$MaxOffsetAdjustment_cm <- KSat_dat$MaxOffsetAdjustment_cm[1]
  KData$FittingParameter_a_cm <- KSat_dat$FittingParameter_a_cm[1]
  KData$FittingParameter_b_s <- KSat_dat$FittingParameter_b_s[1]
  KData$FittingParameter_c_cm <- KSat_dat$FittingParameter_c_cm[1]
  KData$FittingParameter_r2 <- KSat_dat$FittingParameter_r2[1]
  KData$KsTotal_cm_d <- KSat_dat$KsTotal_cm_d[1]
  KData$KsSoil_cm_d <- KSat_dat$KsSoil_cm_d[1]
  KData$KsSoil_m_s <- KSat_dat$KsSoil_m_s[1]
  KData$Norm_KsSoil_cm_d <- KSat_dat$Norm_KsSoil_cm_d[1]
  KData$Norm_KsSoil_m_s <- KSat_dat$Norm_KsSoil_m_s[1]
  
  ##//Add dataframe to output dataframe
  DOut <- rbind(DOut, KData)
  ##//Repeat
  Fdat <- rbind(Fdat, DOut)
  
}


##//Pulling out soil information, sample depth, and replication  
mylist <- str_split(Fdat$File, "_")

#mymaterials <- c()
myreps <- c()
mydepths <- c()
mylocates <- c()
mysites <- c()

#myDepths <- c() ##//Commented out during lab testing phase
i=2
##//Loop through all files
for(i in 1:length(mylist)){
  site <- mylist[[i]][3]
  # material <- mylist[[i]][4]
  locate <- mylist[[i]][4]
  depth <- mylist[[i]][5]
  rep <- mylist[[i]][6]
  
  mysites <- rbind(mysites, site)
  mylocates <- rbind(mylocates, locate)
  mydepths <- rbind(mydepths, depth)
  myreps <- rbind(myreps, rep)



  
}

##//Combining vectors
Fdat <- cbind(Fdat, mysites, mylocates, mydepths, myreps)

#######################################################################################
#######################################################################################
##//Simple plots for viewing raw obsevations
##//Filter
myMeas <- unique(Fdat$File)
Fdat$mylocates <- as.factor(Fdat$mylocates)

Fdat$Runtime_s            
Fdat$PressureHead_cm        
Fdat$Fitting_cm
Fdat$Temp_C
Fdat$Date_ct
Fdat$File


Fdat$H_end_abs_cm <- as.numeric(Fdat$H_end_abs_cm)
Fdat$H_end_rel <- as.numeric(Fdat$H_end_rel)
Fdat$dH_min_cm <- as.numeric(Fdat$dH_min_cm)
Fdat$dH_ini_cm <- as.numeric(Fdat$dH_ini_cm)
Fdat$A_bur_cm_2 <- as.numeric(Fdat$A_bur_cm_2)
Fdat$A_cap_in_cm_2 <- as.numeric(Fdat$A_cap_in_cm_2)
Fdat$A_sample_cm_2 <- as.numeric(Fdat$A_sample_cm_2)
Fdat$L_bur_cm <- as.numeric(Fdat$L_bur_cm)
Fdat$L_sample_cm <- as.numeric(Fdat$L_sample_cm)
Fdat$L_plate_Bottom_cm <- as.numeric(Fdat$L_plate_Bottom_cm)
Fdat$L_plate_Top_cm <- as.numeric(Fdat$L_plate_Top_cm)
Fdat$T_ref_C <- as.numeric(Fdat$T_ref_C)
Fdat$K_plate_cm_d <- as.numeric(Fdat$K_plate_cm_d)
Fdat$Max_Auto_Offset_cm <- as.numeric(Fdat$Max_Auto_Offset_cm)
Fdat$TriggerAuto_Offset_cm <- as.numeric(Fdat$TriggerAuto_Offset_cm)
Fdat$FittingParameter_a_cm <- as.numeric(Fdat$FittingParameter_a_cm)
Fdat$FittingParameter_b_s <- as.numeric(Fdat$FittingParameter_b_s)
Fdat$FittingParameter_c_cm <- as.numeric(Fdat$FittingParameter_c_cm)
Fdat$FittingParameter_r2 <- as.numeric(Fdat$FittingParameter_r2)
Fdat$KsTotal_cm_d <- as.numeric(Fdat$KsTotal_cm_d)
Fdat$KsSoil_cm_d <- as.numeric(Fdat$KsSoil_cm_d)
Fdat$KsSoil_m_s <- as.numeric(Fdat$KsSoil_m_s)
Fdat$Norm_KsSoil_cm_d <- as.numeric(Fdat$Norm_KsSoil_cm_d)
Fdat$Norm_KsSoil_m_s <- as.numeric(Fdat$Norm_KsSoil_m_s)
Fdat$mylocates
Fdat$myreps
Fdat$Runtime_h <- Fdat$Runtime_s / (60*60) 


##//Take in a look
cc <- Fdat$Fitting_cm>0 & Fdat$FittingParameter_r2>=0.999 
plot(Fdat$PressureHead_cm[cc] ~ Fdat$Runtime_s[cc], col = as.factor(Fdat$myreps), pch  = 15, cex = 1.5)


##//Look at it in ggplot
MMSF_Ksat <- Fdat
ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Fdat[Fdat$Fitting_cm>0 & Fdat$FittingParameter_r2>=0.999,], 
             aes(x = Runtime_s, y = PressureHead_cm, color = Temp_C, shape = mydepths),
             show.legend = TRUE,  size=4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  #geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=10,], aes(x = LinFlux , ymin = Corr_LinFlux_HCI,
  #                                                             ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
  #            size = 1, width = .15)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  #xlab(expression(paste("LiCOR Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Pressure Head  [", "cm", "] "  ))) +
  #xlim(0,5)+
  #ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))




###//Summarize the that data
Fdat_sum1 <- Fdat[Fdat$mylocates!="downslope" & Fdat$Fitting_cm>0 & Fdat$FittingParameter_r2>=0.999,] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(mylocates) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit

Fdat_sd <- Fdat[Fdat$mylocates!="downslope" & Fdat$Fitting_cm>0 & Fdat$FittingParameter_r2>=0.999,]  %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(mylocates) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, sd, na.rm = TRUE) %>%  
  na.omit


Griffy_DF <- data.frame(Material = c("alexsdownslope", "alexsupslope", "griffycontrol", "griffytde"),
                        a_param = Fdat_sum1$FittingParameter_a_cm,
                        a_param_LCI = c(Fdat_sum1$FittingParameter_a_cm[1] - 
                                          Fdat_sd$FittingParameter_a_cm[1],
                                        Fdat_sum1$FittingParameter_a_cm[2] - 
                                          Fdat_sd$FittingParameter_a_cm[2],
                                        Fdat_sum1$FittingParameter_a_cm[3] - 
                                          Fdat_sd$FittingParameter_a_cm[3],
                                        Fdat_sum1$FittingParameter_a_cm[4] - 
                                          Fdat_sd$FittingParameter_a_cm[4]),
                        a_param_HCI = c(Fdat_sum1$FittingParameter_a_cm[1] + 
                                          Fdat_sd$FittingParameter_a_cm[1],
                                        Fdat_sum1$FittingParameter_a_cm[2] + 
                                          Fdat_sd$FittingParameter_a_cm[2],
                                        Fdat_sum1$FittingParameter_a_cm[3] + 
                                          Fdat_sd$FittingParameter_a_cm[3],
                                        Fdat_sum1$FittingParameter_a_cm[4] + 
                                          Fdat_sd$FittingParameter_a_cm[4]),
                        b_param = Fdat_sum1$FittingParameter_b_s,
                        b_param_LCI = c(Fdat_sum1$FittingParameter_b_s[1] - 
                                          Fdat_sd$FittingParameter_b_s[1],
                                        Fdat_sum1$FittingParameter_b_s[2] - 
                                          Fdat_sd$FittingParameter_b_s[2],
                                        Fdat_sum1$FittingParameter_b_s[3] - 
                                          Fdat_sd$FittingParameter_b_s[3],
                                        Fdat_sum1$FittingParameter_b_s[4] - 
                                          Fdat_sd$FittingParameter_b_s[4]),
                        b_param_HCI = c(Fdat_sum1$FittingParameter_b_s[1] + 
                                          Fdat_sd$FittingParameter_b_s[1],
                                        Fdat_sum1$FittingParameter_b_s[2] + 
                                          Fdat_sd$FittingParameter_b_s[2],
                                        Fdat_sum1$FittingParameter_b_s[3] + 
                                          Fdat_sd$FittingParameter_b_s[3],
                                        Fdat_sum1$FittingParameter_b_s[4] + 
                                          Fdat_sd$FittingParameter_b_s[4]))

myplots <- unique(Griffy_DF$Material)

MyTimes <- seq(0,500, by = 1)
Griffydouts <- c() 
i = 1
for(i in 1:length(myplots)){
  Pressure <- Griffy_DF$a_param[Griffy_DF$Material==myplots[i]] * 
    exp(Griffy_DF$b_param[Griffy_DF$Material==myplots[i]] * 
          MyTimes)
  LPressure <- Griffy_DF$a_param_LCI[Griffy_DF$Material==myplots[i]] * 
    exp(Griffy_DF$b_param_LCI[Griffy_DF$Material==myplots[i]] * 
          MyTimes)
  HPressure <- Griffy_DF$a_param_HCI[Griffy_DF$Material==myplots[i]] * 
    exp(-abs(Griffy_DF$b_param_HCI[Griffy_DF$Material==myplots[i]]) *
          MyTimes)
  
  Griffydouts <- rbind(Griffydouts, data.frame(Material = myplots[i],
                                               Time = MyTimes, 
                                               Mean_Flow = Pressure,
                                               Flow_LCI = LPressure,
                                               Flow_HCI = HPressure))
  
}
plot(Griffydouts$Time[Griffydouts$Material!="alexsupslope"], 
     Griffydouts$Mean_Flow[Griffydouts$Material!="alexsupslope"], 
     ylim = c(0,7))

lines(Griffydouts$Time[Griffydouts$Material!="alexsupslope"],
      Griffydouts$Flow_LCI[Griffydouts$Material!="alexsupslope"], lwd = 2)
lines(Griffydouts$Time[Griffydouts$Material!="alexsupslope"],
      Griffydouts$Flow_HCI[Griffydouts$Material!="alexsupslope"],
      lwd = 2, col = "blue")


