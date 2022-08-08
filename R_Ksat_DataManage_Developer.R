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
setwd("D:/Dropbox/Projects/Indiana/SoilLab/Data/KSat/RawData/GreenhouseSoils_20220905")
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
mylocates <- c()
myruns <- c()

#myDepths <- c() ##//Commented out during lab testing phase

##//Loop through all files
for(i in 1:length(mylist)){
  locate <- mylist[[i]][3]
 # material <- mylist[[i]][4]
  rep <- mylist[[i]][4]
  run <- mylist[[i]][5]
  
  #depth <- mylist[[i]][XXXXX]
  
  mylocates <- rbind(mylocates, locate)
  myruns <- rbind(myruns, run)
  myreps <- rbind(myreps, rep)
  #myDepths <- rbind(myDepths, depth)
  
}

##//Combining vectors
Fdat <- cbind(Fdat, mylocates, myreps, myextras)

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
cc <- Fdat$FittingParameter_r2>=0.999 & Fdat$Runtime_h>0
plot(Fdat$PressureHead_cm[cc] ~ Fdat$Runtime_s[cc], col = as.factor(Fdat$myreps), pch  = 15, cex = 1.5)
plot(Fdat$PressureHead_cm[cc] ~ Fdat$Runtime_s[cc], col = as.factor(Fdat$myreps), pch  = 15, cex = 1.5)

plot(Fdat$PressureHead_cm[cc] ~ Fdat$Runtime_h[cc])

names(Fdat)

Fdat$ks <- (Fdat$A_bur_cm_2 / Fdat$A_sample_cm_2)  * Fdat$L_sample_cm * Fdat$FittingParameter_b_s
hist(ks)
hist(Fdat$FittingParameter_b_s)

V = Fdat$FittingParameter_a_cm
Tmeas = Fdat$Runtime_h
dP = Fdat$PressureHead_cm
L = Fdat$L_sample_cm
A = Fdat$A_sample_cm_2

plot(Fdat$KsSoil_m_s, Fdat$ks)

hist(Fdat$FittingParameter_a_cm[cc])

outs <- calcKS(V, Tmeas, L, A, dP)
plot(outs)

##//Look at it in ggplot
unique(Fdat$File[Fdat$mylocates=="griffy"])
unique(Fdat$File[Fdat$mylocates=="downslope"])

unique(Fdat$File[Fdat$mylocates!="downslope"])
Fdat$File[Fdat$mylocates=="griffy"]

Fdat$mylocates[Fdat$mylocates=="griffy"] <- "griffycontrol"


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Fdat[Fdat$mylocates!="downslope",], aes(x = Runtime_s, y = PressureHead_cm, color = Temp_C, shape = myreps),
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



Fdat_clean <- Fdat[Fdat$mymaterials!="clay",]


plot(Fdat_clean$mymaterials, Fdat_clean$KsSoil_cm_d)


range(Pressure1[Pressure1<Inf])
#####################################################
ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Fdat_sum1, aes(x = Runtime_s, y = PressureHead_cm, shape = mymaterials, color = myreps),
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
Fdat_sum2 <- Fdat_sum1 %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(mymaterials, Runtime_s) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit

#####################################################
ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Fdat_sum2[Fdat_sum2$mymaterials=="org",], aes(x = Runtime_s, y = PressureHead_cm, shape = mymaterials),
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

library(SoilHyP)


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Fdat_sum2, aes(x = Runtime_s, y = PressureHead_cm, shape = mymaterials),
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


Fdat

names(Fdat)





calcKS(V, Tmeas, L, A, dP)

#####################################################################################################
###??Enter at your own risk



##//LiCOR vs Corrected Lin Flux
LinFluxCompSym <- ggplot() +
  geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=10,], aes(x = LinFlux, y = Corr_LinFlux, color = Symbiont), show.legend = TRUE,  size=4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=10,], aes(x = LinFlux , ymin = Corr_LinFlux_HCI,
                                                                 ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab(expression(paste("LiCOR Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #  xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxCompSym

Ts_Time <- ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
               aes(x = Date, y = SoilT_C, fill = Symbiont), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Date') +
  ylab(expression(paste("Soil Temperature [C]" ))) +
  annotate("text", x = Rs_plots$Date[1], y = 26, label = "a)    ", size = 20) +
  theme(legend.position="NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 45))
Ts_Time

Rs_plots$Site_Pairs <-as.character(paste(Rs_plots$Site, Rs_plots$Pair, sep = "_"))
str(Rs_plots)



Rs_plots_sum <- Rs_plots[Rs_plots$Corr_LinFlux<=6,] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(Site_Pairs, Symbiont) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit



colors = c("#4D54E8", "#ECC01D", "#A2A197")
names(Rs_plots_sum)


Rs_plots_sum <- Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(Date, Hole_Collar, Symbiont) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, median, na.rm = TRUE) %>%  
  na.omit


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilT_C, y = Corr_LinFlux, color = as.factor(Hole_Collar)), 
             show.legend = TRUE,  size = 4) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



names(Rs_plots_sum)

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilT_C, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = SoilT_C , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=10,], aes(x = SoilT_C, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=10,], aes(x = SoilT_C , ymin = Corr_LinFlux_HCI,
                                                                 ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .1)+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  # scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="KentFarm"], na.rm = TRUE)
mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="MorganMonroe"], na.rm = TRUE)
mean(Rs_plots$SoilT_C[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site=="GriffyWoods"], na.rm = TRUE)


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = SoilVWC_pct, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = SoilVWC_pct , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .75, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil VWC [%] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots_sum, aes(x = n_holes, y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots_sum, aes(x = n_holes , ymin = Corr_LinFlux_HCI,
                                         ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Soil Temp. [C] ') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))





ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = as.factor(n_holes), y = Corr_LinFlux, color = Symbiont), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x =as.factor(n_holes), ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Symbiont), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Symbiont), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM"
                               & Rs_plots$Site!="KentFarm",],
               aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Site), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM"
                               & Rs_plots$Site!="KentFarm",], 
               aes(x = as.factor(n_holes), y = Corr_LinFlux, fill = Site), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
               aes(x = Site, y = Corr_LinFlux, fill = Symbiont), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Site') +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



Ts_plot <- ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
               aes(x = as.factor(n_holes), y = SoilT_C, fill = as.factor(n_holes)), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Soil Temperature [C]" ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

Ts_plot


VWC_plot <- ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
               aes(x = as.factor(n_holes), y = SoilVWC_pct, fill = as.factor(n_holes)), color = "black", 
               show.legend = TRUE,  size = 2, alpha = 0.4, position = position_dodge(0.5)) +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  xlab('Number of Holes') +
  ylab(expression(paste("Soil Moisture [% VWC]" ))) +
  theme(legend.position="top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

VWC_plot



LinFluxSite <- ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Site, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Plot_m2 , ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
  ylab('Corrected Efflux') +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxSite



LinFluxSymbiont <- ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Collar, y = Corr_LinFlux, color = Site), 
             show.legend = TRUE,  size = 4, position = position_dodge(0.5)) +
  
  #geom_point(data = stp3[cc1,], aes(x=delta_TLeaf , y= QY_FP.x, color=Spp), show.legend = FALSE,  size=2) +
  geom_errorbar(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Hole_Collar , ymin = Corr_LinFlux_HCI,
                                                                ymax = Corr_LinFlux_LCI, color = Site), show.legend = FALSE,
                size = 1, width = .15, position = position_dodge(0.5))+
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('Holes m2') +
  # ylab('Corrected Efflux') +
  
  #xlab(expression(paste("Predawn  ", psi ["L"], "  [MPa]"))) +
  ylab(expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(0,5)+
  # ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

LinFluxSymbiont




#########################################################################################
###########################################################################################
############################################################################################################
###################################################################################################
##//Efflux and Drivers 
Rs_plots$Hole_Collar <- as.factor(Rs_plots$Hole_Collar)
ggplot() +
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
             aes(x = SoilT_C, y = Corr_LinFlux, color = Hole_Collar, shape = Site), 
             alpha = 1,  size = 5, show.legend = TRUE) +
  xlab('Median Soil Temperature [C]') + 
  ylab(expression(paste("Median CO" ["2 "],  "Efflux   [ ",mu, "mol " , "  CO" ["2"], " m"^"-2 ", "s"^"-1", "]"))) +
  scale_shape_manual(values = c(19,17,18,15)) +
  scale_color_manual(values = GoAvsGo) +
  theme(legend.position= "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Treatment",size=30),
        legend.text=element_text(size=30))

ggplot() +
  geom_point(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
             aes(x = SoilVWC_pct, y = Corr_LinFlux, color = Hole_Collar, shape = Site), 
             alpha = 1,  size = 5, show.legend = TRUE) +
  xlab('Median Soil Moisture [% VWC]') + 
  ylab(expression(paste("Median CO" ["2 "],  "Efflux   [ ",mu, "mol " , "  CO" ["2"], " m"^"-2 ", "s"^"-1", "]"))) +
  
  scale_shape_manual(values = c(19,17,18,15)) +
  scale_color_manual(values = GoAvsGo) +
  theme(legend.position= "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Treatment",size=30),
        legend.text=element_text(size=30))


###################################################################################
###################################################################################

names(Rs_plots)
formula <- bf(
  Corr_LinFlux ~ asym * exp(scale * SoilT_C),
  scale ~ 1, #(1|Site)
  asym ~ 1,
  sigma ~ 1,
  nl = TRUE)
# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 2.5), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.1, 1.5), lb = 0, nlpar = "scale"))


fit_0 <- brm(
  formula,
  data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm"
                  & Rs_plots$Hole_Collar=="0",],
  prior = prior1,
  family =  "normal",
  iter = 10000,
  chains = 4,
  # cores = 4,
  control = list(adapt_delta = 0.90, max_treedepth = 15))
summary(fit_0, prob = 0.94)
bayes_R2(fit_0, prob = c(0.03, 0.97))

plot_model(fit_0, type = "pred", terms = c("SoilT_C")) + 
  theme_bw()

fit_0_ex <- prepare_predictions(fit_0)
plot(density(fit_0_ex$nlpars$scale$fe$b))
survey_0_q10 <- exp(10 * fit_0_ex$nlpars$scale$fe$b)
plot(density(survey_0_q10))
median(survey_0_q10)


fit_1 <- brm(
  formula,
  data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm"
                  & Rs_plots$Hole_Collar=="1",],
  prior = prior1,
  family =  "normal",
  iter = 10000,
  chains = 4,
  # cores = 4,
  control = list(adapt_delta = 0.90, max_treedepth = 15))
summary(fit_1, prob = 0.94)
bayes_R2(fit_1, prob = c(0.03, 0.97))

plot_model(fit_1, type = "pred", terms = c("SoilT_C")) + 
  theme_bw()

fit_1_ex <- prepare_predictions(fit_1)
plot(density(fit_1_ex$nlpars$scale$fe$b))
survey_1_q10 <- exp(10 * fit_1_ex$nlpars$scale$fe$b)
plot(density(survey_1_q10))
median(survey_1_q10)

###################################################################################
###################################################################################

names(Rs_plots)
formula <- bf(
  Corr_LinFlux ~ asym * exp(scale * SoilT_C),
  scale ~ Hole_Collar + (1|Site),
  asym ~ Hole_Collar + (1|Site),
  sigma ~ 1,
  nl = TRUE)
# quick and dirty priors. you need priors to even fit a model
##//Prior must be positive
prior1 <- c(
  prior(normal(0.2, 2.5), nlpar = "asym"),
  # the population average slope is constrained to be positive
  prior(normal(0.1, 1.5), lb = 0, nlpar = "scale"))


fit_all <- brm(
  formula,
  data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Site!="KentFarm",],
  prior = prior1,
  family =  "normal",
  iter = 20000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.90, max_treedepth = 15))
summary(fit_all, prob = 0.94)
bayes_R2(fit_all, prob = c(0.03, 0.97))

# plot_model(fit_all, type = "pred", terms = c("SoilT_C")) + 
#   theme_bw()

fit_all_ex <- prepare_predictions(fit_all)
plot(density(fit_all_ex$nlpars$scale$fe$b))
survey_all_q10 <- exp(10 * fit_all_ex$nlpars$scale$fe$b)
plot(density(survey_all_q10))
median(survey_all_q10)

###########################################################################################
############################################################################################
############################################################################################


##//Getting me some packages
library(aspace)
library(bigleaf)
library(tidyverse)
library(rstan)
library(brms)
library(modelr)
library(sjPlot)
library(sjstats)
library(RColorBrewer)
library(tidybayes)
theme_set(theme_minimal())

###// Get me some data 
##// Five Min. met data from greenhouse summarized to daily 

hist(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=8])
##//Bayesian approan
##// an uniformed prior for simple linear regression
my.prior <- set_prior("uniform(-30,30)", class = "b")
my.prior <- set_prior("normal(0.45,0.08)", class = "b")

Rs_plots$Cicada <- as.factor(Rs_plots$Hole_Collar)

names(Rs_plots)

##//Modeling resistance of canopy
M1 <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont + Site,
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M1, prob = 0.94)
bayes_R2(M1, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2, 
           vline.color = 15, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()




plot_model(M1, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()



plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()


plot_model(M1, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##//Modeling resistance of canopy \
##// NOTE THE INTERACTION WAS TESTED AND NOT AN IMPORTANT COVARIATE!!

M1_r <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M1_r, prob = 0.94)

bayes_R2(M1_r, prob = c(0.03, 0.97))


plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M1_r, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()


##//Modeling resistance of canopy
M2_r <- brm(Corr_LinFlux ~ Cicada + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M2_r, prob = 0.94)

bayes_R2(M2_r, prob = c(0.03, 0.97))

plot_model(M2_r, type = "pred", terms = c("Cicada", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2, ) + 
  theme_bw()

library("ggplot2")
library("ggsci")

plot_model(M2_r, type = "pred", terms = c("Cicada", "Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE,value.size = 2,  dot.size = 2,   line.size = 2,
           vline.color = 2, jitter = 0.5,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL,) + 
  theme_bw()


plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.8,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()




plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2_r, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

get_model_data(
  M2_r, type = "pred", terms = c("Site", "Symbiont", "Cicada"), 
  ci.lvl = 0.89,)


Rs_data$n_holes

hist(Rs_data$Corr_LinFlux[Rs_data$Corr_LinFlux<=6])

##//Modeling resistance of canopy
M3_r <- brm(Corr_LinFlux ~ n_holes + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M3_r, prob = 0.89)

bayes_R2(M3_r, prob = c(0.055, 0.945))

plot_model(M3_r, type = "pred", terms = c("n_holes", "Site"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

library("ggplot2")
library("ggsci")

plot_model(M3_r, type = "pred", terms = c("n_holes", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE,value.size = 2,  dot.size = 2,   line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "Symbiont",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL,) + 
  theme_bw()


plot_model(M2_r, type = "pred", terms = c("Cicada", "Symbiont", "Site"), show.p = TRUE,
           show.data = TRUE, value.size = 25,  dot.size = 2,   line.size = 2,
           vline.color = 15, jitter = 0.8,   title = NULL, colors = c("#4D54E8", "#ECC01D", "#A2A197"),
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           bpe = "mean", bpe.style = "dot", ci.lvl = 0.89, value.offset = .3,
           wrap.title = 1000,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()


#####################################################################################################
#################################################################
plot(Rs_plots$n_holes, Rs_plots$SoilT_C)

median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM"])

median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM"])


AM_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==0])
ECM_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==0])

AM_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==1])
ECM_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==1])

AM_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM" & Rs_plots$n_holes==2])
ECM_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM" & Rs_plots$n_holes==2])

All_0 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==0])

All_1 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==1])

All_2 <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==2])



AM <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM"])
ECM <- median(Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM"])


AM_Pdiff_1 <- abs(AM_0 - AM_1) / ((AM_0 + AM_1) / 2) * 100
AM_Pdiff_2 <- abs(AM_1 - AM_2) / ((AM_1 + AM_2) / 2) * 100

EcM_Pdiff_1 <- abs(ECM_0 - ECM_1) / ((ECM_0 + ECM_1) / 2) * 100
EcM_Pdiff_2 <- abs(ECM_1 - ECM_2) / ((ECM_1 + ECM_2) / 2) * 100

abs(All_0 - All_1) / ((All_0 + All_1) / 2) * 100
abs(All_1 - All_2) / ((All_1 + All_2) / 2) * 100

abs(AM - ECM) / ((AM + ECM) / 2) * 100

###//precent gains based on the scaled fluxes 1 hole ~ 123 holes per m2
##                                            2 holes ~ 246 holes per m2

##AM 

AM_1_scaler <- AM_Pdiff_1 / 123
AM_2_scaler <- AM_Pdiff_2 / 246

EcM_1_scaler <- EcM_Pdiff_1 / 123
EcM_2_scaler <- EcM_Pdiff_2 / 246

##//Average of the 1 and 2 hole estimates
##//Represents the CO2 increase per cicada hole
AM_scaler <- (AM_1_scaler + AM_2_scaler) / 2 
EcM_scaler <- (EcM_1_scaler + EcM_2_scaler) / 2


plot(Rs_plots$Symbiont[Rs_plots$Corr_LinFlux<=6], Rs_plots$Corr_LinFlux[Rs_plots$Corr_LinFlux<=6])

names(Rs_plots)




result <- data.frame(
  Rs_plots$day,
  cut_Date = cut(as.Date(Rs_plots$day), "week"),
  cut_POSIXt = cut(as.POSIXct(Rs_plots$day), "week"),
  stringsAsFactors = FALSE)

Rs_plots$Week <- result$cut_POSIXt

ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==0,], 
               aes(x = as.factor(Week), y = Corr_LinFlux, fill = Symbiont), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('Dates') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$n_holes==1,], 
               aes(x = as.factor(Week), y = Corr_LinFlux, fill = Symbiont), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('Dates') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##################

Rs_plots$N_holes <- as.factor(Rs_plots$n_holes)
ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="AM",], 
               aes(x = as.factor(Week), y = Corr_LinFlux, fill = N_holes), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('Dates') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


############################################
ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6 & Rs_plots$Symbiont=="ECM",], 
               aes(x = as.factor(Week), y = Corr_LinFlux, fill = N_holes), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('Dates') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###########################################

############################################
ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], 
               aes(x = Site, y = Corr_LinFlux, fill = Symbiont), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 40, vjust = 0.7, hjust=0.5))


###########################################
Rs_plots$N_holes
############################################
ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], 
               aes(x = N_holes, y = Corr_LinFlux, fill = Symbiont), 
               show.legend = TRUE,  size = 1, alpha = 0.5, position = position_dodge(0.5)) +
  ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlab('Number of Holes') +
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  scale_fill_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 0, vjust = 0.7, hjust=0.5))


###########################################


ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = Symbiont, y = Corr_LinFlux, fill = as.factor(Week)), 
               show.legend = TRUE,  size = 1) +
  ylab('Holes m2') +
  xlab('Dates') +
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




ggplot() +
  geom_boxplot(data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], aes(x = as.factor(day), y = Corr_LinFlux, fill = Symbiont), 
               show.legend = TRUE,  size = 1) +
  ylab('Holes m2') +
  xlab('Dates') +
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


Rs_plots_sum <- Rs_plots[Rs_plots$Corr_LinFlux<=6,] %>%
  #dplyr::mutate(day = as.Date(Date_ct, format="%d-%m-%Y")) %>%
  dplyr::group_by(Week, Symbiont) %>% # group by the day column ##//mean looks good
  dplyr::summarise_if(is.numeric, mean, na.rm = TRUE) %>%  
  na.omit



ggplot() +
  geom_boxplot(data = Rs_plots_sum, aes(x = as.factor(Week), y = n_holes), 
               show.legend = TRUE,  size = 1) +
  ylab('Holes m2') +
  xlab('Dates') +
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot() +
  geom_boxplot(data = Rs_plots_sum, aes(x = as.factor(Week), y = Corr_LinFlux), 
               show.legend = TRUE,  size = 1) +
  ylab('Holes m2') +
  xlab('Dates') +
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

LinFluxSymbiont






##//Modeling resistance of canopy
M4_r <- brm(Corr_LinFlux ~ n_holes + SoilT_C + Symbiont + (1|Site),
            data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
            family =  "gamma",
            iter = 30000, save_all_pars = TRUE, seed = 1234,
            chains = 4,
            # cores = 4,
            control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M4_r, prob = 0.89)

bayes_R2(M4_r, prob = c(0.055, 0.945))

plot_model(M4_r, type = "pred", terms = c("n_holes", "Site"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.1,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()

plot_model(M4_r, type = "pred", terms = c("Site", "n_holes", "SoilT_C"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.25,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "My Sites",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()



plot_model(M4_r, type = "pred", terms = c( "SoilT_C", "n_holes", "Symbiont"), 
           show.data = TRUE, dot.size = 2, show.p = TRUE,
           line.size = 2,
           vline.color = 2, jitter = 0.25,   title = NULL, 
           axis.title = expression(paste("Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  )),
           axis.labels = c("x", "C"),
           legend.title = "# Holes",
           wrap.title = 100,
           wrap.labels = 50,
           axis.lim = NULL) + 
  theme_bw()










##//Modeling resistance of canopy
M2 <- brm(Corr_LinFlux ~ Hole_Collar * Symbiont + Site,
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M2, prob = 0.94)
bayes_R2(M2, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M2, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M2, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##//Modeling resistance of canopy
M3 <- brm(Corr_LinFlux ~ Hole_Collar + Symbiont * Site,
          data = Rs_plots[Rs_plots$Corr_LinFlux<=6,], prior = my.prior,
          family =  "gamma",
          iter = 30000, save_all_pars = TRUE, seed = 1234,
          chains = 4,
          # cores = 4,
          control = list(adapt_delta = 0.99, max_treedepth = 15))


summary(M3, prob = 0.94)
bayes_R2(M3, prob = c(0.03, 0.97))
#plot(M1)

plot_model(M3, type = "pred", terms = c("Hole_Collar", "Site"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M3, type = "pred", terms = c("Hole_Collar", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

plot_model(M3, type = "pred", terms = c("Site", "Symbiont"), show.p = TRUE,
           show.data = TRUE, dot.size = 2) + 
  theme_bw()

##///Do the model selection WAIC
M1 <- add_criterion(M1, "waic")
M1_r <- add_criterion(M1_r, "waic")
M2_r <- add_criterion(M2_r, "waic")

M2 <- add_criterion(M2, "waic")
M3 <- add_criterion(M3, "waic")

waic(M1); waic(M2); waic(M3); waic(M1_r); waic(M2_r)
M1_loo <- loo(M1, reloo = T); M2_loo <- loo(M2, reloo = T); M3_loo <- loo(M3, reloo = T);

M1_r_loo <- loo(M1_r, reloo = T); M2_r_loo <- loo(M2_r, reloo = T);

# compare both models
loo_compare(M1, M1_r, M2_r, M2, M3, criterion = "waic")
loo_compare(M1_loo, M1_r_loo, M2_r_loo, M2_loo, M3_loo)

# 
# > loo_compare(M1_loo, M1_r_loo, M2_r_loo, M2_loo, M3_loo)
# elpd_diff se_diff
# M1_r   0.0       0.0  
# M2_r   0.0       0.0  
# M1    -5.1       4.4  
# M2    -8.5       4.6  
# M3   -14.8       5.6
