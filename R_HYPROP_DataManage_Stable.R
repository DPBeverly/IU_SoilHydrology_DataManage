##//Data managmenet for Hyprop and WP4C 
##//This is the Development version for lab measurements

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

if (packageVersion("devtools") < 1.6) {
  
  if (!require(devtools)) {
    install.packages("devtools")
  }
  install.packages("devtools")
}

if(!require(nlmrt)){
  install.packages("nlmrt")
}
if(!require(httr)){
  install.packages("httr")
}
if(!require(vadose)){
  devtools::install_github("gowusu/vadose")
}

library(vadose)

##//ggplot theme 
theme_set(theme_minimal())

##//custom color ramp 
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")

##//Where is the data
setwd("D:/Dropbox/Projects/Indiana/SoilLab/Data/HyProp/HypropExports")
dir()

##//List of files in the directory
Flist <- dir(pattern = ".xlsx")


k = 1
##/ 1 Information
##/ 2 Measurements
##/ 3 Spline Points
##/ 4 Evaluation-WP4 manual
##/ 5 Evaluation-Retention Θ(pF)
##/ 6 Evaluation-Conductivity K(pF)
##/ 7 Evaluation-Conductivity K(Θ)
##/ 8 Fitting-Retention Θ(pF)
##/ 9 Fitting-Conductivity K(pF)
##/ 10 Fitting-Conductivity K(Θ)
##/ 11 Fitting-Parameter value
##/ 12 FittingCorrelationMatrix
##/ 13 Fitting-Statistical analysis
##/ 14 Base Points-Retention Θ(pF)

##//Meta Data
text <- read_xlsx(Flist[k], sheet = 1)
##//Just WP4C data
wp4 <- read_xlsx(Flist[k], sheet = 4)
##//All Raw data
dats <- read_xlsx(Flist[k], sheet = 5)
##//Fitted Vanaguchen curves
fits <- read_xlsx(Flist[k], sheet = 8)

##//Building the raw data dataframes
d_fit_out <- c()
d_raw_out <- c()
for(k in 1:length(Flist)){
  
text <- read_xlsx(Flist[k], sheet = 1)
wp4 <- read_xlsx(Flist[k], sheet = 4)

dats <- read_xlsx(Flist[k], sheet = 5)
fits <- read_xlsx(Flist[k], sheet = 8)


##//Isolating the meta data
Sample = text$Value[2]
MeasureStart = text$Value[3]
MeasureEnd = text$Value[4]
MeasureTime = text$Value[5]
Units = text$Value[6]
Software = text$Value[7]
GeomParameters = text$Value[8]
SoilSurfArea_cm2 = text$Value[9]
SoilColumnHeight_cm = text$Value[10]
Vol_cm3 = text$Value[11]
LowerTensiometer_cm = text$Value[12]
UpperTensiometer_cm = text$Value[13]
HYPROP_Parameters = text$Value[14]                                      
MeasurementHeadNetWeight_g = text$Value[15]                       
EmptySoilSamplingRingWeight_g = text$Value[16]                  
AirEntryPressureUpperTensiometer_bar = text$Value[17]         
AirEntryPressureLowerTensiometer_bar = text$Value[18]              
DensitySolidSubstance_g_cm3 = text$Value[19]                     
StatisticalAnalysis = text$Value[20]                                
Tensiometer_hPa = text$Value[21]                                      
Scale_g = text$Value[22]                                     
SensorUnitInformation = text$Value[23]                                 
Busnumber = text$Value[24]                                   
Subaddress = text$Value[25]                                             
SerialNumber = text$Value[26]                                           
SensorUnitName = text$Value[27]                                    
FirmwareVersion = text$Value[28]                                    
ScaleSerialNumber = text$Value[30]                                      
ScaleName = text$Value[31]                                            
Correction = text$Value[32]                                            
VolumeCorrection_ml = text$Value[33]                               
WeightCorrection_g = text$Value[34]                                   
StartStopLine = text$Value[35]                                        
Start = text$Value[36]                                          
TensionTopAirEntryPoint_Stopp = text$Value[37]                         
TensionBottomAirEntryPoint = text$Value[38]                              
CalculationWaterContents = text$Value[39]                             
CalculationWaterContents2 = text$Value[40]                             
InitialWaterContent_Vol_pct = text$Value[41]                               
DrySoilWeight_g = text$Value[42]                              
AdditionalSoilParamters = text$Value[43]                              
Density_g_cm3 = text$Value[44]                                
Porosity = text$Value[45]                                   
FittingParameters = text$Value[46]                                   
INTERPOL = text$Value[47]                                       
Tensiometer_Mean = text$Value[48]                                       
Polynomial_degree = text$Value[49]                                      
wtheta = text$Value[50]                                          
wk = text$Value[51]                                         
Fitting = text$Value[52]                                        
ModelDescription = text$Value[53]                                     
Notes = text$Value[54]                                    
GeneralData = text$Value[55]                                   
SiteCoordinates = text$Value[56]                                 
SamplingDate = text$Value[57]                               
SiteDescription = text$Value[58]                                  
SoilType = text$Value[59]                              
LandUse = text$Value[60]                               
SoilPhysicalData = text$Value[61]                           
Texture = text$Value[62]                        
SoilTextureClass = text$Value[63]                        
SamplingDepth = text$Value[64]                        
OrganicCarbonContent = text$Value[65]                   
SamplePrep = text$Value[66]                     
InitialSaturationTime = text$Value[67]                   
SwellingHandling = text$Value[68]  



dats <- read_xlsx(Flist[k], sheet = 5)


fits <- read_xlsx(Flist[k], sheet = 8)



d_raw <- data.frame(Sample = Sample, 
                    MPa = dats$`[MPa]`,
                    Vol_Water = dats$`Water Content [Vol%]`)

d_fit <- data.frame(Sample = Sample, 
                    MPa = fits$`[MPa]`,
                    Vol_Water = fits$`Water Content [Vol%]`)


d_fit_out <- rbind(d_fit_out, d_fit)
d_raw_out <- rbind(d_raw_out, d_raw)

}

plot(d_fit_out$MPa, d_fit_out$Vol_Water, xlim = c(-0.1,30))
plot(d_raw_out$MPa, d_raw_out$Vol_Water, xlim = c(-0.2,60))


d_raw_out$SampleN <- as.numeric(as.factor(d_raw_out$Sample))
d_fit_out$SampleN <- as.numeric(as.factor(d_fit_out$Sample))

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = d_fit_out[d_fit_out$Vol_Water>0 & 
                                d_fit_out$SampleN<6,], aes(x = MPa, y = Vol_Water, color = Sample), 
             show.legend = TRUE,  size = 4) + 
  #scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('MPa') +
  
  # ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(-0.2, 2.5)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & 
                                d_raw_out$SampleN<6,], aes(x = abs(MPa), y = Vol_Water, color = Sample), 
             show.legend = TRUE,  size = 4) + 
  #scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  xlab('MPa') +
  
  # ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  xlim(-0.2, 2.5)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



d_fit_out



d_fit_sum = d_fit_out %>%
  dplyr::group_by(MPa) %>%
  dplyr::summarise(
    VWC_mn = mean(Vol_Water, na.rm = TRUE),
    VWC_md = median(Vol_Water, na.rm = TRUE),
    VWC_sd = sd(Vol_Water, na.rm = TRUE)) %>%
  as.data.frame()

ggplot() +
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & 
                                d_raw_out$SampleN<6,],
             aes(x = abs(MPa), y = Vol_Water), 
             show.legend = TRUE,  size = 4, alpha = 0.5, color = "black") + 
  geom_ribbon(data = d_fit_sum,
              aes(x = abs(MPa), ymin = VWC_mn - VWC_sd/2, ymax = VWC_mn + VWC_sd/2),
              alpha = 0.25, fill = GoAvsGo[2]) +
  geom_line(data = d_fit_sum, aes(x = abs(MPa), y = VWC_mn), 
            show.legend = TRUE,  size = 2, color = GoAvsGo[2]) + 

  xlab('Soil Water Potential [MPa]') +
  
  ylab(expression(paste("Soil Water Content  [", "%", "] "  ))) +
  #xlim(-0.01, 5.5)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


ggplot() +
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & 
                                d_raw_out$SampleN<6,],
             aes(x = abs(MPa), y = Vol_Water), 
             show.legend = TRUE,  size = 4, alpha = 0.5, color = "black") + 
  geom_ribbon(data = d_fit_sum,
              aes(x = abs(MPa), ymin = VWC_mn - VWC_sd/2, ymax = VWC_mn + VWC_sd/2),
              alpha = 0.25, fill = GoAvsGo[2]) +
  geom_line(data = d_fit_sum, aes(x = abs(MPa), y = VWC_mn), 
            show.legend = TRUE,  size = 2, color = GoAvsGo[2]) + 
  
  xlab('Soil Water Potential [MPa]') +
  
  ylab(expression(paste("Soil Water Content  [", "%", "] "  ))) +
  xlim(-0.01, 5.5)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



ggplot() +
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & 
                                d_raw_out$SampleN<6,],
             aes(x = abs(MPa), y = Vol_Water), 
             show.legend = TRUE,  size = 4, alpha = 0.5, color = "black") + 
  geom_ribbon(data = d_fit_sum,
              aes(x = abs(MPa), ymin = VWC_mn - VWC_sd/2, ymax = VWC_mn + VWC_sd/2),
              alpha = 0.25, fill = GoAvsGo[2]) +
  geom_line(data = d_fit_sum, aes(x = abs(MPa), y = VWC_mn), 
            show.legend = TRUE,  size = 2, color = GoAvsGo[2]) + 
  
  xlab('Soil Water Potential [MPa]') +
  
  ylab(expression(paste("Soil Water Content  [", "%", "] "  ))) +
  xlim(-0.01, 0.25)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))



