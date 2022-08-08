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

library(lme4)
library(tidybayes)
library(rstan)
library(brms)
library(modelr)
library(sjPlot)
library(sjstats)

library(MASS)
library(scales)


# if (packageVersion("devtools") < 1.6) {
#   
#   if (!require(devtools)) {
#     install.packages("devtools")
#   }
#   install.packages("devtools")
# }
# 
# install.packages("cli")
# install.packages("devtools")
# 
# library(devtools)
# library(cli)
# 
# if(!require(nlmrt)){
#   install.packages("nlmrt")
# }
# if(!require(httr)){
#   install.packages("httr")
# }
# if(!require(vadose)){
#   devtools::install_github("gowusu/vadose")
# }
# 
# library(vadose)

##//ggplot theme 
theme_set(theme_minimal())

##//custom color ramp 
GoAvsGo <- c("#6F263D", "#236192", "#A2AAAD", "#000000")

##//Where is the data
setwd("D:/Dropbox/Projects/Indiana/SoilLab/Data/HyProp/HypropExports")
dir()

##//List of files in the directory
Flist <- dir(pattern = ".xlsx")
Flist <- dir(pattern = ".xlsx")[4:6]

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

###//Correct and add parameters
##//Observation Number
d_raw_out$SampleN <- as.numeric(as.factor(d_raw_out$Sample))
d_fit_out$SampleN <- as.numeric(as.factor(d_fit_out$Sample))

d_fit_out$MPa <- as.numeric(d_fit_out$MPa)
d_fit_out$SampleN <- as.numeric(d_fit_out$MPa)


d_fit_out$MPa_Log10 <- (log10(d_fit_out$MPa)) 
d_raw_out$MPa_Log10 <- log10(abs(d_raw_out$MPa)) 

d_raw_out$Vol_Water_d <- d_raw_out$Vol_Water / 100 
d_fit_out$Vol_Water_d <- d_fit_out$Vol_Water / 100 

##//Plot the METER curves with the raw data
ggplot() +
  
  geom_line(data = d_fit_out[d_fit_out$Vol_Water>0 & 
                                d_fit_out$MPa_Log10>-4,], 
            aes(y = MPa, x = Vol_Water/100), color = "gray", 
             show.legend = TRUE,  size = 4, alpha = 0.5) +
  
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & d_raw_out$MPa_Log10> -4,], 
             aes(y = abs(MPa), x = Vol_Water_d, color = Sample), 
             show.legend = TRUE,  size = 4, alpha = 0.5) + 
  
  scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 

  ylab('Matric Potential [MPa]') +
  xlab('Soil Water Content [%]') +
  

  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-4, 10^2))+
  annotation_logticks() +  
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


##//Dataset without some of the weird outliers
##//Cheat filter for when needed
cc <- d_raw_out$Vol_Water>0 & d_raw_out$MPa_Log10> -4

##//Subsetting raw data form model fitting
d_fitting <- d_raw_out[d_raw_out$Vol_Water>0 & d_raw_out$MPa_Log10> -4,]
##//Make sure pressures are all positive for analysis
d_fitting$MPa_Abs <- abs(d_fitting$MPa)

##//Van Genutchen model function
##//Currently simple nls model may move later
vg <- function(suction,thetas,thetar,a,n){
  
  ((thetas-thetar)/((((a*suction)^n)+1)^(1-(1/n))))+thetar

}

##Isolating Pressure and decimal VWC
x <- d_fitting$MPa_Abs
y <- d_fitting$Vol_Water_d

##//Note starting parameters are very sensitive (may need to tweak for each dataset)
fit <- nls(y~vg(x,thetas,thetar,a,n),start=list(thetas=0.74,thetar=0.02,a=20,n=1.55))
##//Summary of model fit
summary(fit)
##//defining object to extract model parameters 
fit_sum <- summary(fit)

##//Model parameters and error
thetas = fit_sum$coefficients[1]
thetas_error = fit_sum$coefficients[5]
thetar = fit_sum$coefficients[2]
thetar_error = fit_sum$coefficients[6]
alpha = fit_sum$coefficients[3]
alpha_error = fit_sum$coefficients[7]
n = fit_sum$coefficients[4]
n_error = fit_sum$coefficients[8]


tests <-   ((thetas-thetar)/((((alpha*x)^n)+1)^(1-(1/n))))+thetar
tests2 <-  ( (  ((thetar- thetas) / (thetar - y)) ^ (n/(n-1)) - 1 ) ^(1/n) ) / alpha
plot(tests,log10(d_fitting$MPa_Abs))
points(d_fitting$Vol_Water_d, log10(d_fitting$MPa_Abs), col = "blue", pch = 16)

plot(log10(tests2)~d_fitting$Vol_Water_d)
points(d_fitting$Vol_Water_d, log10(d_fitting$MPa_Abs), col = "blue", pch = 16)


myVWC <- seq(0, 0.8, length = 1000)
myPSI <-  ( (  ((thetar- thetas) / (thetar - myVWC)) ^ (n/(n-1)) - 1 ) ^(1/n) ) / alpha

plot(myVWC, myPSI)
plot(myVWC, log10(myPSI))


plot(log10(d_fit_out$MPa), d_fit_out$Vol_Water, xlim = c(-6,5))
points(log10(myPSI),myVWC*100,  col = "red")




##################################################################################
####//BELOW IS SCARY
##################################################################################

plot(log10(abs(d_raw_out$MPa)), d_raw_out$Vol_Water, xlim = c(-10,60))






fits <- nls( log10(d_fitting$MPa) ~ ( (  ((d_fitting$theta_r - d_fitting$theta_s) / (d_fitting$theta_r - d_fitting$Vol_Water)) ^ (n/(n-1)) - 1 ) ^(1/n) ) / alpha,
                start = list(alpha = 0.5, n = 15), data = d_fitting  ) 

(-1 + ((- theta_s + theta_r)/(-y + theta_r))^(n/(-1 + n)))^(1/n)/alpha
ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_point(data = d_raw_out[d_raw_out$Vol_Water>0 & d_raw_out$MPa_Log10>-4,], 
             aes(y = log(MPa), x = Vol_Water_d, color = Sample), 
             show.legend = TRUE,  size = 4) + 
  #scale_color_manual(values = c("#4D54E8", "#ECC01D", "#A2A197"))+ 
  #geom_abline(slope = 1, intercept = 0, size = 3) + 
  ylab('Log10 MPa') +
  
  # ylab(expression(paste("Corr. Soil Efflux  [", mu, "mol CO" [2], " m"^2, "  s "^-1, "] "  ))) +
  # xlim(-0.2, 2.5)+
  # ylim(0,5)+
  theme(legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))

max(d_raw_out$MPa_Log10)
max(d_raw_out$Vol_Water)
d_raw_out$theta_s <- max(d_raw_out$Vol_Water) / 100
d_raw_out$theta_r <- min(d_raw_out$Vol_Water[d_raw_out$MPa<50]) / 100
d_raw_out$MPa_Log10_adj <- d_raw_out$MPa_Log10 + 4 


10 ^ -4 

plot(d_raw_out$MPa_Log10_adj[d_raw_out$MPa_Log10_adj>0]~d_raw_out$Vol_Water_d[d_raw_out$MPa_Log10_adj>0])
min(d_raw_out$MPa_Log10_adj)

n <- 2   ##// 1 or greater
m <- 1 - 1 / n
# alpha <- 2   ##// 1 or greater

d_raw_out$Vol_Water_d <- d_raw_out$Vol_Water / 100


mytheta <- seq(0.1,74, length = 20000)

#mypsi <- ( (  ((2.01- 74.17) / (2.01 - mytheta)) ^ (5/(5-1)) - 1 ) ^(1/5) ) / 0.5


d_fitting <- d_raw_out[d_raw_out$Vol_Water>0 & d_raw_out$MPa_Log10> -4,]
# d_fitting$theta_r <- min(d_fitting$Vol_Water[cc], na.rm = T)
# d_fitting$theta_s <- max(d_fitting$Vol_Water[cc], na.rm = T)
# d_fitting$MPa_Log10

d_fitting$A <- (min(d_fitting$Vol_Water[cc], na.rm = T) - max(d_fitting$Vol_Water[cc], na.rm = T)) /
  (min(d_fitting$Vol_Water[cc], na.rm = T) - d_fitting$Vol_Water) 



mod <- nls()

plot(d_fitting$A, d_fitting$MPa)

n <- 0.5

m <- (1 - 1/n)

B <- A^(n/(n-1)) - 1

C <- B^(1/n)

D <- C / .5
D

plot(mytheta, (D))

plot(mytheta, log10(D), ylim=c(-4,2))
points(d_fitting$Vol_Water, log10(abs(d_fitting$MPa)), pch = 15)

d_fitting$MPa_Abs <- abs(d_fitting$MPa)

formula <- bf(
  MPa_Abs ~ ((A^(n/(n-1)) - 1) ^(1/n))  / alpha,
  alpha ~ 1,
  n ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
prior1 <- c(
  prior(uniform(-100, 100), nlpar = "alpha"),
  prior(uniform(-100, 100), nlpar = "n"))


hist((log10(d_fitting$MPa_Abs)))

fit <- brm(
  formula,
  data = d_fitting,
  prior = prior1,
  family =  "lognormal",
  iter = 50000,
  chains = 4,
  # cores = 4,
  control = list(adapt_delta = 0.90, max_treedepth = 15))
summary(fit, prob = 0.89)

plot_model(fit, type = "pred", show.data = TRUE,
           show.values = TRUE, terms = c("Vol_Water")) +
  theme_bw()
bayes_R2(fit, prob = c(0.055, 0.945))



## Not run: 
#optimisation with single data
datartc=read.csv(system.file("ext","sys","retentionVG.csv",package="vadose"))
modrtc<-vg(data=datartc,h="h",theta="theta",thr=0.1, ths=0.1, alp=0.1, n=1)
plot(modrtc)

#optimisation with group data isric

data=read.csv(system.file("ext","sys","isric2.csv",package="vadose"))
#optimisation with group data
#used public initials and multiple Ks
modisrc<-vg(data=data,h="x",theta="y",m="b",thr=0.1, ths=0.1, alp=0.1, n=1,group="Sample",
            Ks=c("Sand","Clay","Silt","silty clay loam"),para="soil")
modisrc<-vg(data=data,h="x",theta="y",m="b",thr=0.1, ths=0.3, alp=0.01, n=2,group="Sample")
plot(modisrc)
mod=vg(h=200)

## End(Not run)




\ 0.02\ +\ \left(0.74-0.02\right)\cdot\left[1\ +\ \left(a\cdot x\right)^{n}\right]^{-\left(1-\frac{1}{n}\right)}

min(d_fitting$MPa_Abs[cc], na.rm = T)
d_fitting$Vol_Water ~ min(d_fitting$Vol_Water[cc], na.rm = T) + 
       ((max(d_fitting$Vol_Water[cc], na.rm = T) - 
               (min(d_fitting$Vol_Water[cc], na.rm = T)))) * ((1 + Alpha * (d_fitting$MPa_Abs*-1))^n)^(1-1/n)
     

d_fitting$A <- (min(d_fitting$Vol_Water[cc], na.rm = T) - max(d_fitting$Vol_Water[cc], na.rm = T)) /
  (min(d_fitting$Vol_Water[cc], na.rm = T) - d_fitting$Vol_Water) 






plot(d_fitting$Vol_Water, d_fitting$MPa)

fits <- nls( log10(d_fitting$MPa_Log10) ~ ( (  ((d_fitting$theta_r - d_fitting$theta_s) / (d_fitting$theta_r - d_fitting$Vol_Water)) ^ (n/(n-1)) - 1 ) ^(1/n) ) / alpha,
             start = list(alpha = 0.5, n = 15), data = d_fitting  ) 

plot(d_fitting$MPa_Log10 ~ d_fitting$Vol_Water)

d_fitting$MPa_abs <- abs(d_fitting$MPa)


( (  ((2.01- 74.17) / (2.01 - mytheta)) ^ (5/(5-1)) - 1 ) ^(1/5) ) / 0.5
# Here I let all the curve features randomly vary
formula <- bf(
   MPa_abs ~ ( (  ((2.01- 74.17) / (2.01 - Vol_Water)) ^ (n/(n-1)) - 1 ) ^(1/n) ) / alpha,
  alpha ~ 1,
  n ~ 1,
  sigma ~ 1,
  nl = TRUE)

# quick and dirty priors. you need priors to even fit a model
prior1 <- c(
  prior(normal(0.5, 2), nlpar = "alpha"),
  prior(normal(15, 8), nlpar = "n"))


hist((log(d_fitting$MPa_abs)))

fit <- brm(
  formula,
  data = d_fitting,
  prior = prior1,
  family =  "lognormal",
  iter = 50000,
  chains = 4,
  # cores = 4,
  control = list(adapt_delta = 0.90, max_treedepth = 15))
summary(fit, prob = 0.89)

plot_model(fit, type = "pred", show.data = TRUE,
           show.values = TRUE, terms = c("Vol_Water")) +
  theme_bw()
bayes_R2(fit, prob = c(0.055, 0.945))






Lag_Cap <- seq(0, 2, length = 50); newdat <- data.frame(Lag_Cap)
Cap_pred <- predict(O_Ks_C, newdata =  newdat, probs = c(0.055, 0.945))
Cap_pred <- cbind(Cap_pred, Lag_Cap); Cap_pred <- as.data.frame(Cap_pred)




grid <- tdat %>%
  group_by(Spp) %>%
  tidyr::expand(SoilMoist = modelr::seq_range(SoilMoist, n = 80)) %>%
  ungroup()

fits <- grid %>%
  add_fitted_draws(fit, n = 100)

ggplot(tdat) +
  aes(x = SoilMoist, y = fluor) +
  geom_line(
    aes(group = interaction(Spp, .draw), y = .value),
    data = fits, alpha = .3) +
  geom_point(color = "red") +
  facet_wrap("Spp")






library(HydroMe)
library(minpack.lm)
library(SoilHyP)


# --------------------------------------------
#  Unimodal van Genuchten
# --------------------------------------------
Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
   par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5),
   modality = 'uni', suc.negativ = FALSE)
# --------------------------------------------
#  Bimodal van Genuchten
# --------------------------------------------
Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'vGM',
   par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02,
                  n = 1.5, tau = 0.5, w2 = 0.1, alfa2 = 0.1, n2 = 3),
   modality = 'bi', suc.negativ = FALSE)
# --------------------------------------------
#  Unimodal Peters-Durner-Iden (PDI)
# --------------------------------------------
Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'PDI', modality = 'uni',
   par.shp = list(Ks = 10, ths = 0.5, thr = 0, alfa = 0.02, n = 1.5, tau = 0.5, omega = 0.001),
   suc.negativ = FALSE)
# --------------------------------------------
#  Brooks and Corey (BC) (only unimodal)
# --------------------------------------------
Ku(suc = seq(1, 1000, by = 1), FUN.shp = 'bc', modality = 'uni',
   par.shp = list(ths = 0.4, thr = 0, lambda =  0.211, alfa = 0.1, tau = 0.5, ks = 10),
   suc.negativ = FALSE)
# --------------------------------------------



data(isric)
require("minpack.lm")
pf1=subset(isric,Sample=="Nicaragua21")
vn.ns <- nlsLM(y ~ SSvgm(x,thr,ths,alp,nscal,mscal),
               data = pf1,
               control = nls.lm.control(maxiter=200,options(warn=-1)))
coef(vn.ns)
plot(fitted(vn.ns)~pf1$y)
abline(a=0,b=1,lty=20, col="blue")


SSvgm(input, thr, ths, alp, nscal, mscal)


d_fit_out

datartc=read.csv(system.file("ext","sys","retentionVG.csv",package="vadose"))
modrtc<-vg(data = datartc, h="h", theta = "theta", thr = 0.1, ths = 0.1,
           alp = 0.1, n = 1, m = "m", Ks = "Nasta", para = NULL,
           group = NULL)
plot(modrtc)
summary(modrtc)
#optimisation with group data isric

data=read.csv(system.file("ext","sys","isric2.csv",package="vadose"))
#optimisation with group data
#used public initials and multiple Ks
modisrc<-vg(data=data,h="x",theta="y",m="b",thr=0.1, ths=0.1, alp=0.1, n=1,group="Sample",
            Ks=c("Sand","Clay","Silt","silty clay loam"),para="soil")
modisrc<-vg(data=data,h="x",theta="y",m="b",thr=0.1, ths=0.3, alp=0.01, n=2,group="Sample")
plot(modisrc)
mod=vg(h=200)



vg(data = NULL, h, theta = NULL, thr = 0.1, ths = 0.1, alp = 0.1,
   n = 1, m = "m", Ks, para = NULL, group = NULL)





d_fit_sum = d_fit_out %>%
  dplyr::group_by(MPa) %>%
  dplyr::summarise(
    VWC_mn = mean(Vol_Water, na.rm = TRUE),
    VWC_md = median(Vol_Water, na.rm = TRUE),
    VWC_sd = sd(Vol_Water, na.rm = TRUE)) %>%
  as.data.frame()
d_raw_out$MPa
d_raw_out$Vol_Water
d_raw_out$SampleN

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



