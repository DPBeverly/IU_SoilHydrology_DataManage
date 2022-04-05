##//Combing the Griffy Woods and Lab Calibrations for 4/6/2022 lab meeting

KsFigs <- rbind(Labdouts,Griffydouts)


KsFigs$Site[KsFigs$Material=="5050"] <- "Lab5050"
KsFigs$Site[KsFigs$Material=="Organic"] <- "LabOrganic"
KsFigs$Site[KsFigs$Material=="Sand"] <- "LabSand"
KsFigs$Site[KsFigs$Material=="griffytde"] <- "GW_TDE"
KsFigs$Site[KsFigs$Material=="griffycontrol"] <- "GW_CTL"
KsFigs$Site[KsFigs$Material=="alexsdownslope"] <- "GW_Dnslope"
KsFigs$Site[KsFigs$Material=="alexsupslope"] <- "Yikes"


unique(KsFigs$Material)

ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_line(data = KsFigs[KsFigs$Material!="alexsupslope",], aes(x = Time, y = Mean_Flow, color = Site),
             show.legend = TRUE,  size=4) +
  
  geom_ribbon(data = KsFigs[KsFigs$Material!="alexsupslope",], 
              aes(x = Time, ymin = Flow_LCI, ymax = Flow_HCI, fill = Site), alpha = 0.2)+
  
  xlab(expression(paste("Time [seconds]"  ))) +
  ylab(expression(paste("Pressure Head  [", "cm", "] "  ))) +
  #xlim(0,5)+
  #ylim(0,5)+
  theme(legend.position = "top",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30))


GriffyWoods_Ksat
LabCalibration_Ksat

BoxKs <- c()
BoxKs <- data.frame(Soil = "Sand",
                    Ks_cm_d = LabCalibration_Ksat$KsSoil_cm_d[LabCalibration_Ksat$mymaterials=="sand"])

BoxKs <- rbind(BoxKs, data.frame(Soil = "5050",
                    Ks_cm_d = LabCalibration_Ksat$KsSoil_cm_d[LabCalibration_Ksat$mymaterials=="5050"]))

BoxKs <- rbind(BoxKs, data.frame(Soil = "GW_TDE",
                                 Ks_cm_d = GriffyWoods_Ksat$KsSoil_cm_d[GriffyWoods_Ksat$mylocates=="griffytde"]))

BoxKs <- rbind(BoxKs, data.frame(Soil = "GW_CTL",
                                 Ks_cm_d = GriffyWoods_Ksat$KsSoil_cm_d[GriffyWoods_Ksat$mylocates=="griffycontrol"]))

BoxKs <- rbind(BoxKs, data.frame(Soil = "GW_Dnslope",
                                 Ks_cm_d = GriffyWoods_Ksat$KsSoil_cm_d[GriffyWoods_Ksat$mylocates=="alexsdownslope"]))


ggplot() +
  #geom_abline(slope = 1, intercept = 0, size = 3, alpha = 0.5, linetype = "dashed") + 
  geom_boxplot(data = BoxKs, aes(x = Soil, y = Ks_cm_d, fill = Soil),
            show.legend = TRUE,  size=1, color = "black", alpha = 0.5) +

  
  xlab(expression(paste("Soil"  ))) +
  ylab(expression(paste("Saturated Hydraulic K [", "cm d-1", "] "  ))) +
  #xlim(0,5)+
  ylim(0,11000)+
  theme(legend.position = "NA",
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.title=element_text("Species",size=30),
        legend.text=element_text(size=30),
        axis.text.x=element_text(angle = 33, vjust = 0.6, hjust=0.55))



.LabCalibration_Ksat$KsSoil_cm_d[LabCalibration_Ksat$mymaterials=="5050"]



