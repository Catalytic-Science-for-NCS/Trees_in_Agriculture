# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: Trees in Agriculture
# ===============
# Description: Create figure 6-- barplot of potential mitigation by biome
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 07/01/2023
# Date updated: 09/18/2023
# ===============
# load libraries
library(tidyverse)
library(ggpattern)
#remotes::install_github("coolbutuseless/ggpattern")

setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/")
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

#results by biome then reformatted to long for ggplot
df <- read.csv("Trees_in_Agriculture/data/06_21_2023/biome_results_06212023.csv")
df <- left_join(df, byBiome[,c("CI95_crop_total","CI95_graze_total","BIOME_NUM","BIOME_NAME")], by=c("BIOME_NUM","BIOME_NAME"))
df_long <- reshape2::melt(df, id.vars=c("BIOME_NUM","BIOME_NAME"), measure.vars=c("Crop_FluxDensity_mean_MgC.ha.yr", "Graze_FluxDensity_mean_MgC.ha.yr"),
                          variable.name="Ag.Type",
                          value.name="Flux_Density_MgC_ha_yr")
df_long_error <- reshape2::melt(df, id.vars=c("BIOME_NUM","BIOME_NAME"), measure.vars=c("CI95_crop_total","CI95_graze_total"),
                           variable.name="Ag.Type",
                           value.name="Error_MgC_ha_yr")


df_long$Ag.Type <- substr(df_long$Ag.Type, 1, 4)
df_long_error$Ag.Type <- substr(df_long_error$Ag.Type, 6, 9)
df_long_error <- df_long_error %>% mutate(Ag.Type = case_when(
  Ag.Type == "crop" ~"Crop" ,
  Ag.Type == "graz" ~"Graz" 
))

df_long <- full_join(df_long, df_long_error, by=c("BIOME_NUM","BIOME_NAME","Ag.Type")) 

#names(df_long) <- c("BIOME_NUM","BIOME_NAME","variable","value")

#cleaning up lots of names and abbreviations
df_long <- df_long %>% mutate(biome_abbrev = case_when(
  BIOME_NAME == "Mediterranean Forests, Woodlands & Scrub" ~"MF" ,
  BIOME_NAME == "Temperate Broadleaf & Mixed Forests" ~"TeBMF" ,
  BIOME_NAME ==  "Temperate Grasslands, Savannas & Shrublands" ~ "TeGSS",
  BIOME_NAME == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TrSDBF",
  BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "TrSGSS",
  BIOME_NAME ==  "Tropical & Subtropical Moist Broadleaf Forests" ~ "TrSMBF",
  BIOME_NAME ==  "Deserts & Xeric Shrublands" ~ "DXS",
  BIOME_NAME ==  "Montane Grasslands & Shrublands" ~ "MGSS",
  BIOME_NAME ==  "Temperate Conifer Forests" ~ "TeCF",
  BIOME_NAME ==  "Tropical & Subtropical Coniferous Forest" ~ "TrSCF",
  BIOME_NAME ==  "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS"
))
df_long$biome_abbrev <- factor(df_long$biome_abbrev, levels=c("DXS","MF","MGSS","TeGSS","TeCF","TeBMF","TrSGSS","TrSDBF","TrSMBF"))

df_long <- df_long %>% mutate(climate=case_when(biome_abbrev=="DXS" ~ "Desert",
                                                biome_abbrev=="MF" ~ "Mediterranean",
                                                biome_abbrev=="MGSS" ~ "Montane",
                                                biome_abbrev=="TeBMF"~"Temperate",
                                                biome_abbrev=="TeGSS"~"Temperate",
                                                biome_abbrev=="TeCF" ~ "Temperate",
                                                biome_abbrev=="TrSDBF" ~ "Tropical/Subtropical",
                                                biome_abbrev=="TrSGSS" ~ "Tropical/Subtropical",
                                                biome_abbrev=="TrSMBF" ~ "Tropical/Subtropical"))

df_long <- df_long %>% mutate(forested=case_when(biome_abbrev=="MGSS" ~ "Forest",
                                                biome_abbrev=="TeBMF"~"Forest",
                                                biome_abbrev=="TeCF" ~ "Forest",
                                                biome_abbrev=="TrSDBF" ~ "Forest",
                                                biome_abbrev=="TrSMBF" ~ "Forest"))
#remove tropical coniferous forests
df_long <- subset(df_long, biome_abbrev!="TeCF")
#conversiont to Pg of CO2
#df_long$Total_MgC <- df_long$Total_MgC/1000000000*(44/12)
#df_long$Error_MgC <- df_long$Error_MgC/1000000000*(44/12)

#conversion to CO2
df_long$Flux_Density_MgCO2_ha_yr <- df_long$Flux_Density_MgC_ha_yr*(44/12)
df_long$Error_MgCO2 <- df_long$Error_MgC_ha_yr*(44/12)

#write.csv(df_long, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/figure6_outputData.csv")

high_n <- subset(df_long, BIOME_NUM!=8 & BIOME_NUM!=10 & BIOME_NUM!=13)#[-c(7,9,15,16)]
crossbar_subset <- df_long[c(5,16),]

theme = theme_set(theme_minimal())
bplot <- ggplot(df_long, aes(x=biome_abbrev, y=Flux_Density_MgCO2_ha_yr, fill=Ag.Type))+
  geom_bar(stat="identity", color="black", position=position_dodge()) +
   ggforce::facet_row(~climate, strip.position = "bottom", scales = "free_x",  space="free") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.title=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.y = element_text(colour="grey20"),
        axis.ticks.y= element_line(colour="grey20"),,
        axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.background=element_rect(color="grey20"))+
# scale_y_continuous(expand = c(0, 0), limits=c(0,40))+
#  labs(x="Biome", y=expression(atop(Total~Additional~Mitigation~Potential,
#                                    (Pg~CO[2]~30~Yr^{-1}))))+#parse(text=paste0("Pg", " ~CO[2]", " ~30Yr^-1")))+
  labs(x="Biome", y=expression(atop(Annual~Flux~Density,
                                    (Mg~CO[2]~Ha^{-1}~Yr^{-1}))))+#parse(text=paste0("Pg", " ~CO[2]", " ~30Yr^-1")))+
   scale_fill_manual(values=c("#CEAB07", "#798E87"), labels=c("Crop","Graze"))

bplot <- bplot + geom_errorbar(data=high_n, aes(ymin=Flux_Density_MgCO2_ha_yr-(Error_MgCO2/2), ymax=Flux_Density_MgCO2_ha_yr+(Error_MgCO2/2)), width=.2,
                      position=position_dodge(.9)) 
#bplot + geom_errorbar(data=crossbar_subset, aes(ymin=Flux_Density_MgCO2_ha_yr-(Error_MgCO2/2), 
 #                                               ymax=Flux_Density_MgCO2_ha_yr+(Error_MgCO2/2)), 
  #                    position = position_dodge(.9), width=.2) 

ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/Figures_Dec2023_Revision/fluxDensity_biome_barplot_errorBars.png",width=8.5, height=7, dpi=600, bg="white")
