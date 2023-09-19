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

setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/")
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

#results by biome then reformatted to long for ggplot
df <- read.csv("data/06_21_2023/biome_results_06212023.csv")
df_long <- reshape2::melt(df, id.vars=c("BIOME_NUM","BIOME_NAME"), measure.vars=c("Crop_TotalC_sum_MgC_30yr", "Graze_TotalC_sum_MgC_30yr"))
names(df_long) <- c("BIOME_NUM","BIOME_NAME","variable","value")

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
df_long$value <- df_long$value/1000000000*(44/12)

#write.csv(df_long, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/figure6_outputData.csv")

theme = theme_set(theme_minimal())
ggplot(df_long, aes(x=biome_abbrev, y=value, fill=variable))+
  geom_bar(stat="identity", color="black",
           position="stack") +
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
  scale_y_continuous(expand = c(0, 0), limits=c(0,40))+
  labs(x="Biome", y=expression(Pg~CO[2]~30~Yr^{-1}))+#parse(text=paste0("Pg", " ~CO[2]", " ~30Yr^-1")))+
  scale_fill_manual(values=c("#CEAB07", "#798E87"), labels=c("Crop","Graze"))
  

ggsave("../../../VivianAnalyses/Draft Figures/totalPotential_biome_barplot.png",width=8.5, height=7, dpi=600, bg="white")
