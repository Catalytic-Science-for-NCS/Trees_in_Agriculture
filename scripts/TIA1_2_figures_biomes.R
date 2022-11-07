# TIA 1 and 2 results by biome
library(tidyverse)
library(ggplot2)
library(patchwork)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)

#ours
comb <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byBiome.csv")
#Millie
chp <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/treesincroplands/output/potential_VSG.csv")
bio_grp <- chp %>% 
  group_by(BIOME_NAME, CP) %>%
  summarize(sum_biome = sum(carbon_potential, na.rm=T)) %>%
  pivot_wider(names_from = CP, values_from  = sum_biome)

comb <- inner_join(comb,bio_grp, by="BIOME_NAME")

#get C potential/ha
comb$C_tic_ha <- comb$C.Potential.TIC/comb$Ag.Area.TIC..has.
comb$C_tip_ha <- comb$C.Potential.TIP/comb$Ag.Area.TIP..has.
comb$C_tic_ha_Chapman <- comb$crop/comb$Ag.Area.TIC..has.
comb$C_tip_ha_Chapman <- comb$pasture/comb$Ag.Area.TIP..has.

long <- pivot_longer(comb, cols=c(C_tic_ha_Chapman, C_tip_ha_Chapman, C_tic_ha, C_tip_ha))
long <- long %>% mutate(biome_abbrev = case_when(
  BIOME_NAME == "Mediterranean Forests, Woodlands & Scrub" ~"MF" ,
  BIOME_NAME == "Temperate Broadleaf & Mixed Forests" ~"TeBMF" ,
  BIOME_NAME ==  "Temperate Grasslands, Savannas & Shrublands" ~ "TeGSS",
  BIOME_NAME == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TrSDBF",
  BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "TrSGSS",
  BIOME_NAME ==  "Tropical & Subtropical Moist Broadleaf Forests" ~ "TrSMBF",
  BIOME_NAME ==  "Deserts & Xeric Shrublands" ~ "DXS",
  BIOME_NAME ==  "Montane Grasslands & Shrublands" ~ "MGSS",
  BIOME_NAME ==  "Temperate Conifer Forests" ~ "TeCF",
  BIOME_NAME ==  "Tropical & Subtropical Coniferous Forests" ~ "TrSCF"))
long$label <- ifelse(long$name=="C_tic_ha" | long$name == "C_tip_ha","TIA","Chapman")
long$CP <- ifelse(long$name == "C_tic_ha" | long$name=="C_tic_ha_Chapman","Crop","Graze")

p <- ggplot(long, aes(x=biome_abbrev, y=value, fill=label))+
  geom_col(position="dodge", width=0.5)+
  labs(x="Biome", y="Mg C potential / ha")+
  theme_minimal()+
  theme(legend.title = element_blank())+
  facet_wrap(~CP)

ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/C_ha_biomes.png",
       p, width=12, height=7, dpi=300, bg="white")

