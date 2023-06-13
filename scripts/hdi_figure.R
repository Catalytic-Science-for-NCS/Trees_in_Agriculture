# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: TIA
# ===============
# Description: make a figure comparing something to HDI
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 05/26/2023
# Date updated: 
# ===============
# load libraries
library(tidyverse)
library(ggplot2)
library(countrycode)
library(ggpmisc)

setwd("GitHub/Trees_in_Agriculture/data/")
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

hdi <- readxl::read_xlsx("HDR21-22_Statistical_Annex_HDI_Table.xlsx", sheet=1)
hdi <- hdi[-c(1:7, 199:nrow(hdi)), -c(4:15)]
names(hdi) <- c("Rank","Country","HDI")
hdi <- hdi[-grep("DEVELOPMENT", hdi$Country, ignore.case = T),]
hdi$ISO3 <- countrycode(hdi$Country, origin="country.name", destination="iso3c", warn=T)
hdi$ISO3[hdi$Country=="Türkiye"] <- "TUR"

tia <- read.csv("02_17_2023/country_results_03172023.csv")
tia <- tia[, c("ISO3","NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC.ha_CORRECT",
               "Graze_FluxDensity_mean_MgC.ha_CORRECT","CropArea_sum_ha","GrazeArea_sum_ha",
               "Crop_ForestCover_mean_percent_positiveDelta","Graze_ForestCover_mean_percent_positiveDelta",
               "Crop_ForestCover_mean_percent_allDelta" , "Graze_ForestCover_mean_percent_allDelta",
               "Crop_Delta_mean_percent" ,"Graze_Delta_mean_percent","Crop_ExpertRecs_mean_percent","Graze_ExpertRecs_mean_percent" )]

df <- inner_join(tia, hdi[,-2], by="ISO3")
#names(df) <- c("ISO3","NAME","Crop_total","Graze_total","Crop_flux","graze_flux","HDI_rank","HDI")
names(df) <- c("ISO3","NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC.ha.yr","Graze_FluxDensity_mean_MgC.ha.yr","CropArea_sum_ha","GrazeArea_sum_ha",                       
               "Crop_ForestCover_mean_percent_positiveDelta","Graze_ForestCover_mean_percent_positiveDelta",
               "Crop_ForestCover_mean_percent_allDelta" , "Graze_ForestCover_mean_percent_allDelta",
              "Crop_Delta_mean_percent","Graze_Delta_mean_percent",               
          "Crop_ExpertRecs_mean_percent","Graze_ExpertRecs_mean_percent",          
          "Rank","HDI")
df_long <- reshape::melt(df, id.vars=c("ISO3", "NAME", "HDI", "Rank"))
df_long$value <- as.numeric(df_long$value)
df_long$HDI <- as.numeric(df_long$HDI)
df_long$Rank <- as.numeric(df_long$Rank)


ggplot(df_long,#[df_long$variable=="Crop_total" | df_long$variable=="Graze_total",],
       aes(x=HDI, y=value)) +
  geom_point()+
  stat_poly_line()+
  stat_poly_eq(use_label(c("R2","p")))+
  facet_wrap(~variable, scales = "free_y")+
  theme(legend.position = "none", strip.text = element_text(size=11))+
  ylab("")

ggsave("../HDI_draft.png",plot = last_plot(),
       width = 14,
       height = 14)
  
