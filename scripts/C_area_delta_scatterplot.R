library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(terra)
library(patchwork)

cts <- read.csv("GitHub/Trees_in_Agriculture/data/02_17_2023/country_results_03172023.csv")

cts <- cts[,c("ISO3","NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
              "Crop_FluxDensity_mean_MgC.ha_CORRECT", "Graze_FluxDensity_mean_MgC.ha_CORRECT",
              "CropArea_sum_ha","GrazeArea_sum_ha",
             "Crop_Delta_mean_percent","Graze_Delta_mean_percent")]
#cts$All_TotalC_sum_MgC <- rowSums(cts[,c("Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC")], na.rm = T)
#cts$AllArea_sum_ha <- rowSums(cts[,c("CropArea_sum_ha","GrazeArea_sum_ha")], na.rm=T)
# 
# order.scores <- order(cts$AllArea_sum_ha, cts$NAME , decreasing = T)
# cts$rank_agarea <- NA
# cts$rank_agarea[order.scores] <- 1:nrow(cts)
# order.scores <- order(cts$All_TotalC_sum_MgC, cts$NAME , decreasing = T)
# cts$rank_total_MgC <- NA
# cts$rank_total_MgC[order.scores] <- 1:nrow(cts)

standarize <- function(x){(x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))}

#cts$All_TotalC_sum_MgC_01 <- standarize(cts$All_TotalC_sum_MgC)
#cts$AllArea_sum_ha_01 <- standarize(cts$AllArea_sum_ha)
cts$Crop_TotalC_sum_MgC_01 <- standarize(cts$Crop_TotalC_sum_MgC)
cts$Graze_TotalC_sum_MgC_01 <- standarize(cts$Graze_TotalC_sum_MgC)
cts$CropArea_sum_ha_01 <- standarize(cts$CropArea_sum_ha)
cts$GrazeArea_sum_ha_01 <- standarize(cts$GrazeArea_sum_ha)


long_delta <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_Delta_mean_percent","Graze_Delta_mean_percent"),
                      # Name of the destination column that will identify the original
                      # column that the measurement came from
                      variable.name="Ag.Type",
                      value.name="Delta")
long_ha <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("CropArea_sum_ha","GrazeArea_sum_ha"),
                          variable.name="Ag.Type",
                          value.name="AgArea_ha")
long_mgC <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC"),
                             variable.name="Ag.Type",
                             value.name="Total_MgC")
long_mgC_standardized <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_TotalC_sum_MgC_01","Graze_TotalC_sum_MgC_01"),
                           variable.name="Ag.Type",
                           value.name="Total_MgC_standardized")
long_flux <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_FluxDensity_mean_MgC.ha_CORRECT","Graze_FluxDensity_mean_MgC.ha_CORRECT"),
                                        variable.name="Ag.Type",
                                        value.name="Flux_density")

long_delta$Ag.Type <- substr(long_delta$Ag.Type, 1, 4)
long_ha$Ag.Type <- substr(long_ha$Ag.Type, 1, 4)
long_mgC$Ag.Type <- substr(long_mgC$Ag.Type, 1, 4)
long_mgC_standardized$Ag.Type <- substr(long_mgC_standardized$Ag.Type, 1, 4)
long_flux$Ag.Type <- substr(long_flux$Ag.Type,1,4)


long <- full_join(long_delta, long_ha, by=c("ISO3","NAME","Ag.Type")) %>%
  full_join(long_mgC, by=c("ISO3","NAME","Ag.Type")) %>%
  full_join(long_mgC_standardized, by=c("ISO3","NAME","Ag.Type")) %>%
  full_join(long_flux, by=c("ISO3","NAME","Ag.Type"))

biom <- vect("TIA/expertRecs_lessComplex_GADM_tropic_nontropic.shp")
biom <- as.data.frame(biom)
trop_nontrop <- biom %>% 
  group_by(GID_0) %>%
  drop_na(tropic_non)%>%
  summarize("lat" = ifelse(all(tropic_non == "Tropic"), "Tropical",
                           ifelse(all(tropic_non == "Nontropic"), "Temperate", "Mixed")))

test <- left_join(long, trop_nontrop, by=c("ISO3"="GID_0"))


c_ha <- ggplot(test[!is.na(test$lat),], aes(x=AgArea_ha, y=Delta, color=Flux_density, label=NAME))+
  geom_point(size=4)+
  facet_wrap(~Ag.Type)+
  ggrepel::geom_label_repel(data= subset(test, Flux_density >= 1),
                           size=3,
                           #color="black",
               #  nudge_y= 0.5,
                  # size          = 4,
                  # box.padding   = 1.5,
                  # point.padding = 0.5,
                  # force         = 100,
                 segment.size  = 0.2,
               show.legend = F)+
#  theme_minimal()+
  labs(x="Log10 Area (ha)", y="Mean Delta Tree Cover (%)")+
  scale_colour_distiller(type="seq", direction=1)+
  scale_x_log10()

c_tot <- ggplot(test[!is.na(test$lat),], aes(x=AgArea_ha, y=Delta, color=Total_MgC_standardized, label=NAME))+
  geom_point(size=4)+
  facet_wrap(~Ag.Type)+
  ggrepel::geom_label_repel(data= subset(test, Total_MgC_standardized >= 0.25),
                            size=3,
                            #color="black",
                            #  nudge_y= 0.5,
                            # size          = 4,
                            # box.padding   = 1.5,
                            # point.padding = 0.5,
                            # force         = 100,
                            segment.size  = 0.2,
                            show.legend = F)+
  #  theme_minimal()+
  labs(x="Log10 Area (ha)", y="Mean Delta Tree Cover (%)")+
  scale_colour_gradientn(guide = 'legend', colours = brewer.pal(n=5,name = 'BuGn'))+
  scale_x_log10()


c_ha/c_tot




ggsave("../VivianAnalyses/draft_main_figure.png",width=10, height=10)                 
  
# biom <- vect("TIA/expertRecs_lessComplex_GADM.shp")
# biom$tropic_nontropic <- case_when(biom$BIOME_NAME=="Deserts & Xeric Shrublands" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Mediterranean Forests, Woodlands & Scrub" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Montane Grasslands & Shrublands" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Temperate Broadleaf & Mixed Forests" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Temperate Conifer Forests" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" ~ "Nontropic",
#                                    biom$BIOME_NAME=="Tropical & Subtropical Coniferous Forests" ~ "Tropic",
#                                    biom$BIOME_NAME=="Tropical & Subtropical Dry Broadleaf Forests" ~ "Tropic",
#                                    biom$BIOME_NAME=="Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Tropic",
#                                    biom$BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" ~ "Tropic")
#writeVector(biom, "TIA/expertRecs_lessComplex_GADM_tropic_nontropic.shp")



    