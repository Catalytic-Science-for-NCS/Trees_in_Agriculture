library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(terra)
library(patchwork)
library(sf)

cts <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/country_results_partial_06212023.csv")

cts <- cts[,c("ISO3","NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
              "Crop_FluxDensity_mean_MgC.ha.yr", "Graze_FluxDensity_mean_MgC.ha.yr",
              "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta",
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
long_ha <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta"),
                          variable.name="Ag.Type",
                          value.name="AgArea_ha")
long_mgC <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC"),
                             variable.name="Ag.Type",
                             value.name="Total_MgC")
# long_mgC_standardized <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_TotalC_sum_MgC_01","Graze_TotalC_sum_MgC_01"),
#                            variable.name="Ag.Type",
#                            value.name="Total_MgC_standardized")
long_flux <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr"),
                                        variable.name="Ag.Type",
                                        value.name="Flux_density")

long_delta$Ag.Type <- substr(long_delta$Ag.Type, 1, 4)
long_ha$Ag.Type <- substr(long_ha$Ag.Type, 1, 4)
long_mgC$Ag.Type <- substr(long_mgC$Ag.Type, 1, 4)
#long_mgC_standardized$Ag.Type <- substr(long_mgC_standardized$Ag.Type, 1, 4)
long_flux$Ag.Type <- substr(long_flux$Ag.Type,1,4)


long <- full_join(long_delta, long_ha, by=c("ISO3","NAME","Ag.Type")) %>%
  full_join(long_mgC, by=c("ISO3","NAME","Ag.Type")) %>%
  #full_join(long_mgC_standardized, by=c("ISO3","NAME","Ag.Type")) %>%
  full_join(long_flux, by=c("ISO3","NAME","Ag.Type"))
long$NAME <- ifelse(long$ISO3=="COG", "Congo",long$NAME)


# biom <- vect("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/TIA/expertRecs_lessComplex_GADM_tropic_nontropic.shp")
# biom <- as.data.frame(biom)
# trop_nontrop <- biom %>% 
#   group_by(GID_0) %>%
#   drop_na(tropic_non)%>%
#   summarize("lat" = ifelse(all(tropic_non == "Tropic"), "Tropical Only",
#                            ifelse(all(tropic_non == "Nontropic"), "Subtropical and Temperate", "Mixed")))

long_updated <- left_join(long, as.data.frame(countries_shp)[,c("ISO3","lat")], by="ISO3")
long_updated$lat <- factor(long_updated$lat, levels=c("Tropical Only","Subtropical and Temperate","Mixed"))



long_updated_crop <- long_updated[long_updated$Ag.Type=="Crop",]
long_updated_crop <- long_updated_crop[complete.cases(long_updated_crop),]

order.scores<-order(long_updated_crop$Flux_density, long_updated_crop$ISO3, decreasing = T)
long_updated_crop$flux_rank <- NA
long_updated_crop$flux_rank[order.scores] <- 1:nrow(long_updated_crop)
long_updated_crop$flux_rank <- ifelse(is.na(long_updated_crop$Flux_density), NA, long_updated_crop$flux_rank)
max(long_updated_crop$flux_rank, na.rm = T)
long_updated_crop$flux_rank <- ifelse(long_updated_crop$flux_rank <=5, "Global Top 5",ifelse(
  long_updated_crop$flux_rank >= 172, "Global Bottom 5", "Other Countries")
)

order.scores<-order(long_updated_crop$Total_MgC, long_updated_crop$ISO3 , decreasing = T)
long_updated_crop$total_MgC_rank <- NA
long_updated_crop$total_MgC_rank[order.scores] <- 1:nrow(long_updated_crop)
#long_updated_crop$total_MgC_rank <- ifelse(long_updated_crop$Total_MgC==0, NA, long_updated_crop$total_MgC_rank)
max(long_updated_crop$total_MgC_rank, na.rm = T)
long_updated_crop$total_MgC_rank <- ifelse(long_updated_crop$total_MgC_rank <=5, "Global Top 5",ifelse(
  long_updated_crop$total_MgC_rank >= 172 , "Global Bottom 5", "Other Countries")
)


long_updated_graz <- long_updated[long_updated$Ag.Type=="Graz",]
long_updated_graz <- long_updated_graz[complete.cases(long_updated_graz),]

order.scores <- order(long_updated_graz$Flux_density, long_updated_graz$ISO3, decreasing = T)
long_updated_graz$flux_rank <- NA
long_updated_graz$flux_rank[order.scores] <- 1:nrow(long_updated_graz)
long_updated_graz$flux_rank <- ifelse(is.na(long_updated_graz$Flux_density), NA, long_updated_graz$flux_rank)
max(long_updated_graz$flux_rank, na.rm = T)
long_updated_graz$flux_rank <- ifelse(long_updated_graz$flux_rank <=5, "Global Top 5",ifelse(
  long_updated_graz$flux_rank >= 78, "Global Bottom 5", "Other Countries")
)

order.scores<-order(long_updated_graz$Total_MgC, long_updated_graz$ISO3 , decreasing = T)
long_updated_graz$total_MgC_rank <- NA
long_updated_graz$total_MgC_rank[order.scores] <- 1:nrow(long_updated_graz)
#long_updated_graz$total_MgC_rank <- ifelse(long_updated_graz$Total_MgC==0, NA, long_updated_graz$total_MgC_rank)
max(long_updated_graz$total_MgC_rank, na.rm = T)
long_updated_graz$total_MgC_rank <- ifelse(long_updated_graz$total_MgC_rank <=5, "Global Top 5",ifelse(
   long_updated_graz$total_MgC_rank >= 78, "Global Bottom 5", "Other Countries")
)

long_updated <- rbind(long_updated_crop, long_updated_graz)

long_updated$flux_rank <- factor(long_updated$flux_rank, levels=c("Global Top 5","Other Countries","Global Bottom 5"))
long_updated$total_MgC_rank <- factor(long_updated$total_MgC_rank, levels=c("Global Top 5","Other Countries","Global Bottom 5"))


long_updated$Rank <- ifelse(long_updated$total_MgC_rank=="Global Top 5" | long_updated$flux_rank=="Global Top 5", "Global Top 5",
                            ifelse(long_updated$total_MgC_rank=="Global Bottom 5" | long_updated$flux_rank=="Global Bottom 5", "Global Bottom 5", "Other Countries"))



long_updated$Rank <- factor(long_updated$Rank, levels=c("Global Top 5","Global Bottom 5","Other Countries"))
# long_updated$Rank_group_color <- ifelse(long_updated$flux_rank=="Global Top 5" | long_updated$total_MgC_rank=="Global Top 5" | long_updated$flux_rank=="Global Bottom 5" | long_updated$total_MgC_rank=="Global Bottom 5",
#                                         "black","greyedout")

long_updated$Rank_group_label <- ifelse(long_updated$flux_rank=="Global Top 5" | long_updated$flux_rank=="Global Bottom 5" ,"flux",
                                        ifelse(long_updated$total_MgC_rank=="Global Top 5" | long_updated$total_MgC_rank=="Global Bottom 5", "total_mgC", "greyedout"))

long_updated$Ag.Type <- ifelse(long_updated$Ag.Type=="Graz","Grazing Land","Cropland")
long_updated$int <- paste(long_updated$Rank, long_updated$Rank_group_label, sep=".")
long_updated$int <- factor(long_updated$int, levels=c("Global Top 5.flux","Global Bottom 5.flux",
                                                      "Global Top 5.total_mgC","Global Bottom 5.total_mgC",
                                                      "Other Countries.greyedout"))
#flxunits <- c(expression(paste("Top 5 ",MgCO[2]," ", Ha^-1," ", Yr^-1)))

long_updated$lat_new <- ifelse(long_updated$lat=="Tropical Only", "Tropical Only", "Not Tropical Only")
long_updated$lat_new <- factor(long_updated$lat_new, levels=c("Tropical Only","Not Tropical Only"))

long_updated$int_new <- ifelse(long_updated$int=="Global Bottom 5.flux" | long_updated$int=="Global Bottom 5.total_mgC",
                               "Other Countries.greyedout", long_updated$int)
long_updated$int_new <- ifelse(long_updated$int_new==5, "Other Countries.greyedout", long_updated$int_new)
#long_updated$int_new <- factor(long_updated$int, levels=c("1","3",
#                                                      "Other Countries.greyedout"))
long_updated$Rank_group_label_new <- ifelse(long_updated$int_new=="Other Countries.greyedout","greyedout",long_updated$Rank_group_label)
long_updated$Rank_group_label_new <- factor(long_updated$Rank_group_label_new, levels=c("flux","total_mgC","greyedout"))

long_updated <- long_updated[order(long_updated$Rank_group_label_new),]



labss <- c("Top 5 Flux Density", "Bottom 5 Flux Density","Top 5 Total MgC","Bottom 5 Total MgC", "Other Countries")
#labss <- c("Top 5 Flux Density","Top 5 Total MgC","Other Countries")
c_ha <- ggplot(data=long_updated[!is.na(long_updated$AgArea_ha),],
               aes(x=AgArea_ha, y=Delta, fill=int, shape=int, color=int, alpha=int))+
  geom_point(size=2)+
  facet_grid(lat_new~Ag.Type)+ 
  scale_alpha_manual(values=c(1, 0.3, 1, 0.5, 1),
                     name="Global Rank",
                     labels=labss)+
  scale_color_manual(values = c("black","grey","black","grey", "grey"), 
                     name="Global Rank",
                     labels=labss) +
  scale_shape_manual(values=c(24,24,22, 22, 21),
                     name="Global Rank",
                     labels=labss) +
  scale_fill_manual(values=c("#a6611a", "#a6611a","#80cdc1","#80cdc1", "grey"), 
                    name="Global Rank",
                    labels=labss)+
  ggrepel::geom_label_repel(data=long_updated[!is.na(long_updated$AgArea_ha) & long_updated$Rank_group_label!="greyedout" ,], 
                           inherit.aes = F, 
                           aes(x=AgArea_ha, y=Delta, label=NAME),
                           size=2,
                           nudge_y = 0.5,
                           segment.size  = 0.2,
                           min.segment.length = 0,
                           box.padding = 0.15,
                           label.padding = 0.15)+
  labs(x="Area (ha)", y="Mean Additional Tree Cover Potential (%)")+
  scale_x_continuous(trans = "log10", 
                     labels = trans_format("log10", math_format(10^.x)))+
  theme(strip.placement = "outside",
        panel.spacing = unit(1, "lines"));c_ha


ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/Draft Figures/draft_main_figure.png",
       c_ha, width=8, height=8, bg='white')                 
  
countries_shp <- vect("../../../../Downloads/countries_shp/countries.shp") %>%
  aggregate("ISO3")

coords_trop <- rbind(c(-180, -23.5), c(-180, 23.5),  c(180, 23.5), c(180, -23.5))
colnames(coords_trop) <- c('x', 'y')
p_trop <- vect(coords_trop, "polygons", crs="epsg:4326")

coords_temp_N <- rbind(c(-180, 23.5), c(-180, 85),  c(180, 85), c(180, 23.5))
colnames(coords_temp_N) <- c('x', 'y')
p_temp_N <- vect(coords_temp_N, "polygons", crs="epsg:4326")

coords_temp_S <- rbind(c(-180, -23.5), c(-180, -85),  c(180, -85), c(180, -23.5))
colnames(coords_temp_S) <- c('x', 'y')
p_temp_S <- vect(coords_temp_S, "polygons", crs="epsg:4326")



countries_tropical_intersect <- intersect(countries_shp, p_trop) %>%
  as.data.frame()
countries_tropical_intersect$tropical <- "Y"
countries_tropical_intersect <- countries_tropical_intersect[!duplicated(countries_tropical_intersect$ISO3),]

countries_temperateN_intersect <- intersect(countries_shp, p_temp_N)%>%
  as.data.frame()
countries_temperateN_intersect$temperateN <- "Y"

countries_temperateS_intersect <- intersect(countries_shp, p_temp_S)%>%
  as.data.frame()
countries_temperateS_intersect$temperateS <- "Y"

countries_shp <- st_as_sf(countries_shp)
countries_shp <- left_join(countries_shp, 
                           countries_temperateS_intersect[,c("ISO3","temperateS")])
countries_shp <- left_join(countries_shp, 
                           countries_temperateN_intersect[,c("ISO3","temperateN")])
countries_shp <- left_join(countries_shp, 
                           countries_tropical_intersect[,c("ISO3","tropical")])

countries_shp$temperate_only <- ifelse((countries_shp$temperateN=="Y"|countries_shp$temperateS=="Y") & is.na(countries_shp$tropical), "Y","N")
countries_shp$tropical_only <- ifelse(countries_shp$tropical=="Y" & is.na(countries_shp$temperateS) & is.na(countries_shp$temperateN), "Y","N")
countries_shp$mixed <- ifelse(countries_shp$tropical_only=="N" & countries_shp$temperate_only=="N", "Y", "N")
countries_shp$lat <- ifelse(countries_shp$temperate_only=="Y","Subtropical and Temperate",ifelse(
  countries_shp$tropical_only=="Y", "Tropical Only", "Mixed"
))
