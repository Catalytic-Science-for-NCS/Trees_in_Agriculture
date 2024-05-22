# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: Trees in Agriculture
# ===============
# Description: Create figure 9-- scatterplot of potential mitigation and flux density
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 07/01/2023
# Date updated: 09/19/2023
# ===============
# load libraries
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggrepel)
library(terra)
library(patchwork)
library(sf)
library(scales)

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------


#read in results by country
cts <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/data/06_21_2023/country_results_06212023.csv")

cts <- cts[,c("ISO3","NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
              "Crop_FluxDensity_mean_MgC.ha.yr", "Graze_FluxDensity_mean_MgC.ha.yr",
              "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta",
             "Crop_Delta_mean_percent","Graze_Delta_mean_percent")]

#################
#make multiple dataframes of each measure of interest into long format then combine together
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
long_flux <- reshape2::melt(cts, id.vars=c("ISO3","NAME"), measure.vars=c("Crop_FluxDensity_mean_MgC.ha.yr","Graze_FluxDensity_mean_MgC.ha.yr"),
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




#################
#create majority tropical/outside of tropics division

countries_shp <- vect("C:/Users/vgriffey/Downloads/countries_shp/countries.shp") %>%
  aggregate("ISO3")

coords_trop <- rbind(c(-180, -23.5), c(-180, 23.5),  c(180, 23.5), c(180, -23.5))
colnames(coords_trop) <- c('x', 'y')
p_trop <- vect(coords_trop, "polygons", crs="epsg:4326")

#get countries that intersect tropics bounding box and mark as intersecting
countries_tropical_intersect <- intersect(countries_shp, p_trop) %>%
  as.data.frame()
countries_tropical_intersect$tropical <- "Y"
countries_tropical_intersect <- countries_tropical_intersect[!duplicated(countries_tropical_intersect$ISO3),]

#join countries that intersect tropics back with full country shapefile
countries_shp <- st_as_sf(countries_shp)
countries_shp <- left_join(countries_shp,
                           countries_tropical_intersect[,c("ISO3","tropical")])

##make dataframe of majority tropical countries
#quick and dirty bc I don't feel like coding
countries_shp_sub <- subset(countries_shp, tropical=="Y")
countries_shp_sub <- subset(countries_shp_sub, COUNTRY != "Argentina" & COUNTRY!="French Southern Territories" & COUNTRY!="Australia"&
                              COUNTRY!="BAHAMAS" & COUNTRY!="Chile" & COUNTRY!="China" & COUNTRY!="Algeria" & COUNTRY!="Egypt" & COUNTRY!="Western Sahara"
                            & COUNTRY!="Saudi Arabia" & COUNTRY!="United States" & COUNTRY!="United States Minor Outlying Islands" & COUNTRY!="South Africa")
countries_shp_sub$tropical_majority <- "Y"
countries_shp_sub <- as.data.frame(countries_shp_sub)
countries_shp <- left_join(st_as_sf(countries_shp),
                           countries_shp_sub[,c("ISO3","tropical_majority")])

#combine finished majority tropical/outside tropics dataframe with long formatted df from earlier
long_updated <- left_join(long, as.data.frame(countries_shp)[,c("ISO3","tropical_majority")], by="ISO3")
long_updated$lat <- ifelse(is.na(long_updated$tropical_majority), "Outside Tropics", "Majority Tropical")
long_updated$lat <- factor(long_updated$lat, levels=c("Majority Tropical","Outside Tropics"))
long_updated$tropical_majority <- NULL


# need to deal only with cropland measurements so we can rank top performing countries within cropland
long_updated_crop <- long_updated[long_updated$Ag.Type=="Crop",]
long_updated_crop <- long_updated_crop[complete.cases(long_updated_crop),]

# #create rank of crop flux density
# order.scores<-order(long_updated_crop$Flux_density, long_updated_crop$ISO3, decreasing = T)
# long_updated_crop$flux_rank <- NA
# long_updated_crop$flux_rank[order.scores] <- 1:nrow(long_updated_crop)
# long_updated_crop$flux_rank <- ifelse(is.na(long_updated_crop$Flux_density), NA, long_updated_crop$flux_rank)
# max(long_updated_crop$flux_rank, na.rm = T)
# long_updated_crop$flux_rank <- ifelse(long_updated_crop$flux_rank <=5, "Global Top 5",ifelse(
#   long_updated_crop$flux_rank >= 172, "Global Bottom 5", "Other Countries")
# )
# 
# #create rank of crop total mitigation in 30 years
# order.scores<-order(long_updated_crop$Total_MgC, long_updated_crop$ISO3 , decreasing = T)
# long_updated_crop$total_MgC_rank <- NA
# long_updated_crop$total_MgC_rank[order.scores] <- 1:nrow(long_updated_crop)
# max(long_updated_crop$total_MgC_rank, na.rm = T)
# long_updated_crop$total_MgC_rank <- ifelse(long_updated_crop$total_MgC_rank <=5, "Global Top 5",ifelse(
#   long_updated_crop$total_MgC_rank >= 172 , "Global Bottom 5", "Other Countries")
# )





# need to deal only with grazing land measurements so we can rank top performing countries within grazing land
long_updated_graz <- long_updated[long_updated$Ag.Type=="Graz",]
long_updated_graz <- long_updated_graz[complete.cases(long_updated_graz),]

# #create rank of grazing flux density
# order.scores <- order(long_updated_graz$Flux_density, long_updated_graz$ISO3, decreasing = T)
# long_updated_graz$flux_rank <- NA
# long_updated_graz$flux_rank[order.scores] <- 1:nrow(long_updated_graz)
# long_updated_graz$flux_rank <- ifelse(is.na(long_updated_graz$Flux_density), NA, long_updated_graz$flux_rank)
# max(long_updated_graz$flux_rank, na.rm = T)
# long_updated_graz$flux_rank <- ifelse(long_updated_graz$flux_rank <=5, "Global Top 5",ifelse(
#   long_updated_graz$flux_rank >= 78, "Global Bottom 5", "Other Countries")
# )
# 
# #create rank of grazing total mitigation in 30 years
# order.scores<-order(long_updated_graz$Total_MgC, long_updated_graz$ISO3 , decreasing = T)
# long_updated_graz$total_MgC_rank <- NA
# long_updated_graz$total_MgC_rank[order.scores] <- 1:nrow(long_updated_graz)
# max(long_updated_graz$total_MgC_rank, na.rm = T)
# long_updated_graz$total_MgC_rank <- ifelse(long_updated_graz$total_MgC_rank <=5, "Global Top 5",ifelse(
#    long_updated_graz$total_MgC_rank >= 78, "Global Bottom 5", "Other Countries")
# )




#combine cropland and grazing land rank datasets back together
long_updated <- rbind(long_updated_crop, long_updated_graz)


# #update labels and factors for various columns
# long_updated$flux_rank <- factor(long_updated$flux_rank, levels=c("Global Top 5","Other Countries","Global Bottom 5"))
# long_updated$total_MgC_rank <- factor(long_updated$total_MgC_rank, levels=c("Global Top 5","Other Countries","Global Bottom 5"))
# 
# 
# long_updated$Rank <- ifelse(long_updated$total_MgC_rank=="Global Top 5" | long_updated$flux_rank=="Global Top 5", "Global Top 5",
#                             ifelse(long_updated$total_MgC_rank=="Global Bottom 5" | long_updated$flux_rank=="Global Bottom 5", "Global Bottom 5", "Other Countries"))
# long_updated$Rank <- factor(long_updated$Rank, levels=c("Global Top 5","Global Bottom 5","Other Countries"))
# 
# 
# long_updated$Rank_group_label <- ifelse(long_updated$flux_rank=="Global Top 5" | long_updated$flux_rank=="Global Bottom 5" ,"flux",
#                                         ifelse(long_updated$total_MgC_rank=="Global Top 5" | long_updated$total_MgC_rank=="Global Bottom 5", "total_mgC", "greyedout"))

long_updated$Ag.Type <- ifelse(long_updated$Ag.Type=="Graz","Grazing Land","Cropland")

long_updated$Geography <- long_updated$lat

#calculate 90th percentile of total mitigation and flux density values 
totmit_crop_90 <- quantile(long_updated$Total_MgC, c(0.90))
totmit_graze_90 <- quantile(cts$Graze_TotalC_sum_MgC, c(0.90))
fluxdens_crop_90 <- quantile(cts$Crop_FluxDensity_mean_MgC.ha.yr, c(0.90), na.rm=T)
fluxdens_graze_90 <- quantile(cts$Graze_FluxDensity_mean_MgC.ha.yr, c(0.90), na.rm=T)


long_updated$total_mitigation_90 <- ifelse(long_updated$Ag.Type=="Cropland", totmit_crop_90, totmit_graze_90)
long_updated$fluxdens_90 <- ifelse(long_updated$Ag.Type=="Cropland", fluxdens_crop_90, fluxdens_graze_90)
long_updated$country_label <- ifelse(long_updated$Flux_density<long_updated$fluxdens_90 & long_updated$Total_MgC<long_updated$total_mitigation_90, "label", "nolabel")
long_updated$country_label[long_updated$NAME=="Cambodia"] <- "label"


#write.csv(long_updated, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/figure9_outputData.csv")

## FIGURE 9 time to plot
labss <- c("Top 5 Flux Density", "Bottom 5 Flux Density","Top 5 Mean Annual Removal","Bottom 5 Mean Annual Removal", "Other Countries")

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="right", panel.grid.major.x=element_blank())
theme = theme_update(axis.line.y = element_blank(), axis.text.y = element_text(colour="grey20"), axis.ticks.y= element_line(colour="grey20"))

fig9 <- ggplot(data=long_updated[!is.na(long_updated$AgArea_ha),],
       aes(x=Total_MgC/30/1e+9*(44/12), y=Flux_density*(44/12), fill=lat, color=lat))+ #modifying total_MgC and flux density to convert from C to CO2 and to take total MgC from 30 years to annual and to Pg
  geom_hline(aes(yintercept = fluxdens_90*(44/12)), color = "grey40", linetype="dashed")+
  geom_vline(aes(xintercept = total_mitigation_90/30/1e+9*(44/12)), color = "grey40", linetype="dashed")+
  geom_point(shape=21)+
  facet_grid(~Ag.Type)+
  ggrepel::geom_label_repel(inherit.aes = F,
                            data = long_updated[long_updated$country_label!="label",],
                            aes(x=Total_MgC/30/1e+9*(44/12), y=Flux_density*(44/12), label=NAME),
                            size=2.2,
                            nudge_y = 0.25,
                            segment.size  = 0.15,
                            min.segment.length = 0.1,
                            box.padding = 0.10,
                            label.padding = 0.10,
                            max.overlaps = 3,
                           color="grey40")+
  ggrepel::geom_label_repel(inherit.aes = F,
                            data = long_updated[long_updated$country_label=="label",],
                            aes(x=Total_MgC/30/1e+9*(44/12), y=Flux_density*(44/12), label=NAME),
                            size=2.2,
                             nudge_y = 0.25,
                            segment.size  = 0.15,
                            min.segment.length = 0.2,
                            box.padding = 0.10,
                            label.padding = 0.10,
                            max.overlaps = 5,
                            color="grey40")+
  labs(x=expression(Estimated~Total~Annual~Carbon~Dioxide~Removal~Potential~(Pg~CO[2]~Yr^{-1})), y=expression(atop(Estimated~Annual~Carbon~Dioxide~Removal~Potential~Per~Hectare,
                                                                                        (Mg~CO[2]~Ha^{-1}~Yr^{-1}))))+
  theme(strip.placement = "outside",
        panel.spacing = unit(1, "lines"),
        legend.spacing.y = unit(0.75, 'cm'))+
  scale_fill_discrete(name="Geography")+
  scale_colour_discrete(name="Geography");fig9

ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/Draft Figures/draft_fig8_annualPotential_fluxDensity_90.png",
       fig9, width=10, height=5, bg='white')      
