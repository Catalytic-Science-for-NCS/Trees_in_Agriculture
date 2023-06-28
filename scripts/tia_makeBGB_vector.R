
########################################
#### rasterize bgb table for upload to google earth engine
#### in order to make maps for figures

library(terra)
library(tidyverse)
library(sf)

#Mokany et al. 2006
rootshoot <- readxl::read_xlsx("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/data/RootShoot.xlsx")
rootshoot <- rootshoot[1:11,]

#biome vector
ecos <- vect("C:/Users/vgriffey/Downloads/Ecoregions2017/Ecoregions2017.shp") %>%
  project("epsg:4326")

bios <- aggregate(ecos, by='BIOME_NUM', dissolve=T)
bios_sim <- simplifyGeom(bios, tolerance=0.1, preserveTopology=TRUE, makeValid=TRUE)
bios_sim <- bios_sim[bios_sim$BIOME_NAME!="Mangroves" & bios_sim$BIOME_NAME!="NA" & bios_sim$BIOME_NAME!="Flooded Grasslands & Savannas",]
bios_sim <- project(bios_sim, "epsg:4326")

bios_sim <- st_as_sf(bios_sim) %>% subset(select=c(BIOME_NUM, BIOME_NAME))
pairs <- inner_join(bios_sim, rootshoot, by=c("BIOME_NAME")) %>%
  vect()
names(pairs) <- c("BIOME_NUM","BIOME_NAME","Equivalent_Mokany_system","Median_rootShoot_ratio","SE")
writeVector(pairs, "TIA/biome_BGB.shp", overwrite=T)
