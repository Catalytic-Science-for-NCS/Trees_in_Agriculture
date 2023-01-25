library(tidyverse)
library(terra)
library(ggplot2)
library(sf)

deps <- vect("Colombia/Departamentos_202112_shp/departamento.shp") %>%
  terra::project("epsg:4326") %>% subset(.$DeNombre=="Huila")

#clipped ecoregions to huila in arc b/c R was very unhappy with full ecos shp
ecos <- vect("Colombia/Mapa_Ecosistemas_Colombia_2017/E_ECCMC_Ver21_100K_Huila.shp") %>%
  terra::project("epsg:4326") 
#using unidad_biotic as ecoregions
uni_bio <- aggregate(ecos, "UNI_BIOTIC")


#distance from roads aka CAN I DRIVE HERE
#will want to get distance of road from center of mgmt unit
rds <- vect("Colombia/Roads/roads_huila.shp") %>% terra::project("epsg:9377")
rds$road_ID <- rep(1:nrow(rds))


### CLOUDS AND BIG MOUNTAINS AND SUCH
#era5 instead of wordlclim. check out ERA5_climate_norms in GEE for script to create input
#points are raster centroids of huila
#27830 meters res

clim <- vect("Colombia/Climate/temp_rainfall_huila.shp") %>% terra::project("epsg:9377")
clim$pt_ID_clim <- rep(1:nrow(clim))

elv <- vect("Colombia/elevation_slope/elevation_CO.shp")%>% terra::project("epsg:9377")
elv$pt_ID_elv <- rep(1:nrow(elv))

slp <- vect("Colombia/elevation_slope/slope_CO.shp")%>% terra::project("epsg:9377")
slp$pt_ID_slp <- rep(1:nrow(slp))

##cool but not necessary
# better_rasterize <- function(v) {
#   r1 <- rast(v)
#   nams <- names(v)
#   lapply(nams, function(x) {
#     rasterize(v, r1,
#               field = x,
#               touches = TRUE
#     )
#   })
# }
# 
# s <- lapply(list(clim, elv, slp), better_rasterize)
# # Merge (bind) all objects. cleaner list
# s <- do.call("c", s)
# names(s) <- c(names(clim), names(elv), names(slp))


#clipped ecoregions to huila in arc b/c R was very unhappy with full ecos shp
ecos <- vect("Colombia/Mapa_Ecosistemas_Colombia_2017/E_ECCMC_Ver21_100K_Huila.shp") %>%
  terra::project("epsg:9377") 
#using unidad_biotic as ecoregions
uni_bio <- aggregate(ecos, "UNI_BIOTIC")

################
#let's load in those treelines
#artisanl hand drawn features in ArcGIS Pro project "Colombia" 
lns <- vect("Colombia/10_lines_huila.shp") %>% terra::project("epsg:9377")
polys <- vect("Colombia/polys_10_huila.shp") %>% terra::project("epsg:9377")
ctds <- centroids(polys)


# locate for each field the nearest covariate
nearest_things <- st_join(st_as_sf(ctds), st_as_sf(slp), join = st_nearest_feature) %>%
  st_join(st_as_sf(elv), join= st_nearest_feature) %>%
  st_join(st_as_sf(clim), join=st_nearest_feature) %>%
  st_join(st_as_sf(rds), join= st_nearest_feature) %>%
  st_join(.,st_as_sf(uni_bio[,c("UNI_BIOTIC")]))

for(i in 1:nrow(nearest_things)){
  tmp <- nearest_things[i,]
  ind <- tmp$road_ID
  d <- distance(vect(tmp), rds[rds$road_ID==ind,])
  nearest_things[i,"road_ID_distance"] <- d
}  

treelines <- merge(lns, as.data.frame(nearest_things), by="Field_ID")
treelines <- treelines %>% st_as_sf() %>% select(c("Field_ID", "Shape_Leng.x","Tree", "Shape_Area","slope" , "pt_ID_slp", "elevation",
                                        "mean_2m_ai","v_componen","mean_sea_l" , "minimum_2m","dewpoint_2","maximum_2m",
                                        "total_prec", "surface_pr", "u_componen", "road_ID", "UNI_BIOTIC",      
                                        "geometry", "road_ID_distance")) %>% vect()
names(treelines) <- c("Field_ID", "field_line_length","Trees_PA", "Field_Area","slope", "pt_ID_slp", "elevation",
                      "mean_2m_air_temperature","v_component_wind","mean_sea_level_pressure", 
                      "minimum_2m_air_temperature","dewpoint_2m_temperature","maximum_2m_air_temperature",
                      "total_precip", "surface_pressure", "u_component_wind", "road_ID", "UNI_BIOTIC",      
                      "dist2road")

total_perim <- as.data.frame(treelines) %>%
  group_by(Field_ID) %>%
  summarise(total_field_perim = sum(field_line_length)) 
treelines <- merge(treelines, total_perim)

perim_percent <- as.data.frame(treelines) %>%
  group_by(Field_ID,Trees_PA) %>%
  summarise(percent_perim_in_treeline = sum(field_line_length)/total_field_perim)
#perim_percent <- perim_percent[!duplicated(perim_percent),]

treelines <- merge(treelines, perim_percent)


