library(tidyverse)
library(terra)
library(ggplot2)
library(sf)
library(MatchIt)
library(cobalt)
library(exactextractr)

setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/")

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
polys <- vect("Colombia/polys_huila.shp") %>% terra::project("epsg:9377")
polys$area_new_ha <- expanse(polys)/10000
ctds <- centroids(polys[polys$BUFF_DIST==0,])

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

treelines <- merge(polys, as.data.frame(nearest_things), by="Field_ID")
treelines <- treelines %>% st_as_sf() %>% select(c("Field_ID","area_new_ha.x","slope" , "elevation",
                                                   "mean_2m_ai","v_componen","mean_sea_l" , "minimum_2m","dewpoint_2","maximum_2m",
                                                   "total_prec", "surface_pr", "u_componen", "UNI_BIOTIC",      
                                                   "geometry", "road_ID_distance")) %>% vect()
names(treelines) <- c("Field_ID","area_ha","slope", "elevation",
                      "mean_2m_air_temperature","v_component_wind","mean_sea_level_pressure", 
                      "minimum_2m_air_temperature","dewpoint_2m_temperature","maximum_2m_air_temperature",
                      "total_precip", "surface_pressure", "u_component_wind", "UNI_BIOTIC",      
                      "dist2road")


treelines$field_perimeter_m <- perim(treelines)

# perim_percent <- as.data.frame(treelines) %>%
#   group_by(Field_ID,Trees_PA) %>%
#   summarise(percent_perim_in_treeline = sum(field_line_length)/total_field_perim)
# #perim_percent <- perim_percent[!duplicated(perim_percent),]
# 
# treelines <- merge(treelines, perim_percent)


treelines$trt_ID <- ifelse(treelines$Field_ID==12,1,0)
treelines$UNI_BIOTIC <- as.factor(treelines$UNI_BIOTIC)
treelines$dist2road <- as.numeric(treelines$dist2road)

####################################################

## calculate mean and sum hectares of cover and delta in t0 and t5

# delta_t0 <- rast("Colombia/huila_delta_30m_ag_t0_epsg9377.tif")
# delta_t5 <- rast("Colombia/huila_delta_30m_ag_t5_epsg9377.tif")
# 
# cover_t0 <- rast("Colombia/huila_cover_30m_ag_t0_epsg9377.tif")
# cover_t5 <- rast("Colombia/huila_cover_30m_ag_t5_epsg9377.tif")

huila <- vect("Colombia/Departamentos_202112_shp/departamento.shp") 
huila <- huila[huila$DeNombre=="Huila",] #1,813,707 ha total
recs <- vect("TIA/expertRecs_lessComplex.shp")
recs_huila <- subset(recs, recs$COUNTRY=="Colombia") %>% project(huila) %>% crop(huila)


fc <- rast("Colombia/huila_gee/tree_cover_2020_huila.tif") %>%
  project("epsg:9377", method="bilinear")
#f_nf <- ifel(fc>25,1,2)
# 
bufs <- buffer(polys, width=-10)
bufs_inner <- erase(polys, bufs)
bufs_inner$buffer_area_ha <- expanse(bufs_inner)/10000
# 
# 
# #for by field measures
# fieldid <- unique(polys$Field_ID)
# parcel_area <- c()
# to_add <- c()
# delta <- c()
# 
# for(i in fieldid){
#   r <- crop(fc, polys[polys$Field_ID==i,])
#   p <- as.polygons(r, dissolve=F) %>%
#     crop(polys[polys$Field_ID==i,])
#   
#   p$expertrec <- 42
#   p$delta <- p$expertrec-p$tree_canopy_cover
#   p$area_ha <- expanse(p)/10000
#   
#   p_buf <- crop(p, bufs_inner[bufs_inner$Field_ID==i,])
#   p_buf$area_ha <- expanse(p_buf)/10000
# 
#   sum(p_buf$area_ha) 
#   #do we need to check further into how much of parcel is >25%?
#   stopifnot(range(p_buf$tree_canopy_cover)[[2]]<=26) 
#   
#   plot(p, "delta")
#   plot(p_buf, add=T)
#   
#   #what is total parcel area?
#   parcel_area <- c(parcel_area, sum(p$area_ha))
#   #do we need to check further into how much of parcel is >25%? 
#   stopifnot(range(p$tree_canopy_cover)[[2]]<=26) 
#   #what is delta area?
#   to_add <- c(to_add, sum(p$delta/100*p$area_ha))
# 
# }
# 
# df <- cbind(fieldid, parcel_area, to_add) %>% as.data.frame()
# 

#for by pixel measures
#Make a matrix where each row is a pixel
#fill in pixel tree cover
#percent of pixel that is within buffer
#area of pixel in buffer
#percent of pixel within parcel
#area of pixel within parcel
#

area_parcel_m2_list <- c()
percent_parcel_in_forest_cover_list <- c()
area_buffer_available_planting_m2_list<- c()
fieldid_list <- c()
area_buffer_m2_list <- c()
buffer_over_parcel_percent_list <- c()

for(i in unique(polys$Field_ID)){
    
  r <- crop(fc, polys[polys$Field_ID==i,]) %>% mask(polys[polys$Field_ID==i,])
    
  x_parcel <- exact_extract(raster::raster(r), st_as_sf(polys[polys$Field_ID==i,]),
                          coverage_area = TRUE, include_area=TRUE, include_xy=TRUE) %>%
      bind_rows()
  x_parcel$percent_in_parcel <- x_parcel$coverage_area/x_parcel$area
    
  x_buffer <- exact_extract(raster::raster(r), st_as_sf(bufs_inner[bufs_inner$Field_ID==i,]), 
                              coverage_area = TRUE, include_area=TRUE, include_xy=TRUE) %>%
      bind_rows()
  x_buffer$percent_in_parcel <- x_buffer$coverage_area/x_buffer$area
    
  r_df <- as.data.frame(r, xy=T, na.rm=T) %>% 
    inner_join(x_parcel[,c("x","y","area", "coverage_area", "percent_in_parcel")], by=c("x","y")) %>%
    left_join(x_buffer[,c("x","y","coverage_area", "percent_in_parcel")], by=c("x","y"))
  names(r_df) <- c("x","y","tree_canopy_cover","pixelarea_m2","area_pixel_in_parcel_m2","percent_pixel_in_parcel",
                   "area_pixel_in_buffer_m2", "percent_pixel_in_buffer")
  
  area_buffer_m2 <- sum(r_df$area_pixel_in_buffer_m2, na.rm = T)
  area_parcel_m2 <- sum(r_df$area_pixel_in_parcel_m2, na.rm = T)
  
  r_df$forest_area_in_pixel_in_buffer_m2 <- r_df$tree_canopy_cover*r_df$area_pixel_in_buffer_m2/100
  r_df$forest_area_in_pixel_in_parcel_m2 <- r_df$tree_canopy_cover*r_df$area_pixel_in_parcel_m2/100
  
  percent_parcel_in_forest_cover <- sum(r_df$forest_area_in_pixel_in_parcel_m2, na.rm = T)/area_parcel_m2
  forest_cover_in_buffer_as_total_parcel_m2 <- sum(r_df$forest_area_in_pixel_in_buffer_m2, na.rm = T)/area_parcel_m2
  delta_percent <- 42 - percent_parcel_in_forest_cover
  area_parcel_available_planting_m2 <- area_parcel_m2*delta_percent/100
  area_buffer_available_planting_m2 <- (1-forest_cover_in_buffer_as_total_parcel_m2)*area_buffer_m2
  area_parcel_outside_buffer_in_forest_cover_percent <- (percent_parcel_in_forest_cover - forest_cover_in_buffer_as_total_parcel_m2)*area_parcel_m2
  buffer_over_parcel_percent <- area_buffer_m2/area_parcel_m2
  print(buffer_over_parcel_percent > delta_percent)
  #if answer to above is > delta_percent, adjust to be within TIA threshold
  
  area_parcel_m2_list <- c(area_parcel_m2_list, area_parcel_m2)
  percent_parcel_in_forest_cover_list <- c(percent_parcel_in_forest_cover_list, percent_parcel_in_forest_cover)
  area_buffer_available_planting_m2_list <- c(area_buffer_available_planting_m2_list, area_buffer_available_planting_m2)
  fieldid_list <- c(fieldid_list, i)
  area_buffer_m2_list <- c(area_buffer_m2_list, area_buffer_m2)
  buffer_over_parcel_percent_list <- c(buffer_over_parcel_percent_list, buffer_over_parcel_percent)
}
df <- cbind(fieldid_list, area_parcel_m2_list) %>% 
  cbind(percent_parcel_in_forest_cover_list) %>% 
  cbind(area_buffer_available_planting_m2_list) %>%
  cbind(area_buffer_m2_list) %>%
  cbind(buffer_over_parcel_percent_list) %>%
  as.data.frame()


tline_df <- inner_join(as.data.frame(treelines), as.data.frame(polys), by="Field_ID")
names(tline_df) <- c("Field_ID", "area_ha","slope_degrees","elevation_m", "mean_2m_air_temperature_K","v_component_wind_m/s",
                     "mean_sea_level_pressure_pascal","min_2m_air_temperature_K","dewpoint_2m_temperature_K",
                     "max_2m_air_temperature_K","total_precip_m","surface_pressure_pascal", "u_component_wind_m/s",
                     "biotic_state","distance2road_m","field_perimeter_m","trt_ID","Shape_Length","Tree_Presence",
                     "BUFF_DIST","ORIG_FID","Shape_Length_1","Shape_Area","area_new_ha")
#write.csv(tline_df, "Colombia/project_control_measurements.csv")


###########################################
# matching!
#matchmaker matchmaker make me a match

library(tidyverse)
library(ggplot2)
library(MatchIt)
library(cobalt)

setwd("")
tline_df <- read.csv("Colombia/project_control_measurements.csv")


### Matching approach #1: using nearest neighbor matching 
#with mahalanobis distance 
#without replacement
m.out0 <- matchit(trt_ID ~ area_new_ha + slope_degrees + elevation_m + mean_2m_air_temperature_K + 
                    min_2m_air_temperature_K +max_2m_air_temperature_K + total_precip_m + distance2road_m,
                  data = tline_df,
                  method = NULL, distance = "mahalanobis")
summary(m.out0)

m.out1 <- matchit(trt_ID ~ area_new_ha + slope_degrees + elevation_m + mean_2m_air_temperature_K + 
                    min_2m_air_temperature_K +max_2m_air_temperature_K + total_precip_m + distance2road_m,
                  data = tline_df,
                  method = "nearest", distance = "mahalanobis")
m.out1
summary(m.out1, un = FALSE)


treat_group_mahal <- match.data(m.out1, group = 'treat')
control_group_mahal <- match.data(m.out1, group = 'control')

# new.names <- c(Hectares = "Burnt area (ha)",
#                Mean_acc = "Accessibity (travel time)",
#                Mean_elev = "Elevation (m)",
#                Mean_slope = "Slope (degree)",
#                predist = "Pre-fire sagebrush cover (%)",
#                tmax = "Maximum temperature (°C)",
#                tmean = "Mean temperature (°C)",
#                tmin = "Minimum Temperature (°C)",
#                total_ppt = "Precipitation (mm)"
# )

library(cobalt)
love.plot(m.out1, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = FALSE, 
          thresholds = c(m = .1),
          #var.names = new.names,
          colors = c("red", "blue"),
          #shapes = c("triangle filled", "circle filled"),
          #size = c(6,6),
          sample.names = c("Unmatched", "Matched"))
          #limits = c(0, .51),
          #position = c(0.85, .25)) +
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1),
        legend.text = element_text(size=24),
        legend.title = element_text(size=26)) +
  theme(axis.text = element_text(size = 24),
        axis.title.x = element_text(size = 26),
        plot.title = element_text(size = 26))   
#ggsave("outputs/loveplot_balance_mahal.jpeg",plot = last_plot())

plot(m.out1, type = "ecdf")
plot(m.out1, type = "qq")

#density plot
bal.plot(m.out1, type = "density",which="both",sample.names = c("Unmatched", "Matched"))  




### Matching approach #2: matching with GLM using Mahalanobis distance 
# with propensity score estimated for use in a caliper
# with replacement
match_mahvars_ps <- matchit(trt_ID ~ area_new_ha + slope_degrees + elevation_m + mean_2m_air_temperature_K + 
                              min_2m_air_temperature_K +max_2m_air_temperature_K + total_precip_m + distance2road_m,
                            data = tline_df,
                            distance = "glm",
                            mahvars = ~ area_new_ha + slope_degrees + elevation_m + mean_2m_air_temperature_K + 
                              min_2m_air_temperature_K +max_2m_air_temperature_K + total_precip_m + distance2road_m,
                            caliper = 9, ratio = 3, replace=T)

match_mahvars_ps

summary(match_mahvars_ps)

# extract control and treated groups
treat_group <- match.data(match_mahvars_ps,group = 'treat')
control_group <- match.data(match_mahvars_ps,group = 'control')
#write.csv(treat_group,'outputs/treat_group_mahvars_ps.csv',row.names = F)
#write.csv(control_group,'outputs/control_group_mahvars_ps.csv',row.names = F)
#-------------

### Diagnostic of matching to assess balance of covariates

#love plot
# new.names <- c(Hectares = "Burnt area (ha)",
#                Mean_acc = "Accessibity (travel time)",
#                Mean_elev = "Elevation (m)",
#                Mean_slope = "Slope (degree)",
#                predist = "Pre-fire sagebrush cover (%)",
#                tmax = "Maximum temperature (°C)",
#                tmean = "Mean temperature (°C)",
#                tmin = "Minimum Temperature (°C)",
#                total_ppt = "Precipitation (mm)"
# )

love.plot(match_mahvars_ps, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = FALSE) 

#ggsave("outputs/loveplot_mahvars_ps.jpeg",plot = last_plot())
#ggsave("xxx",plot = last_plot(),width = 20,height = 15) 


#eCDF plot
plot(match_mahvars_ps, type = "ecdf")
#eQQ plot
plot(match_mahvars_ps, type = "qq")
#density plot
plot(match_mahvars_ps, type = "density")


# match pairs
match_mahvars_data <- match.data(match_mahvars_ps)


#write.csv(tb$sum.matched,'matching_output_summary.csv',row.names = T)
