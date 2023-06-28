# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: Distributed tree restoration (for CLUA)
# ===============
# Description: calculate per parcel increases in cover and ha of cover based on TIA expert recs
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 03/07/2023
# Date updated: 06/20/2023
# ===============
# load libraries
library(tidyverse)
library(terra)
library(ggplot2)
library(sf)
library(MatchIt)
library(cobalt)
library(exactextractr)
setwd("")
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

## calculate mean and sum hectares of cover and delta in t0 and t5

huila <- vect("Colombia/Departamentos_202112_shp/departamento.shp") 
huila <- huila[huila$DeNombre=="Huila",] #1,813,707 ha total
recs <- vect("TIA/expertRecs_lessComplex.shp")
recs_huila <- subset(recs, recs$COUNTRY=="Colombia") %>% project(huila) %>% crop(huila)

#clipping done in Arc
#tree cover from 2020 based on trees in mosaic landscapes dataset
#replace with 2015 and 2010 (NASA GFCC) data
#replace with Mato Grosso
fc <- rast("Colombia/huila_gee/tree_cover_2020_huila.tif") %>%
  project("epsg:9377", method="bilinear")

#create interior buffers of 10m 
bufs <- buffer(polys, width=-10)
bufs_inner <- erase(polys, bufs)
bufs_inner$buffer_area_ha <- expanse(bufs_inner)/10000


#creatr matrix where each row is parcel and columns are tree cover and ha facts about parcel
area_parcel_m2_list <- c()
percent_parcel_in_forest_cover_list <- c()
area_buffer_available_planting_m2_list<- c()
fieldid_list <- c()
area_buffer_m2_list <- c()
buffer_over_parcel_percent_list <- c()
for(i in unique(polys$Field_ID)){
  
  #look at cover in just the parcel  
  r <- crop(fc, polys[polys$Field_ID==i,]) %>% mask(polys[polys$Field_ID==i,])
    
  #Extracts the values of cells in a raster that are covered by polygons
  #Returns a summary of the extracted values
  #so do that for parcel
  x_parcel <- exact_extract(raster::raster(r), st_as_sf(polys[polys$Field_ID==i,]),
                          coverage_area = TRUE, include_area=TRUE, include_xy=TRUE) %>%
      bind_rows()
  x_parcel$percent_in_parcel <- x_parcel$coverage_area/x_parcel$area
  
  #now for just the buffer  
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

#this df is what we want
df <- cbind(fieldid_list, area_parcel_m2_list) %>% 
  cbind(percent_parcel_in_forest_cover_list) %>% 
  cbind(area_buffer_available_planting_m2_list) %>%
  cbind(area_buffer_m2_list) %>%
  cbind(buffer_over_parcel_percent_list) %>%
  as.data.frame()

write.csv(df, "per_parcel_stats.csv")

##### extra
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




