## COLOMBIA TREELINE ##

library(tidyverse)
library(terra)
library(ggplot2)
library(sf)

bounds <- vect("Colombia/Colombia_country_border/co_boundary.shp")
x1 <- rbind(  c(-65, -4.5),c(-80,-4.5), c(-80,13.25),c(-65,13.25))
colnames(x1)[1:2] <- c('x', 'y')
p <- vect(x1, "polygons")

bds <- terra::intersect(bounds, p)
bds <- project(bds, "EPSG:4326")
vecs <- vect("Colombia/test_pts_buffer.shp") %>%
  terra::project("epsg:4326")

##load in coffee shapefiles
cof <- list.files("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Verra ARR Methodology/Colombia/POLIGONOS VISITA EQUIPO ESTADOS UNIDOS/POLIGONOS VISITA EQUIPO ESTADOS UNIDOS/", 
                       pattern="*.shp$", full.names = T)
cof <- lapply(cof, vect)


#hold constant by one ecoregion/biome
#hold constant by department (Huila for now)
#mask by it
deps <- vect("Colombia/Departamentos_202112_shp/departamento.shp") %>%
  terra::project("epsg:4326") %>% subset(.$DeNombre=="Huila")

#clipped ecoregions to huila in arc b/c R was very unhappy with full ecos shp
ecos <- vect("Colombia/Mapa_Ecosistemas_Colombia_2017/E_ECCMC_Ver21_100K_Huila.shp") %>%
  terra::project("epsg:4326") 
#using unidad_biotic as ecoregions
uni_bio <- aggregate(ecos, "UNI_BIOTIC")

#mask by ag land cover here
#maybe also Millie Chapman's? use 
#landcov <- vect("Colombia/COBERTURAS CORINE 2018/shape coberturas 2018/cobertura_tierra_clc_2018.shp")
#above is difficult to use, using chapman for now
# chp <- rast("TIA/GEE_downloads_US/chapman_TIC_mosaic_CO.tif")
# chp_huila <- crop(chp,deps) %>% mask(deps)
# chp_huila <- ifel(chp_huila!=-999, 1, 0)

#distance from roads
#will want to get distance of road from center of mgmt unit
rds <- vect("Colombia/Roads/roads_huila.shp") %>% terra::project("epsg:4326")


#combine rasters together
#clim_l <- rast(list.files("Colombia/WorldClim/", "*.tif$", full.names = T))
#harris <- rast(list.files("Colombia/Harris_datasets/", "*px.tif$", full.names = T, recursive = T))%>%
#  crop(bds)
#era5 instead of wordlclim. check out ERA5_climate_norms in GEE for script to create input
#points are raster centroids of huila
#27830 meters res

clim <- vect("Colombia/Climate/temp_rainfall_huila.shp") %>% terra::project("epsg:9377")
elv <- vect("Colombia/elevation_slope/elevation_CO.shp")%>% terra::project("epsg:9377")
slp <- vect("Colombia/elevation_slope/slope_CO.shp")%>% terra::project("epsg:9377")

better_rasterize <- function(v) {
  r1 <- rast(v)
  nams <- names(v)
  lapply(nams, function(x) {
    rasterize(v, r1,
              field = x,
              touches = TRUE
    )
  })
}

s <- lapply(list(clim, elv, slp), better_rasterize)
# Merge (bind) all objects. cleaner list
s <- do.call("c", s)
names(s) <- c(names(clim), names(elv), names(slp))


#mask rasters and clip vectors to project areas 

#combine rasters into dataframe first and have that df ready to aggregate by polygon 
#what do we do with edge pixels? keep them in 


dummy_ps <- cof[c(11, 13, 15)] %>% do.call(rbind, .)
if(crs(dummy_ps)!=crs(clim)){
  dummy_ps <- terra::project(dummy_ps, "EPSG:4326")
}

ancillary_vec <- list(uni_bio, rds, clim, elv, slp)
ancillary_rast <- c(chp_huila)

new_v <- lapply(ancillary_vec, function(i){
  v <- terra::intersect(i, dummy_ps)
})


