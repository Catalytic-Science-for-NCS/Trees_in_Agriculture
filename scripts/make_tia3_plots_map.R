library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

## create map of treeline lit review studies

lit <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Treeline_Literature_Review/Most recent databases/treeline_database_complete.csv") %>%
  subset(select=c(study.id ,site.id, site.sitename,site.state,site.country, lat, lon, 
                  lat_dec, lat_deg,lat_min, n_s, long_dec, long_deg, long_min, e_w, 
                  other.reference, masl, mat, map))
lit$lat <- as.numeric(lit$lat)
lit$lon <- as.numeric(lit$lon)
lit <- lit[!is.na(lit$lat) | !is.na(lit$lon),]


coords <- st_as_sf(lit, coords=c("lon", "lat"),crs=4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
fornow <- inner_join(world, lit, by=c("name"="site.country"))
sf_use_s2(FALSE)

agg <- fornow %>%
  group_by(study.id) %>% 
  tally()

yup <- inner_join(agg, lit[, c("study.id","site.country")])
yup <- yup[!duplicated(yup),]

st_yup <- st_join(yup, world, by=c("site.country"="name"))

ggplot(data = st_yup) +
  geom_sf(aes(fill = n))

ggplot(data = world) +
  geom_sf()+
  geom_sf(data=test) +
  coord_sf(expand = FALSE)+
  geom_text(data = lit, aes(x=lon, y=lat, label = site.country), size = 2)
  
