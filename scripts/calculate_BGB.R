## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)
dir <- "C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/"
dir <- "C:/Users/vsgri/Downloads/"
setwd(dir)

get_metric <- function(pattern, sum_or_mean){
  fls <- list.files("02_17_2023/", pattern=pattern, full.names = T) 
  tbs <- lapply(fls, read.csv)
  stopifnot(nrow(tbs[[1]])==nrow(tbs[[2]]))
  comb <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
  colnames(comb) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
  comb$full_name <- countrycode::countrycode(comb$ISO3, "iso3c","country.name")
  comb <- comb[comb$ISO3!="C--" & comb$ISO3!="P--" & comb$ISO3!="S--",]
  
  #now lets get root:shoot ratios per biome and join with agc per biomeCountry
  #Mokany et al. 2006
  rootshoot <- readxl::read_xlsx("RootShoot.xlsx")
  rootshoot <- rootshoot[1:10,]
  join <- left_join(comb, rootshoot[,c("BIOME_NAME","Equivalent System in Mokany",
                                       "Median Root:Shoot Ratio","Standard Error")], by="BIOME_NAME")
  
  stopifnot(nrow(join)==nrow(comb))
  
  #we join each root:shoot ratio by biome to the biomeCountry aboveground value then
  #multiply aboveground value perBiomeCountry by root:shoot ratio for each biome
  join$crop_bgc_perBiomeCountry <- join$tic*join$`Median Root:Shoot Ratio`
  join$graze_bgc_perBiomeCountry <- join$tip*join$`Median Root:Shoot Ratio`
  join$crop_total_perBiomeCountry <- rowSums(join[, c("tic","crop_bgc_perBiomeCountry")], na.rm=T)
  join$graze_total_perBiomeCountry <- rowSums(join[, c("tip","graze_bgc_perBiomeCountry")], na.rm=T)
  
  #calculate your metric of interest per biomeCountry to country level
  country <- join %>%
    group_by(ISO3) %>%
    summarize(#crop_agc_perCountry= sum_or_mean(tic),
              #graze_agc_perCountry = sum_or_mean(tip),
              
              #crop_bgc_perCountry= sum_or_mean(crop_bgc_perBiomeCountry),
              #graze_bgc_perCountry = sum_or_mean(graze_bgc_perBiomeCountry),
              
              #crop_total_perCountry = sum(crop_bgc_perCountry, crop_agc_perCountry),
              #graze_total_perCountry = sum(graze_bgc_perCountry, graze_agc_perCountry))
              crop_C_perCountry = sum_or_mean(crop_total_perBiomeCountry),
              graze_C_perCountry = sum_or_mean(graze_total_perBiomeCountry))
              
  
  biome <- join %>%
    group_by(BIOME_NAME) %>%
    summarize(#crop_agc_perBiome= sum_or_mean(tic),
              #graze_agc_perBiome = sum_or_mean(tip),
              
              #crop_bgc_perBiome= sum_or_mean(crop_bgc_perBiomeCountry),
              #graze_bgc_perBiome = sum_or_mean(graze_bgc_perBiomeCountry),
              
              #crop_total_perBiome = sum(crop_bgc_perBiome, crop_agc_perBiome),
              #graze_total_perBiome = sum(graze_bgc_perBiome, graze_agc_perBiome))
              crop_C_perBiome = sum_or_mean(crop_total_perBiomeCountry),
              graze_C_perBiome = sum_or_mean(graze_total_perBiomeCountry))
  return(list(country,biome))
  
}



##################################################################

sum_or_mean <- function(x){mean(x, na.rm=T)}
flx <- get_metric(pattern="FluxDensityByBiomeCountry_mean", sum_or_mean = sum_or_mean)
mean(flx[[1]]$crop_C_perCountry, na.rm=T)
mean(flx[[1]]$graze_C_perCountry, na.rm=T)
mean(flx[[2]]$crop_C_perBiome, na.rm=T)
mean(flx[[2]]$graze_C_perBiome, na.rm=T)
#why don't these all equal each other??

sum_or_mean <- function(x){sum(x, na.rm=T)}
cacc <- get_metric(pattern="CarbonAcc",sum_or_mean = sum_or_mean)
sum(cacc[[1]]$crop_C_perCountry)/1000000000/30 #~15 
sum(cacc[[1]]$graze_C_perCountry)/1000000000/30 #~14.2
sum(cacc[[2]]$crop_C_perBiome)/1000000000/30 #~15 
sum(cacc[[2]]$graze_C_perBiome)/1000000000/30 #~14.2
#these match up with each other so that's good
#but crop estimates don't match up with crop flux density
#why?

#compare to Chapman
chp <- read.csv("country_summary.csv")
sum(chp$potential_crop_mg, na.rm=T)/1000000000
sum(chp$potential_pasture_mg, na.rm=T)/1000000000/0.46
#if you divide Chapman by the flux densities from above, takes somewhere from 90-140 years to reach



####################################################################


#get m2 per crop and graze per country
fls <- list.files("02_17_2023/",
                  pattern="AreaByBiomeCountry", full.names = T)
tbs <- lapply(fls, read.csv)
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
colnames(agarea) <- c("ISO3","NAME","BIOME_NAME", "BIOME_NUM", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
#agarea$full_name <- countrycode::countrycode(agarea$GID_0, origin="iso3c", destination="country.name", warn=T)
#agarea[is.na(agarea$full_name),]
#agarea <- agarea[!is.na(agarea$full_name),]
agarea$CropAreaByBiomeCountry_tic_ha <- agarea$tic/10000
agarea$GrazeAreaByBiomeCountry_tip_ha <- agarea$tip/10000

agarea_byCountry <- agarea %>%
  group_by(ISO3) %>%
  summarize(CropArea_ha = sum(CropAreaByBiomeCountry_tic_ha),
         GrazeArea_ha = sum(GrazeAreaByBiomeCountry_tip_ha))

#agarea_byCountry$CropArea_ha_mil <- agarea_byCountry$CropArea_ha/1000000
#agarea_byCountry$GrazeArea_ha_mil <- agarea_byCountry$GrazeArea_ha/1000000

agarea_byBiome <- agarea %>%
  group_by(BIOME_NAME) %>%
  summarize(CropArea_ha = sum(CropAreaByBiomeCountry_tic_ha),
            GrazeArea_ha = sum(GrazeAreaByBiomeCountry_tip_ha))




#get mean forest cover
fls <- list.files("02_17_2023/",
                  pattern="ForestCover", full.names = T)
tbs <- lapply(fls, read.csv)
cover <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
colnames(cover) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))

cover_byCountry <- cover %>%
  group_by(ISO3) %>%
  summarize(Crop_ForestCover_mean = mean(tic, na.rm=T),
            Graze_ForestCover_mean = mean(tip, na.rm=T))
range(cover_byCountry$Crop_ForestCover_mean, na.rm=T)
range(cover_byCountry$Graze_ForestCover_mean, na.rm=T)

cover_byBiome <- cover %>%
  group_by(BIOME_NAME) %>%
  summarize(Crop_ForestCover_mean = mean(tic, na.rm=T),
            Graze_ForestCover_mean = mean(tip, na.rm=T))
range(cover_byBiome$Crop_ForestCover_mean, na.rm=T)
range(cover_byBiome$Graze_ForestCover_mean, na.rm=T)



#get mean delta
fls <- list.files("02_17_2023/",
                  pattern="^Delta", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
colnames(delta) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))

delta_byCountry <- delta %>%
  group_by(ISO3) %>%
  summarize(Crop_Delta_mean = mean(tic, na.rm=T),
            Graze_Delta_mean = mean(tip, na.rm=T))
range(delta_byCountry$Crop_Delta_mean, na.rm=T)
range(delta_byCountry$Graze_Delta_mean, na.rm=T)

delta_byBiome <- delta %>%
  group_by(BIOME_NAME) %>%
  summarize(Crop_Delta_mean = mean(tic, na.rm=T),
            Graze_Delta_mean = mean(tip, na.rm=T))
range(delta_byBiome$Crop_Delta_mean, na.rm=T)
range(delta_byBiome$Graze_Delta_mean, na.rm=T)


#get mean expert Recs per country
fls <- list.files("02_17_2023/",
                  pattern="Expert", full.names = T)
tbs <- lapply(fls, read.csv)
expertRecs <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
colnames(delta) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))

delta_byCountry <- delta %>%
  group_by(ISO3) %>%
  summarize(Crop_Delta_mean = mean(tic, na.rm=T),
            Graze_Delta_mean = mean(tip, na.rm=T))
range(delta_byCountry$Crop_Delta_mean, na.rm=T)
range(delta_byCountry$Graze_Delta_mean, na.rm=T)




#make biome dataframe
biome_df <- inner_join(flx[[2]], cacc[[2]], by=c("BIOME_NAME")) %>%
  inner_join(agarea_byBiome, by="BIOME_NAME") %>%
  inner_join(cover_byBiome, by="BIOME_NAME") %>%
  inner_join(delta_byBiome, by="BIOME_NAME")
biome_df <- subset(biome_df, select=c("BIOME_NAME", "crop_total_perBiome.x",
                                      "graze_total_perBiome.x", "crop_total_perBiome.y",
                                      "graze_total_perBiome.y", "CropArea_ha","GrazeArea_ha" ,         
                                      "Crop_ForestCover_mean", "Graze_ForestCover_mean",
                                      "Crop_Delta_mean", "Graze_Delta_mean"))
colnames(biome_df) <- c("BIOME_NAME", "Crop_FluxDensity_mean_MgC/ha","Graze_FluxDensity_mean_MgC/ha",
                        "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC","Crop_Area_sum_ha","Graze_Area_sum_ha",
                        "Crop_ForestCover_mean_percent", "Graze_ForestCover_mean_percent",
                        "Crop_Delta_mean_percent", "Graze_Delta_mean_percent")
write.csv(biome_df, "02_17_2023/biome_results_02262023.csv")


country_df <- inner_join(flx[[1]], cacc[[1]], by=c("ISO3")) %>%
  inner_join(agarea_byCountry, by="ISO3") %>%
  inner_join(cover_byCountry, by="ISO3") %>%
  inner_join(delta_byCountry, by="ISO3")
country_df <- subset(country_df, select=c("ISO3", "crop_total_perCountry.x",
                                      "graze_total_perCountry.x", "crop_total_perCountry.y",
                                      "graze_total_perCountry.y", "CropArea_ha","GrazeArea_ha" ,         
                                      "Crop_ForestCover_mean", "Graze_ForestCover_mean",
                                      "Crop_Delta_mean", "Graze_Delta_mean"))
colnames(country_df) <- c("ISO3", "Crop_FluxDensity_mean_MgC/ha","Graze_FluxDensity_mean_MgC/ha",
                        "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC","Crop_Area_sum_ha","Graze_Area_sum_ha",
                        "Crop_ForestCover_mean_percent", "Graze_ForestCover_mean_percent",
                        "Crop_Delta_mean_percent", "Graze_Delta_mean_percent")
country_df$country_name <- countrycode::countrycode(country_df$ISO3, "iso3c","country.name")
write.csv(country_df, "02_17_2023/country_results_02262023.csv")
