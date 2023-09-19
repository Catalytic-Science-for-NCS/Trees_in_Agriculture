# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: Trees in Agriculture
# ===============
# Description: calculate total (ag and bg) carbon per country and globally per year per ha
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 07/01/2023
# Date updated: 09/19/2023
# ===============
# load libraries
library(tidyverse)
library(sf)
dir <- "C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/"
setwd(dir)
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

### First let's calculate agricultural area country
#used for flux density

#get m2 per crop and graze per country
fls <- list.files(paste0(dir, "data/06_21_2023/"),
                  pattern="*.area*.*all", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NAME", "BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME", "BIOME_NUM")])

#combine crop and grazing together if the rows are the same, otherwise break
if (nrow(tbs[[1]])==nrow(tbs[[2]])) { 
  agarea <- cbind(tbs[[1]], tbs[[2]][,"sum"]) %>%
    select(-X)
} else {
  break
}

#your basenames should have tic (trees in croplands) or tip (trees in pasturelands) in their names
colnames(agarea) <- c( "BIOME_NAME","BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
#use countrycode package to get consistent country names from ISO3 codes
agarea$full_name <- countrycode::countrycode(agarea$ISO3, "iso3c","country.name")
agarea <- agarea[agarea$ISO3!="C--" & agarea$ISO3!="P--" & agarea$ISO3!="S--",] #remove 3 weird small islands
agarea[agarea==0] <- NA
agarea <- agarea[!duplicated(agarea[,c("ISO3", "BIOME_NAME","BIOME_NUM")]),]

#calculate amount of crop and grazing land per country
agarea_byCountry <- agarea %>%
  group_by(ISO3) %>%
  summarize(CropArea_ha_positive = sum(tic, na.rm=T),
            GrazeArea_ha_positive = sum(tip, na.rm=T))
agarea_byCountry$ADM0_CODE <- countrycode::countrycode(agarea_byCountry$ISO3, "iso3c","gaul")
agarea_byCountry$full_name <- countrycode::countrycode(agarea_byCountry$ISO3, "iso3c","country.name")

#calculate amount of crop and grazing land per biome
agarea_byBiome <- agarea %>%
  group_by(BIOME_NAME, BIOME_NUM) %>%
  summarize(CropArea_ha_positive = sum(tic, na.rm=T),
            GrazeArea_ha_positive = sum(tip,na.rm = T))

#calculate amount of crop and grazing land per tropics/non-tropics (based on biomes)
agarea_tropicnontropic <- agarea %>% 
  mutate(tropic_nontropic= case_when(BIOME_NAME=="Deserts & Xeric Shrublands" ~ "Nontropic",
                                    BIOME_NAME=="Mediterranean Forests, Woodlands & Scrub" ~ "Nontropic",
                                    BIOME_NAME=="Montane Grasslands & Shrublands" ~ "Nontropic",
                                    BIOME_NAME=="Temperate Broadleaf & Mixed Forests" ~ "Nontropic",
                                    BIOME_NAME=="Temperate Conifer Forests" ~ "Nontropic",
                                    BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" ~ "Nontropic",
                                    BIOME_NAME=="Tropical & Subtropical Coniferous Forests" ~ "Tropic",
                                    BIOME_NAME=="Tropical & Subtropical Dry Broadleaf Forests" ~ "Tropic",
                                    BIOME_NAME=="Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Tropic",
                                    BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" ~ "Tropic",
                                    BIOME_NAME=="Boreal Forests/Taiga"~ "Nontropic")) %>%
  group_by(tropic_nontropic) %>%
  summarize(CropArea_ha_positive = sum(tic, na.rm=T),
            GrazeArea_ha_positive = sum(tip,na.rm = T))


crop_extent_positive = sum(agarea$tic, na.rm = T)
graze_extent_positive = sum(agarea$tip, na.rm = T)


### Then let's calculate actual total mitigation 

#function to get files based on filename pattern, check that they fit together, and combine into one dataframe
#also adds in belowground carbon using root:shoot ratios by biome

get_metric <- function(pattern, sum_or_mean_column){
  #load files into list
  fls <- list.files(paste0(dir, "data/06_21_2023/"), pattern=pattern, full.names = T)
  #fls <- fls[grep("boreal",fls, invert=T)]
  tbs <- lapply(fls, read.csv)
  
  #check to make sure they are the same dimensions and combine
  check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY","BIOME_NUM")])
  if((nrow(tbs[[1]])==nrow(tbs[[2]])) & check){
    comb <- cbind(tbs[[1]], tbs[[2]][,"sum"]) %>%
      select(-X)
  } else {
    break
  }
  
  #clean up
  colnames(comb) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
  comb$full_name <- countrycode::countrycode(comb$ISO3, "iso3c","country.name")
  comb <- comb[comb$ISO3!="C--" & comb$ISO3!="P--" & comb$ISO3!="S--",]
  comb[comb==0] <- NA
  comb <- comb[!duplicated(comb[,c("ISO3","BIOME_NAME","BIOME_NUM")]),]
  
  #get root:shoot ratios per biome and join with agc per biomeCountry
  #Mokany et al. 2006
  rootshoot <- readxl::read_xlsx(paste0(dir, "data/RootShoot.xlsx"))
  rootshoot <- rootshoot[1:11,]
  join <- left_join(comb, rootshoot[,c("BIOME_NAME","Equivalent System in Mokany",
                                       "Median Root:Shoot Ratio","Standard Error")], by="BIOME_NAME")
  
  if((nrow(join)==nrow(comb))==FALSE){
    print("stop now or die")
  }
  
  #we join each root:shoot ratio by biome to the biomeCountry aboveground value then
  #multiply aboveground value perBiomeCountry by root:shoot ratio for each biome
  join$crop_bgc_perBiomeCountry <- join$tic*join$`Median Root:Shoot Ratio`
  join$graze_bgc_perBiomeCountry <- join$tip*join$`Median Root:Shoot Ratio`
  join$crop_total_perBiomeCountry <- rowSums(join[, c("tic","crop_bgc_perBiomeCountry")])
  join$graze_total_perBiomeCountry <- rowSums(join[, c("tip","graze_bgc_perBiomeCountry")])
  
  return(join)
}



##################################################################

## TOTAL MITIGATION

cacc <- get_metric(pattern="CarbonAcc.*all")
cacc <- cacc %>% mutate(tropic_nontropic= case_when(BIOME_NAME=="Deserts & Xeric Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Mediterranean Forests, Woodlands & Scrub" ~ "Nontropic",
                                                    BIOME_NAME=="Montane Grasslands & Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Broadleaf & Mixed Forests" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Conifer Forests" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Tropical & Subtropical Coniferous Forests" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Dry Broadleaf Forests" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" ~ "Tropic",
                                                    BIOME_NAME=="Boreal Forests/Taiga" ~ "Nontropic"))

#sum per biomeCountry to country and biome level
country_cacc <- cacc %>%
  subset(BIOME_NAME!="Boreal Forests/Taiga") %>%
  group_by(ISO3) %>% 
  summarize(crop_C_perCountry = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C_perCountry = sum(graze_total_perBiomeCountry, na.rm=T))
#country_cacc$ADM0_CODE <- countrycode::countrycode(country_cacc$ISO3, "iso3c","gaul")
#country_cacc$full_name <- countrycode::countrycode(country_cacc$ISO3, "iso3c","country.name")


biome_cacc <- cacc %>%
  group_by(BIOME_NAME) %>%
  summarize(crop_C_perBiome = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C_perBiome = sum(graze_total_perBiomeCountry, na.rm=T))

tropic_nontropic_cacc <- cacc %>%
  subset(BIOME_NAME!="Boreal Forests/Taiga") %>%
  group_by(tropic_nontropic) %>%
  summarize(crop_C = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C = sum(graze_total_perBiomeCountry, na.rm=T))

## TOTAL GT C OVER 30 YEARS
#repeated to double check they match
sum(country_cacc$crop_C_perCountry, na.rm=T)/1000000000
sum(country_cacc$graze_C_perCountry, na.rm=T)/1000000000
sum(biome_cacc$crop_C_perBiome[biome_cacc$BIOME_NAME!="Boreal Forests/Taiga"], na.rm=T)/1000000000
sum(biome_cacc$graze_C_perBiome[biome_cacc$BIOME_NAME!="Boreal Forests/Taiga"], na.rm=T)/1000000000

#Tropic crop and graze 
sum(tropic_nontropic_cacc$crop_C[tropic_nontropic_cacc$tropic_nontropic=="Tropic"])/1000000000
sum(tropic_nontropic_cacc$graze_C[tropic_nontropic_cacc$tropic_nontropic=="Tropic"])/1000000000

#Nontropic crop and graze 
sum(tropic_nontropic_cacc$crop_C[tropic_nontropic_cacc$tropic_nontropic=="Nontropic"])/1000000000
sum(tropic_nontropic_cacc$graze_C[tropic_nontropic_cacc$tropic_nontropic=="Nontropic"])/1000000000

## FLUX DENSITY
#double check global flux densities
sum(country_cacc$crop_C_perCountry, na.rm=T)/(crop_extent_positive/1000000)/30/1000000 #divide by 1mil for million ha, then by 30 for annual, then by 1000000 for Gt
sum(country_cacc$graze_C_perCountry, na.rm=T)/(graze_extent_positive/1000000)/30/1000000
sum(biome_cacc$crop_C_perBiome[biome_cacc$BIOME_NAME!="Boreal Forests/Taiga"], na.rm=T)/(crop_extent_positive/1000000)/30/1000000
sum(biome_cacc$graze_C_perBiome[biome_cacc$BIOME_NAME!="Boreal Forests/Taiga"], na.rm=T)/(graze_extent_positive/1000000)/30/1000000

#Tropic crop and graze flux density
tropic_nontropic_cacc$crop_C[tropic_nontropic_cacc$tropic_nontropic=="Tropic"]/(agarea_tropicnontropic$CropArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Tropic"]/1000000)/30/1000000
tropic_nontropic_cacc$graze_C[tropic_nontropic_cacc$tropic_nontropic=="Tropic"]/(agarea_tropicnontropic$GrazeArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Tropic"]/1000000)/30/1000000

#Nontropic crop and graze flux density
tropic_nontropic_cacc$crop_C[tropic_nontropic_cacc$tropic_nontropic=="Nontropic"]/(agarea_tropicnontropic$CropArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Nontropic"]/1000000)/30/1000000
tropic_nontropic_cacc$graze_C[tropic_nontropic_cacc$tropic_nontropic=="Nontropic"]/(agarea_tropicnontropic$GrazeArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Nontropic"]/1000000)/30/1000000



####################################################################
###### make final dataframe for COUNTRIES #######
#with other covariates

#get mean forest cover
fls <- list.files("data/06_21_2023/",
                  pattern="cover", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ADM0_CODE","ADM0_NAME")], tbs[[2]][,c("ADM0_CODE","ADM0_NAME")])
if (nrow(tbs[[1]])==nrow(tbs[[2]])) {
  cover <- cbind(tbs[[1]], tbs[[2]][,"mean"]) 
} else {
  break
}
colnames(cover) <- c("ADM0_CODE","ADM0_NAME", "areaHa",str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
country_cover <- cover %>%
  group_by(ADM0_CODE) %>%
  summarize(mean_cover_tic = weighted.mean(tic, areaHa, na.rm=T),
            mean_cover_tip = weighted.mean(tip, areaHa, na.rm=T),
            not_wt_tic = mean(tic, na.rm=T),
            not_wt_tip = mean(tip, na.rm=T))
country_cover$ISO3 <- countrycode::countrycode(country_cover$ADM0_CODE, "gaul", "iso3c")
country_cover$full_name <- countrycode::countrycode(country_cover$ADM0_CODE, "gaul", "country.name")
country_cover$ISO3[country_cover$ADM0_CODE==147295] <- "CHN"
country_cover$ISO3[country_cover$ADM0_CODE==147296] <- "TWN"
country_cover$full_name[country_cover$ADM0_CODE==147295] <- "China"
country_cover$full_name[country_cover$ADM0_CODE==147296] <- "Taiwan"

country_cover <- subset(country_cover, !is.na(ISO3))
country_cover <- country_cover[!duplicated(country_cover),]

#get mean additional tree cover potential ("delta")
fls <- list.files("data/06_21_2023/",
                  pattern="^PositiveDelta_ByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ADM0_CODE","ADM0_NAME")], tbs[[2]][,c("ADM0_CODE","ADM0_NAME")])
if (nrow(tbs[[1]])==nrow(tbs[[2]])) {
  delta <- cbind(tbs[[1]], tbs[[2]][,"mean"]) 
} else {
  break
}
colnames(delta) <- c("ADM0_CODE","ADM0_NAME","areaHa", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
country_delta <- delta %>%
  group_by(ADM0_CODE) %>%
  summarize(mean_delta_tic = weighted.mean(tic, areaHa, na.rm=T),
            mean_delta_tip = weighted.mean(tip, areaHa, na.rm=T))
country_delta$ISO3 <- countrycode::countrycode(country_delta$ADM0_CODE, "gaul", "iso3c")
country_delta$full_name <- countrycode::countrycode(country_delta$ADM0_CODE, "gaul", "country.name")
country_delta$ISO3[country_delta$ADM0_CODE==147295] <- "CHN"
country_delta$ISO3[country_delta$ADM0_CODE==147296] <- "TWN"
country_delta$full_name[country_delta$ADM0_CODE==147295] <- "China"
country_delta$full_name[country_delta$ADM0_CODE==147296] <- "Taiwan"

country_delta <- subset(country_delta, !is.na(ISO3))
country_delta <- country_delta[!duplicated(country_delta),]



#make country dataframe of covariates
ctry_df <- inner_join(cover, delta, by="ADM0_CODE") %>%
  inner_join(agarea_byCountry, country_cacc, by="ISO3") %>%
  inner_join(country_cacc, by="ADM0_CODE") 

#cover <- cover[!is.na(cover$full_name) | cover$ADM0_NAME=="Taiwan" | cover$ADM0_NAME=="China" | cover$ADM0_NAME=="China/India",]
#cover[cover==0] <- NA
#cover <- cover[!duplicated(cover[,c("ADM0_CODE","ADM0_NAME")]),]

ctry_df$crop_flux <- ctry_df$crop_C_perCountry/(ctry_df$CropArea_ha_positive/1000000)/30/1000000
ctry_df$graze_flux <- ctry_df$graze_C_perCountry/(ctry_df$GrazeArea_ha_positive/1000000)/30/1000000
ctry_df$full_name <- countrycode::countrycode(ctry_df$ISO3, "iso3c","country.name")
ctry_df <- ctry_df %>% select(-c(full_name.x, full_name.y, ADM0_CODE.x, ADM0_CODE.y))


colnames(ctry_df) <- c("ISO3",
                       #"Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
                       
                       #"Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent",
                       "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta",
                       "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
                       "Crop_Delta_mean_percent", "Graze_Delta_mean_percent", 
                       "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr","NAME")

col_order <- c("ISO3","NAME", "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr",
               "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta",
               #"Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
               "Crop_Delta_mean_percent", "Graze_Delta_mean_percent")
               #"Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent")
ctry_df <- ctry_df[, col_order]

#country_df$country_name <- countrycode::countrycode(country_df$ISO3, "iso3c","country.name")
write.csv(ctry_df, "data/06_16_2023/country_results_06212023.csv")


## add total ag area
ctry_df <- read.csv("data/06_21_2023/country_results_06212023.csv")

fls <- list.files("data/06_21_2023/", pattern="potapov.*byCountry|ramankutty.*byCountry", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ADM0_CODE","ADM0_NAME")], tbs[[2]][,c("ADM0_CODE","ADM0_NAME")])
if (nrow(tbs[[1]])==nrow(tbs[[2]])) {
  tot_ag <- cbind(tbs[[1]], tbs[[2]][,"sum"]) 
} else {
  break
}
colnames(tot_ag) <- c("ADM0_CODE","ADM0_NAME", "areaHa",str_extract(gsub(".csv", "", basename(fls[[1]])), "potapov|ramankutty"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "potapov|ramankutty"))

country_tot_ag <- tot_ag %>%
  group_by(ADM0_CODE) %>%
  summarize(sum_tot_ag_tic = sum(potapov, na.rm=T),
            sum_tot_ag_tip = sum(ramankutty, na.rm=T))
country_tot_ag$ISO3 <- countrycode::countrycode(country_tot_ag$ADM0_CODE, "gaul", "iso3c")
country_tot_ag$full_name <- countrycode::countrycode(country_tot_ag$ADM0_CODE, "gaul", "country.name")
country_tot_ag$ISO3[country_tot_ag$ADM0_CODE==147295] <- "CHN"
country_tot_ag$ISO3[country_tot_ag$ADM0_CODE==147296] <- "TWN"
country_tot_ag$full_name[country_tot_ag$ADM0_CODE==147295] <- "China"
country_tot_ag$full_name[country_tot_ag$ADM0_CODE==147296] <- "Taiwan"

country_tot_ag <- subset(country_tot_ag, !is.na(ISO3))
country_tot_ag <- country_tot_ag[!duplicated(country_tot_ag),]

ctry_df <- left_join(ctry_df, country_tot_ag, by="ISO3")
write.csv(ctry_df, "data/06_21_2023/country_results_06212023.csv")




############################################## 
###### make final dataframe for BIOMES #######


#get mean forest cover
fls <- list.files("data/06_21_2023/",
                  pattern="ForestCover_positiveDelta_ByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
cover <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NUM", "BIOME_NAME")) 
colnames(cover) <- c("BIOME_NAME","BIOME_NUM", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"),
                    str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
class(cover$BIOME_NUM) <- class(as.numeric(cover$BIOME_NUM))


#get mean delta
fls <- list.files("data/06_21_2023/",
                  pattern="PositiveDelta_ByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NUM", "BIOME_NAME"))
colnames(delta) <- c("BIOME_NAME", "BIOME_NUM", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))

#make biome dataframe
biome_df <- inner_join(cover, delta, by=c("BIOME_NUM", "BIOME_NAME")) %>%
  inner_join(agarea_byBiome, by=c("BIOME_NUM", "BIOME_NAME")) %>%
  inner_join(biome_cacc, by="BIOME_NAME") %>%
  inner_join(tot_ag, by="BIOME_NAME")


biome_df$crop_flux <- biome_df$crop_C_perBiome/(biome_df$CropArea_ha_positive/1000000)/30/1000000
biome_df$graze_flux <- biome_df$graze_C_perBiome/(biome_df$GrazeArea_ha_positive/1000000)/30/1000000

  
colnames(biome_df) <- c("BIOME_NAME","BIOME_NUM","Crop_TreeCover_mean_percent_positiveDelta", "Graze_TreeCover_mean_percent_positiveDelta", 
                        "Crop_positiveDelta_mean_percent", "Graze_positiveDelta_mean_percent",
                        "Crop_Area_sum_ha_positiveDelta","Graze_Area_sum_ha_positiveDelta",
                        "Crop_TotalC_sum_MgC_30yr","Graze_TotalC_sum_MgC_30yr",
                        "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr")

col_order <- c("BIOME_NUM","BIOME_NAME","Crop_TotalC_sum_MgC_30yr","Graze_TotalC_sum_MgC_30yr",
               "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr",
               "Crop_Area_sum_ha_positiveDelta","Graze_Area_sum_ha_positiveDelta",
               "Crop_TreeCover_mean_percent_positiveDelta", "Graze_TreeCover_mean_percent_positiveDelta",
               "Crop_positiveDelta_mean_percent", "Graze_positiveDelta_mean_percent")
biome_df <- biome_df[, col_order]

biome_df <- read.csv("data/06_21_2023/biome_results_06212023.csv")

fls <- list.files("data/06_21_2023/",
                  pattern="potapov.*byBiome|ramankutty.*byBiome", full.names = T)
tbs <- lapply(fls, read.csv)
tot_ag <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NUM", "BIOME_NAME"))[c(1:10),] 
colnames(tot_ag) <- c("BIOME_NAME","BIOME_NUM", "tic", "tip")
biome_df <- left_join(biome_df, tot_ag, by="BIOME_NUM")

write.csv(biome_df, "data/06_21_2023/biome_results_06212023.csv")










## combine boreal csvs into main csvs

library(tidyverse)
dir <- "C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/"
setwd(dir)

#get m2 per crop and graze per country
fls <- list.files(paste0(dir, "data/06_21_2023/"), pattern="area",
                  full.names = T)
nms <- basename(tools::file_path_sans_ext(fls))
tbs <- lapply(fls, read.csv)
names(tbs) <- nms
names(tbs[["tic_area_ha_sum_30m_boreal"]]) <- c("BIOME_NAME","BIOME_NUM","ISO3","sum")
names(tbs[["tip_area_ha_sum_1000m_boreal"]]) <- c("BIOME_NAME","BIOME_NUM","ISO3","sum")

lapply(tbs, names)

tbs$tic_area_ha_sum_30m_boreal$COUNTRY <- NA
tbs$tip_area_ha_sum_1000m_boreal$COUNTRY <- NA

tbs$tic_area_ha_sum_30m_boreal <- tbs$tic_area_ha_sum_30m_boreal[,c("BIOME_NAME", "BIOME_NUM",  "ISO3", "COUNTRY","sum")]
tbs$tip_area_ha_sum_1000m_boreal <- tbs$tip_area_ha_sum_1000m_boreal[,c("BIOME_NAME", "BIOME_NUM",  "ISO3", "COUNTRY","sum")]

lapply(tbs, names)


tic_comb <- rbind(tbs$tic_area_ha_sum_30m, tbs$tic_area_ha_sum_30m_boreal)
tip_comb <- rbind(tbs$tip_area_ha_sum_1000m, tbs$tip_area_ha_sum_1000m_boreal)

tic_comb$BIOME_NAME <- ifelse(is.na(tic_comb$BIOME_NAME), "Boreal Forests/Taiga", tic_comb$BIOME_NAME)
tip_comb$BIOME_NAME <- ifelse(is.na(tip_comb$BIOME_NAME), "Boreal Forests/Taiga", tip_comb$BIOME_NAME)

write.csv(tic_comb, "data/06_21_2023/tic_area_ha_sum_30m_all.csv")
write.csv(tip_comb, "data/06_21_2023/tip_area_ha_sum_1000m_all.csv")
