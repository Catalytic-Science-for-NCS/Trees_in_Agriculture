## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)
library(sf)
dir <- "C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/"
setwd(dir)

#get m2 per crop and graze per country
fls <- list.files(paste0(dir, "data/06_21_2023/"),
                  pattern="*.area*.*all", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NAME", "BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME", "BIOME_NUM")])
#check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NAME", "BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY","BIOME_NAME", "BIOME_NUM")])
#check2 <- identical(tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[3]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])
#check3 <- identical(tbs[[3]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[4]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])

if (nrow(tbs[[1]])==nrow(tbs[[2]])) { # & nrow(tbs[[2]]==nrow(tbs[[3]]))& nrow(tbs[[3]]==nrow(tbs[[4]])) & check & check2 & check3){
  agarea <- cbind(tbs[[1]], tbs[[2]][,"sum"]) %>%
    select(-X)
   # cbind(tbs[[3]][,"sum"]) %>%
  #  cbind(tbs[[4]][,"sum"])
  
} else {
  break
}

colnames(agarea) <- c( "BIOME_NAME","BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))# str_extract(gsub(".csv", "", basename(fls[[3]])), "tic|tip"))
#agarea$CropAreaByBiomeCountry_tic_ha_positive <- agarea$tic
#agarea$CropAreaByBiomeCountry_tic_ha_all <- agarea$tic_all/10000
#agarea$GrazeAreaByBiomeCountry_tip_ha_positive <- agarea$tip/10000
#agarea$GrazeAreaByBiomeCountry_tip_ha_all <- agarea$tip_all/10000
agarea$full_name <- countrycode::countrycode(agarea$ISO3, "iso3c","country.name")
agarea <- agarea[agarea$ISO3!="C--" & agarea$ISO3!="P--" & agarea$ISO3!="S--",]
agarea[agarea==0] <- NA
agarea <- agarea[!duplicated(agarea[,c("ISO3", "BIOME_NAME","BIOME_NUM")]),]

agarea_byCountry <- agarea %>%
  group_by(ISO3) %>%
  summarize(CropArea_ha_positive = sum(tic, na.rm=T),
            GrazeArea_ha_positive = sum(tip))
agarea_byCountry$ADM0_CODE <- countrycode::countrycode(agarea_byCountry$ISO3, "iso3c","gaul")
agarea_byCountry$full_name <- countrycode::countrycode(agarea_byCountry$ISO3, "iso3c","country.name")


#agarea_byCountry$CropArea_ha_mil <- agarea_byCountry$CropArea_ha/1000000
#agarea_byCountry$GrazeArea_ha_mil <- agarea_byCountry$GrazeArea_ha/1000000

agarea_byBiome <- agarea %>%
  group_by(BIOME_NAME, BIOME_NUM) %>%
  summarize(CropArea_ha_positive = sum(tic, na.rm=T),
            GrazeArea_ha_positive = sum(tip,na.rm = T))

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




get_metric <- function(pattern, sum_or_mean_column){
  fls <- list.files(paste0(dir, "data/06_21_2023/"), pattern=pattern, full.names = T)
  #fls <- fls[grep("boreal",fls, invert=T)]
  tbs <- lapply(fls, read.csv)
  check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY","BIOME_NUM")])
  if((nrow(tbs[[1]])==nrow(tbs[[2]])) & check){
    comb <- cbind(tbs[[1]], tbs[[2]][,"sum"]) %>%
      select(-X)
  } else {
    break
  }
  colnames(comb) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
  comb$full_name <- countrycode::countrycode(comb$ISO3, "iso3c","country.name")
  comb <- comb[comb$ISO3!="C--" & comb$ISO3!="P--" & comb$ISO3!="S--",]
  comb[comb==0] <- NA
  comb <- comb[!duplicated(comb[,c("ISO3","BIOME_NAME","BIOME_NUM")]),]
  
  #now lets get root:shoot ratios per biome and join with agc per biomeCountry
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

# flx <- get_metric(pattern="FluxDensityByBiomeCountry_mean_*", sum_or_mean_column="mean")
# any(isFALSE(flx[,c("ISO3", "BIOME_NAME","BIOME_NUM")]==agarea[,c("ISO3", "BIOME_NAME","BIOME_NUM")]))
# #should be false
# 
# if((nrow(flx)==nrow(agarea))){
# flx_df <- cbind(flx, agarea[,c("CropAreaByBiomeCountry_tic_ha_positive")])
# } else {
#   break
# }
# 
# names(flx_df)[[12]] <- "CropAreaByBiomeCountry_tic_ha_positive"
# 
# country_flx <- flx_df %>%
#   group_by(ISO3) %>%
#   summarize(crop_C_perCountry_positive = weighted.mean(crop_total_perBiomeCountry, CropAreaByBiomeCountry_tic_ha_positive, na.rm=T),
#            # crop_C_perCountry_all = weighted.mean(crop_total_perBiomeCountry, CropAreaByBiomeCountry_tic_ha_all, na.rm=T))
#             CropArea_tic_ha= sum(CropAreaByBiomeCountry_tic_ha_positive, na.rm = T))
#             #GrazeArea_tip_ha = sum(GrazeAreaByBiomeCountry_tip_ha, na.rm=T))
# 
# biome_flx <- flx_df %>%
#   group_by(BIOME_NAME) %>%
#   summarize(crop_C_perBiome_positive = weighted.mean(crop_total_perBiomeCountry, CropAreaByBiomeCountry_tic_ha_positive, na.rm=T),
#             #crop_C_perBiome_all = weighted.mean(crop_total_perBiomeCountry, CropAreaByBiomeCountry_tic_ha_all, na.rm=T))
#             CropArea_tic_ha= sum(CropAreaByBiomeCountry_tic_ha_positive, na.rm = T))
#             #GrazeArea_tip_ha = sum(GrazeAreaByBiomeCountry_tip_ha, na.rm=T))
# 
# test <- flx_df[!is.na(flx_df$crop_total_perBiomeCountry) & !is.na(flx_df$CropAreaByBiomeCountry_tic_ha_positive),]
# 
# 
# weighted_mean = function(x, w, ..., na.rm=F){
#   if(na.rm){
#     keep = !is.na(x)&!is.na(w)
#     w = w[keep]
#     x = x[keep]
#   }
#   weighted.mean(x, w, ..., na.rm=F)
# }
# 
# 
# weighted.mean(test$crop_total_perBiomeCountry, test$CropAreaByBiomeCountry_tic_ha_positive, na.rm=T)
# weighted.mean(flx_df$tip, flx_df$GrazeAreaByBiomeCountry_tip_ha, na.rm=T)



## CARBON ACCUMULATION

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



#compare to Chapman
chp <- read.csv("../treesincroplands/country_summary.csv")
sum(chp$potential_crop_mg, na.rm=T)/1000000000/30
sum(chp$potential_pasture_mg, na.rm=T)/1000000000/30
#if you divide Chapman by the flux densities from above, takes somewhere from 90-140 years to reach

#compare to roe
roe <- read.csv('')

####################################################################
#use cacc right now to join up with other variables


#get mean forest cover
fls <- list.files("data/06_21_2023/",
                  pattern="TreeCover_PositiveDelta_ByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ADM0_CODE","ADM0_NAME")], tbs[[2]][,c("ADM0_CODE","ADM0_NAME")])
if (nrow(tbs[[1]])==nrow(tbs[[2]])) {
  cover <- cbind(tbs[[1]], tbs[[2]][,"mean"]) 
} else {
  break
}
colnames(cover) <- c("ADM0_CODE","ADM0_NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))


#get mean delta
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
  summarize(mean_delta_tic = weighted.mean(tic, areaHa),
            mean_delta_tip = weighted.mean(tip, areaHa))
country_delta$ISO3 <- countrycode::countrycode(country_delta$ADM0_CODE, "gaul", "iso3c")
country_delta$full_name <- countrycode::countrycode(country_delta$ADM0_CODE, "gaul", "country.name")
country_delta$ISO3[country_delta$ADM0_CODE==147295] <- "CHN"
country_delta <- subset(country_delta, !is.na(ISO3))
country_delta <- country_delta[!duplicated(country_delta),]

# #get mean expert Recs per country
# fls <- list.files("data/06_21_2023/",
#                   pattern="ExpertRecsByCountry", full.names = T)
# tbs <- lapply(fls, read.csv)
# expertRecs <- dplyr::inner_join(tbs[[1]], tbs[[2]], by="GID_0")
# colnames(expertRecs) <- c("ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
#                      str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))



#make country dataframe of covariates
ctry_df <- #inner_join(cover, delta, by="ADM0_CODE")# %>%
  #inner_join(expertRecs[,c("ISO3","tic","tip")], by="ISO3") %>%
  inner_join(agarea_byCountry,country_cacc, by="ISO3") %>%
  inner_join(country_delta, by="ISO3")
  #inner_join(country_cacc, by="ADM0_CODE") 
#inner_join(country_flx, by="ISO3")

#cover <- cover[!is.na(cover$full_name) | cover$ADM0_NAME=="Taiwan" | cover$ADM0_NAME=="China" | cover$ADM0_NAME=="China/India",]
#cover[cover==0] <- NA
#cover <- cover[!duplicated(cover[,c("ADM0_CODE","ADM0_NAME")]),]

ctry_df$crop_flux <- ctry_df$crop_C_perCountry/(ctry_df$CropArea_ha_positive/1000000)/30/1000000
ctry_df$graze_flux <- ctry_df$graze_C_perCountry/(ctry_df$GrazeArea_ha_positive/1000000)/30/1000000
ctry_df$full_name <- countrycode::countrycode(ctry_df$ISO3, "iso3c","country.name")

colnames(ctry_df) <- c("ISO3",
                       #"Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
                       
                       #"Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent",
                       "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta",
                       "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC", "ADM0_CODE",
                       "Crop_Delta_mean_percent", "Graze_Delta_mean_percent", "NAME",
                       "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr")

col_order <- c("ISO3","NAME", "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC/ha/yr","Graze_FluxDensity_mean_MgC/ha/yr",
               "CropArea_sum_ha_positiveDelta","GrazeArea_sum_ha_positiveDelta")
               #"Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
               #"Crop_Delta_mean_percent", "Graze_Delta_mean_percent",
               #"Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent")
ctry_df <- ctry_df[, col_order]

#country_df$country_name <- countrycode::countrycode(country_df$ISO3, "iso3c","country.name")
write.csv(ctry_df, "data/06_16_2023/country_results_partial_06212023.csv")








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
  inner_join(biome_cacc, by="BIOME_NAME")


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

write.csv(biome_df, "data/06_21_2023/biome_results_06212023.csv")




