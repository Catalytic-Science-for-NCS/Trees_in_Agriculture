## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)
library(sf)
dir <- "C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/"
setwd(dir)

#get m2 per crop and graze per country
fls <- list.files(paste0(dir, "data/02_17_2023/"),
                  pattern="AreaByBiomeCountry", full.names = T)
tbs <- lapply(fls, read.csv)
check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])
check2 <- identical(tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[3]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])
check3 <- identical(tbs[[3]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[4]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])

if((nrow(tbs[[1]])==nrow(tbs[[2]])) & nrow(tbs[[2]]==nrow(tbs[[3]]))& nrow(tbs[[3]]==nrow(tbs[[4]])) & check & check2 & check3){
  agarea <- cbind(tbs[[1]], tbs[[2]][,"sum"]) %>%
    cbind(tbs[[3]][,"sum"]) %>%
    cbind(tbs[[4]][,"sum"])
  
} else {
  break
}

colnames(agarea) <- c("ISO3","NAME","BIOME_NAME", "BIOME_NUM", "tic_all","tic_positive","tip_all","tip_positive" )#str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                      #str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"), str_extract(gsub(".csv", "", basename(fls[[3]])), "tic|tip"))

agarea$CropAreaByBiomeCountry_tic_ha_positive <- agarea$tic_positive/10000
agarea$CropAreaByBiomeCountry_tic_ha_all <- agarea$tic_all/10000
agarea$GrazeAreaByBiomeCountry_tip_ha_positive <- agarea$tip_positive/10000
agarea$GrazeAreaByBiomeCountry_tip_ha_all <- agarea$tip_all/10000
agarea$full_name <- countrycode::countrycode(agarea$ISO3, "iso3c","country.name")
agarea <- agarea[agarea$ISO3!="C--" & agarea$ISO3!="P--" & agarea$ISO3!="S--",]
agarea[agarea==0] <- NA
agarea <- agarea[!duplicated(agarea[,c("ISO3","BIOME_NAME","BIOME_NUM")]),]

agarea_byCountry <- agarea %>%
  group_by(ISO3) %>%
  summarize(CropArea_ha_positive = sum(CropAreaByBiomeCountry_tic_ha_positive, na.rm=T),
            CropArea_ha_all = sum(CropAreaByBiomeCountry_tic_ha_all),
            GrazeArea_ha_positive = sum(GrazeAreaByBiomeCountry_tip_ha_positive),
            GrazeArea_ha_all = sum(GrazeAreaByBiomeCountry_tip_ha_all))

#agarea_byCountry$CropArea_ha_mil <- agarea_byCountry$CropArea_ha/1000000
#agarea_byCountry$GrazeArea_ha_mil <- agarea_byCountry$GrazeArea_ha/1000000

agarea_byBiome <- agarea %>%
  group_by(BIOME_NAME, BIOME_NUM) %>%
  summarize(CropArea_ha_positive = sum(CropAreaByBiomeCountry_tic_ha_positive, na.rm=T),
            CropArea_ha_all = sum(CropAreaByBiomeCountry_tic_ha_all, na.rm=T),
            GrazeArea_ha_positive = sum(GrazeAreaByBiomeCountry_tip_ha_positive,na.rm = T),
            GrazeArea_ha_all = sum(GrazeAreaByBiomeCountry_tip_ha_all,na.rm = T))

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
                                    BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" ~ "Tropic")) %>%
  group_by(tropic_nontropic) %>%
  summarize(CropArea_ha_positive = sum(CropAreaByBiomeCountry_tic_ha_positive, na.rm=T),
            CropArea_ha_all = sum(CropAreaByBiomeCountry_tic_ha_all, na.rm=T),
            GrazeArea_ha_positive = sum(GrazeAreaByBiomeCountry_tip_ha_positive,na.rm = T),
            GrazeArea_ha_all = sum(GrazeAreaByBiomeCountry_tip_ha_all,na.rm = T))



crop_extent_positive = sum(agarea$CropAreaByBiomeCountry_tic_ha_positive, na.rm = T)
graze_extent_all = sum(agarea$GrazeAreaByBiomeCountry_tip_ha_all, na.rm = T)




get_metric <- function(pattern, sum_or_mean_column){
  fls <- list.files(paste0(dir, "data/02_17_2023/"), pattern=pattern, full.names = T)
  fls <- fls[grep("copernicus",fls, invert=T)]
  tbs <- lapply(fls, read.csv)
  check <- identical(tbs[[1]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")], tbs[[2]][,c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM")])
  if((nrow(tbs[[1]])==nrow(tbs[[2]])) & check){
    comb <- cbind(tbs[[1]], tbs[[2]][,sum_or_mean_column])
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
  rootshoot <- rootshoot[1:10,]
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

cacc <- get_metric(pattern="CarbonAcc", sum_or_mean_column="sum")
cacc <- cacc %>% mutate(tropic_nontropic= case_when(BIOME_NAME=="Deserts & Xeric Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Mediterranean Forests, Woodlands & Scrub" ~ "Nontropic",
                                                    BIOME_NAME=="Montane Grasslands & Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Broadleaf & Mixed Forests" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Conifer Forests" ~ "Nontropic",
                                                    BIOME_NAME=="Temperate Grasslands, Savannas & Shrublands" ~ "Nontropic",
                                                    BIOME_NAME=="Tropical & Subtropical Coniferous Forests" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Dry Broadleaf Forests" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "Tropic",
                                                    BIOME_NAME=="Tropical & Subtropical Moist Broadleaf Forests" ~ "Tropic"))

#sum per biomeCountry to country and biome level
country_cacc <- cacc %>%
  group_by(ISO3) %>%
  summarize(crop_C_perCountry = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C_perCountry = sum(graze_total_perBiomeCountry, na.rm=T))

biome_cacc <- cacc %>%
  group_by(BIOME_NAME) %>%
  summarize(crop_C_perBiome = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C_perBiome = sum(graze_total_perBiomeCountry, na.rm=T))

tropic_nontropic_cacc <- cacc %>%
  group_by(tropic_nontropic) %>%
  summarize(crop_C = sum(crop_total_perBiomeCountry, na.rm=T),
            graze_C = sum(graze_total_perBiomeCountry, na.rm=T))

sum(country_cacc$crop_C_perCountry, na.rm=T)/(crop_extent_all/1000000)/30/1000000 
sum(country_cacc$graze_C_perCountry, na.rm=T)/(graze_extent_all/1000000)/30/1000000
sum(biome_cacc$crop_C_perBiome, na.rm=T)/(crop_extent_positive/1000000)/30/1000000
sum(biome_cacc$graze_C_perBiome, na.rm=T)/(graze_extent_all/1000000)/30/1000000


sum(tropic_nontropic_cacc$crop_C, na.rm=T)/(agarea_tropicnontropic$CropArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Tropic"]/1000000)/30/1000000
sum(tropic_nontropic_cacc$graze_C, na.rm=T)/(agarea_tropicnontropic$GrazeArea_ha_positive[agarea_tropicnontropic$tropic_nontropic=="Tropic"]/1000000)/30/1000000


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
fls <- list.files("data/02_17_2023/",
                  pattern="ForestCoverByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
cover <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0"))%>% 
  inner_join(tbs[[3]], by="GID_0") %>%
  inner_join(tbs[[4]], by="GID_0")
colnames(cover) <- c("ISO3", "NAME","tic_allDelta", "tic_positiveDelta","tip_allDelta","tip_positiveDelta")


#get mean delta
fls <- list.files("data/02_17_2023/",
                  pattern="^DeltaByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by="GID_0")
colnames(delta) <- c("ISO3", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))


#get mean expert Recs per country
fls <- list.files("data/02_17_2023/",
                  pattern="ExpertRecsByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
expertRecs <- dplyr::inner_join(tbs[[1]], tbs[[2]], by="GID_0")
colnames(expertRecs) <- c("ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))



#make country dataframe of covariates
ctry_df <- inner_join(cover, delta, by="ISO3") %>%
  inner_join(expertRecs[,c("ISO3","tic","tip")], by="ISO3") %>%
  inner_join(agarea_byCountry, by="ISO3") %>%
  inner_join(country_cacc, by="ISO3") 
#inner_join(country_flx, by="ISO3")

ctry_df$crop_flux <- ctry_df$crop_C_perCountry/(ctry_df$CropArea_ha_all/1000000)/30/1000000
ctry_df$graze_flux <- ctry_df$graze_C_perCountry/(ctry_df$GrazeArea_ha_all/1000000)/30/1000000

colnames(ctry_df) <- c("ISO3","NAME",
                       "Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
                       "Crop_Delta_mean_percent", "Graze_Delta_mean_percent",
                       "Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent",
                       "CropArea_sum_ha","GrazeArea_sum_ha",
                       "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
                       "Crop_FluxDensity_mean_MgC/ha_WRONG", "Graze_FluxDensity_mean_MgC/ha_WRONG",
                       "Crop_FluxDensity_mean_MgC/ha_CORRECT","Graze_FluxDensity_mean_MgC/ha_CORRECT")

col_order <- c("ISO3","NAME", "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC/ha_CORRECT","Graze_FluxDensity_mean_MgC/ha_CORRECT",
               "Crop_FluxDensity_mean_MgC/ha_WRONG", "Graze_FluxDensity_mean_MgC/ha_WRONG",
               "CropArea_sum_ha","GrazeArea_sum_ha", 
               "Crop_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta",
               "Crop_Delta_mean_percent", "Graze_Delta_mean_percent",
               "Crop_ExpertRecs_mean_percent", "Graze_ExpertRecs_mean_percent")
ctry_df <- ctry_df[, col_order]

#country_df$country_name <- countrycode::countrycode(country_df$ISO3, "iso3c","country.name")
write.csv(ctry_df, "data/02_17_2023/country_results_03172023.csv")








#get mean forest cover
fls <- list.files("data/02_17_2023/",
                  pattern="ForestCoverByBiome_", full.names = T)
tbs <- lapply(fls, read.csv)
cover <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NUM")) %>% 
  inner_join(tbs[[3]], by="BIOME_NUM") %>%
  inner_join(tbs[[4]][2:3], by="BIOME_NUM")
#colnames(cover) <- c("BIOME_NUM", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
#                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))
colnames(cover) <- c("BIOME_NUM","tic","tip","tip_positivedelta", "tic_positivedelta")

#get mean delta
fls <- list.files("data/02_17_2023/",
                  pattern="^DeltaByBiome_", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by="BIOME_NUM")
colnames(delta) <- c("BIOME_NUM", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                     str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))


#make biome dataframe
biome_df <- inner_join(cover, delta, by=c("BIOME_NUM")) %>%
  inner_join(agarea_byBiome, by="BIOME_NUM") %>%
  inner_join(biome_cacc, by="BIOME_NAME") %>%
  inner_join(biome_flx, by="BIOME_NAME")


biome_df$crop_flux <- biome_df$crop_C_perBiome.x/(biome_df$CropArea_ha/1000000)/30/1000000
biome_df$graze_flux <- biome_df$graze_C_perBiome.x/(biome_df$GrazeArea_ha/1000000)/30/1000000

  
colnames(biome_df) <- c("BIOME_NUM", "Crop_ForestCover_mean_percent_allDelta", "Graze_ForestCover_mean_percent_allDelta","Graze_ForestCover_mean_percent_positiveDelta","Crop_ForestCover_mean_percent_positiveDelta",
                        "Crop_Delta_mean_percent", "Graze_Delta_mean_percent","BIOME_NAME",
                        "Crop_Area_sum_ha","Graze_Area_sum_ha",
                        "Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
                        "Crop_FluxDensity_mean_MgC/ha_WRONG","Graze_FluxDensity_mean_MgC/ha_WRONG",
                        "Crop_FluxDensity_mean_MgC/ha_CORRECT","Graze_FluxDensity_mean_MgC/ha_CORRECT")

col_order <- c("BIOME_NUM","BIOME_NAME","Crop_TotalC_sum_MgC","Graze_TotalC_sum_MgC",
               "Crop_FluxDensity_mean_MgC/ha_CORRECT","Graze_FluxDensity_mean_MgC/ha_CORRECT",
               "Crop_FluxDensity_mean_MgC/ha_WRONG","Graze_FluxDensity_mean_MgC/ha_WRONG",
               "Crop_Area_sum_ha","Graze_Area_sum_ha", "Crop_ForestCover_mean_percent_allDelta", "Graze_ForestCover_mean_percent_allDelta","Crop_ForestCover_mean_percent_positiveDelta","Graze_ForestCover_mean_percent_positiveDelta",
               "Crop_Delta_mean_percent", "Graze_Delta_mean_percent")
biome_df <- biome_df[, col_order]

write.csv(biome_df, "data/02_17_2023/biome_results_03162023.csv")




