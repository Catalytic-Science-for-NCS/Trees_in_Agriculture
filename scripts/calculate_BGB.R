## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)

#read in aboveground carbon accumulation totals by country
#from GEE
tic <- read.csv("TIA/Results_Potapov_GADM_no100s/CarbonAccByBiomeCountry_sum_tic_2015_GADM_no100s.csv")
tip <- read.csv("TIA/Results_Potapov_GADM_no100s/CarbonAccByBiomeCountry_sum_tip_2000_GADM.csv")
country_agc <- inner_join(tic, tip, by=c("BIOME_NAME", "BIOME_NUM", "GID_0", "biome_code"))
colnames(country_agc) <- c("BIOME_NAME", "BIOME_NUM", "GID_0", "biome_code", "crop_agc", "graze_agc")

country_agc <- subset(country_agc, GID_0!="XCA" & GID_0!="XAD" & GID_0!="XPI" & GID_0!="XCL" & GID_0!="XSP")
#get rid of XCA (Caspian sea)
#XAD are british bases on Cyprus, get rid of
#XKO is Kosovo, keep
#XPI and XSP are Paracel and Spratly Islands and are disputed. very small, get rid of
#XCL is Clipperton Island and may be part of France but its an atoll so get rid of

#now lets get root:shoot ratios per biome and join with agc per biomeCountry
#Mokany et al. 2006
rootshoot <- readxl::read_xlsx("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/RootShoot.xlsx")
rootshoot <- rootshoot[1:10,]
join <- left_join(country_agc, rootshoot[,c("BIOME_NAME","Equivalent System in Mokany",
                                     "Median Root:Shoot Ratio","Standard Error")], by="BIOME_NAME")

#we join each root:shoot ratio by biome to the biomeCountry AGC then
#multiply agc perBiomeCountry by root:shoot ratio for each biome
join$crop_bgc_perBiomeCountry <- join$crop_agc*join$`Median Root:Shoot Ratio`
join$graze_bgc_perBiomeCountry <- join$graze_agc*join$`Median Root:Shoot Ratio`

#sum up bgc per biomeCountry to country level
country_bgb <- join %>%
  group_by(GID_0) %>%
  summarize(crop_bgc_perCountry= sum(crop_bgc_perBiomeCountry),
            graze_bgc_perCountry = sum(graze_bgc_perBiomeCountry))

#sum up agc per biomeCountry to country level
country_agb <- join %>%
  group_by(GID_0) %>%
  summarize(crop_agc_perCountry= sum(crop_agc),
            graze_agc_perCountry = sum(graze_agc))


#get m2 per crop and graze per country
tic_area <- read.csv("TIA/Results_Potapov_GADM_no100s/CropAreaByCountry_tic_positiveDelta.csv")
tip_area <- read.csv("TIA/Results_Potapov_GADM_no100s/GrazeAreaByCountry_tip_positiveDelta.csv")
area <- inner_join(tic_area[,c("GID_0","sum")], tip_area[,c("GID_0","sum")], by="GID_0")
colnames(area) <- c("GID_0","crop_area_m2","graze_area_m2")
area$crop_area_ha <- area$crop_area_m2/10000
area$graze_area_ha <- area$graze_area_m2/10000
area$crop_area_ha_mil <- area$crop_area_ha/1000000
area$graze_area_ha_mil <- area$graze_area_ha/1000000

#combine country level totals of agc and bgc and m2 per crop and graze area
country_total_C <- inner_join(country_bgb, country_agb, by="GID_0") %>%
  inner_join(., area[,c("GID_0","crop_area_ha","crop_area_ha_mil", "graze_area_ha", "graze_area_ha_mil")], by="GID_0")

#add bgc and agc together
country_total_C$crop_total_C <- rowSums(country_total_C[,c("crop_bgc_perCountry","crop_agc_perCountry")])
country_total_C$graze_total_C <- rowSums(country_total_C[,c("graze_bgc_perCountry","graze_agc_perCountry")])

#get annual rate
country_total_C$crop_total_C_annual <- country_total_C$crop_total_C/30
country_total_C$graze_total_C_annual <- country_total_C$graze_total_C/30

#get per hectare
country_total_C$crop_total_C_annual_perHa <- country_total_C$crop_total_C_annual/country_total_C$crop_area_ha
country_total_C$graze_total_C_annual_perHa <- country_total_C$graze_total_C_annual/country_total_C$graze_area_ha


country_total_C[is.na(country_total_C)] <- 0 #is.na(NaN)=T
country_total_C$full_name <- countrycode::countrycode(country_total_C$GID_0, origin="iso3c", destination = "country.name", warn=T)
country_total_C$full_name[country_total_C$GID_0=="XKO"] <- "Kosovo"

write.csv(country_total_C[,c("GID_0", "full_name", "crop_area_ha_mil","graze_area_ha_mil","crop_total_C_annual_perHa", "graze_total_C_annual_perHa")],
          "TIA/Results_Potapov_GADM_no100s/total_C_byCountry_TIA1_TIA2.csv")


########################################
# calculate global potential total (agc and bgc) carbon
tic <- read.csv("TIA/Results_Potapov_GADM_no100s/CarbonAccByBiome_sum_tic.csv")[,1:3]
tip <- read.csv("TIA/Results_Potapov_GADM_no100s/CarbonAccByBiome_sum_tip.csv")[,1:3]
biome_agc <- inner_join(tic, tip, by=c("BIOME_NAME", "BIOME_NUM"))
colnames(biome_agc) <- c("BIOME_NAME", "BIOME_NUM", "crop_agc", "graze_agc")

#Mokany et al. 2006
rootshoot <- readxl::read_xlsx("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/RootShoot.xlsx")
rootshoot <- rootshoot[1:10,]
join <- left_join(biome_agc, rootshoot[,c("BIOME_NAME","Equivalent System in Mokany",
                                            "Median Root:Shoot Ratio","Standard Error")], by="BIOME_NAME")

#we join each root:shoot ratio by biome to the biomeCountry AGC then
#multiply agc perBiomeCountry by root:shoot ratio for each biome
join$crop_bgc_perBiome <- join$crop_agc*join$`Median Root:Shoot Ratio`
join$graze_bgc_perBiome <- join$graze_agc*join$`Median Root:Shoot Ratio`


#get m2 per crop and graze per country
tic_area <- read.csv("TIA/Results_Potapov_GADM_no100s/CropAreaByBiome_tic_positiveDelta.csv")
tip_area <- read.csv("TIA/Results_Potapov_GADM_no100s/GrazeAreaByBiome_tip_positiveDelta.csv")
area <- inner_join(tic_area[,c("BIOME_NAME","sum")], tip_area[,c("BIOME_NAME","sum")], by="BIOME_NAME")
colnames(area) <- c("BIOME_NAME","crop_area_m2","graze_area_m2")
area$crop_area_ha <- area$crop_area_m2/10000
area$graze_area_ha <- area$graze_area_m2/10000

#combine country level totals of agc and bgc and m2 per crop and graze area
biome_total_C <- inner_join(join, area[,c("BIOME_NAME","crop_area_ha", "graze_area_ha")], by="BIOME_NAME")

#add bgc and agc together
biome_total_C$crop_total_C <- rowSums(biome_total_C[,c("crop_bgc_perBiome","crop_agc")])
biome_total_C$graze_total_C <- rowSums(biome_total_C[,c("graze_bgc_perBiome","graze_agc")])

#get annual rate
biome_total_C$crop_total_C_annual <- biome_total_C$crop_total_C/30
biome_total_C$graze_total_C_annual <- biome_total_C$graze_total_C/30

#get per hectare
biome_total_C$crop_total_C_annual_perHa <- biome_total_C$crop_total_C_annual/biome_total_C$crop_area_ha
biome_total_C$graze_total_C_annual_perHa <- biome_total_C$graze_total_C_annual/biome_total_C$graze_area_ha


biome_total_C[is.na(biome_total_C)] <- 0 #is.na(NaN)=T
sum(biome_total_C$crop_total_C_annual_perHa)
sum(biome_total_C$graze_total_C_annual_perHa)

write.csv(biome_total_C[,c("BIOME_NAME", "BIOME_NUM", "crop_total_C_annual_perHa", "graze_total_C_annual_perHa")],
          "TIA/Results_Potapov_GADM_no100s/total_C_bybiome_TIA1_TIA2.csv")
