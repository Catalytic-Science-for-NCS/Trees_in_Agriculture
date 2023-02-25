## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)
setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/")
#read in aboveground carbon accumulation totals by country
#from GEE
tic <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/FluxDensityByBiomeCountry_mean_tic_2015_no100s_lessComplex.csv")
tip <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/FluxDensityByBiomeCountry_mean_tip_2000_no100s_lessComplex.csv")
country_agc <- full_join(tic, tip, by=c("BIOME_NAME", "BIOME_NUM","COUNTRY", "ISO3")) 
colnames(country_agc) <- c("BIOME_NAME", "BIOME_NUM", "ISO3", "NAME", "crop_agc", "graze_agc")

#country_agc <- subset(country_agc, GID_0!="XCA" & GID_0!="XAD" & GID_0!="XPI" & GID_0!="XCL" & GID_0!="XSP")
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

#get m2 per crop and graze per country
tic_area <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/CropAreaByBiomeCountry_tic_positiveDelta_6.csv")
tip_area <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/GrazingAreaByBiomeCountry_tip_positiveDelta_6.csv")
area <- inner_join(tic_area, tip_area, by=c("ISO3","COUNTRY","BIOME_NAME","BIOME_NUM"))
colnames(area) <- c("ISO3","COUNTRY","BIOME_NAME","BIOME_NUM","crop_area_m2","graze_area_m2")
area$crop_area_ha <- area$crop_area_m2/10000
area$graze_area_ha <- area$graze_area_m2/10000
area$crop_area_ha_mil <- area$crop_area_ha/1000000
area$graze_area_ha_mil <- area$graze_area_ha/1000000

join <- inner_join(join, area, by=c("ISO3","BIOME_NAME","BIOME_NUM"))

write.csv(join, "TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/FluxDensity_byBiomeCountry_TIA1_TIA2.csv")


#average up carbon flux density per biomeCountry to country level
country <- join %>%
  group_by(ISO3) %>%
  summarize(crop_bgc_perCountry= mean(crop_bgc_perBiomeCountry, na.rm=T),
            graze_bgc_perCountry = mean(graze_bgc_perBiomeCountry, na.rm=T),
            crop_agc_perCountry= mean(crop_agc, na.rm=T),
            graze_agc_perCountry = mean(graze_agc, na.rm=T),
            crop_totalFlux_perCountry = sum(crop_bgc_perCountry, crop_agc_perCountry),
            graze_totalFlux_perCountry = sum(graze_bgc_perCountry, graze_agc_perCountry))
country$full_name <- countrycode::countrycode(country$ISO3, "iso3c","country.name")
country <- country[country$ISO3!="C--" & country$ISO3!="P--" & country$ISO3!="S--",]

#combine country level totals of agc and bgc and m2 per crop and graze area
country_FluxDensity <- inner_join(country, area[,c("ISO3","crop_area_ha","crop_area_ha_mil", "graze_area_ha", "graze_area_ha_mil")],
                                  by=c("ISO3"))

mean(country_FluxDensity$crop_totalFlux_perCountry, na.rm=T)
mean(country_FluxDensity$graze_totalFlux_perCountry, na.rm=T)
sum(country_FluxDensity$crop_area_ha_mil)
sum(country_FluxDensity$graze_area_ha_mil)


write.csv(country_FluxDensity,"TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/FluxDensity_byCountry_TIA1_TIA2.csv")


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
area$crop_area_ha_mil <- area$crop_area_ha/1000000
area$graze_area_ha_mil <- area$graze_area_ha/1000000

#combine country level totals of agc and bgc and m2 per crop and graze area
biome_total_C <- inner_join(join, area[,c("BIOME_NAME","crop_area_ha", "graze_area_ha","crop_area_ha_mil", "graze_area_ha_mil")], by="BIOME_NAME")

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
sum(biome_total_C$crop_area_ha_mil)
sum(biome_total_C$graze_area_ha_mil)

write.csv(biome_total_C[,c("BIOME_NAME", "BIOME_NUM", "crop_total_C_annual_perHa", "graze_total_C_annual_perHa")],
          "TIA/Results_Potapov_GADM_no100s/total_C_bybiome_TIA1_TIA2.csv")


biome <- join %>%
  group_by(BIOME_NUM) %>%
  summarize(crop_bgc_perBiome= mean(crop_bgc_perBiomeCountry, na.rm=T),
            graze_bgc_perBiome = mean(graze_bgc_perBiomeCountry, na.rm=T),
            crop_agc_perBiome= mean(crop_agc, na.rm=T),
            graze_agc_perBiome = mean(graze_agc, na.rm=T),
            crop_totalFlux_perBiome = sum(crop_bgc_perBiome, crop_agc_perBiome),
            graze_totalFlux_perBiome = sum(graze_bgc_perBiome, graze_agc_perBiome))
mean(biome$crop_totalFlux_perBiome, na.rm=T)
mean(biome$graze_totalFlux_perBiome, na.rm=T)


##########################################################
##########################################################
##########################################################
## calculate total (ag and bg) carbon per country and globally per year per ha
#requires using root:shoot ratio (by biome) to estimate bg

library(tidyverse)
setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/")
#read in aboveground carbon accumulation totals by country
#from GEE
tic <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/drive-download-20230217T140245Z-001/CarbonAcc_pixel_30yrByBiomeCountry_sum_tic_2015_no100s_lessComplex.csv")
tip <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/drive-download-20230217T140245Z-001/CarbonAcc_pixel_30yrByBiomeCountry_sum_tip_2000_no100s_lessComplex.csv")
country_agc <- full_join(tic, tip, by=c("BIOME_NAME", "BIOME_NUM","COUNTRY", "ISO3")) 
colnames(country_agc) <- c("BIOME_NAME", "BIOME_NUM", "ISO3", "NAME", "crop_agc", "graze_agc")

#country_agc <- subset(country_agc, GID_0!="XCA" & GID_0!="XAD" & GID_0!="XPI" & GID_0!="XCL" & GID_0!="XSP")
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

#average up carbon per biomeCountry to country level
country <- join %>%
  group_by(ISO3) %>%
  summarize(crop_bgc_perCountry= sum(crop_bgc_perBiomeCountry),
            graze_bgc_perCountry = sum(graze_bgc_perBiomeCountry),
            crop_agc_perCountry= sum(crop_agc),
            graze_agc_perCountry = sum(graze_agc))
country$crop_total <- rowSums(country[,c("crop_bgc_perCountry","crop_agc_perCountry")])
country$graze_total <- rowSums(country[,c("graze_bgc_perCountry","graze_agc_perCountry" )])
country$full_name <- countrycode::countrycode(country$ISO3, "iso3c","country.name")
country <- country[country$ISO3!="C--" & country$ISO3!="P--" & country$ISO3!="S--",]

#get m2 per crop and graze per country
tic_area <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/drive-download-20230217T140245Z-001/CropAreaByBiomeCountry_tic_positiveDelta_6.csv")
tip_area <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/drive-download-20230217T140245Z-001/GrazingAreaByBiomeCountry_tip_positiveDelta_6.csv")
area <- inner_join(tic_area[,c("ISO3","sum")], tip_area[,c("ISO3","sum")], by="ISO3")
colnames(area) <- c("ISO3","crop_area_m2","graze_area_m2")

area <- area %>%
  group_by(ISO3) %>%
  summarize(crop_totalArea = sum(),
            graze_totalCarbonAcc = sum(graze_bgc_perCountry, graze_agc_perCountry))
country$full_name <- countrycode::countrycode(country$ISO3, "iso3c","country.name")

area$crop_area_ha <- area$crop_area_m2/10000
area$graze_area_ha <- area$graze_area_m2/10000
area$crop_area_ha_mil <- area$crop_area_ha/1000000
area$graze_area_ha_mil <- area$graze_area_ha/1000000

#combine country level totals of agc and bgc and m2 per crop and graze area
country_CarbonAcc <- inner_join(country, area[,c("ISO3","crop_area_ha","crop_area_ha_mil", "graze_area_ha", "graze_area_ha_mil")],
                                  by="ISO3")

sum(country_CarbonAcc$crop_totalCarbonAcc)/1000000000
sum(country_CarbonAcc$graze_totalCarbonAcc)/1000000000

chp <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/treesincroplands/country_summary.csv")
sum(chp$potential_crop_mg, na.rm=T)/1000000000
sum(chp$potential_pasture_mg, na.rm=T)/1000000000


write.csv(country_FluxDensity,"TIA/Results_Potapov_GADM_no100s/FluxDensity_byCountry_TIA1_TIA2.csv")


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
area$crop_area_ha_mil <- area$crop_area_ha/1000000
area$graze_area_ha_mil <- area$graze_area_ha/1000000

#combine country level totals of agc and bgc and m2 per crop and graze area
biome_total_C <- inner_join(join, area[,c("BIOME_NAME","crop_area_ha", "graze_area_ha","crop_area_ha_mil", "graze_area_ha_mil")], by="BIOME_NAME")

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
sum(biome_total_C$crop_area_ha_mil)
sum(biome_total_C$graze_area_ha_mil)

write.csv(biome_total_C[,c("BIOME_NAME", "BIOME_NUM", "crop_total_C_annual_perHa", "graze_total_C_annual_perHa")],
          "TIA/Results_Potapov_GADM_no100s/total_C_bybiome_TIA1_TIA2.csv")
