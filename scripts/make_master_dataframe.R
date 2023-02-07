library(tidyverse)
library(countrycode)

## make master data table for countries

fls <- list.files("TIA/Results_Potapov_GADM_no100s/", pattern="CarbonAccByCountry", full.names = T) 
tbs <- lapply(fls, read.csv)
comb <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(comb) <- c("GID_0", "NAME_0",  gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))

comb$full_name <- countrycode::countrycode(comb$GID_0, origin="iso3c", destination="country.name", warn=T)
#double check which ones got left out, make sure you're ok with it
#comb[is.na(comb$full_name),]
#comb <- comb[!is.na(comb$full_name),]

##add agricultural hectares
fls <- list.files("TIA/Results_Potapov_GADM_no100s/", pattern="AreaByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(agarea) <- c("GID_0", "NAME_0", gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))
agarea$full_name <- countrycode::countrycode(agarea$GID_0, origin="iso3c", destination="country.name", warn=T)
#agarea[is.na(agarea$full_name),]
#agarea <- agarea[!is.na(agarea$full_name),]
agarea$CropAreaByCountry_tic_ha <- agarea$CropAreaByCountry_tic/10000
agarea$GrazeAreaByCountry_tip_ha <- agarea$GrazeAreaByCountry_tip/10000

## add mean tree cover
fls <- list.files("TIA/Results_Potapov_GADM_no100s/02022023/", pattern="CoverByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
treecov <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(treecov) <- c("GID_0", "NAME_0",  gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))
treecov$full_name <- countrycode::countrycode(treecov$GID_0, origin="iso3c", destination="country.name", warn=T)
#treecov[is.na(treecov$full_name),]
#treecov <- treecov[!is.na(treecov$full_name),]

## add mean expert recs
fls <- list.files("TIA/Results_Potapov_GADM_no100s/02022023/", pattern="ExpertRecsByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
expertRecs <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(expertRecs) <- c("GID_0", "NAME_0",  gsub(".csv", "", basename(fls[[1]])), 
                       gsub(".csv", "", basename(fls[[2]])))
expertRecs$full_name <- countrycode::countrycode(expertRecs$GID_0, origin="iso3c", destination="country.name", warn=T)


## add delta
fls <- list.files("TIA/Results_Potapov_GADM_no100s/02022023/", pattern="DeltaByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(delta) <- c("GID_0", "NAME_0",  gsub(".csv", "", basename(fls[[1]])), 
                          gsub(".csv", "", basename(fls[[2]])))
delta$full_name <- countrycode::countrycode(delta$GID_0, origin="iso3c", destination="country.name", warn=T)



#"TIA1 rank" "TIA 2 rank" "Total potential rank" and "Potential / ha rank"
full <- inner_join(comb[,c("GID_0", "CarbonAccByCountry_sum_tic","CarbonAccByCountry_sum_tip","full_name")],
                   agarea[,c("CropAreaByCountry_tic_positiveDelta", "GrazeAreaByCountry_tip_positiveDelta", "GID_0")],by="GID_0") %>%
  inner_join(treecov[,c("ForestCoverByCountry_mean_tic_2015_GADM_no100s","ForestCoverByCountry_mean_tip_2000_GADM","GID_0")], by="GID_0") %>%
  inner_join(expertRecs[,c("ExpertRecsByCountry_mean_tic_2015_GADM_no100s","ExpertRecsByCountry_mean_tip_2000_GADM","GID_0")], by="GID_0") %>%
  inner_join(delta[,c("DeltaByCountry_mean_tic_2015_GADM_no100s","DeltaByCountry_mean_tip_2000_GADM","GID_0")], by="GID_0")
  

colnames(full) <- c("iso3","CarbonAccumulation_sum_TIA1_MgC", "CarbonAccumulation_sum_TIA2_MgC", "country_name", "CropArea_m2","GrazeArea_m2",
                    "ForestCover_mean_2015_TIA1","ForestCover_mean_2000_TIA2","ExpertRec_mean_TIA1","ExpertRec_mean_TIA2","Delta_mean_TIA1",
                    "Delta_mean_TIA2")


full$CarbonAccByCountry_sum_TIA1_50 <- full$CarbonAccumulation_sum_TIA1_MgC/2
full$CarbonAccByCountry_sum_TIA1_75 <- full$CarbonAccumulation_sum_TIA1_MgC*0.75
full$CarbonAccByCountry_sum_TIA2_50 <- full$CarbonAccumulation_sum_TIA2_MgC/2
full$CarbonAccByCountry_sum_TIA2_75 <- full$CarbonAccumulation_sum_TIA2_MgC*0.75

full$CarbonAccumulation_TIA1_MgCperHA <- full$CarbonAccumulation_sum_TIA1_MgC/full$CropArea_m2/10000
full$CarbonAccumulation_TIA2_MgCperHA <- full$CarbonAccumulation_sum_TIA2_MgC/full$GrazeArea_m2/10000

full$CarbonAccumulation_sum_TIA1_TIA2_MgC <- rowSums(full[, c("CarbonAccumulation_sum_TIA1_MgC", "CarbonAccumulation_sum_TIA2_MgC")])


## make rank orders ##

order.scores<-order(full$CarbonAccumulation_sum_TIA1_MgC, full$country_name , decreasing = T)
full$TIA1_rank <- NA
full$TIA1_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$CarbonAccumulation_TIA1_MgCperHA, full$country_name , decreasing = T)
full$TIA1_perHA_rank <- NA
full$TIA1_perHA_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$CarbonAccumulation_sum_TIA2_MgC, full$country_name , decreasing = T)
full$TIA2_rank <- NA
full$TIA2_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$CarbonAccumulation_TIA2_MgCperHA, full$country_name, decreasing = T)
full$TIA2_perHA_rank <- NA
full$TIA2_perHA_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$CarbonAccumulation_sum_TIA1_TIA2_MgC, full$country_name , decreasing = T)
full$TIA1_TIA2_rank <- NA
full$TIA1_TIA2_rank[order.scores] <- 1:nrow(full)


#order alphabetically
full <- full[order(full$country_name), ]


colnames(full) <- c("iso3 country_code","Mg C Potential TIA1 100%", "Mg C Potential TIA2 100%", "country_name",
                    "Crop Area (m2)","Graze Area (m2)","Baseline mean tree cover TIA1 (2015)","Baseline mean tree cover TIA2 (2000)",
                    "Mean expert rec TIA1", "Mean expert rec TIA2", "Mean delta TIA1", "Mean delta TIA2",
                    "Mg C Potential TIA1 50%", "Mg C Potential TIA1 75%", "Mg C Potential TIA2 50%", "Mg C Potential TIA2 75%",
                    "TIA1_perHA","TIA2_perHA","TIA1_TIA2_sum","TIA1_total_rank","TIA1_perHA_rank","TIA2_total_rank","TIA2_perHA_rank", "TIA1_TIA2_total_rank")

write.csv(full, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byCountry.csv")






## make master data table for biomes

fls <- list.files("TIA/Results_Potapov_GADM_no100s/", pattern="CarbonAccByBiome", full.names = T)[1:2]
tbs <- lapply(fls, read.csv)
comb <- dplyr::inner_join(tbs[[1]][,c("BIOME_NAME","BIOME_NUM","sum")], tbs[[2]][,c("BIOME_NAME","BIOME_NUM","sum")], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(comb) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))

#add agricultural hectares
fls <- list.files("TIA/Results_Potapov_GADM_no100s/", pattern="AreaByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(agarea) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))
agarea$CropAreaByBiome_tic_ha <- agarea$CropAreaByBiome_tic_positiveDelta/10000
agarea$GrazeAreaByBiome_tip_ha <- agarea$GrazeAreaByBiome_tip_positiveDelta/10000


## add mean tree cover
fls <- list.files("TIA/Results_Potapov_GADM_no100s/02022023/", pattern="CoverByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
treecov <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(treecov) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))

## add delta
fls <- list.files("TIA/Results_Potapov_GADM_no100s/02022023/", pattern="DeltaByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(delta) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                       gsub(".csv", "", basename(fls[[2]])))



full <- inner_join(comb[,c("BIOME_NAME", "CarbonAccByBiome_sum_tic","CarbonAccByBiome_sum_tip")],
                   agarea[,c("CropAreaByBiome_tic_positiveDelta", "GrazeAreaByBiome_tip_positiveDelta","CropAreaByBiome_tic_ha","GrazeAreaByBiome_tip_ha","BIOME_NAME")],by="BIOME_NAME") %>%
  inner_join(treecov[,c("ForestCoverByBiome_mean_tic_2015_GADM_no100s","ForestCoverByBiome_mean_tip_2000_GADM","BIOME_NAME")], by="BIOME_NAME") %>%
  inner_join(delta[,c("DeltaByBiome_mean_tic_2015_GADM_no100s","DeltaByBiome_mean_tip_2000_GADM","BIOME_NAME")], by="BIOME_NAME")
  






colnames(full) <- c("BIOME_NAME","Mg C Potential TIA1", "Mg C Potential TIA2",
                    "Crop Area (m2)","Graze Area (m2)","Crop Area (ha)","Graze Area (ha)",
                    "Baseline mean tree cover TIA1 (2015)","Baseline mean tree cover TIA2 (2000)",
                    "Mean delta TIA1", "Mean delta TIA2")


write.csv(full, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byBiome.csv")
