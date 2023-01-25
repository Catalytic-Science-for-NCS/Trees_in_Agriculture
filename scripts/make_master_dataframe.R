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
comb[is.na(comb$full_name),]
comb <- comb[!is.na(comb$full_name),]

##add agricultural hectares
fls <- list.files("TIA/Results_Potapov_GADM_no100s/", pattern="AreaByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("GID_0", "NAME_0"))
colnames(agarea) <- c("GID_0", "NAME_0", gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))
agarea$full_name <- countrycode::countrycode(agarea$GID_0, origin="iso3c", destination="country.name", warn=T)
agarea[is.na(agarea$full_name),]
agarea <- agarea[!is.na(agarea$full_name),]
agarea$CropAreaByCountry_tic_ha <- agarea$CropAreaByCountry_tic/10000
agarea$GrazeAreaByCountry_tip_ha <- agarea$GrazeAreaByCountry_tip/10000

## add mean tree cover
fls <- list.files("TIA/Results/", pattern="CoverByCountry", full.names = T)
tbs <- lapply(fls, read.csv)
treecov <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("country_co", "country_na"))
colnames(treecov) <- c("country_code","country",  gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))
treecov$full_name <- countrycode::countrycode(treecov$country_code, origin="fips", destination="country.name", warn=T)
treecov[is.na(treecov$full_name),]
treecov <- treecov[!is.na(treecov$full_name),]

## add mean expert recs
fls <- list.files("TIA/Results/", pattern="expertRecsByCountry", full.names = T)
tbs <- lapply(fls, function(x){
  y <- read.csv(x) %>% 
    subset(select=c(country_co, country_na, `mean`))%>%
    group_by(country_co) %>%
    mutate(full=mean(`mean`, na.rm=T)) %>%
    distinct(country_co, full, .keep_all = TRUE) %>%
    subset(select=-`mean`)
})
expertrecs <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("country_co", "country_na"))
colnames(expertrecs) <- c("country_code","country", gsub(".csv", "", basename(fls[[1]])), 
                       gsub(".csv", "", basename(fls[[2]])))
expertrecs$full_name <- countrycode::countrycode(expertrecs$country_code, origin="fips", destination="country.name", warn=T)
expertrecs[is.na(expertrecs$full_name),]
expertrecs <- expertrecs[!is.na(expertrecs$full_name),]

fls <- list.files("TIA/Results/", pattern="DeltaByCountry", full.names = T)
tbs <- lapply(fls, function(x){
  y <- read.csv(x) %>% 
    subset(select=c(country_co, country_na, `mean`))%>%
    group_by(country_co) %>%
    mutate(full=mean(`mean`, na.rm=T)) %>%
    distinct(country_co, full, .keep_all = TRUE) %>%
    subset(select=-`mean`)
})
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("country_co", "country_na"))
colnames(delta) <- c("country_code","country", gsub(".csv", "", basename(fls[[1]])), 
                          gsub(".csv", "", basename(fls[[2]])))
delta$full_name <- countrycode::countrycode(delta$country_code, origin="fips", destination="country.name", warn=T)


#"TIA1 rank" "TIA 2 rank" "Total potential rank" and "Potential / ha rank"
full <- inner_join(comb[,c("country_code", "CarbonAccByCountry_sum_tic","CarbonAccByCountry_sum_tip","full_name")],
                   agarea[,c("AgAreaByCountry_tic", "AgAreaByCountry_tip", "full_name")],by="full_name") %>%
  inner_join(treecov[,c("CoverByCountry_mean_tic","CoverByCountry_mean_tip","full_name")], by="full_name") %>%
  inner_join(expertrecs[,c("expertRecsByCountry_mean_tic","expertRecsByCountry_mean_tip","full_name")], by="full_name") %>%
  inner_join(delta[,c("DeltaByCountry_mean_tic","DeltaByCountry_mean_tip","full_name")], by="full_name")
  
full$CarbonAccByCountry_sum_tic_50 <- full$CarbonAccByCountry_sum_tic/2
full$CarbonAccByCountry_sum_tic_75 <- full$CarbonAccByCountry_sum_tic*0.75
full$CarbonAccByCountry_sum_tip_50 <- full$CarbonAccByCountry_sum_tip/2
full$CarbonAccByCountry_sum_tip_75 <- full$CarbonAccByCountry_sum_tip*0.75

full$TIA1_perHA <- full$CarbonAccByCountry_sum_tic/full$AgAreaByCountry_tic
full$TIA2_perHA <- full$CarbonAccByCountry_sum_tip/full$AgAreaByCountry_tip

full$TIA1_TIA2_sum <- rowSums(full[, c("CarbonAccByCountry_sum_tic", "CarbonAccByCountry_sum_tip")])


## make rank orders ##

order.scores<-order(full$CarbonAccByCountry_sum_tic, full$full_name , decreasing = T)
full$TIA1_rank <- NA
full$TIA1_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$TIA1_perHA, full$full_name , decreasing = T)
full$TIA1_perHA_rank <- NA
full$TIA1_perHA_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$CarbonAccByCountry_sum_tip, full$full_name , decreasing = T)
full$TIA2_rank <- NA
full$TIA2_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$TIA2_perHA, full$full_name , decreasing = T)
full$TIA2_perHA_rank <- NA
full$TIA2_perHA_rank[order.scores] <- 1:nrow(full)

order.scores<-order(full$TIA1_TIA2_sum, full$full_name , decreasing = T)
full$TIA_total_rank <- NA
full$TIA_total_rank[order.scores] <- 1:nrow(full)


#order alphabetically
full <- full[order(full$full_name), ]


colnames(full) <- c("country_code","Mg C Potential TIA1 100%", "Mg C Potential TIA2 100%", "country_name",
                    "Ag Area TIA1 (has)","Ag Area TIA2 (has)","Baseline mean tree cover TIA1","Baseline mean tree cover TIA2",
                    "Mean expert rec TIA1", "Mean expert rec TIA2", "Mean delta TIA1", "Mean delta TIA2",
                    "Mg C Potential TIA1 50%", "Mg C Potential TIA1 75%", "Mg C Potential TIA2 50%", "Mg C Potential TIA2 75%",
                    "TIA1_perHA","TIA2_perHA","TIA1_TIA2_sum","TIA1_total_rank","TIA1_perHA_rank","TIA2_total_rank","TIA2_perHA_rank", "TIA_total_rank")

write.csv(full, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byCountry.csv")






## make master data table for biomes

fls <- list.files("TIA/Results/", pattern="CarbonAccByBiome", full.names = T) 
tbs <- lapply(fls, read.csv)
comb <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(comb) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))

#add agricultural hectares
fls <- list.files("TIA/Results/", pattern="AgAreaByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(agarea) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))
agarea$AgAreaByBiome_tic <- agarea$AgAreaByBiome_tic/10000
agarea$AgAreaByBiome_tip <- agarea$AgAreaByBiome_tip/10000


## add mean tree cover
fls <- list.files("TIA/Results/", pattern="CoverByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
treecov <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(treecov) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))

## add delta
fls <- list.files("TIA/Results/", pattern="DeltaByBiome", full.names = T)
tbs <- lapply(fls, read.csv)
delta <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("BIOME_NAME", "BIOME_NUM"))
colnames(delta) <- c("BIOME_NAME","BIOME_NUM", gsub(".csv", "", basename(fls[[1]])), 
                       gsub(".csv", "", basename(fls[[2]])))



full <- inner_join(comb[,c("BIOME_NAME", "CarbonAccByBiome_sum_tic","CarbonAccByBiome_sum_tip")],
                   agarea[,c("AgAreaByBiome_tic", "AgAreaByBiome_tip", "BIOME_NAME")],by="BIOME_NAME") %>%
  inner_join(treecov[,c("CoverByBiome_mean_tic","CoverByBiome_mean_tip","BIOME_NAME")], by="BIOME_NAME") %>%
  inner_join(delta[,c("DeltaByBiome_mean_tic","DeltaByBiome_mean_tip","BIOME_NAME")], by="BIOME_NAME")
  






colnames(full) <- c("BIOME_NAME","C Potential TIC", "C Potential TIP",
                    "Ag Area TIC (has)","Ag Area TIP (has)",
                    "Baseline mean tree cover TIC","Baseline mean tree cover tip",
                    "Mean delta TIC", "Mean delta TIP")


write.csv(full, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byBiome.csv")
