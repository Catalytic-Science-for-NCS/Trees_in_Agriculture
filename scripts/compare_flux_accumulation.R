#double check flux density and carbon acc calcs between one another


#carbon acc
fls <- list.files("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/",
                  pattern="CarbonAcc", full.names = T) 
tbs <- lapply(fls, read.csv)
comb <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("ISO3","COUNTRY", "BIOME_NAME","BIOME_NUM"))
colnames(comb) <- c("BIOME_NAME", "BIOME_NUM","ISO3","NAME", str_extract(gsub(".csv", "", basename(fls[[1]])), "tic|tip"), 
                    str_extract(gsub(".csv", "", basename(fls[[2]])), "tic|tip"))

fls <- list.files("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/",
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

join <- inner_join(comb, agarea[,-c(5,6)], by=c("ISO3","NAME","BIOME_NAME","BIOME_NUM"))
nrow(join)
nrow(comb)
nrow(agarea)
#why aren't these the same?

#flux density
flx <- read.csv("TIA/Results_Potapov_GADM_no100s/drive-download-20230213T231648Z-001/02_17_2023/FluxDensity_byBiomeCountry_TIA1_TIA2.csv")
