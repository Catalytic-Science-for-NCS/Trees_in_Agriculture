library(tidyverse)
library(countrycode)

## make master data table for countries

fls <- list.files("TIA/Results/", pattern="CarbonAccByCountry", full.names = T) 
##some of the countries have multiple entries. need to add together so that there is one row per country
add_duplicate_rows <- function(df){
  df %>% 
    group_by(country_co) %>%
    mutate(full=sum(sum))
}

tbs <- lapply(fls, function(x){
  y <- read.csv(x) %>% 
    subset(select=c(country_co, country_na, sum))%>%
    add_duplicate_rows() %>%
    distinct(country_co, full, .keep_all = TRUE) %>%
    subset(select=-`sum`)
})
  
comb <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("country_co", "country_na"))
colnames(comb) <- c("country_code","country",  gsub(".csv", "", basename(fls[[1]])), 
                    gsub(".csv", "", basename(fls[[2]])))

comb$full_name <- countrycode::countrycode(comb$country_code, origin="fips", destination="country.name", warn=T)
#double check which ones got left out, make sure you're ok with it
comb[is.na(comb$full_name),]
comb <- comb[!is.na(comb$full_name),]


##add agricultural hectares
fls <- list.files("TIA/Results/", pattern="AgAreaByCountry", full.names = T)
tbs <- lapply(fls, function(x){
  y <- read.csv(x) %>% 
    subset(select=c(country_co, country_na, sum))%>%
    add_duplicate_rows() %>%
    distinct(country_co, full, .keep_all = TRUE) %>%
    subset(select=-`sum`)
})
agarea <- dplyr::inner_join(tbs[[1]], tbs[[2]], by=c("country_co", "country_na"))
colnames(agarea) <- c("country_code","country", gsub(".csv", "", basename(fls[[1]])), 
                      gsub(".csv", "", basename(fls[[2]])))
agarea$full_name <- countrycode::countrycode(agarea$country_code, origin="fips", destination="country.name", warn=T)
agarea[is.na(agarea$full_name),]
agarea <- agarea[!is.na(agarea$full_name),]
agarea$AgAreaByBiome_tic <- agarea$AgAreaByCountry_tic/10000
agarea$AgAreaByBiome_tip <- agarea$AgAreaByCountry_tip/10000

## add mean tree cover
fls <- list.files("TIA/Results/", pattern="CoverByCountry", full.names = T)
tbs <- lapply(fls, function(x){
  y <- read.csv(x) %>% 
    subset(select=c(country_co, country_na, `mean`))%>%
    group_by(country_co) %>%
    mutate(full=mean(`mean`, na.rm=T)) %>%
    distinct(country_co, full, .keep_all = TRUE) %>%
    subset(select=-`mean`)
})
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



full <- inner_join(comb[,c("country_code", "CarbonAccByCountry_sum_tic","CarbonAccByCountry_sum_tip","full_name")],
                   agarea[,c("AgAreaByCountry_tic", "AgAreaByCountry_tip", "full_name")],by="full_name") %>%
  inner_join(treecov[,c("CoverByCountry_mean_tic","CoverByCountry_mean_tip","full_name")], by="full_name") %>%
  inner_join(expertrecs[,c("expertRecsByCountry_mean_tic","expertRecsByCountry_mean_tip","full_name")], by="full_name") %>%
  inner_join(delta[,c("DeltaByCountry_mean_tic","DeltaByCountry_mean_tip","full_name")], by="full_name")
  
  

colnames(full) <- c("country_code","C Potential TIC", "C Potential TIP", "country_name",
                    "Ag Area TIC (has)","Ag Area TIP (has)","Baseline mean tree cover TIC","Baseline mean tree cover tip",
                    "Mean expert rec TIC", "Mean expert rec TIP", "Mean delta TIC", "Mean delta TIP")

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
