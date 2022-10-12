## Create boxplot figures of expert recommended tree cover
## By biome, by crop vs grazing
# Vivian Griffey, vgriffey@conservation.org
# Last updated: 10/11/2022

library(tidyverse)
library(patchwork)

data11<-read_delim("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/anovaData11.csv",delim=",",col_names=T)
data11<-data11 %>% 
  transmute(
    id=ID,
    anova=ANOVA,
    continent=`CONTINENT: Please select the continent you will address in this questionnaire.  Please select only one.  We welcome responses for multiple continents; however; as responses will likely vary by bio...`,
    avgHeight=`TreeAvgMatureHeight(M)R2`,
    avgWidth=`TreeAvgMatureWidth(M)R2`,
    treeArea=`Tree Area (m2)RecalculatedR2`,
    crop=Crop,
    biome=Biome,
    climate=Clim,
    vegetation=Veg,
    cover_byCrop=`CS_Final1OptimalCropSpecificTotal TreeCover/ha (%) w/ Mechanization scaled across all cropland by level of mech(CY)GivenR2`,
    tph=`Tha_2FinalOptimalAcrossAll CropsTrees/ha_GivenR2`,
    cover_allCrops=`AA_3FinalOptimalAcrossAllCrops%cover_GivenR2`  
  )
#drop unecessary columns
data11 <- subset(data11, select=c(id,anova, continent, biome, climate, vegetation, cover_byCrop, tph, cover_allCrops, crop))

#remove % sign
data11$cover_allCrops <- gsub("%","", data11$cover_allCrops)
data11$cover_byCrop <- gsub("%","", data11$cover_byCrop)

#make full biome names
data11 <- data11 %>% mutate(biome_fullName=case_when(
  biome == "MF" ~ "Mediterranean Forests, Woodland, & Scrub",
  biome == "TBMF" ~ "Temperate Broadleaf & Mixed Forests",
  biome == "TGSS"~ "Temperate Grasslands, Savannas, and Shrublands",
  biome == "TrSDBF"~"Tropical & Subtropical Dry Broadleaf Forests",
  biome =="TrSGSS"~"Tropical & Subtropical Grasslands, Savannas, and Shrublands",
  biome == "TrSMBF" ~ "Tropical & Subtropical Moist Broadleaf Forests"
))

#change data classes as needed
data11$crop <- as.factor(data11$crop)
data11$tph <- as.numeric(data11$tph)
data11$cover_allCrops <- as.numeric(data11$cover_allCrops)
data11$cover_byCrop <- as.numeric(data11$cover_byCrop)
data11$vegetation <- as.factor(data11$vegetation)
data11$biome <- as.factor(data11$biome)
data11$continent <-as.factor(data11$continent)
data11$climate <- as.factor(data11$climate)
data11$biome_fullName <- as.factor(data11$biome_fullName)

#remove the weird one row of NA
data11 <- data11[!is.na(data11$id),]
#remove duplicates
data11_sub <- data11[data11$anova!=2,]


#setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/")
cow <- read.csv("TIA/TIA2_reccs2.csv", check.names = F)
colnames(cow) <- c("id","anova","Name","continent", "biome_fullName","Ungulate_Code","cover_bySys","cover_all")

#drop NA column
cow <- subset(cow, select=-`NA`)

#change data classes as needed
#cow$P_R <- as.factor(cow$P_R)
cow$cover_all <- as.numeric(cow$cover_all)
cow$cover_bySys <- as.numeric(cow$cover_bySys)
cow$biome_fullName <- as.factor(cow$biome_fullName)
cow$continent <-as.factor(cow$continent)
cow$Ungulate_Code <- as.factor(cow$Ungulate_Code)

#convert to percent
cow$cover_all <- cow$cover_all*100
cow$cover_bySys <- cow$cover_bySys*100

#remove duplicates for all ungulates
cow <- subset(cow, anova!=2)

#calculate n for above each boxplot
n_sys <- cow %>% 
  group_by(biome_fullName) %>% 
  tally()

byGrazer <- ggplot(cow, aes(x=biome_fullName, y=cover_all))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Cover (%)")+
  geom_text(data = n_sys, aes(Biome, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=70, size=5, vjust=0.9, hjust=0.85))
byGrazer

#get summary stats

grazing_biome <- cow %>%
  group_by(biome_fullName, Ungulate_Code) %>%
  summarize(mn = mean(cover_all),
            stdev = sd(cover_all),
            p25 = quantile(cover_all, c(0.25)),
            p75 = quantile(cover_all, c(0.75)))
write.csv(grazing_biome, "TIA/grazers_byUngulateBiome.csv")

#combine with crop data
data11_sub$C_G <- "Crop"
cow$C_G <- "Grazing"
data11_long <- pivot_longer(data11_sub, c("cover_allCrops"))
cow_long <- pivot_longer(cow, "cover_all")
tet <- rbind(cow_long[,c("biome_fullName","C_G","name","value")], 
      data11_long[, c("biome_fullName","C_G", "name", "value")])
desert_crop <- c("Deserts & Xeric Shrublands", "Crop", "cover_allCrops", 4)
montane_crop <- c("Montane Grasslands & Shrublands","Crop","cover_allCrops", 27.66667)
tempgrass_graze <- c("Temperate Grasslands, Savannas, and Shrublands",
               "Grazing","cover_all", 27.66667)
conifer_crop <- c("Temperate Conifer Forests", "Crop", "cover_allCrops", 19.7)
conifer_graze <- c("Temperate Conifer Forests", "Grazing", "cover_all", 25.2)
trop_conifer_crop <- c("Tropical & Subtropical Coniferous Forest", "Crop", "cover_allCrops", 19.6)
trop_conifer_graze <- c("Tropical & Subtropical Coniferous Forest", "Grazing", "cover_all", 28.3)

tet_missing <- rbind(desert_crop, montane_crop) %>% rbind(tempgrass_graze) %>% rbind(conifer_crop) %>%
  rbind(conifer_graze) %>% rbind(trop_conifer_crop) %>% rbind(trop_conifer_graze)%>% as.data.frame() %>% as.tibble()
colnames(tet_missing) <- colnames(tet)
tet_missing$value <- as.numeric(tet_missing$value)

tet <- tet %>% mutate(biome_abbrev = case_when(
  biome_fullName == "Mediterranean Forests, Woodland, & Scrub" ~"MF" ,
  biome_fullName == "Temperate Broadleaf & Mixed Forests" ~"TeBMF" ,
  biome_fullName ==  "Temperate Grasslands, Savannas, and Shrublands" ~ "TeGSS",
  biome_fullName == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TrSDBF",
  biome_fullName == "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS",
  biome_fullName ==  "Tropical & Subtropical Moist Broadleaf Forests" ~ "TrSMBF",
  biome_fullName ==  "Deserts & Xeric Shrublands" ~ "DXS",
  biome_fullName ==  "Montane Grasslands & Shrublands" ~ "MGSS",
  biome_fullName ==  "Temperate Conifer Forests" ~ "TeCF",
  biome_fullName ==  "Tropical & Subtropical Coniferous Forest" ~ "TrSCF",
  biome_fullName ==  "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS"
))

tet_missing <- tet_missing %>% mutate(biome_abbrev = case_when(
  biome_fullName == "Mediterranean Forests, Woodland, & Scrub" ~"MF" ,
  biome_fullName == "Temperate Broadleaf & Mixed Forests" ~"TeBMF" ,
  biome_fullName ==  "Temperate Grasslands, Savannas, and Shrublands" ~ "TeGSS",
  biome_fullName == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TrSDBF",
  biome_fullName == "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS",
  biome_fullName ==  "Tropical & Subtropical Moist Broadleaf Forests" ~ "TrSMBF",
  biome_fullName ==  "Deserts & Xeric Shrublands" ~ "DXS",
  biome_fullName ==  "Montane Grasslands & Shrublands" ~ "MGSS",
  biome_fullName ==  "Temperate Conifer Forests" ~ "TeCF",
  biome_fullName ==  "Tropical & Subtropical Coniferous Forest" ~ "TrSCF",
  biome_fullName ==  "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS"
))


n_both <- rbind(tet,tet_missing) %>% 
  group_by(C_G, biome_abbrev) %>% 
  tally()
#n_both$combo_ID <- paste0(n_both$C_G, "_", n_both$biome_fullName)

ggplot(tet, aes(x=biome_abbrev, y=value, color=C_G))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Expert Recommended Tree Cover (%)")+
 geom_text(data = n_both, aes(biome_abbrev, Inf, label = n, color=C_G), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())+
  geom_point(data=tet_missing, aes(x=biome_abbrev, y=value, color=C_G))


#does tree cover per hectare, 

#biome potential as % C of total



## Boxplots for comparing final TIA 1 and 2 expert recommended tree cover across biomes
## Boxplots for comparing final TIA 1 and 2 expert recommended tree cover across biomes
fin <- read.csv("TIA/biomes_stats_update_2.csv") %>% subset(select=c(X.1, X.2, TIA1,TIA1.4, TIA2, TIA2.4))
colnames(fin) <- c("biome_abbrev","biome_fullName","TIA1_mn","TIA1_n","TIA2_mn","TIA2_n")
fin <- fin[2:nrow(fin),]

#change data classes as needed
#cow$P_R <- as.factor(cow$P_R)
fin$TIA1_mn <- as.numeric(fin$TIA1_mn)
fin$TIA2_mn <- as.numeric(fin$TIA2_mn)
fin$TIA1_n <- as.numeric(fin$TIA1_n)
fin$TIA2_n <- as.numeric(fin$TIA2_n)
fin$biome_fullName <- as.factor(fin$biome_fullName)
fin$biome_abbrev <- as.factor(fin$biome_abbrev)

#convert to long format for ggplot
long <- pivot_longer(fin, cols=c(TIA1_mn, TIA2_mn, TIA1_n, TIA2_n))

byGrazer <- ggplot(subset(long, name=="TIA1_mn" | name=="TIA2_mn"), aes(x=biome_abbrev, y=value, color=name))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Expert Recommended Tree Cover (%)")+
  #geom_text(subset(long, value=="TIA1_n" | value=="TIA2_n"), aes(x=biome_abbrev, y=Inf, label = value), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())
byGrazer

#get summary stats

grazing_biome <- cow %>%
  group_by(biome_fullName, Ungulate_Code) %>%
  summarize(mn = mean(cover_all),
            stdev = sd(cover_all),
            p25 = quantile(cover_all, c(0.25)),
            p75 = quantile(cover_all, c(0.75)))
write.csv(grazing_biome, "TIA/grazers_byUngulateBiome.csv")

#combine with crop data
data11_sub$C_G <- "C"
cow$C_G <- "G"
data11_long <- pivot_longer(data11_sub, c("cover_allCrops"))
cow_long <- pivot_longer(cow, "cover_all")
tet <- rbind(cow_long[,c("biome_fullName","C_G","name","value")], 
             data11_long[, c("biome_fullName", "C_G", "name", "value")])
desert_crop <- c("Deserts & Xeric Shrublands", "C", "cover_allCrops", 4)
montane_crop <- c("Montane Grasslands & Shrublands","C","cover_allCrops", 27.7)
tempgrass_graze <- c("Temperate Grasslands, Savannas, and Shrublands",
                     "G","cover_all", 27.7)
conifer_crop <- c("Temperate Conifer Forests", "C", "cover_allCrops", 19.7)

tet_missing <- rbind(desert_crop, montane_crop) %>% rbind(tempgrass_graze) %>% rbind(conifer_crop) %>% as.data.frame() %>% as.tibble()
colnames(tet_missing) <- colnames(tet)
tet_missing$value <- as.numeric(tet_missing$value)

n_both <- tet %>% 
  group_by(C_G, biome_fullName) %>% 
  tally()
n_both$combo_ID <- paste0(n_both$C_G, "_", n_both$biome_fullName)

ggplot(tet, aes(x=biome_fullName, y=value, color=C_G))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Cover (%)")+
  # geom_text(data = n_both, aes(combo_ID, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=70, size=5, vjust=0.9, hjust=0.85))+
  geom_point(data=tet_missing, aes(x=biome_fullName, y=value, color=C_G))


#does tree cover per hectare, 

#biome potential as % C of total