
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: TIA
# ===============
# Description: Create boxplot figures of expert recommended tree cover
#
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 
# Date updated:01/27/2023
# ===============
# load libraries
library(tidyverse)
library(patchwork)
library(grid)
library(pBrackets)


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------


data11<-read_delim("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/Trees_in_Agriculture/anovaData11.csv",delim=",",col_names=T)
data11<-data11 %>% 
  transmute(
    id=ID,
    Name=`Name (First Last):`,
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
data11 <- subset(data11, select=c(id, anova, continent, biome, climate, vegetation, cover_byCrop, tph, cover_allCrops, crop))

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


setwd("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/")
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

# #calculate n for above each boxplot
# n_sys <- cow %>% 
#   group_by(biome_fullName) %>% 
#   tally()
# 
# byGrazer <- ggplot(cow, aes(x=biome_fullName, y=cover_all))+ 
#   #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
#   geom_boxplot(outlier.shape=1)+    
#   stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
#   theme_minimal()+
#   labs(x="Biome", y="Cover (%)")+
#   geom_text(data = n_sys, aes(Biome, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(angle=70, size=5, vjust=0.9, hjust=0.85))
# byGrazer

#get summary stats

# grazing_biome <- cow %>%
#   group_by(biome_fullName, Ungulate_Code) %>%
#   summarize(mn = mean(cover_all),
#             stdev = sd(cover_all),
#             p25 = quantile(cover_all, c(0.25)),
#             p75 = quantile(cover_all, c(0.75)))
# write.csv(grazing_biome, "TIA/grazers_byUngulateBiome.csv")

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
tet$missing <- "no"

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

tet_missing$missing <- "yes"



both <- rbind(tet,tet_missing)
both$biome_abbrev <- factor(both$biome_abbrev, levels=c("DXS","MF","MGSS","TeGSS","TeCF",
                                                            "TeBMF","TrSGSS","TrSCF","TrSDBF","TrSMBF"))

#remove 2 100% recommended cover
both <- subset(both, both$value!=100)
#remove coniferous forests (temperate and tropical)
both <- subset(both, biome_fullName!="Temperate Conifer Forests" & biome_fullName!="Tropical & Subtropical Coniferous Forest")
#add biome groupings 
both <- both %>% mutate(climate=case_when(biome_abbrev=="DXS" ~ "Desert",
                                          biome_abbrev=="MF" ~ "Mediterranean",
                                          biome_abbrev=="MGSS" ~ "Montane",
                                          biome_abbrev=="TeBMF"~"Temperate",
                                          biome_abbrev=="TeGSS"~"Temperate",
                                          biome_abbrev=="TrSDBF" ~ "Tropical/Subtropical",
                                          biome_abbrev=="TrSGSS" ~ "Tropical/Subtropical",
                                          biome_abbrev=="TrSMBF" ~ "Tropical/Subtropical"))


n_both <-both %>%
  group_by(C_G, biome_abbrev, missing) %>% 
  tally()
both <- right_join(n_both, both, by=c("C_G","biome_abbrev", "missing"))
both$n <- 

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="right", legend.title=element_blank(), panel.grid.major.x=element_blank())
theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="grey20"), axis.ticks.y= element_line(colour="grey20"))
cols= c("#CEAB07", "#798E87")



bplot <- ggplot(both, aes(x=biome_abbrev, y=value, color=C_G))+ 
  geom_boxplot(outlier.colour = NULL, aes(color=C_G), width = 0.5, position = "dodge", show.legend = F)+
  stat_summary(fun=mean, geom="line", linewidth=1.5, position=position_dodge(width=0.5), show.legend=T)+   #line for the mean
  geom_text(aes(y=104, label=n), 
           size=3.5, vjust = 1, position=position_dodge(width = 0.5), show.legend = F)+
  facet_wrap(~climate, strip.position = "bottom", scales = "free_x", nrow=1) +
  theme(legend.title = element_blank(),
        axis.title.y = element_text( angle = 90),
        strip.placement = "outside",
        strip.background=element_rect(color="grey20"))+
  labs(x="Biome", y="Expert Estimated Tree Cover (%)") +  
  scale_color_manual(values = cols);bplot
 

ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/expertRecs_boxplots.png",
       bplot, width=10, height=6, dpi=300, bg="white")


#### area-weighted mean by climate group for ABACUS label
crop <- read.csv("GitHub/Trees_in_Agriculture/data/06_21_2023/potapov_area_ha_sum_30m.csv") %>% select(-BIOME_NAME)
graze <- read.csv("GitHub/Trees_in_Agriculture/data/06_21_2023/ramankutty_area_ha_sum_30m.csv")%>% select(-BIOME_NAME)
has <- rbind(crop, graze)
has$C_G <- c(rep("Crop",10), rep("Grazing",10))
colnames(has) <- c("BIOME_NUM","hectares","C_G")

df <- both %>% mutate(BIOME_NUM = case_when(biome_abbrev=="DXS"~13,
                                              biome_abbrev=="MF"~12,
                                              biome_abbrev=="MGSS"~10,
                                              biome_abbrev=="TeGSS"~8,
                                              biome_abbrev=="TeBMF"~4,
                                              biome_abbrev=="TrSGSS"~7,
                                              biome_abbrev=="TrSDBF"~2,
                                              biome_abbrev=="TrSMBF"~1)) %>%
  select(-c(missing, name))
df <- left_join(df, has, by=c("BIOME_NUM","C_G"))

mns <- df %>%
  dplyr::group_by(C_G, climate) %>%
  dplyr::summarize(mn = mean(value, na.rm=T),
                   wt.mn = weighted.mean(x=value, w=hectares, na.rm=T),
                   quant25 = quantile(value, probs=c(0.25)),
                   wt.quant25 = Hmisc::wtd.quantile(value, weights=hectares, probs=c(0.25), type=c('quantile')))

ns <- as.data.frame(matrix(data=NA, nrow=nrow(mns), ncol=3))
names(ns) <- c("C_G","climate", "n")
ns$C_G <- mns$C_G
ns$climate <- mns$climate
ns$n <- c(1, 4, 1, 16, 22, 4, 4, 2, 7, 21)
mns <- left_join(mns, ns, by=c("C_G","climate"))

write.csv(mns, "GitHub/Trees_in_Agriculture/data/summary_expertRecs_abacus.csv")
