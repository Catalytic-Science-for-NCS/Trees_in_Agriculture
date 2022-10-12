#ok let's do the dang thing
#TIA ANOVA
#how do experts' responses vary by crop and biome (and climate and veg)?
#experts' responses = cover_byCrop, tph, cover_allCrops
#potential spanners in the works:
#an effect of ‘continent’ as a potential explanatory variable
#expert ^^
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

#format long continent names (consider for biome names as well)
data11$continent <- ifelse(data11$continent=="North/Central America (includes Caribbean)",
                           "North/Central America\n (includes Caribbean)",
                           data11$continent)

#spell out F and G
data11$vegetation <- ifelse(data11$vegetation=="F", "Forest","Grass")

  
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


#calculate n for above each boxplot
n_crop <- data11 %>% 
  group_by(crop, biome) %>% 
  tally()

bycrop1 <- ggplot(data11, aes(x=crop, y=cover_allCrops, color=biome))+ 
#  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Crop", y="Cover (%)", title="By crop and biome")+
  geom_text(data = n_crop, aes(crop, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())

# bycrop2 <- ggplot(data11, aes(x=crop, y=cover_byCrop, color=biome))+ 
#   #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
#   geom_boxplot(outlier.shape=1)+    
#   stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
#   theme_minimal()+
#   labs(x="Crop", y="Cover (by crop)")+
#   geom_text(data = n_crop, aes(crop, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))

#library(patchwork) allows arranging ggplots using +, -, / etc
#bycrop1/bycrop2

#need to remove duplicates after taking out crops
data11_sub <- data11[data11$anova!=2,]


n_biome <- data11_sub %>% 
  group_by(biome) %>% 
  tally()
n_climate <- data11_sub %>% 
  group_by(climate, vegetation) %>% 
  tally()
n_biome_cont <- data11_sub %>%
  group_by(biome, continent) %>%
  tally()


bybiome1 <- ggplot(data11_sub, aes(x=biome, y=cover_allCrops))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="", title="By biome only")+
  theme(axis.text.x=element_text(angle=90))+
  geom_text(data = n_biome, aes(biome, Inf, label = n), vjust = 1)+
  theme(legend.title = element_blank())

# bybiome2 <- ggplot(data11_sub, aes(x=biome, y=cover_byCrop))+ 
#   #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
#   geom_boxplot(outlier.shape=1)+    
#   stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
#   theme_minimal()+
#   labs(x="Biome", y="Cover (overall)")+
#   theme(axis.text.x=element_text(angle=90))+
#   geom_text(data = n_biome, aes(biome, Inf, label = n), vjust = 1)
# 
# bybiome1/bybiome2

byclimate1 <- ggplot(data11_sub, aes(climate, cover_allCrops, color=vegetation))+
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Climate", y="",title="By climate and veg")+
  #theme(axis.text.x=element_text(angle=90))+
  geom_text(data = n_climate, aes(climate, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())

# byclimate2 <- ggplot(data11_sub, aes(climate, cover_byCrop, color=vegetation))+
#   geom_boxplot(outlier.shape=1)+    
#   stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
#   theme_minimal()+
#   labs(x="Climate", y="Cover (overall)")+
#   #theme(axis.text.x=element_text(angle=90))+
#   geom_text(data = n_climate, aes(climate, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))
# 
# byclimate1/byclimate2

cont1 <- ggplot(data11_sub, aes(x=biome, y=cover_allCrops, color=continent))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Cover (%)", title="By biome and continent")+
  theme(axis.text.x=element_text(angle=90))+
  geom_text(data = n_biome_cont, aes(biome, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())

#altogether now
#change x axislabels to abbrevs, changed y axis to "expert reco tree cov"

tot <- bycrop1 + byclimate1 + cont1 + bybiome1 + plot_layout(ncol=2) +plot_annotation(title="Tree Cover Overall")
ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/boxplots_TIA1.png",
       tot, width=9, height=6, dpi=300)


#if we removed the rows that Starry filled in for and duplicates
data11_sub_starry <- data11_sub[grep("x", data11_sub$id, invert = T),]
n_biome <- data11_sub_starry %>% 
  group_by(biome) %>% 
  tally()
n_biome_cont <- data11_sub_starry %>%
  group_by(biome, continent) %>%
  tally()

bybiome2 <- ggplot(data11_sub_starry, aes(x=biome, y=cover_allCrops))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="", title="By biome only")+
  theme(axis.text.x=element_text(angle=90))+
  geom_text(data = n_biome, aes(biome, Inf, label = n), vjust = 1)+
  theme(legend.title = element_blank())

cont2 <- ggplot(data11_sub_starry, aes(x=biome, y=cover_allCrops, color=continent))+ 
  #  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun=mean, geom="point", size=2, position=position_dodge(width=0.755))+   #dot for the mean  theme_minimal()
  theme_minimal()+
  labs(x="Biome", y="Cover (%)", title="By biome and continent")+
  theme(axis.text.x=element_text(angle=90))+
  geom_text(data = n_biome_cont, aes(biome, Inf, label = n), vjust = 1, position=position_dodge(width=0.755))+
  theme(legend.title = element_blank())

remove_fillins <-  cont2 + bybiome2
ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/boxplots_TIA1_noFilledIn.png",
       remove_fillins, width=9, height=4, dpi=300)

##################################################################################
## ANOVA/KW/other analyses below
## currently not in use bc of small sample size and subjective dependent variable
data11 %>% 
  group_by(crop, biome) %>% 
  summarize(mn_crop_biome=mean(cover_allCrops))
data11 %>% 
  group_by(biome) %>% 
  summarize(mn_biome=mean(cover_allCrops))
data11 %>% 
  group_by(climate, vegetation) %>% 
  summarize(mn=mean(cover_allCrops))

#to test for additive (+) to test for interaction (*)
#ANOVA using all rows, do crops do anything to responses?
#no
#probably because sample size is v smol
#response ~ predictor
crop_tph <- aov(tph ~ crop, data = data11)
crop_cov <- aov(cover_allCrops~ crop, data = data11)
crop_covMech <- aov(cover_byCrop ~ crop, data = data11)
summary(crop_covMech)

#check for non-normal residuals
par(mfrow=c(2,2))
plot(crop_tph) #suss
plot(crop_cov) #also suss
plot(crop_covMech) #hmmm

#Tukey's HSD tests what the diff is
tuk <- TukeyHSD(crop_tph)
plot(tuk, las = 1) #you want your error bars to not overlap

#red line representing the mean of the residuals should be horizontal and centered on zero (or on one, in the scale-location plot), meaning that there are no large outliers that would cause bias in the model.
#in QQ plot, closer to a slope of 1 the better
#if not normal, use Kruskal-Wallis (yup!)
#A collection of data samples are independent if they come from unrelated populations
# and the samples do not affect each other. 
#Using the Kruskal-Wallis Test, we can decide whether the population distributions 
# are identical without assuming them to follow the normal distribution.
#Can use as long as the two distributions are similar
#If your samples have very different distributions or very unequal variances, 
# these tests will cause an inflated Type I error rate

kruskal.test(tph ~ crop, data = data11)
kruskal.test(cover_byCrop ~ crop, data = data11)
kruskal.test(cover_allCrops ~ crop, data = data11)

# pairwise.wilcox.test(data11$tph, data11$crop,
#                      p.adjust.method = "BH")
# pairwise.wilcox.test(data11$cover_byCrop, data11$crop,
#                      p.adjust.method = "BH")
# pairwise.wilcox.test(data11$cover_allCrops, data11$crop,
#                      p.adjust.method = "BH")
# #In wilcox.test.default(xi, xj, paired = paired, ...) :
# #cannot compute exact p-value with ties
# #what does this mean??
#means you need to use dunn's test that's what

#another way to do KW test
#response ~ predictor
data11 %>% rstatix::kruskal_test(cover_allCrops ~ crop)
data11 %>% rstatix::kruskal_test(tph ~ crop)
data11 %>% rstatix::kruskal_test(cover_byCrop ~ crop)

# Effect size
# The eta squared, based on the H-statistic, can be used as the measure of the Kruskal-Wallis test effect size. It is calculated as follow : eta2[H] = (H - k + 1)/(n - k); where H is the value obtained in the Kruskal-Wallis test; k is the number of groups; n is the total number of observations (M. T. Tomczak and Tomczak 2014).
# The eta-squared estimate assumes values from 0 to 1 and multiplied by 100 indicates the percentage of variance in the dependent variable explained by the independent variable.
# The interpretation values commonly in published literature are: 
#0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).

data11 %>% rstatix::kruskal_effsize(tph ~ crop)
data11 %>% rstatix::kruskal_effsize(cover_byCrop ~ crop)
data11 %>% rstatix::kruskal_effsize(cover_allCrops ~ crop)

#From above, we know that there is a significant difference between groups in tph
#but we don’t know which pairs of groups are different
#if p<0.05, then follow up with Dunn’s test to identify which groups are different
#could also use the Wilcoxon’s test to calculate pairwise comparisons between group levels 
#with corrections for multiple testing
data11 %>% rstatix::dunn_test(tph ~ crop, p.adjust.method = "bonferroni") 


#what about biome
#remove any crop duplicates
bio <- subset(data11, select=-c(crop))
bio <- bio[!duplicated(bio),]

ggplot(bio, aes(x=biome, y=cover_byCrop))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean
  theme_minimal()

ggplot(bio, aes(x=biome, y=cover_allCrops))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean  theme_minimal()
  theme_minimal()

ggplot(bio, aes(x=biome, y=tph))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2) +  #dot for the mean  theme_minimal()
  theme_minimal()

#probably because sample size is v smol
biome_tph <- aov(tph ~ biome, data = bio)
biome_cov <- aov(cover_allCrops~ biome, data = bio)
biome_covMech <- aov(cover_byCrop ~ biome, data = bio)
summary(biome_tph)

#check for non-normal residuals
par(mfrow=c(2,2))
plot(biome_tph) #suss
plot(biome_cov) #also suss
plot(biome_covMech) #hmmm

#another way to do KW test
bio %>% rstatix::kruskal_test(cover_allCrops ~ biome)
bio %>% rstatix::kruskal_test(tph ~ biome)
bio %>% rstatix::kruskal_test(cover_byCrop ~ biome)

# Effect size
bio %>% rstatix::kruskal_effsize(tph ~ biome)
bio %>% rstatix::kruskal_effsize(cover_byCrop ~ biome)
bio %>% rstatix::kruskal_effsize(cover_allCrops ~ biome)

#dunn's test 
bio %>% rstatix::dunn_test(tph ~ biome, p.adjust.method = "bonferroni") 
bio %>% rstatix::dunn_test(cover_byCrop ~ biome, p.adjust.method = "bonferroni") 
bio %>% rstatix::dunn_test(cover_allCrops ~ biome, p.adjust.method = "bonferroni") 

#do with cover type and climate
bio %>% rstatix::kruskal_test(cover_allCrops ~ climate)
bio %>% rstatix::kruskal_test(tph ~ biome)
bio %>% rstatix::kruskal_test(cover_byCrop ~ biome)

## OK SO
## TURNS OUT
## you need to use Friedman's ANOVA for non-parametric two way anova (i.e. with interactions)
# KW test is only for one-way ANOVA (non-parametric)
# Remove NAs
# bio <- na.omit(bio[,c("id","continent","biome","climate","vegetation", "cover_byCrop",
#                           "tph","cover_allCrops")])

# Re-organise data so the scores are stacked, and a column added with the original column name as a factor
bio <-  with(bio, bio[order(climate, vegetation),])

bio %>% as.data.frame() %>% rstatix::friedman_test(cover_allCrops ~ climate |vegetation)
