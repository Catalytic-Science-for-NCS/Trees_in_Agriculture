#ok let's do the dang thing
#TIA ANOVA
#how do experts' responses vary by crop and biome (and climate and veg)?
#experts' responses = percentCover_optimalWithMech, treesPerHec_optimalOverall, percentCover_optimalOverall
#potential spanners in the works:
#an effect of ‘continent’ as a potential explanatory variable
#expert ^^
library(tidyverse)

data11<-read_delim("anovaData11.csv",delim=",",col_names=T)
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
    percentCover_optimalWithMech=`CS_Final1OptimalCropSpecificTotal TreeCover/ha (%) w/ Mechanization scaled across all cropland by level of mech(CY)GivenR2`,
    treesPerHec_optimalOverall=`Tha_2FinalOptimalAcrossAll CropsTrees/ha_GivenR2`,
    percentCover_optimalOverall=`AA_3FinalOptimalAcrossAllCrops%cover_GivenR2`  
  )
#remove % sign
data11$percentCover_optimalOverall <- gsub("%","", data11$percentCover_optimalOverall)
data11$percentCover_optimalWithMech <- gsub("%","", data11$percentCover_optimalWithMech)

#make full biome names
data11 <- data11 %>% mutate(biome_fullName=case_when(
  "MF"~
))
  
  
#change data classes as needed
data11$crop <- as.factor(data11$crop)
data11$treesPerHec_optimalOverall <- as.numeric(data11$treesPerHec_optimalOverall)
data11$percentCover_optimalOverall <- as.numeric(data11$percentCover_optimalOverall)
data11$percentCover_optimalWithMech <- as.numeric(data11$percentCover_optimalWithMech)

#remove the weird one row of NA
data11 <- data11[!is.na(data11$id),]

ggplot(data11, aes(x=crop, y=percentCover_optimalOverall, color=biome))+ 
  geom_bar(position=position_dodge(), aes(y=percentCover_optimalOverall), stat="identity") +
  geom_errorbar(position=position_dodge(width=0.9), colour="black") +
  geom_point(position=position_dodge(width=0.9), aes(y=percentCover_optimalOverall, colour=biome))+
  theme_minimal()+
  labs(x="Crop", y="Percent Cover Optimal Overall")

ggplot(data11, aes(x=crop, y=percentCover_optimalWithMech, color=biome))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean  theme_minimal()
  theme_minimal()

ggplot(data11, aes(x=crop, y=treesPerHec_optimalOverall, color=biome))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean  theme_minimal()
  theme_minimal()

#to test for additive (+) to test for interaction (*)
#ANOVA using all rows, do crops do anything to responses?
#no
#probably because sample size is v smol
crop_tph <- aov(treesPerHec_optimalOverall ~ crop, data = data11)
crop_cov <- aov(percentCover_optimalOverall~ crop, data = data11)
crop_covMech <- aov(percentCover_optimalWithMech ~ crop, data = data11)
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

kruskal.test(treesPerHec_optimalOverall ~ crop+biome, data = data11)
kruskal.test(percentCover_optimalWithMech ~ crop, data = data11)
kruskal.test(percentCover_optimalOverall ~ crop, data = data11)

pairwise.wilcox.test(data11$treesPerHec_optimalOverall, data11$crop,
                     p.adjust.method = "BH")
pairwise.wilcox.test(data11$percentCover_optimalWithMech, data11$crop,
                     p.adjust.method = "BH")
pairwise.wilcox.test(data11$percentCover_optimalOverall, data11$crop,
                     p.adjust.method = "BH")
#In wilcox.test.default(xi, xj, paired = paired, ...) :
#cannot compute exact p-value with ties
#what does this mean??

#another way to do KW test
data11 %>% rstatix::kruskal_test(percentCover_optimalOverall ~ crop)
data11 %>% rstatix::kruskal_test(treesPerHec_optimalOverall ~ crop)
data11 %>% rstatix::kruskal_test(percentCover_optimalWithMech ~ crop)

# Effect size
# The eta squared, based on the H-statistic, can be used as the measure of the Kruskal-Wallis test effect size. It is calculated as follow : eta2[H] = (H - k + 1)/(n - k); where H is the value obtained in the Kruskal-Wallis test; k is the number of groups; n is the total number of observations (M. T. Tomczak and Tomczak 2014).
# 
# The eta-squared estimate assumes values from 0 to 1 and multiplied by 100 indicates the percentage of variance in the dependent variable explained by the independent variable.
# 
# The interpretation values commonly in published literature are: 
#0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).

data11 %>% rstatix::kruskal_effsize(treesPerHec_optimalOverall ~ crop)
data11 %>% rstatix::kruskal_effsize(percentCover_optimalWithMech ~ crop)
data11 %>% rstatix::kruskal_effsize(percentCover_optimalOverall ~ crop)

#From above, we know that there is a significant difference between groups in tph
#but we don’t know which pairs of groups are different
#if p<0.05, then follow up with Dunn’s test to identify which groups are different
#could also use the Wilcoxon’s test to calculate pairwise comparisons between group levels 
#with corrections for multiple testing
data11 %>% rstatix::dunn_test(treesPerHec_optimalOverall ~ crop, p.adjust.method = "bonferroni") 


#what about biome
#remove any crop duplicates
bio <- subset(data11, select=-c(crop))
bio <- bio[!duplicated(bio),]

ggplot(bio, aes(x=biome, y=percentCover_optimalWithMech))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean
  theme_minimal()

ggplot(bio, aes(x=biome, y=percentCover_optimalOverall))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2)+   #dot for the mean  theme_minimal()
  theme_minimal()

ggplot(bio, aes(x=biome, y=treesPerHec_optimalOverall))+ 
  stat_boxplot(geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot(outlier.shape=1)+    
  stat_summary(fun.y=mean, geom="point", size=2) +  #dot for the mean  theme_minimal()
  theme_minimal()

#probably because sample size is v smol
biome_tph <- aov(treesPerHec_optimalOverall ~ biome, data = bio)
biome_cov <- aov(percentCover_optimalOverall~ biome, data = bio)
biome_covMech <- aov(percentCover_optimalWithMech ~ biome, data = bio)
summary(biome_tph)

#check for non-normal residuals
par(mfrow=c(2,2))
plot(biome_tph) #suss
plot(biome_cov) #also suss
plot(biome_covMech) #hmmm

#another way to do KW test
bio %>% rstatix::kruskal_test(percentCover_optimalOverall ~ biome)
bio %>% rstatix::kruskal_test(treesPerHec_optimalOverall ~ biome)
bio %>% rstatix::kruskal_test(percentCover_optimalWithMech ~ biome)

# Effect size
bio %>% rstatix::kruskal_effsize(treesPerHec_optimalOverall ~ biome)
bio %>% rstatix::kruskal_effsize(percentCover_optimalWithMech ~ biome)
bio %>% rstatix::kruskal_effsize(percentCover_optimalOverall ~ biome)

#dunn's test 
bio %>% rstatix::dunn_test(treesPerHec_optimalOverall ~ biome, p.adjust.method = "bonferroni") 
bio %>% rstatix::dunn_test(percentCover_optimalWithMech ~ biome, p.adjust.method = "bonferroni") 
bio %>% rstatix::dunn_test(percentCover_optimalOverall ~ biome, p.adjust.method = "bonferroni") 

  