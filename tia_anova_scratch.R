#ok let's do the dang thing
#TIA ANOVA
#how do experts' responses vary by crop and biome (and climate and veg)?
#experts' responses = percentCover_optimalWithMech, treesPerHec_optimalOverall, percentCover_optimalOverall
#potential spanners in the works:
#an effect of ‘continent’ as a potential explanatory variable
#expert ^^


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

#change data classes as needed
data11$crop <- as.factor(data11$crop)
data11$treesPerHec_optimalOverall <- as.numeric(data11$treesPerHec_optimalOverall)
data11$percentCover_optimalOverall <- as.numeric(data11$percentCover_optimalOverall)
data11$percentCover_optimalWithMech <- as.numeric(data11$percentCover_optimalWithMech)


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

#ANOVAs (or KW?) test just if there is a difference, not what the difference is
#Tukey's HSD tests what the diff is
tuk <- TukeyHSD(crop_tph)
plot(tuk, las = 1) #you want your error bars to not overlap

#what about biome



