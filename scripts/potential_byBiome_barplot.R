# bar chart w/total potential  per biome showing the  crop + grazing division line within the bar
#+ uncertainty?

library(tidyverse)
#df <- read.csv("../XR_Synthesis/ci95_byBiome_totalPotential.csv")
#names(df)[5:6] <- c("Croplands", "Grazing lands")
df <- read.csv("GitHub/Trees_in_Agriculture/data/06_21_2023/biome_results_06212023.csv")
df_long <- reshape2::melt(df, id.vars=c("BIOME_NUM","BIOME_NAME"), measure.vars=c("Crop_TotalC_sum_MgC_30yr", "Graze_TotalC_sum_MgC_30yr"))
names(df_long) <- c("BIOME_NUM","BIOME_NAME","variable","value")

df_long <- df_long %>% mutate(biome_abbrev = case_when(
  BIOME_NAME == "Mediterranean Forests, Woodlands & Scrub" ~"MF" ,
  BIOME_NAME == "Temperate Broadleaf & Mixed Forests" ~"TeBMF" ,
  BIOME_NAME ==  "Temperate Grasslands, Savannas & Shrublands" ~ "TeGSS",
  BIOME_NAME == "Tropical & Subtropical Dry Broadleaf Forests" ~ "TrSDBF",
  BIOME_NAME == "Tropical & Subtropical Grasslands, Savannas & Shrublands" ~ "TrSGSS",
  BIOME_NAME ==  "Tropical & Subtropical Moist Broadleaf Forests" ~ "TrSMBF",
  BIOME_NAME ==  "Deserts & Xeric Shrublands" ~ "DXS",
  BIOME_NAME ==  "Montane Grasslands & Shrublands" ~ "MGSS",
  BIOME_NAME ==  "Temperate Conifer Forests" ~ "TeCF",
  BIOME_NAME ==  "Tropical & Subtropical Coniferous Forest" ~ "TrSCF",
  BIOME_NAME ==  "Tropical & Subtropical Grasslands, Savannas, and Shrublands" ~ "TrSGSS"
))
df_long$biome_abbrev <- factor(df_long$biome_abbrev, levels=c("DXS","MF","MGSS","TeGSS","TeCF","TeBMF","TrSGSS","TrSDBF","TrSMBF"))

df_long <- df_long %>% mutate(climate=case_when(biome_abbrev=="DXS" ~ "Desert",
                                                biome_abbrev=="MF" ~ "Mediterranean",
                                                biome_abbrev=="MGSS" ~ "Montane",
                                                biome_abbrev=="TeBMF"~"Temperate",
                                                biome_abbrev=="TeGSS"~"Temperate",
                                                biome_abbrev=="TeCF" ~ "Temperate",
                                                biome_abbrev=="TrSDBF" ~ "Tropical/Subtropical",
                                                biome_abbrev=="TrSGSS" ~ "Tropical/Subtropical",
                                                biome_abbrev=="TrSMBF" ~ "Tropical/Subtropical"))

df_long <- df_long %>% mutate(forested=case_when(biome_abbrev=="MGSS" ~ "Forest",
                                                biome_abbrev=="TeBMF"~"Forest",
                                                biome_abbrev=="TeCF" ~ "Forest",
                                                biome_abbrev=="TrSDBF" ~ "Forest",
                                                biome_abbrev=="TrSMBF" ~ "Forest"))

df_long <- subset(df_long, biome_abbrev!="TeCF")
df_long$value <- df_long$value/1000000000*(44/12)/30


theme = theme_set(theme_minimal())
ggplot(df_long, aes(x=biome_abbrev, y=value, fill=variable))+
  # geom_bar_pattern(aes(x=biome_abbrev, y=total_potential/1000000000, pattern=forested),
  #                  stat='identity',
  #                  position = "stack",
  #                  color = "black", 
  #                  pattern_fill = "black",
  #                  pattern_angle = 45,
  #                  pattern_density = 0.1,
  #                  pattern_spacing = 0.025,
  #                  pattern_key_scale_factor = 0.6)+
  geom_bar(stat="identity", color="black",
           position="stack") +
   ggforce::facet_row(~climate, strip.position = "bottom", scales = "free_x",  space="free") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.title=element_blank(),
        panel.grid.major.x=element_blank(),
        axis.text.y = element_text(colour="grey20"),
        axis.ticks.y= element_line(colour="grey20"),,
        axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        strip.placement = "outside",
        strip.background=element_rect(color="grey20"))+
  scale_y_continuous(expand = c(0, 0))+
  labs(x="Biome", y=parse(text=paste0("Pg", " ~CO[2]", " ~Yr^-1")))+
  scale_fill_manual(values=c("#CEAB07", "#798E87"), labels=c("Crop","Graze"))
  

ggsave("../VivianAnalyses/Draft Figures/totalPotential_biome_barplot.png",width=8.5, height=7, dpi=600, bg="white")
