library(tidyverse)
library(ggplot2)
library(patchwork)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)

#let's look at these results!! by country
#ours
comb <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/master_data_byCountry.csv") %>% subset(select=-X)
colnames(comb)[[4]] <- "full_name"

#Millie
chp <- read.csv("C:/Users/vgriffey/OneDrive - Conservation International Foundation/Documents/GitHub/treesincroplands/country_summary.csv")
chp$full_name <- countrycode::countrycode(chp$ISO_A3, origin="iso3c", destination = "country.name", warn=T)

roe <- readxl::read_excel("TIA/Roe_byCountry.xlsx")
#roe <- subset(roe, Mitigation.measure == "Agroforestry", select=c(ISO, Country, RoeTechnical))
roe <- roe[-c(1:7),]
colnames(roe) <- roe[1,]
roe <- roe[-1,]
roe$full_name <- countrycode::countrycode(roe$ISO, origin="iso3c", destination="country.name", warn=T)
roe$agrofor_techcum <- as.numeric(roe$agrofor_techcum)

comb_roe <- inner_join(chp[,c("full_name","potential_crop_mg", "potential_pasture_mg","total_potential_mg")], 
                       roe[,c("agrofor_tech","agrofor_techcum", "full_name")], 
                       by=c("full_name"))
comb_roe <- inner_join(comb_roe, comb[,c("C.Potential.TIC", 
                                         "C.Potential.TIP", "Ag.Area.TIC..has.",    
                                         "Ag.Area.TIP..has.","full_name", "Mean.expert.rec.TIC", "Mean.expert.rec.TIP",
                                         "Mean.delta.TIC","Mean.delta.TIP")],
                       by="full_name")
#colnames(comb_roe)[[1]] <- "country"

#get total potential for TIA
comb_roe$sum_TIA1_2 <- rowSums(comb_roe[, c("C.Potential.TIC", "C.Potential.TIP")])

##make columns of ranks for each of the different carbon totals
# order.scores<-order(comb_roe$sum_CarbonAccByCountry_tic, comb_roe$full_name , decreasing = T)
# comb_roe$tic_rank <- NA
# comb_roe$tic_rank[order.scores] <- 1:nrow(comb_roe)
# 
# order.scores<-order(comb_roe$sum_CarbonAccByCountry_tip, comb_roe$full_name , decreasing = T)
# comb_roe$tip_rank <- NA
# comb_roe$tip_rank[order.scores] <- 1:nrow(comb_roe)

order.scores<-order(comb_roe$sum_TIA1_2, comb_roe$full_name , decreasing = T)
comb_roe$TIA1_2_rank <- NA
comb_roe$TIA1_2_rank[order.scores] <- 1:nrow(comb_roe)

#order.scores<-order(comb_roe$agrofor_techcum, comb_roe$full_name , decreasing = T)
#comb_roe$roe_rank <- NA
#comb_roe$roe_rank[order.scores] <- 1:nrow(comb_roe)

# order.scores<-order(comb_roe$potential_crop_mg, comb_roe$full_name , decreasing = T)
# comb_roe$chapman_crop_rank <- NA
# comb_roe$chapman_crop_rank[order.scores] <- 1:nrow(comb_roe)

# order.scores<-order(comb_roe$potential_pasture, comb_roe$full_name , decreasing = T)
# comb_roe$chapman_pasture_rank <- NA
# comb_roe$chapman_pasture_rank[order.scores] <- 1:nrow(comb_roe)

#order.scores<-order(comb_roe$total_potential, comb_roe$full_name , decreasing = T)
#comb_roe$chapman_total_potential_rank <- NA
#comb_roe$chapman_total_potential_rank[order.scores] <- 1:nrow(comb_roe)


#what units are we in? Mg C. Which just means tons
#1 megaton carbon = 1000000 tons
#divide by 2 (Roe did 50% implementation) and convert to CO2 (*3.67) and divide by 1000000 (to Mt) to compare to Roe
comb_roe$sum_CarbonAccByCountry_tic_CO2_Mt <- comb_roe$C.Potential.TIC/2*3.67/1000000
comb_roe$sum_CarbonAccByCountry_tip_CO2_Mt <- comb_roe$C.Potential.TIP/2*3.67/1000000
comb_roe$sum_TIA1_2_CO2_Mt <- comb_roe$sum_TIA1_2/2*3.67/1000000
comb_roe$total_potential_CO2_Mt <- comb_roe$total_potential_mg/2*3.67/1000000
comb_roe$potential_crop_CO2_Mt <- comb_roe$potential_crop_mg/2*3.67/1000000
comb_roe$potential_pasture_CO2_Mt <- comb_roe$potential_pasture_mg/2*3.67/1000000
##write.csv(comb_roe, "TIA/second_pass_compare.csv")

comb_roe$C_tic_mg_ha <- comb_roe$sum_CarbonAccByCountry_tic/comb_roe$Ag.Area.TIC..has.
comb_roe$C_tip_mg_ha <- comb_roe$sum_CarbonAccByCountry_tip/comb_roe$Ag.Area.TIP..has.
comb_roe$C_tic_mg_ha_chap <- comb_roe$potential_crop_mg/comb_roe$Ag.Area.TIC..has.
comb_roe$C_tip_mg_ha_chap <- comb_roe$potential_pasture_mg/comb_roe$Ag.Area.TIP..has.

#write.csv(comb_roe, "C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/second_pass_compare_Roe_chapman.csv")



##########################################
## PLOTTING ##
##########################################

#plot difference between us and Chapman AND CO2 differences 
test <- subset(comb_roe, TIA1_2_rank <50,  select=c(TIA1_2_rank, full_name, sum_TIA1_2_CO2_Mt, total_potential_CO2_Mt,
                                                    sum_CarbonAccByCountry_tic_CO2_Mt, sum_CarbonAccByCountry_tip_CO2_Mt, 
                                                    potential_crop_CO2_Mt, potential_pasture_CO2_Mt))
ord <- test$full_name[order(test$TIA1_2_rank, test$full_name)]
test$net_TIA_Chap <- test$total_potential_CO2_Mt - test$sum_TIA1_2_CO2_Mt
test$net_TIA_Chap_crops <- test$potential_crop_CO2_Mt - test$sum_CarbonAccByCountry_tic_CO2_Mt
test$net_TIA_Chap_pasture <- test$potential_pasture_CO2_Mt - test$sum_CarbonAccByCountry_tip_CO2_Mt


long <- pivot_longer(test, cols=c(sum_CarbonAccByCountry_tic_CO2_Mt, sum_CarbonAccByCountry_tip_CO2_Mt, 
                                  potential_crop_CO2_Mt, potential_pasture_CO2_Mt, 
                                  net_TIA_Chap_crops, net_TIA_Chap_pasture))
long$full_name  <- factor(long$full_name , levels=ord)
long$label <- ifelse(long$name=="sum_CarbonAccByCountry_tic_CO2_Mt"| long$name=="sum_CarbonAccByCountry_tip_CO2_Mt", "TIA", 
                     ifelse(long$name=="potential_crop_CO2_Mt" | long$name=="potential_pasture_CO2_Mt","Chapman", NA))
long$CP <- ifelse(long$name=="sum_CarbonAccByCountry_tic_CO2_Mt" | long$name=="potential_crop_CO2_Mt", "Crop", 
                  ifelse(long$name=="sum_CarbonAccByCountry_tip_CO2_Mt" | long$name=="potential_pasture_CO2_Mt", "Graze", NA))

# r <- ggplot(subset(long, name=="TIA1_2_rank" | name=="chapman_total_potential_rank"), aes(x=full_name , y=value, fill=label))+
#   geom_col(position="dodge")+
#   theme_minimal()+
#   theme(axis.text = element_text(angle=90, hjust=0.95,vjust=0.2),
#         legend.title = element_blank())+
#   labs(x="Countries in TIA Rank Order", y="Rank (Highest NCS Potential)", title="Rank Order")

co2 <- ggplot(subset(long, name=="sum_CarbonAccByCountry_tic_CO2_Mt" | name=="potential_crop_CO2_Mt" | name=="sum_CarbonAccByCountry_tip_CO2_Mt" | name=="potential_pasture_CO2_Mt"),
              aes(x=full_name, y=value, fill=label))+
  geom_col(position="dodge", width=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12))+
#        legend.position = "none")+
  labs( y="Mt CO2 in 30 yrs",x="", title="CO2 Potential (50% adoption)")+
  facet_wrap(~CP, ncol=1)
ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/TIA_Chapman_compare_CO2.png",
       co2, height=6, width=11, bg="white")


net_crop <- ggplot(subset(long, name=="net_TIA_Chap_crops"),
       aes(x=full_name, y=value))+
  geom_col(position="dodge")+
  theme_minimal()+
  theme(axis.text.x = element_blank())+#element_text(angle=90, hjust=0.95,vjust=0.2))+
  labs( y="Net CO2 potential\n ( Mt / 30 years )", x="", title="Crop")

net_graze <-  ggplot(subset(long, name=="net_TIA_Chap_pasture"),
                     aes(x=full_name, y=value))+
  geom_col(position="dodge")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2))+
  labs( y="Net CO2 potential\n ( Mt / 30 years )", x="", title="Graze")

net <- net_crop/net_graze + plot_annotation(title="Chapman minus TIA CO2 Potential (50% adoption)")

ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/TIA_Chapman_compare_net.png",
       net, height=8, width=12, bg="white")


## plot countries potential/ha
library(scales)
tb <- subset(comb_roe,  select=c(full_name, C_tic_mg_ha, C_tip_mg_ha))

ord_crop <- tb$full_name[order(tb$C_tic_mg_ha, tb$full_name, decreasing = T)]
crop_long <- pivot_longer(tb, cols=c(C_tic_mg_ha ))
crop_long$full_name  <- factor(crop_long$full_name , levels=ord_crop)

ord_graze <- tb$full_name[order(tb$C_tip_mg_ha, tb$full_name, decreasing = T)]
graze_long <- pivot_longer(tb, cols=c(C_tip_mg_ha))
graze_long$full_name  <- factor(graze_long$full_name , levels=ord_graze)

crop <- ggplot(subset(crop_long, full_name %in% ord_crop[1:50]), aes(x=full_name, y=value))+
  geom_col(position="dodge", width=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12))+
  #        legend.position = "none")+
  labs( y="Mg C/ha",x="", title="Crop")
  #scale_y_continuous(labels = comma)

graze <- ggplot(subset(graze_long, full_name %in% ord_graze[1:50]), aes(x=full_name, y=value))+
  geom_col(position="dodge", width=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12),
          legend.position = "none")+
  labs( y="Mg C/ha",x="", title="Graze")
  #scale_y_continuous(labels = comma)

crop/graze + plot_annotation(title = "Carbon Potential per Hectare")




## plot countries with top delta
tb <- subset(comb_roe, select=c(full_name, Mean.delta.TIC, Mean.delta.TIP))

long <- pivot_longer(tb, cols=c(Mean.delta.TIC, Mean.delta.TIP))
#long$full_name  <- factor(long$full_name , levels=ord)
long$CP <- ifelse(long$name=="Mean.delta.TIC", "Crop", "Graze")

ord_crop <- tb$full_name[order(tb$Mean.delta.TIC, tb$full_name, decreasing = T)]
crop_long <- pivot_longer(tb, cols=c(Mean.delta.TIC, Mean.delta.TIP))
crop_long$full_name  <- factor(crop_long$full_name , levels=ord_crop)
crop_long$CP <- ifelse(crop_long$name=="Mean.delta.TIC", "Crop", "Graze")
crop_long <- subset(crop_long, CP=="Crop")

ord_graze <- tb$full_name[order(tb$Mean.delta.TIP, tb$full_name, decreasing = T)]
graze_long <- pivot_longer(tb, cols=c(Mean.delta.TIC, Mean.delta.TIP))
graze_long$full_name  <- factor(graze_long$full_name , levels=ord_graze)
graze_long$CP <- ifelse(graze_long$name=="Mean.delta.TIC", "Crop", "Graze")
graze_long <- subset(graze_long, CP=="Graze")

crop <- ggplot(subset(crop_long, full_name %in% ord_crop[1:50]), aes(x=full_name, y=value))+
  geom_col(position="dodge", width=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12))+
  #        legend.position = "none")+
  labs( y="Tree Cover (%)",x="", title="Crop")

graze <- ggplot(subset(graze_long, full_name %in% ord_graze[1:50]), aes(x=full_name, y=value))+
  geom_col(position="dodge", width=0.75)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, hjust=0.95,vjust=0.2),
        legend.title = element_blank(),
        strip.text.x = element_text(size=12))+
  #        legend.position = "none")+
  labs( y="Tree Cover (%)",x="", title="Graze")

crop/graze + plot_annotation(title = "Expert Recs minus Baseline Tree Cover")


ggsave("C:/Users/vgriffey/OneDrive - Conservation International Foundation/VivianAnalyses/TIA_Chapman_compare_CO2.png",
       co2, height=6, width=11, bg="white")




##if you want to map anything out
library(giscoR)
tomap <- subset(comb_roe, select=c(full_name, Mean.delta.TIC, Mean.delta.TIP))

world <- ne_countries(scale = "medium", returnclass = "sf")
world$full_name <- countrycode::countrycode(world$iso_a3, origin="iso3c",destination="country.name", warn=T)
fornow <- inner_join(world, tomap, by="full_name")


# Clean NAs from initial dataset
fornow <- fornow %>%
  filter(!is.na(Mean.delta.TIC))

fornow$ <- ifelse()

# Create breaks and discretize values
br <- c(0, 10, 20, 30, 40, 50, 60)
#BAMMtools::getJenksBreaks

fornow$breaks <- cut(fornow$Mean.delta.TIP,
                                         breaks = br,
                                         dig.lab = 5)

# Create custom labels - e.g. (0k-10k]
#labs_plot <- paste0("(", br[1:6], "k-", br[2:7], "k]")
labs_plot <- c("0-10%","10-20%","20-30","30-40%","40-50%","50-60%", "60-70%")

# Palette
pal <- hcl.colors(7, "Inferno", rev = TRUE, alpha = 0.7)

# Add overlay
ggplot() +
  # Add choropleth overlay
  geom_sf(data = fornow,
          aes(fill = breaks), color = "grey60", lwd=0.25) +
  scale_fill_manual(values = pal,
                    drop = FALSE,
                    na.value = "grey80",
                    label = labs_plot,
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom")) +
  # Theme
  theme_void() +
  theme(plot.caption = element_text(size = 7, face = "italic"),
        legend.position = "bottom")



## ratio of chapman to TIA
tb <- subset(comb_roe, select=c(total_potential_mg, sum_TIA1_2,C.Potential.TIC, C.Potential.TIP,                  
                                Ag.Area.TIC..has., Ag.Area.TIP..has., potential_crop_CO2_Mt, 
                                potential_pasture_CO2_Mt, C_tic_mg_ha, C_tip_mg_ha, 
                                C_tic_mg_ha_chap, C_tip_mg_ha_chap))
tb$rat <- tb$total_potential_mg/tb$sum_TIA1_2
tb$ag_tot <- rowSums(tb[,c("Ag.Area.TIC..has.", "Ag.Area.TIP..has.")])


