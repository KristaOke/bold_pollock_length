#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models WITH lags to most likely parents

#Created by Krista, Sept, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")

#data
wd <- getwd()
lagged2prev <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_prevyr_pollock_length.csv", sep=""), row.names=1)

lagged2prev <- lagged2prev[which(lagged2prev$AGE<16),]
lagged2prev$YEAR <- as.factor(lagged2prev$YEAR)
lagged2prev$AGE <- as.factor(lagged2prev$AGE)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=lagged2prev) + theme_bw() 

#drop stations in northern bering
lagged2prev <- lagged2prev[which(lagged2prev$STRATUM!="81"&
                                   lagged2prev$STRATUM!="71" &
                                   lagged2prev$STRATUM!="70"),]

#----


lagged2prev$cohort <- as.numeric(as.character(lagged2prev$YEAR)) - as.numeric(as.character(lagged2prev$AGE))
lagged2prev$cohort <- as.factor(lagged2prev$cohort)


#running models on data that isn't scaled is looking not good!

lagged2prevsub <- lagged2prev[,c("LATITUDE", "LONGITUDE", "YEAR", "HAUL",
                                 "BOTTOM_TEMPERATURE", "SPECIMENID", "SEX", "LENGTH",                     
                                 "WEIGHT", "AGE", "julian", "south.sst.amj",
                                 "mean_annual_F3plus.x",
                                 "pollock_abun_bil_at_age",
                                 "pollock_survey_abun_mil_at_age",
                                 "apex_pred_biomass",
                                 "forage_fish_biomass",
                                 "pelagic_forager_biomass",
                                 "lag1_F", "lag2_F", "lag3_F",          
                                 "lag4_F", "lag5_F", "lag6_F",              
                                 "lag7_F", "lag8_F", "lag9_F",      
                                 "lag10_F", "prevyr_prevage_F", "cohort")]

#mutate is turning cohort and only cohort into a string of NAs?
lagged2prevsub$cohort <- as.character(lagged2prevsub$cohort)

#z-score variables of interest BY AGE

scaledprevtbl <- lagged2prevsub %>% group_by(AGE) %>%
  mutate(length_scaled=scale(LENGTH),
         weight_scaled=scale(WEIGHT),
         mean_ann_F3plus_scaled=scale(mean_annual_F3plus.x),
         pol_abun_bil_at_age_scaled=scale(pollock_abun_bil_at_age),
         pollock_survey_abun_mil_at_age_scaled=scale(pollock_survey_abun_mil_at_age),
         apex_pred_biom_scaled=scale(apex_pred_biomass),
         forage_fish_biom_scaled=scale(forage_fish_biomass),
         pelagic_forager_biom_scaled=scale(pelagic_forager_biomass),
         lag1_F_scaled=scale(lag1_F),
         lag2_F_scaled=scale(lag2_F),
         lag3_F_scaled=scale(lag3_F),
         lag4_F_scaled=scale(lag4_F),
         lag5_F_scaled=scale(lag5_F),
         lag6_F_scaled=scale(lag6_F),
         lag7_F_scaled=scale(lag7_F),
         lag8_F_scaled=scale(lag8_F),
         lag9_F_scaled=scale(lag9_F),
         lag10_F_scaled=scale(lag10_F),
         prevyr_prevage_F_scaled = scale(prevyr_prevage_F),
         julian_scaled=scale(julian),
         south.sst.amj.scaled=scale(south.sst.amj))

scaled_prev <- as.data.frame(scaledprevtbl)

scaled_prev$cohort <- as.factor(scaled_prev$cohort)

#----

scaled_prev$min_parent_yr <- as.numeric(as.character(scaled_prev$cohort)) - 3

mass_at_age <- read.csv(file=paste(wd,"/data/2021_assessment_table1-17_EBS_pollock_mass_kg_at_age.csv", sep="")) #from survey

mil_at_age <- read.csv(file=paste(wd,"/data/estimated_billions_pollock_at_age_from_assmodel_table1-28_2021assessment.csv", 
                                  sep=""))


