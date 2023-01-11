#========================================================================================
#Summary and intro plots
#
#Krista
#2022-12-07
#========================================================================================
#Notes:
#========================================================================================

library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")
library(tidyverse)

#get data-----

wd <- getwd()
datwparents <- read.csv(file=paste(wd,"/data/analysis_ready_data_w_mean_parent_Fs.csv", sep=""), row.names=1)

datwparents$cohort <- as.factor(datwparents$cohort)
datwparents$YEAR <- as.factor(datwparents$YEAR)


#map plot=======

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 63), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE), size=0.2, data=datwparents) + theme_bw() 

#poster map

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 62.1), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE), size=0.2, data=datwparents) + theme_bw() +
  xlab("Longitude") + ylab("Latitude")

#covars plot====

#hmm let's do this with NONscaled since they are scaled by age!

covarsdat <- datwparents[,c(3,8:18,29,52)]

covar_long <- covarsdat %>% pivot_longer(!c(YEAR, AGE), names_to = "covariate", values_to = "value")

covar_general_sub <- covar_long[which( covar_long$covariate=="apex_pred_biomass"|
                                covar_long$covariate=="forage_fish_biomass"|
                                covar_long$covariate=="pelagic_forager_biomass"|
                                covar_long$covariate=="south.sst.amj" ),]

labs <- c("Apex predator biomass", "Forage fish biomass",
          "Pelagic foragers biomass", "April-June SST")

ggplot(covar_general_sub, aes(as.numeric(as.character(YEAR)), value)) + geom_point() + geom_line() +
  facet_wrap(~covariate, scales="free", strip.position="left",
             labeller = labeller(covariate=c("apex_pred_biomass"="Apex predator biomass", 
                                                                        "forage_fish_biomass"="Forage fish biomass",
                                                                        "pelagic_forager_biomass"="Pelagic foragers biomass", 
                                                                        "south.sst.amj"="April-June SST"))) + 
  xlab("Year") +
  ylab("") + theme_bw() + theme(strip.placement = "outside",
                                                strip.background = element_blank()) +
  geom_vline(xintercept=1987, 
             color = "red", size=0.25)


covar_byage_sub <- covar_long[which(covar_long$covariate=="pollock_survey_abun_mil_at_age"|
  covar_long$covariate=="prevyr_prevage_F"|
                                        covar_long$covariate=="weighted_parent_mean_F" ),]

ggplot(covar_byage_sub, aes(as.numeric(as.character(YEAR)), value, col=as.factor(AGE))) + geom_point() + geom_line() +
  facet_wrap(~covariate, scales="free")


ggplot(covar_byage_sub[which(covar_byage_sub$covariate=="weighted_parent_mean_F"),], aes(as.numeric(as.character(YEAR)), value, col=as.factor(AGE))) + geom_point() + geom_line() +
  facet_wrap(~covariate, scales="free")

ggplot(covar_byage_sub[which(covar_byage_sub$covariate=="weighted_parent_mean_F"),], aes(as.numeric(as.character(YEAR)), value, col=as.factor(AGE))) + geom_point() + geom_line() +
  facet_wrap(~AGE)



ggplot(covar_byage_sub[which(covar_byage_sub$AGE!=0 &
  covar_byage_sub$covariate=="pollock_survey_abun_mil_at_age"),], aes(as.numeric(as.character(YEAR)), value)) + geom_point() + geom_line() +
  facet_wrap(~AGE, ncol=5) + xlab("Year") + ylab("Abundance-at-age (millions)") +
  geom_vline(xintercept=1987, 
             color = "red", size=0.25) + theme_bw()

#look at years-----

ggplot(covar_long, aes(YEAR, covariate, size=value))  + geom_point()



