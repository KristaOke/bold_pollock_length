#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models WITH lags to PREVIOUS year and age

#Created by Krista, Jan 20, 2022
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


#GAMs-----

library(mgcv)
library(gamm4)
library(car)

#age 1 doesn't work because no previous age!
#NEED TO UPDATE THE FISHING METRIC TO PREV YR

#age 2------
lag2pre <- scaled_prev[which(scaled_prev$AGE==2),]

lag2.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
  pollock_survey_abun_mil_at_age_scaled +
  apex_pred_biom_scaled + 
  forage_fish_biom_scaled + 
  pelagic_forager_biom_scaled +
  s(cohort, bs="re"),
random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag2pre )
gam.check(lag2.p$gam) #not bad
summary(lag2.p$gam) #
summary(lag2.p$mer) #
AIC_2lagp <- AIC(lag2.p$mer) #
plot(lag2.p$gam)

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 3------
lag3pre <-  scaled_prev[which(scaled_prev$AGE==3),]

lag3.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag3pre )
gam.check(lag3.p$gam) #not bad
summary(lag3.p$gam) #
summary(lag3.p$mer) #
AIC_3lagp <- AIC(lag3.p$mer) #
plot(lag3.p$gam)

#Oh not a ton of variety in F
ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 4------
lag4pre <-  scaled_prev[which(scaled_prev$AGE==4),]

lag4.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag4pre )
gam.check(lag4.p$gam) #
summary(lag4.p$gam) #
summary(lag4.p$mer) #
AIC_4lagp <- AIC(lag4.p$mer) #
plot(lag4.p$gam)

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 5------
lag5pre <-  scaled_prev[which(scaled_prev$AGE==5),]

lag5.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag5pre )
gam.check(lag5.p$gam) #
summary(lag5.p$gam) #
summary(lag5.p$mer) #
AIC_5lagp <- AIC(lag5.p$mer) #
plot(lag5.p$gam)

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 6------
lag6pre <-  scaled_prev[which(scaled_prev$AGE==6),]

lag6.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag6pre )
gam.check(lag6.p$gam) # 
summary(lag6.p$gam) #
summary(lag6.p$mer) #
AIC_6lagp <- AIC(lag6.p$mer) #
plot(lag6.p$gam)


ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 7------
lag7pre <-  scaled_prev[which(scaled_prev$AGE==7),]

lag7.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag7pre )
gam.check(lag7.p$gam) #
summary(lag7.p$gam) #
summary(lag7.p$mer) #
AIC_7lagp <- AIC(lag7.p$mer) #
plot(lag7.p$gam)

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 8------
lag8pre <-  scaled_prev[which(scaled_prev$AGE==8),]

lag8.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag8pre )
gam.check(lag8.p$gam) #
summary(lag8.p$gam) #
summary(lag8.p$mer) #
AIC_8lagp <- AIC(lag8.p$mer) #
plot(lag8.p$gam)

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


#age 9------
lag9pre <-  scaled_prev[which(scaled_prev$AGE==9),]

lag9.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                  prevyr_prevage_F_scaled + 
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE, data=lag9pre )
gam.check(lag9.p$gam) #
summary(lag9.p$gam) #
summary(lag9.p$mer) #
AIC_9lagp <- AIC(lag9.p$mer) #
plot(lag9.p$gam)

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 10------
lag10pre <-  scaled_prev[which(scaled_prev$AGE==10),]

lag10.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag10pre )
gam.check(lag10.p$gam) #
summary(lag10.p$gam) #
summary(lag10.p$mer) #
AIC_10lagp <- AIC(lag10.p$mer) #
plot(lag10.p$gam)


ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 11------
lag11pre <-  scaled_prev[which(scaled_prev$AGE==11),] #all the prev yr prev age F are NA for age 11 but not age 12??

lag11.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag11pre )
gam.check(lag11.p$gam) #
summary(lag11.p$gam) #
summary(lag11.p$mer) #
AIC_11lagp <- AIC(lag11.p$mer) #
plot(lag11.p$gam)


ggplot(lag11pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag11pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag11pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 12------
lag12pre <-  scaled_prev[which(scaled_prev$AGE==12),]

lag12.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag12pre )
gam.check(lag12.p$gam) #
summary(lag12.p$gam) #
summary(lag12.p$mer) #
AIC_12lagp <- AIC(lag12.p$mer) #
plot(lag12.p$gam)

ggplot(lag12pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag12pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag12pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 13------
lag13pre <-  scaled_prev[which(scaled_prev$AGE==13),]

lag13.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag13pre )
gam.check(lag13.p$gam) #
summary(lag13.p$gam) #
summary(lag13.p$mer) #
AIC_13lagp <- AIC(lag13.p$mer) #
plot(lag13.p$gam)

ggplot(lag13pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag13pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag13pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 14------
lag14pre <-  scaled_prev[which(scaled_prev$AGE==14),]

lag14.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag14pre )
gam.check(lag14.p$gam) #
summary(lag14.p$gam) #
summary(lag14.p$mer) #
AIC_14lagp <- AIC(lag14.p$mer) #
plot(lag14.p$gam)

ggplot(lag14pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag14pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag14pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 15------
lag15pre <-  scaled_prev[which(scaled_prev$AGE==15),]

lag15.p <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                   prevyr_prevage_F_scaled + 
                   pollock_survey_abun_mil_at_age_scaled +
                   apex_pred_biom_scaled + 
                   forage_fish_biom_scaled + 
                   pelagic_forager_biom_scaled +
                   s(cohort, bs="re"),
                 random=~(1|YEAR/HAUL),  REML=FALSE, data=lag15pre )
gam.check(lag15.p$gam) #
summary(lag15.p$gam) #
summary(lag15.p$mer) #
AIC_15lagp <- AIC(lag15.p$mer) #
plot(lag15.p$gam)

ggplot(lag15pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag15pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag15pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")

ggplot(lag15pre, aes(as.numeric(as.character(YEAR)), LENGTH)) + geom_point() + geom_smooth(method="lm")

ggplot(lag15pre, aes(as.numeric(as.character(YEAR)), prevyr_prevage_F)) + geom_point() + geom_smooth(method="lm")

ggplot(scaled_prev, aes(as.numeric(as.character(YEAR)), prevyr_prevage_F)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(~AGE)

ggplot(scaled_prev, aes(as.numeric(as.character(YEAR)), LENGTH)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(~AGE) #plot mean length?
