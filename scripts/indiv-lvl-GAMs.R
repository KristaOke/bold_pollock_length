#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models copycatting previous work

#Created by Krista, Jan 5, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: The models in this script try to follow as directly from the EBS SST-weight-at-age paper as possible
#in terms of model design/structure
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")

#get data saved in load_explore_covar_data
indiv_lvl_dat <- read.csv(file=paste(wd,"/data/analysis_ready_individual_data_pollock_length.csv", sep=""), row.names=1)

indiv_lvl_dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE<16),]
indiv_lvl_dat$YEAR <- as.factor(indiv_lvl_dat$YEAR)


summary(indiv_lvl_dat)


world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=indiv_lvl_dat) + theme_bw() 

#drop stations in northern bering
indiv_lvl_dat <- indiv_lvl_dat[which(indiv_lvl_dat$STRATUM!="81"&
                                       indiv_lvl_dat$STRATUM!="71" &
                                       indiv_lvl_dat$STRATUM!="70"),]

#look at covars
ggplot(indiv_lvl_dat, aes(YEAR, pelagic_forager_biomass)) +
  geom_point() + geom_smooth()

ggplot(indiv_lvl_dat, aes(pelagic_forager_biomass, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")



ggplot(indiv_lvl_dat, aes(YEAR, benthic_forager_biomass)) +
  geom_point()+ geom_smooth()

ggplot(indiv_lvl_dat, aes(benthic_forager_biomass, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(indiv_lvl_dat, aes(YEAR, forage_fish_biomass)) +
  geom_point()+ geom_smooth()

ggplot(indiv_lvl_dat, aes(forage_fish_biomass, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(indiv_lvl_dat, aes(YEAR, epifauna_biomass)) +
  geom_point()+ geom_smooth()

ggplot(indiv_lvl_dat, aes(epifauna_biomass, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")



ggplot(indiv_lvl_dat, aes(YEAR, apex_pred_biomass)) +
  geom_point()+ geom_smooth()

ggplot(indiv_lvl_dat, aes(apex_pred_biomass, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(indiv_lvl_dat, aes(YEAR, pollock_abun_bil_at_age)) +
  geom_point() + facet_wrap(~AGE, scales="free")+ geom_smooth()

ggplot(indiv_lvl_dat, aes(pollock_abun_bil_at_age, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(indiv_lvl_dat, aes(YEAR, south.sst.amj)) +
  geom_point()+ geom_smooth()

ggplot(indiv_lvl_dat, aes(south.sst.amj, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(indiv_lvl_dat, aes(YEAR, mean_annual_F3plus)) +
  geom_point()+ geom_smooth() +facet_wrap(~AGE)

ggplot(indiv_lvl_dat, aes(mean_annual_F3plus, LENGTH, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(indiv_lvl_dat, aes(south.sst.amj, mean_annual_F3plus, col=as.factor(YEAR))) + 
  geom_point() + facet_wrap(~AGE, scales = "free")

#GAMs-----

library(mgcv)
library(gamm4)
library(car)

wd <- getwd()
lagged_dat <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_data_pollock_length.csv", sep=""), row.names=1)

lagged_dat <- lagged_dat[which(lagged_dat$AGE<16),]
lagged_dat$YEAR <- as.factor(lagged_dat$YEAR)
lagged_dat$AGE <- as.factor(lagged_dat$AGE)

#drop stations in northern bering
lagged_dat <- lagged_dat[which(lagged_dat$STRATUM!="81"&
                                 lagged_dat$STRATUM!="71" &
                                 lagged_dat$STRATUM!="70"),]

lagged_dat$cohort <- as.numeric(as.character(lagged_dat$YEAR)) - as.numeric(as.character(lagged_dat$AGE))
lagged_dat$cohort <- as.factor(lagged_dat$cohort)


#running models on data that isn't scaled is looking not good!

lagged_sub <- lagged_dat[,c("LATITUDE", "LONGITUDE", "YEAR", "HAUL",
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
                            "lag10_F", "cohort")]

#mutate is turning cohort and only cohort into a string of NAs?
lagged_sub$cohort <- as.character(lagged_sub$cohort)

#z-score variables of interest BY AGE

scaledtbl <- lagged_sub %>% group_by(AGE) %>%
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
         julian_scaled=scale(julian),
         south.sst.amj.scaled=scale(south.sst.amj))

scaled_dat <- as.data.frame(scaledtbl)

scaled_dat$cohort <- as.factor(scaled_dat$cohort)


#age 1------
scaled1dat <- scaled_dat[which(scaled_dat$AGE==1),]

#none look particularly nonlinear, let's try linear interactions

age1nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    mean_ann_F3plus_scaled + 
                    pollock_survey_abun_mil_at_age_scaled +
                    apex_pred_biom_scaled + 
                    forage_fish_biom_scaled + 
                    pelagic_forager_biom_scaled +
                    s(cohort, bs="re"),
                  random=~(1|YEAR/HAUL), data=scaled1dat )
gam.check(age1nolag$gam) #
summary(age1nolag$gam) #
summary(age1nolag$mer) #
AIC_age1nolag <- AIC(age1nolag$mer) #
plot(age1nolag$gam)
anova(age1nolag$gam)
saveRDS(age1nolag, file=paste(wd,"/scripts/model_output_age1_lin_surv-abun.rds", sep=""))
age1nolag <- readRDS(file=paste(wd,"/scripts/model_output_age1_lin_surv-abun.rds", sep=""))

#nonlinear
age1nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     s(mean_ann_F3plus_scaled, k=4) + 
                     s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                     s(apex_pred_biom_scaled, k=4) + 
                     s(forage_fish_biom_scaled, k=4) + 
                     s(pelagic_forager_biom_scaled, k=4) +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled1dat )
gam.check(age1nolag_sm$gam) #
summary(age1nolag_sm$gam) #
summary(age1nolag_sm$mer) #
AIC_age1nolag_sm <- AIC(age1nolag_sm$mer) #
plot(age1nolag_sm$gam)
anova(age1nolag_sm$gam)
#saveRDS(age1nolag_sm, file=paste(wd,"/scripts/model_output_age1_lin_surv-abun_nonlin.rds", sep=""))
age1nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age1_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled1dat)
gam.check(base1$gam) #hmm qq
summary(base1$gam) #
summary(base1$mer) #
AIC(base1$mer)

#saveRDS(base1, file=paste(wd,"/scripts/model_output_age1_base.rds", sep=""))
base1 <- readRDS(file=paste(wd,"/scripts/model_output_age1_base.rds", sep=""))



#age 2------
scaled2dat <- scaled_dat[which(scaled_dat$AGE==2),]

#none look particularly nonlinear, let's try linear interactions

age2nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled2dat )
gam.check(age2nolag$gam) #
summary(age2nolag$gam) #
summary(age2nolag$mer) #
AIC_age2nolag <- AIC(age2nolag$mer) #
plot(age2nolag$gam)
anova(age2nolag$gam)
#saveRDS(age2nolag, file=paste(wd,"/scripts/model_output_age2_lin_surv-abun.rds", sep=""))
age2nolag <- readRDS(file=paste(wd,"/scripts/model_output_age2_lin_surv-abun.rds", sep=""))

#nonlinear
age2nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled2dat )
gam.check(age2nolag_sm$gam) #
summary(age2nolag_sm$gam) #
summary(age2nolag_sm$mer) #
AIC_age2nolag_sm <- AIC(age2nolag_sm$mer) #
plot(age2nolag_sm$gam)
anova(age2nolag_sm$gam)
#saveRDS(age2nolag_sm, file=paste(wd,"/scripts/model_output_age2_lin_surv-abun_nonlin.rds", sep=""))
age2nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age2_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base2 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled2dat)
gam.check(base2$gam) #hmm qq
summary(base2$gam) #
summary(base2$mer) #
AIC(base2$mer)

#saveRDS(base2, file=paste(wd,"/scripts/model_output_age2_base.rds", sep=""))
base2 <- readRDS(file=paste(wd,"/scripts/model_output_age2_base.rds", sep=""))



#age 3------
scaled3dat <- scaled_dat[which(scaled_dat$AGE==3),]

#none look particularly nonlinear, let's try linear interactions

age3nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled3dat )
gam.check(age3nolag$gam) #
summary(age3nolag$gam) #
summary(age3nolag$mer) #
AIC_age3nolag <- AIC(age3nolag$mer) #
plot(age3nolag$gam)
anova(age3nolag$gam)
#saveRDS(age3nolag, file=paste(wd,"/scripts/model_output_age3_lin_surv-abun.rds", sep=""))
age3nolag <- readRDS(file=paste(wd,"/scripts/model_output_age3_lin_surv-abun.rds", sep=""))

#nonlinear
age3nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled3dat )
gam.check(age3nolag_sm$gam) #
summary(age3nolag_sm$gam) #
summary(age3nolag_sm$mer) #
AIC_age3nolag_sm <- AIC(age3nolag_sm$mer) #
plot(age3nolag_sm$gam)
anova(age3nolag_sm$gam)
#saveRDS(age3nolag_sm, file=paste(wd,"/scripts/model_output_age3_lin_surv-abun_nonlin.rds", sep=""))
age3nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age3_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base3 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled3dat)
gam.check(base3$gam) #hmm qq
summary(base3$gam) #
summary(base3$mer) #
AIC(base3$mer)

#saveRDS(base3, file=paste(wd,"/scripts/model_output_age3_base.rds", sep=""))
base3 <- readRDS(file=paste(wd,"/scripts/model_output_age3_base.rds", sep=""))



#age 4------
scaled4dat <- scaled_dat[which(scaled_dat$AGE==4),]

#none look particularly nonlinear, let's try linear interactions

age4nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled4dat )
gam.check(age4nolag$gam) #
summary(age4nolag$gam) #
summary(age4nolag$mer) #
AIC_age4nolag <- AIC(age4nolag$mer) #
plot(age4nolag$gam)
anova(age4nolag$gam)
#saveRDS(age4nolag, file=paste(wd,"/scripts/model_output_age4_lin_surv-abun.rds", sep=""))
age4nolag <- readRDS(file=paste(wd,"/scripts/model_output_age4_lin_surv-abun.rds", sep=""))

#nonlinear
age4nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled4dat )
gam.check(age4nolag_sm$gam) #
summary(age4nolag_sm$gam) #
summary(age4nolag_sm$mer) #
AIC_age4nolag_sm <- AIC(age4nolag_sm$mer) #
plot(age4nolag_sm$gam)
anova(age4nolag_sm$gam)
#saveRDS(age4nolag_sm, file=paste(wd,"/scripts/model_output_age4_lin_surv-abun_nonlin.rds", sep=""))
age4nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age4_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled4dat)
gam.check(base4$gam) #hmm qq
summary(base4$gam) #
summary(base4$mer) #
AIC(base4$mer)

#saveRDS(base4, file=paste(wd,"/scripts/model_output_age4_base.rds", sep=""))
base4 <- readRDS(file=paste(wd,"/scripts/model_output_age4_base.rds", sep=""))




#age 5------
scaled5dat <- scaled_dat[which(scaled_dat$AGE==5),]

#none look particularly nonlinear, let's try linear interactions

age5nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled5dat )
gam.check(age5nolag$gam) #
summary(age5nolag$gam) #
summary(age5nolag$mer) #
AIC_age5nolag <- AIC(age5nolag$mer) #
plot(age5nolag$gam)
anova(age5nolag$gam)
#saveRDS(age5nolag, file=paste(wd,"/scripts/model_output_age5_lin_surv-abun.rds", sep=""))
age5nolag <- readRDS(file=paste(wd,"/scripts/model_output_age5_lin_surv-abun.rds", sep=""))

#nonlinear
age5nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled5dat )
gam.check(age5nolag_sm$gam) #
summary(age5nolag_sm$gam) #
summary(age5nolag_sm$mer) #
AIC_age5nolag_sm <- AIC(age5nolag_sm$mer) #
plot(age5nolag_sm$gam)
anova(age5nolag_sm$gam)
#saveRDS(age5nolag_sm, file=paste(wd,"/scripts/model_output_age5_lin_surv-abun_nonlin.rds", sep=""))
age5nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age5_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base5 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled5dat)
gam.check(base5$gam) #hmm qq
summary(base5$gam) #
summary(base5$mer) #
AIC(base5$mer)

#saveRDS(base5, file=paste(wd,"/scripts/model_output_age5_base.rds", sep=""))
base5 <- readRDS(file=paste(wd,"/scripts/model_output_age5_base.rds", sep=""))




#age 6------
scaled6dat <- scaled_dat[which(scaled_dat$AGE==6),]

#none look particularly nonlinear, let's try linear interactions

age6nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled6dat )
gam.check(age6nolag$gam) #
summary(age6nolag$gam) #
summary(age6nolag$mer) #
AIC_age6nolag <- AIC(age6nolag$mer) #
plot(age6nolag$gam)
anova(age6nolag$gam)
#saveRDS(age6nolag, file=paste(wd,"/scripts/model_output_age6_lin_surv-abun.rds", sep=""))
age6nolag <- readRDS(file=paste(wd,"/scripts/model_output_age6_lin_surv-abun.rds", sep=""))

#nonlinear
age6nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled6dat )
gam.check(age6nolag_sm$gam) #
summary(age6nolag_sm$gam) #
summary(age6nolag_sm$mer) #
AIC_age6nolag_sm <- AIC(age6nolag_sm$mer) #
plot(age6nolag_sm$gam)
anova(age6nolag_sm$gam)
#saveRDS(age6nolag_sm, file=paste(wd,"/scripts/model_output_age6_lin_surv-abun_nonlin.rds", sep=""))
age6nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age6_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled6dat)
gam.check(base6$gam) #hmm qq
summary(base6$gam) #
summary(base6$mer) #
AIC(base6$mer)

#saveRDS(base6, file=paste(wd,"/scripts/model_output_age6_base.rds", sep=""))
base6 <- readRDS(file=paste(wd,"/scripts/model_output_age6_base.rds", sep=""))






#age 7------
scaled7dat <- scaled_dat[which(scaled_dat$AGE==7),]

#none look particularly nonlinear, let's try linear interactions

age7nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled7dat )
gam.check(age7nolag$gam) #
summary(age7nolag$gam) #
summary(age7nolag$mer) #
AIC_age7nolag <- AIC(age7nolag$mer) #
plot(age7nolag$gam)
anova(age7nolag$gam)
#saveRDS(age7nolag, file=paste(wd,"/scripts/model_output_age7_lin_surv-abun.rds", sep=""))
age7nolag <- readRDS(file=paste(wd,"/scripts/model_output_age7_lin_surv-abun.rds", sep=""))

#nonlinear
age7nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled7dat )
gam.check(age7nolag_sm$gam) #
summary(age7nolag_sm$gam) #
summary(age7nolag_sm$mer) #
AIC_age7nolag_sm <- AIC(age7nolag_sm$mer) #
plot(age7nolag_sm$gam)
anova(age7nolag_sm$gam)
#saveRDS(age7nolag_sm, file=paste(wd,"/scripts/model_output_age7_lin_surv-abun_nonlin.rds", sep=""))
age7nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age7_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base7 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled7dat)
gam.check(base7$gam) #hmm qq
summary(base7$gam) #
summary(base7$mer) #
AIC(base7$mer)

#saveRDS(base7, file=paste(wd,"/scripts/model_output_age7_base.rds", sep=""))
base7 <- readRDS(file=paste(wd,"/scripts/model_output_age7_base.rds", sep=""))






#age 8------
scaled8dat <- scaled_dat[which(scaled_dat$AGE==8),]

#none look particularly nonlinear, let's try linear interactions

age8nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled8dat )
gam.check(age8nolag$gam) #
summary(age8nolag$gam) #
summary(age8nolag$mer) #
AIC_age8nolag <- AIC(age8nolag$mer) #
plot(age8nolag$gam)
anova(age8nolag$gam)
#saveRDS(age8nolag, file=paste(wd,"/scripts/model_output_age8_lin_surv-abun.rds", sep=""))
age8nolag <- readRDS(file=paste(wd,"/scripts/model_output_age8_lin_surv-abun.rds", sep=""))

#nonlinear
age8nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled8dat )
gam.check(age8nolag_sm$gam) #
summary(age8nolag_sm$gam) #
summary(age8nolag_sm$mer) #
AIC_age8nolag_sm <- AIC(age8nolag_sm$mer) #
plot(age8nolag_sm$gam)
anova(age8nolag_sm$gam)
#saveRDS(age8nolag_sm, file=paste(wd,"/scripts/model_output_age8_lin_surv-abun_nonlin.rds", sep=""))
age8nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age8_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled8dat)
gam.check(base8$gam) #hmm qq
summary(base8$gam) #
summary(base8$mer) #
AIC(base8$mer)

#saveRDS(base8, file=paste(wd,"/scripts/model_output_age8_base.rds", sep=""))
base8 <- readRDS(file=paste(wd,"/scripts/model_output_age8_base.rds", sep=""))







#age 9------
scaled9dat <- scaled_dat[which(scaled_dat$AGE==9),]

#none look particularly nonlinear, let's try linear interactions

age9nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled9dat )
gam.check(age9nolag$gam) #
summary(age9nolag$gam) #
summary(age9nolag$mer) #
AIC_age9nolag <- AIC(age8nolag$mer) #
plot(age9nolag$gam)
anova(age9nolag$gam)
#saveRDS(age9nolag, file=paste(wd,"/scripts/model_output_age9_lin_surv-abun.rds", sep=""))
age9nolag <- readRDS(file=paste(wd,"/scripts/model_output_age9_lin_surv-abun.rds", sep=""))

#nonlinear
age9nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled9dat )
gam.check(age9nolag_sm$gam) #
summary(age9nolag_sm$gam) #
summary(age9nolag_sm$mer) #
AIC_age9nolag_sm <- AIC(age9nolag_sm$mer) #
plot(age9nolag_sm$gam)
anova(age9nolag_sm$gam)
#saveRDS(age9nolag_sm, file=paste(wd,"/scripts/model_output_age9_lin_surv-abun_nonlin.rds", sep=""))
age9nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age9_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base9 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled9dat)
gam.check(base9$gam) #hmm qq
summary(base9$gam) #
summary(base9$mer) #
AIC(base9$mer)

#saveRDS(base9, file=paste(wd,"/scripts/model_output_age9_base.rds", sep=""))
base9 <- readRDS(file=paste(wd,"/scripts/model_output_age9_base.rds", sep=""))







#age 10------
scaled10dat <- scaled_dat[which(scaled_dat$AGE==10),]

#none look particularly nonlinear, let's try linear interactions

age10nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     mean_ann_F3plus_scaled + 
                     pollock_survey_abun_mil_at_age_scaled +
                     apex_pred_biom_scaled + 
                     forage_fish_biom_scaled + 
                     pelagic_forager_biom_scaled +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled10dat )
gam.check(age10nolag$gam) #
summary(age10nolag$gam) #
summary(age10nolag$mer) #
AIC_age10nolag <- AIC(age10nolag$mer) #
plot(age10nolag$gam)
anova(age10nolag$gam)
#saveRDS(age10nolag, file=paste(wd,"/scripts/model_output_age10_lin_surv-abun.rds", sep=""))
age10nolag <- readRDS(file=paste(wd,"/scripts/model_output_age10_lin_surv-abun.rds", sep=""))

#nonlinear
age10nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                        s(mean_ann_F3plus_scaled, k=4) + 
                        s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                        s(apex_pred_biom_scaled, k=4) + 
                        s(forage_fish_biom_scaled, k=4) + 
                        s(pelagic_forager_biom_scaled, k=4) +
                        s(cohort, bs="re"),
                      random=~(1|YEAR/HAUL), data=scaled10dat )
gam.check(age10nolag_sm$gam) #
summary(age10nolag_sm$gam) #
summary(age10nolag_sm$mer) #
AIC_age10nolag_sm <- AIC(age10nolag_sm$mer) #
plot(age10nolag_sm$gam)
anova(age10nolag_sm$gam)
#saveRDS(age10nolag_sm, file=paste(wd,"/scripts/model_output_age10_lin_surv-abun_nonlin.rds", sep=""))
age10nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age10_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=scaled10dat)
gam.check(base10$gam) #hmm qq
summary(base10$gam) #
summary(base10$mer) #
AIC(base10$mer)

#saveRDS(base10, file=paste(wd,"/scripts/model_output_age10_base.rds", sep=""))
base10 <- readRDS(file=paste(wd,"/scripts/model_output_age10_base.rds", sep=""))




#age 11------
scaled11dat <- scaled_dat[which(scaled_dat$AGE==11),]

#none look particularly nonlinear, let's try linear interactions

age11nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled + 
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=scaled11dat )
gam.check(age11nolag$gam) #
summary(age11nolag$gam) #
summary(age11nolag$mer) #
AIC_age11nolag <- AIC(age11nolag$mer) #
plot(age11nolag$gam)
anova(age11nolag$gam)
#saveRDS(age11nolag, file=paste(wd,"/scripts/model_output_age11_lin_surv-abun.rds", sep=""))
age11nolag <- readRDS(file=paste(wd,"/scripts/model_output_age11_lin_surv-abun.rds", sep=""))

#nonlinear
age11nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                         s(mean_ann_F3plus_scaled, k=4) + 
                         s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                         s(apex_pred_biom_scaled, k=4) + 
                         s(forage_fish_biom_scaled, k=4) + 
                         s(pelagic_forager_biom_scaled, k=4) +
                         s(cohort, bs="re"),
                       random=~(1|YEAR/HAUL), data=scaled11dat )
gam.check(age11nolag_sm$gam) #
summary(age11nolag_sm$gam) #
summary(age11nolag_sm$mer) #
AIC_age11nolag_sm <- AIC(age11nolag_sm$mer) #
plot(age11nolag_sm$gam)
anova(age11nolag_sm$gam)
#saveRDS(age11nolag_sm, file=paste(wd,"/scripts/model_output_age11_lin_surv-abun_nonlin.rds", sep=""))
age11nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age11_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base11 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                random=~(1|YEAR/HAUL), data=scaled11dat)
gam.check(base11$gam) #hmm qq
summary(base11$gam) #
summary(base11$mer) #
AIC(base11$mer)

#saveRDS(base11, file=paste(wd,"/scripts/model_output_age11_base.rds", sep=""))
base11 <- readRDS(file=paste(wd,"/scripts/model_output_age11_base.rds", sep=""))





#age 12------
lag12dat <- lagged_dat[which(lagged_dat$AGE==12),]

#none look particularly nonlinear, let's try linear interactions

age12nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled + 
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=lag12dat )
gam.check(age12nolag$gam) #
summary(age12nolag$gam) #
summary(age12nolag$mer) #
AIC_age12nolag <- AIC(age12nolag$mer) #
plot(age12nolag$gam)
anova(age12nolag$gam)
#saveRDS(age12nolag, file=paste(wd,"/scripts/model_output_age12_lin_surv-abun.rds", sep=""))
age12nolag <- readRDS(file=paste(wd,"/scripts/model_output_age12_lin_surv-abun.rds", sep=""))

#nonlinear
age12nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                         s(mean_ann_F3plus_scaled, k=4) + 
                         s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                         s(apex_pred_biom_scaled, k=4) + 
                         s(forage_fish_biom_scaled, k=4) + 
                         s(pelagic_forager_biom_scaled, k=4) +
                         s(cohort, bs="re"),
                       random=~(1|YEAR/HAUL), data=lag12dat )
gam.check(age12nolag_sm$gam) #
summary(age12nolag_sm$gam) #
summary(age12nolag_sm$mer) #
AIC_age12nolag_sm <- AIC(age12nolag_sm$mer) #
plot(age12nolag_sm$gam)
anova(age12nolag_sm$gam)
#saveRDS(age12nolag_sm, file=paste(wd,"/scripts/model_output_age12_lin_surv-abun_nonlin.rds", sep=""))
age12nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age12_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base12 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                random=~(1|YEAR/HAUL), data=lag12dat)
gam.check(base12$gam) #hmm qq
summary(base12$gam) #
summary(base12$mer) #
AIC(base12$mer)

#saveRDS(base12, file=paste(wd,"/scripts/model_output_age12_base.rds", sep=""))
base12 <- readRDS(file=paste(wd,"/scripts/model_output_age12_base.rds", sep=""))





#age 13------
lag13dat <- lagged_dat[which(lagged_dat$AGE==13),]

#none look particularly nonlinear, let's try linear interactions

age13nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled + 
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=lag13dat )
gam.check(age13nolag$gam) #
summary(age13nolag$gam) #
summary(age13nolag$mer) #
AIC_age13nolag <- AIC(age13nolag$mer) #
plot(age13nolag$gam)
anova(age13nolag$gam)
#saveRDS(age13nolag, file=paste(wd,"/scripts/model_output_age13_lin_surv-abun.rds", sep=""))
age13nolag <- readRDS(file=paste(wd,"/scripts/model_output_age13_lin_surv-abun.rds", sep=""))

#nonlinear
age13nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                         s(mean_ann_F3plus_scaled, k=4) + 
                         s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                         s(apex_pred_biom_scaled, k=4) + 
                         s(forage_fish_biom_scaled, k=4) + 
                         s(pelagic_forager_biom_scaled, k=4) +
                         s(cohort, bs="re"),
                       random=~(1|YEAR/HAUL), data=lag13dat )
gam.check(age13nolag_sm$gam) #
summary(age13nolag_sm$gam) #
summary(age13nolag_sm$mer) #
AIC_age13nolag_sm <- AIC(age13nolag_sm$mer) #
plot(age13nolag_sm$gam)
anova(age13nolag_sm$gam)
#saveRDS(age13nolag_sm, file=paste(wd,"/scripts/model_output_age13_lin_surv-abun_nonlin.rds", sep=""))
age13nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age13_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base13 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                random=~(1|YEAR/HAUL), data=lag13dat)
gam.check(base13$gam) #hmm qq
summary(base13$gam) #
summary(base13$mer) #
AIC(base13$mer)

#saveRDS(base13, file=paste(wd,"/scripts/model_output_age13_base.rds", sep=""))
base13 <- readRDS(file=paste(wd,"/scripts/model_output_age13_base.rds", sep=""))





#age 14------
lag14dat <- lagged_dat[which(lagged_dat$AGE==14),]

#none look particularly nonlinear, let's try linear interactions

age14nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled + 
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=lag14dat )
gam.check(age14nolag$gam) #
summary(age14nolag$gam) #
summary(age14nolag$mer) #
AIC_age14nolag <- AIC(age14nolag$mer) #
plot(age14nolag$gam)
anova(age14nolag$gam)
#saveRDS(age14nolag, file=paste(wd,"/scripts/model_output_age14_lin_surv-abun.rds", sep=""))
age14nolag <- readRDS(file=paste(wd,"/scripts/model_output_age14_lin_surv-abun.rds", sep=""))

#nonlinear
age14nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                         s(mean_ann_F3plus_scaled, k=4) + 
                         s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                         s(apex_pred_biom_scaled, k=4) + 
                         s(forage_fish_biom_scaled, k=4) + 
                         s(pelagic_forager_biom_scaled, k=4) +
                         s(cohort, bs="re"),
                       random=~(1|YEAR/HAUL), data=lag14dat )
gam.check(age14nolag_sm$gam) #
summary(age14nolag_sm$gam) #
summary(age14nolag_sm$mer) #
AIC_age14nolag_sm <- AIC(age14nolag_sm$mer) #
plot(age14nolag_sm$gam)
anova(age14nolag_sm$gam)
#saveRDS(age14nolag_sm, file=paste(wd,"/scripts/model_output_age14_lin_surv-abun_nonlin.rds", sep=""))
age14nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age14_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base14 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                random=~(1|YEAR/HAUL), data=lag14dat)
gam.check(base14$gam) #hmm qq
summary(base14$gam) #
summary(base14$mer) #
AIC(base14$mer)

#saveRDS(base14, file=paste(wd,"/scripts/model_output_age14_base.rds", sep=""))
base14 <- readRDS(file=paste(wd,"/scripts/model_output_age14_base.rds", sep=""))








#age 15------
lag15dat <- lagged_dat[which(lagged_dat$AGE==15),]

#none look particularly nonlinear, let's try linear interactions

age15nolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled + 
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=lag15dat )
gam.check(age15nolag$gam) #
summary(age15nolag$gam) #
summary(age15nolag$mer) #
AIC_age15nolag <- AIC(age15nolag$mer) #
plot(age15nolag$gam)
anova(age15nolag$gam)
#saveRDS(age15nolag, file=paste(wd,"/scripts/model_output_age15_lin_surv-abun.rds", sep=""))
age15nolag <- readRDS(file=paste(wd,"/scripts/model_output_age15_lin_surv-abun.rds", sep=""))

#nonlinear
age15nolag_sm <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                         s(mean_ann_F3plus_scaled, k=4) + 
                         s(pollock_survey_abun_mil_at_age_scaled, k=4) +
                         s(apex_pred_biom_scaled, k=4) + 
                         s(forage_fish_biom_scaled, k=4) + 
                         s(pelagic_forager_biom_scaled, k=4) +
                         s(cohort, bs="re"),
                       random=~(1|YEAR/HAUL), data=lag15dat )
gam.check(age15nolag_sm$gam) #
summary(age15nolag_sm$gam) #
summary(age15nolag_sm$mer) #
AIC_age15nolag_sm <- AIC(age15nolag_sm$mer) #
plot(age15nolag_sm$gam)
anova(age15nolag_sm$gam)
#saveRDS(age15nolag_sm, file=paste(wd,"/scripts/model_output_age15_lin_surv-abun_nonlin.rds", sep=""))
age15nolag_sm <- readRDS(file=paste(wd,"/scripts/model_output_age15_lin_surv-abun_nonlin.rds", sep=""))



#compare to base
base15 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
                random=~(1|YEAR/HAUL), data=lag15dat)
gam.check(base15$gam) #hmm qq
summary(base15$gam) #
summary(base15$mer) #
AIC(base15$mer)

#saveRDS(base15, file=paste(wd,"/scripts/model_output_age15_base.rds", sep=""))
base15 <- readRDS(file=paste(wd,"/scripts/model_output_age15_base.rds", sep=""))













