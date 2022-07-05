#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models run on ALL AGES TOGETHER

#Created by Krista, Apr 20, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")
library(gratia)
library(visreg)
library(sjPlot)


#data
wd <- getwd()
lagged_dat <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_data_pollock_length.csv", sep=""), row.names=1)

lagged_dat <- lagged_dat[which(lagged_dat$AGE<16),]
lagged_dat$YEAR <- as.factor(lagged_dat$YEAR)
lagged_dat$AGE <- as.factor(lagged_dat$AGE)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=lagged_dat) + theme_bw() 

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

#GAMs-----

library(mgcv)
library(gamm4)
library(car)


#lets start out with no lag, all ages, with interactions

#including all at once doesn't seem to work, maybe first look to see if any look nonlinear

ggplot(lagged_dat, aes(mean_annual_F3plus.x, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

ggplot(lagged_dat, aes(pollock_abun_bil_at_age, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

ggplot(lagged_dat, aes(pollock_survey_abun_mil_at_age, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

ggplot(lagged_dat, aes(apex_pred_biomass, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

ggplot(lagged_dat, aes(forage_fish_biomass, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

ggplot(lagged_dat, aes(pelagic_forager_biomass, LENGTH)) + geom_point() + geom_smooth() +
  facet_wrap(~AGE, scales="free")

#scaled
ggplot(scaled_dat, aes(mean_ann_F3plus_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(pol_abun_bil_at_age_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(pollock_survey_abun_mil_at_age_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(apex_pred_biom_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(forage_fish_biom_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(pelagic_forager_biom_scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")

ggplot(scaled_dat, aes(south.sst.amj.scaled, length_scaled, col=AGE)) + geom_point() + 
  geom_smooth(method="lm", col="black") +
  facet_wrap(~AGE, scales="free")


#none look particularly nonlinear, let's try linear interactions

allnolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    mean_ann_F3plus_scaled:AGE + 
                    pol_abun_bil_at_age_scaled:AGE +
                  apex_pred_biom_scaled:AGE + 
                  forage_fish_biom_scaled:AGE + 
                  pelagic_forager_biom_scaled:AGE +
                    s(cohort, bs="re"),
                random=~(1|YEAR/HAUL), data=scaled_dat )
gam.check(allnolag$gam) #
summary(allnolag$gam) #
summary(allnolag$mer) #
AIC_allnolag <- AIC(allnolag$mer) #
plot(allnolag$gam)
anova(allnolag$gam)
plot_model(allnolag[[2]], type="int")
plot_model(allnolag$gam) 
#saveRDS(allnolag, file=paste(wd,"/scripts/model_output_all-ages_lin_interactions.rds", sep=""))

allnolagML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    mean_ann_F3plus_scaled:AGE + 
                    pol_abun_bil_at_age_scaled:AGE +
                    apex_pred_biom_scaled:AGE + 
                    forage_fish_biom_scaled:AGE + 
                    pelagic_forager_biom_scaled:AGE +
                    s(cohort, bs="re"),
                  random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
#saveRDS(allnolagML, file=paste(wd,"/scripts/model_output_all-ages_lin_interactionsML.rds", sep=""))


allnonlin <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                    s(pol_abun_bil_at_age_scaled, by=AGE, k=4) +
                    s(apex_pred_biom_scaled, by=AGE, k=4) + 
                    s(forage_fish_biom_scaled, by=AGE, k=4) + 
                    s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                    s(cohort, bs="re"),
                  random=~(1|YEAR/HAUL), data=scaled_dat)
gam.check(allnonlin$gam)
#saveRDS(allnonlin, file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactions.rds", sep=""))
allnonlin <- readRDS(file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactions.rds", sep=""))
summary(allnonlin$gam)
anova(allnonlin$gam)
plot(allnonlin$gam)

allnonlinML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                     s(pol_abun_bil_at_age_scaled, by=AGE, k=4) +
                     s(apex_pred_biom_scaled, by=AGE, k=4) + 
                     s(forage_fish_biom_scaled, by=AGE, k=4) + 
                     s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
#saveRDS(allnonlinML, file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactionsML.rds", sep=""))
gam.check(allnonlinML$gam)
summary(allnonlinML$gam)
plot(allnonlinML$gam)



#repeat with SURVEY ABUNDANCES (more age classes)-----

#none look particularly nonlinear, let's try linear interactions

surnolag <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    mean_ann_F3plus_scaled:AGE + 
                    pollock_survey_abun_mil_at_age_scaled:AGE +
                    apex_pred_biom_scaled:AGE + 
                    forage_fish_biom_scaled:AGE + 
                    pelagic_forager_biom_scaled:AGE +
                    s(cohort, bs="re"),
                  random=~(1|YEAR/HAUL), data=scaled_dat )
gam.check(surnolag$gam) #
summary(surnolag$gam) #
summary(surnolag$mer) #
AIC_surnolag <- AIC(surnolag$mer) #
plot(surnolag$gam)
anova(surnolag$gam)
plot_model(surnolag[[2]], type="int")
plot_model(surnolag$gam) 
#saveRDS(surnolag, file=paste(wd,"/scripts/model_output_all-ages_lin_interactions_surv-abun.rds", sep=""))

surnolagML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_ann_F3plus_scaled:AGE + 
                      pollock_survey_abun_mil_at_age_scaled:AGE +
                      apex_pred_biom_scaled:AGE + 
                      forage_fish_biom_scaled:AGE + 
                      pelagic_forager_biom_scaled:AGE +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
#saveRDS(surnolagML, file=paste(wd,"/scripts/model_output_all-ages_lin_interaction_surv-abunsML.rds", sep=""))
summary(surnolagML$gam)
plot(surnolagML$gam)


surnonlin <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                     s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                     s(apex_pred_biom_scaled, by=AGE, k=4) + 
                     s(forage_fish_biom_scaled, by=AGE, k=4) + 
                     s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=scaled_dat)
gam.check(surnonlin$gam)
#saveRDS(surnonlin, file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactions_surv-abun.rds", sep=""))
surnonlin <- readRDS(file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactions_surv-abun.rds", sep=""))
summary(surnonlin$gam)
anova(surnonlin$gam)
plot(surnonlin$gam)

surnonlinML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                       s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                       s(apex_pred_biom_scaled, by=AGE, k=4) + 
                       s(forage_fish_biom_scaled, by=AGE, k=4) + 
                       s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
#saveRDS(surnonlinML, file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactionsML_surv-abun.rds", sep=""))
surnonlinML <- readRDS(file=paste(wd,"/scripts/model_output_all-ages_nonlin_interactionsML_surv-abun.rds", sep=""))

gam.check(surnonlinML$gam)
summary(surnonlinML$gam)
plot(surnonlinML$gam)

par(mfrow = c(3, 5))




#switch individually to linear since many seem not very linear but still significant

linF3ML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_ann_F3plus_scaled:AGE + 
                       s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                       s(apex_pred_biom_scaled, by=AGE, k=4) + 
                       s(forage_fish_biom_scaled, by=AGE, k=4) + 
                       s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
saveRDS(linF3ML, file=paste(wd,"/scripts/model_output_all-ages_linearF3ML_surv-abun.rds", sep=""))

linsurvML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                       pollock_survey_abun_mil_at_age_scaled:AGE +
                       s(apex_pred_biom_scaled, by=AGE, k=4) + 
                       s(forage_fish_biom_scaled, by=AGE, k=4) + 
                       s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
saveRDS(linsurvML, file=paste(wd,"/scripts/model_output_all-ages_linearsurvML_surv-abun.rds", sep=""))


linapexML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                       s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                       apex_pred_biom_scaled:AGE + 
                       s(forage_fish_biom_scaled, by=AGE, k=4) + 
                       s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
saveRDS(linapexML, file=paste(wd,"/scripts/model_output_all-ages_linearapexML_surv-abun.rds", sep=""))


linforageML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                       s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                       s(apex_pred_biom_scaled, by=AGE, k=4) + 
                       forage_fish_biom_scaled:AGE + 
                       s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
saveRDS(linforageML, file=paste(wd,"/scripts/model_output_all-ages_linearforageML_surv-abun.rds", sep=""))


linpelagicML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                       s(pollock_survey_abun_mil_at_age_scaled, by=AGE, k=4) +
                       s(apex_pred_biom_scaled, by=AGE, k=4) + 
                       s(forage_fish_biom_scaled, by=AGE, k=4) + 
                       pelagic_forager_biom_scaled:AGE +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL), data=scaled_dat, REML=FALSE )
saveRDS(linpelagicML, file=paste(wd,"/scripts/model_output_all-ages_linearpelagicML_surv-abun.rds", sep=""))

AIC(linpelagicML$mer, linforageML$mer, linapexML$mer, linsurvML$mer, linF3ML$mer, surnolagML$mer, 
    surnonlinML$mer)
#nonlinear is lowest AIC but far momre parameters

#model selection using dredge--------------

#change na global option
options(na.action = "na.fail")

library(MuMIn)


#now that na action is set need to re-run the 'global' model we want to use
#need a dataset with no NAs, let's first narrow down to only columns we're using
scaled_sub <- scaled_dat[,c("length_scaled", "AGE", 
                            "LONGITUDE", "LATITUDE", "julian_scaled",
                            "south.sst.amj.scaled", "mean_ann_F3plus_scaled",
                            "pol_abun_bil_at_age_scaled", "apex_pred_biom_scaled",
                            "forage_fish_biom_scaled", "pelagic_forager_biom_scaled",
                            "cohort", "YEAR", "HAUL")]

nona_scaled <- na.omit(scaled_sub)
unique(nona_scaled$YEAR)

glob_mod <- gamm4(length_scaled ~  s(south.sst.amj.scaled, by=AGE, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                     s(mean_ann_F3plus_scaled, by=AGE, k=4) + 
                     s(pol_abun_bil_at_age_scaled, by=AGE, k=4) +
                     s(apex_pred_biom_scaled, by=AGE, k=4) + 
                     s(forage_fish_biom_scaled, by=AGE, k=4) + 
                     s(pelagic_forager_biom_scaled, by=AGE, k=4) +
                     s(cohort, bs="re"),
                   random=~(1|YEAR/HAUL), data=nona_scaled)
gam.check(glob_mod$gam)
#saveRDS(glob_mod, file=paste(wd,"/scripts/global_model_2_dredge.rds", sep=""))
glob_mod <- readRDS(file=paste(wd,"/scripts/global_model_2_dredge.rds", sep=""))

#following https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/inst/doc/gamm.pdf?revision=91&root=mumin&pathrev=91
#we set functions so that dredge/MuMIn know how to handle gamm4 output which is usually a list
gamm4 <- function(...) structure(c(gamm4::gamm4(...), list(call = match.call())),
                       class = c("gamm", "list"))
logLik.gamm <- function(object, ...) logLik(object[[if (is.null(object$lme)) "mer" else "lme"]],  ...)
 formula.gamm <- function(x, ...) formula(x$gam, ...)
 nobs.gamm <- function(object, ...) nobs(object$gam, ...)
 nobs.gam <- function(object, ...) stats:::nobs.glm(object, ...)
  coeffs.gamm <- function(model) coef(model$gam)
  getAllTerms.gamm <- function(x, ...) getAllTerms(x$gam)
  tTable.gamm <- function(model, ...) tTable(model$gam)

dredge_out <- dredge(glob_mod, beta="sd")
#need to confirm the beta is correct here
