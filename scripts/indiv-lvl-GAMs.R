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

indiv_lvl_dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE<11),]
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

#age 1------
ind1dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==1),]

mind1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
               s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
                       random=~(1|YEAR/HAUL), data=ind1dat)
gam.check(mind1$gam) #not terrible
summary(mind1$gam) #
summary(mind1$mer) #
AIC(mind1$mer) #
plot(mind1$gam) #several terms don't look nonlinear

#compare to base
base1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind1dat)
gam.check(base1$gam) #hmm qq
summary(base1$gam) #
summary(base1$mer) #
AIC(base1$mer)




#age 2------
ind2dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==2),]

mind2 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind2dat)
gam.check(mind2$gam) #not bad
summary(mind2$gam) #not many nonlinear, just forage fish
summary(mind2$mer) #
AIC(mind2$mer) #
plot(mind2$gam) #

#compare to base
base2 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind2dat)
gam.check(base2$gam) #fine
summary(base2$gam) #
summary(base2$mer) #
AIC(base2$mer)



#age 3------
ind3dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==3),]

mind3 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind3dat)
gam.check(mind3$gam) #ok?
summary(mind3$gam) #nonlinear inc sst, julian?,F3plus,forage
summary(mind3$mer) #
AIC(mind3$mer) #
plot(mind3$gam) #

#compare to base
base3 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind3dat)
gam.check(base3$gam) #
summary(base3$gam) #
summary(base3$mer) #
AIC(base3$mer)



#age 4------
ind4dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==4),]

mind4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind4dat)
gam.check(mind4$gam) #
summary(mind4$gam) #more nonlinear, only polluck abun, apex, and pelegic look linear
summary(mind4$mer) #
AIC(mind4$mer) #
plot(mind4$gam) #

#compare to base
base4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind4dat)
gam.check(base4$gam) #
summary(base4$gam) #
summary(base4$mer) #
AIC(base4$mer)




#age 5------
ind5dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==5),]

mind5 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind5dat)
gam.check(mind5$gam) #
summary(mind5$gam) #F3plus and forage look nonlinear
summary(mind5$mer) #
AIC(mind5$mer) #
plot(mind5$gam) #

#compare to base
base5 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind5dat)
gam.check(base5$gam) #
summary(base5$gam) #
summary(base5$mer) #
AIC(base5$mer)





#age 6------
ind6dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==6),]

mind6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind6dat)
gam.check(mind6$gam) #
summary(mind6$gam) #nonlinear inc sst, julian, F3plus, forage
summary(mind6$mer) #
AIC(mind6$mer) #
plot(mind6$gam) #

#compare to base
base6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind6dat)
gam.check(base6$gam) #
summary(base6$gam) #
summary(base6$mer) #
AIC(base6$mer)




#age 7------
ind7dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==7),]

mind7 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind7dat)
gam.check(mind7$gam) #
summary(mind7$gam) #nonlinear inc sst, julian, apex, forage
summary(mind7$mer) #
AIC(mind7$mer) #
plot(mind7$gam) #

#compare to base
base7 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind7dat)
gam.check(base7$gam) #
summary(base7$gam) #
summary(base7$mer) #
AIC(base7$mer)




#age 8------
ind8dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==8),]

mind8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind8dat)
gam.check(mind8$gam) #
summary(mind8$gam) #nonlinear julian, pelagic
summary(mind8$mer) #
AIC(mind8$mer) #
plot(mind8$gam) #

#compare to base
base8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind8dat)
gam.check(base8$gam) #
summary(base8$gam) #
summary(base8$mer) #
AIC(base8$mer)




#age 9------
ind9dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==9),]

mind9 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind9dat)
gam.check(mind9$gam) #
summary(mind9$gam) #only nonlinear is pollock abun
summary(mind9$mer) #
AIC(mind9$mer) #
plot(mind9$gam) #

#compare to base
base9 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind9dat)
gam.check(base9$gam) #
summary(base9$gam) #
summary(base9$mer) #
AIC(base9$mer)





#age 10------
ind10dat <- indiv_lvl_dat[which(indiv_lvl_dat$AGE==10),]

mind10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=ind10dat)
gam.check(mind10$gam) #
summary(mind10$gam) #nonlinear F3plus, pelagic
summary(mind10$mer) #
AIC(mind10$mer) #
plot(mind10$gam) #

#compare to base
base10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random=~(1|YEAR/HAUL), data=ind10dat)
gam.check(base10$gam) #
summary(base10$gam) #
summary(base10$mer) #
AIC(base10$mer)


