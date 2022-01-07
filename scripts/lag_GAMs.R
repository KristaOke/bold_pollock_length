#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models WITH lags

#Created by Krista, Jan 6, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)

#data

lagged_dat <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_data_pollock_length.csv", sep=""), row.names=1)

lagged_dat <- lagged_dat[which(lagged_dat$AGE<11),]
lagged_dat$YEAR <- as.factor(lagged_dat$YEAR)

#GAMs-----

library(mgcv)
library(gamm4)
library(car)


#age 1------
lag1dat <- lagged_dat[which(lagged_dat$AGE==1),]

#age 1, no lag - - - - 
lag1.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                 s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                 s(apex_pred_biomass, k=4) + 
                 s(forage_fish_biomass, k=4) + 
                 s(pelagic_forager_biomass, k=4),
               random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.0$gam) #not terrible
summary(lag1.0$gam) #
summary(lag1.0$mer) #
AIC_1lag0 <- AIC(lag1.0$mer) #
plot(lag1.0$gam)

#age 1, 1 yr lag - - - - 

lag1.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.1$gam) #
summary(lag1.1$gam) #nonlinear julian and forage
summary(lag1.1$mer) #
AIC_1lag1 <- AIC(lag1.1$mer) #
plot(lag1.1$gam)

#age 1, 4 yr lag - - - - 

lag1.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.4$gam) #
summary(lag1.4$gam) #nonlinear julian and forage
summary(lag1.4$mer) #
AIC_1lag4 <- AIC(lag1.4$mer) #
plot(lag1.4$gam)

#age 1, 6 yr lag - - - - 

lag1.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.6$gam) #
summary(lag1.6$gam) ##nonlinear julian and forage and pelagic
summary(lag1.6$mer) #
AIC_1lag6 <- AIC(lag1.6$mer) #
plot(lag1.6$gam)



#age 1, 8 yr lag - - - - 

lag1.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.8$gam) #
summary(lag1.8$gam) #nonlinear julian and forage and pelagic
summary(lag1.8$mer) #
AIC_1lag8 <- AIC(lag1.8$mer) #
plot(lag1.8$gam)

#age 1, 10 yr lag - - - - 

lag1.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.10$gam) #
summary(lag1.10$gam) #nonlinear julian and forage and pelagic
summary(lag1.10$mer) #
AIC_1lag10 <- AIC(lag1.10$mer) #
plot(lag1.10$gam)

#age 1, no F - - - - 
lag1.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag1dat )
gam.check(lag1.noF$gam) #
summary(lag1.noF$gam) #
summary(lag1.noF$mer) #
AIC_1lagnoF <- AIC(lag1.noF$mer) #
plot(lag1.noF$gam)

AIC_1lag0
AIC_1lag1
AIC_1lag4
AIC_1lag6
AIC_1lag8
AIC_1lag0 #no real difference among these

AIC_1lagnoF #no improvement by including F?





#age 2------
lag2dat <- lagged_dat[which(lagged_dat$AGE==2),]

#age 2, no lag - - - - 
lag2.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.0$gam) #
summary(lag2.0$gam) #nonlinear forage
summary(lag2.0$mer) #
AIC_2lag0 <- AIC(lag2.0$mer) #
plot(lag2.0$gam)

#age 2, 1 yr lag - - - - 

lag2.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.1$gam) #
summary(lag2.1$gam) #nonlinear forage and maybe apex
summary(lag2.1$mer) #
AIC_2lag1 <- AIC(lag2.1$mer) #
plot(lag2.1$gam)

#age 2, 4 yr lag - - - - 

lag2.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.4$gam) #
summary(lag2.4$gam) #nonlinear inc forage
summary(lag2.4$mer) #
AIC_2lag4 <- AIC(lag2.4$mer) #
plot(lag2.4$gam)

#age 2, 6 yr lag - - - - 

lag2.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.6$gam) #
summary(lag2.6$gam) #nonlinear inc F, pollock abun, forage, apex
summary(lag2.6$mer) #
AIC_2lag6 <- AIC(lag2.6$mer) #
plot(lag2.6$gam)



#age 2, 8 yr lag - - - - 

lag2.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.8$gam) #
summary(lag2.8$gam) #nonlinear forage
summary(lag2.8$mer) #
AIC_2lag8 <- AIC(lag2.8$mer) #
plot(lag2.8$gam)

#age 2, 10 yr lag - - - - 

lag2.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.10$gam) #
summary(lag2.10$gam) #forage nonlinear
summary(lag2.10$mer) #
AIC_2lag10 <- AIC(lag2.10$mer) #
plot(lag2.10$gam)

#age 2, no F - - - - 
lag2.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag2dat )
gam.check(lag2.noF$gam) #
summary(lag2.noF$gam) #nonlinear forage and maybe apex
summary(lag2.noF$mer) #
AIC_2lagnoF <- AIC(lag2.noF$mer) #
plot(lag2.noF$gam)

AIC_2lag0
AIC_2lag1
AIC_2lag4
AIC_2lag6
AIC_2lag8
AIC_2lag0 #very little difference

AIC_2lagnoF #ditto







#age 3------
lag3dat <- lagged_dat[which(lagged_dat$AGE==3),]

#age 3, no lag - - - - 
lag3.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.0$gam) #
summary(lag3.0$gam) #nonlinear is F, julian, sst, forage
summary(lag3.0$mer) #
AIC_3lag0 <- AIC(lag3.0$mer) #
plot(lag3.0$gam)

#age 3, 1 yr lag - - - - 

lag3.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.1$gam) #
summary(lag3.1$gam) #nonlinear are sst, julian, forage
summary(lag3.1$mer) #
AIC_3lag1 <- AIC(lag3.1$mer) #
plot(lag3.1$gam)

#age 3, 4 yr lag - - - - 

lag3.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.4$gam) #
summary(lag3.4$gam) #nonlinear sst, julian, forage
summary(lag3.4$mer) #
AIC_3lag4 <- AIC(lag3.4$mer) #
plot(lag3.4$gam)

#age 3, 6 yr lag - - - - 

lag3.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.6$gam) #
summary(lag3.6$gam) #nonlinear sst, julian, forage
summary(lag3.6$mer) #
AIC_3lag6 <- AIC(lag3.6$mer) #
plot(lag3.6$gam)



#age 3, 8 yr lag - - - - 

lag3.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.8$gam) #
summary(lag3.8$gam) #nonlinear sst, julian, forage
summary(lag3.8$mer) #
AIC_3lag8 <- AIC(lag3.8$mer) #
plot(lag3.8$gam)

#age 3, 10 yr lag - - - - 

lag3.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.10$gam) #
summary(lag3.10$gam) #nonlinear sst, julian, F, forage
summary(lag3.10$mer) #
AIC_3lag10 <- AIC(lag3.10$mer) #
plot(lag3.10$gam)

#age 3, no F - - - - 
lag3.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag3dat )
gam.check(lag3.noF$gam) #
summary(lag3.noF$gam) #nonlinear sst, julian, forage
summary(lag3.noF$mer) #
AIC_3lagnoF <- AIC(lag3.noF$mer) #
plot(lag3.noF$gam)

AIC_3lag0
AIC_3lag1
AIC_3lag4
AIC_3lag6
AIC_3lag8 #this one is 6 lower than next best, otherwise very similar
AIC_3lag0 #

AIC_3lagnoF #







#age 4------
lag4dat <- lagged_dat[which(lagged_dat$AGE==4),]

#age 4, no lag - - - - 
lag4.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.0$gam) #
summary(lag4.0$gam) #nonlinear sst, julian, F, forage
summary(lag4.0$mer) #
AIC_4lag0 <- AIC(lag4.0$mer) #
plot(lag4.0$gam)

#age 4, 1 yr lag - - - - 

lag4.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.1$gam) #
summary(lag4.1$gam) #nonlinear sst, julian, forage
summary(lag4.1$mer) #
AIC_4lag1 <- AIC(lag4.1$mer) #
plot(lag4.1$gam)

#age 4, 4 yr lag - - - - 

lag4.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.4$gam) #
summary(lag4.4$gam) #nonlinear sst, julian, forage
summary(lag4.4$mer) #
AIC_4lag4 <- AIC(lag4.4$mer) #
plot(lag4.4$gam)

#age 4, 6 yr lag - - - - 

lag4.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.6$gam) #
summary(lag4.6$gam) #nonlinear sst, julian, forage
summary(lag4.6$mer) #
AIC_4lag6 <- AIC(lag4.6$mer) #
plot(lag4.6$gam)



#age 4, 8 yr lag - - - - 

lag4.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.8$gam) #
summary(lag4.8$gam) #nonlinear sst, julian, forage
summary(lag4.8$mer) #
AIC_4lag8 <- AIC(lag4.8$mer) #
plot(lag4.8$gam)

#age 4, 10 yr lag - - - - 

lag4.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.10$gam) #
summary(lag4.10$gam) #
summary(lag4.10$mer) #
AIC_4lag10 <- AIC(lag4.10$mer) #
plot(lag4.10$gam)

#age 4, no F - - - - 
lag4.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag4dat )
gam.check(lag4.noF$gam) #
summary(lag4.noF$gam) #nonlinear sst, julian, forage
summary(lag4.noF$mer) #
AIC_4lagnoF <- AIC(lag4.noF$mer) #
plot(lag4.noF$gam)

AIC_4lag0
AIC_4lag1
AIC_4lag4
AIC_4lag6
AIC_4lag8
AIC_4lag0 #all within about 5 units

AIC_4lagnoF #






#age 5------
lag5dat <- lagged_dat[which(lagged_dat$AGE==5),]

#age 5, no lag - - - - 
lag5.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.0$gam) #
summary(lag5.0$gam) # nonlinear F, forage
summary(lag5.0$mer) #
AIC_5lag0 <- AIC(lag5.0$mer) #
plot(lag5.0$gam)

#age 5, 1 yr lag - - - - 

lag5.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.1$gam) #
summary(lag5.1$gam) #nonlinear apex, forage
summary(lag5.1$mer) #
AIC_5lag1 <- AIC(lag5.1$mer) #
plot(lag5.1$gam)

#age 5, 4 yr lag - - - - 

lag5.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.4$gam) #
summary(lag5.4$gam) #nonlinear F, forage
summary(lag5.4$mer) #
AIC_5lag4 <- AIC(lag5.4$mer) #
plot(lag5.4$gam)

#age 5, 6 yr lag - - - - 

lag5.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.6$gam) #
summary(lag5.6$gam) #nonlinear forage
summary(lag5.6$mer) #
AIC_5lag6 <- AIC(lag5.6$mer) #
plot(lag5.6$gam)



#age 5, 8 yr lag - - - - 

lag5.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.8$gam) #
summary(lag5.8$gam) #nonlinear forage
summary(lag5.8$mer) #
AIC_5lag8 <- AIC(lag5.8$mer) #
plot(lag5.8$gam)

#age 5, 10 yr lag - - - - 

lag5.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.10$gam) #
summary(lag5.10$gam) #nonlinear forage
summary(lag5.10$mer) #
AIC_5lag10 <- AIC(lag5.10$mer) #
plot(lag5.10$gam)

#age 5, no F - - - - 
lag5.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag5dat )
gam.check(lag5.noF$gam) #
summary(lag5.noF$gam) #nonlinear forage
summary(lag5.noF$mer) #
AIC_5lagnoF <- AIC(lag5.noF$mer) #
plot(lag5.noF$gam)

AIC_5lag0 #better than big lags or no lags, not better than 1 yr lag or 4 yr lag
AIC_5lag1
AIC_5lag4
AIC_5lag6
AIC_5lag8
AIC_5lag10 #

AIC_5lagnoF #







#age 6------
lag6dat <- lagged_dat[which(lagged_dat$AGE==6),]

#age 6, no lag - - - - 
lag6.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.0$gam) #
summary(lag6.0$gam) #nonlinear sst, julian, F, forage
summary(lag6.0$mer) #
AIC_6lag0 <- AIC(lag6.0$mer) #
plot(lag6.0$gam)

#age 6, 1 yr lag - - - - 

lag6.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.1$gam) #
summary(lag6.1$gam) #nonlinear sst, julian, apex, forage
summary(lag6.1$mer) #
AIC_6lag1 <- AIC(lag6.1$mer) #
plot(lag6.1$gam)

#age 6, 4 yr lag - - - - 

lag6.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.4$gam) #
summary(lag6.4$gam) #nonlinear sst, julian, F, forage
summary(lag6.4$mer) #
AIC_6lag4 <- AIC(lag6.4$mer) #
plot(lag6.4$gam)

#age 6, 6 yr lag - - - - 

lag6.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.6$gam) #
summary(lag6.6$gam) #nonlinear sst, julian, forage
summary(lag6.6$mer) #
AIC_6lag6 <- AIC(lag6.6$mer) #
plot(lag6.6$gam)



#age 6, 8 yr lag - - - - 

lag6.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.8$gam) #
summary(lag6.8$gam) #nonlinear sst, julian, forage
summary(lag6.8$mer) #
AIC_6lag8 <- AIC(lag6.8$mer) #
plot(lag6.8$gam)

#age 6, 10 yr lag - - - - 

lag6.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.10$gam) #
summary(lag6.10$gam) #nonlinear sst, julian, apex, forage
summary(lag6.10$mer) #
AIC_6lag10 <- AIC(lag6.10$mer) #
plot(lag6.10$gam)

#age 6, no F - - - - 
lag6.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag6dat )
gam.check(lag6.noF$gam) #
summary(lag6.noF$gam) #nonlinear sst, julian, forage
summary(lag6.noF$mer) #
AIC_6lagnoF <- AIC(lag6.noF$mer) #
plot(lag6.noF$gam)

AIC_6lag0
AIC_6lag1
AIC_6lag4 #best but within 7 units of worst
AIC_6lag6
AIC_6lag8
AIC_6lag10 #

AIC_6lagnoF #





#age 7------
lag7dat <- lagged_dat[which(lagged_dat$AGE==7),]

#age 7, no lag - - - - 
lag7.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.0$gam) #
summary(lag7.0$gam) #nonlinear sst, julian, apex, forage
summary(lag7.0$mer) #
AIC_7lag0 <- AIC(lag7.0$mer) #
plot(lag7.0$gam)

#age 7, 1 yr lag - - - - 

lag7.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.1$gam) #
summary(lag7.1$gam) #nonlinear sst, julian, apex, forage
summary(lag7.1$mer) #
AIC_7lag1 <- AIC(lag7.1$mer) #
plot(lag7.1$gam)

#age 7, 4 yr lag - - - - 

lag7.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.4$gam) #
summary(lag7.4$gam) #nonlinear sst, julian, F, apex, forage
summary(lag7.4$mer) #
AIC_7lag4 <- AIC(lag7.4$mer) #
plot(lag7.4$gam)

#age 7, 6 yr lag - - - - 

lag7.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.6$gam) #
summary(lag7.6$gam) #nonlinear sst, julian, forage
summary(lag7.6$mer) #
AIC_7lag6 <- AIC(lag7.6$mer) #
plot(lag7.6$gam)



#age 7, 8 yr lag - - - - 

lag7.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.8$gam) #
summary(lag7.8$gam) #nonlinear sst, julian, apex, forage
summary(lag7.8$mer) #
AIC_7lag8 <- AIC(lag7.8$mer) #
plot(lag7.8$gam)

#age 7, 10 yr lag - - - - 

lag7.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.10$gam) #
summary(lag7.10$gam) #nonlinear sst, julian, apex, forage
summary(lag7.10$mer) #
AIC_7lag10 <- AIC(lag7.10$mer) #
plot(lag7.10$gam)

#age 7, no F - - - - 
lag7.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag7dat )
gam.check(lag7.noF$gam) #
summary(lag7.noF$gam) #nonlinear sst, julian, apex, forage
summary(lag7.noF$mer) #
AIC_7lagnoF <- AIC(lag7.noF$mer) #
plot(lag7.noF$gam)

AIC_7lag0
AIC_7lag1
AIC_7lag4
AIC_7lag6
AIC_7lag8
AIC_7lag10 #

AIC_7lagnoF #all within about 4 units





#age 8------
lag8dat <- lagged_dat[which(lagged_dat$AGE==8),]

#age 8, no lag - - - - 
lag8.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.0$gam) #
summary(lag8.0$gam) #nonlinear julian, pelagic
summary(lag8.0$mer) #
AIC_8lag0 <- AIC(lag8.0$mer) #
plot(lag8.0$gam)

#age 8, 1 yr lag - - - - 

lag8.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.1$gam) #
summary(lag8.1$gam) #nonlinear julian
summary(lag8.1$mer) #
AIC_8lag1 <- AIC(lag8.1$mer) #
plot(lag8.1$gam)

#age 8, 4 yr lag - - - - 

lag8.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.4$gam) #
summary(lag8.4$gam) #nonlinear julian
summary(lag8.4$mer) #
AIC_8lag4 <- AIC(lag8.4$mer) #
plot(lag8.4$gam)

#age 8, 6 yr lag - - - - 

lag8.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.6$gam) #
summary(lag8.6$gam) #nonlinear julian, F
summary(lag8.6$mer) #
AIC_8lag6 <- AIC(lag8.6$mer) #
plot(lag8.6$gam)



#age 8, 8 yr lag - - - - 

lag8.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.8$gam) #
summary(lag8.8$gam) #nonlinear julian
summary(lag8.8$mer) #
AIC_8lag8 <- AIC(lag8.8$mer) #
plot(lag8.8$gam)

#age 8, 10 yr lag - - - - 

lag8.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.10$gam) #
summary(lag8.10$gam) #nonlinear julian
summary(lag8.10$mer) #
AIC_8lag10 <- AIC(lag8.10$mer) #
plot(lag8.10$gam)

#age 8, no F - - - - 
lag8.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag8dat )
gam.check(lag8.noF$gam) #
summary(lag8.noF$gam) #nonlinear julian
summary(lag8.noF$mer) #
AIC_8lagnoF <- AIC(lag8.noF$mer) #
plot(lag8.noF$gam)

AIC_8lag0
AIC_8lag1
AIC_8lag4
AIC_8lag6 #better by 4 otherwise all v similar
AIC_8lag8
AIC_8lag10 #

AIC_8lagnoF #





#age 9------
lag9dat <- lagged_dat[which(lagged_dat$AGE==9),]

#age 9, no lag - - - - 
lag9.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.0$gam) #
summary(lag9.0$gam) #nonlinear pollock abun
summary(lag9.0$mer) #
AIC_9lag0 <- AIC(lag9.0$mer) #
plot(lag9.0$gam)

#age 9, 1 yr lag - - - - 

lag9.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.1$gam) #
summary(lag9.1$gam) #nonlinear F, pollock abun
summary(lag9.1$mer) #
AIC_9lag1 <- AIC(lag9.1$mer) #
plot(lag9.1$gam)

#age 9, 4 yr lag - - - - 

lag9.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.4$gam) #
summary(lag9.4$gam) #nonlinear pollock abun
summary(lag9.4$mer) #
AIC_9lag4 <- AIC(lag9.4$mer) #
plot(lag9.4$gam)

#age 9, 6 yr lag - - - - 

lag9.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.6$gam) #
summary(lag9.6$gam) #nonlinear not much
summary(lag9.6$mer) #
AIC_9lag6 <- AIC(lag9.6$mer) #
plot(lag9.6$gam)



#age 9, 8 yr lag - - - - 

lag9.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.8$gam) #
summary(lag9.8$gam) #nonlinear F, pollock abun
summary(lag9.8$mer) #
AIC_9lag8 <- AIC(lag9.8$mer) #
plot(lag9.8$gam)

#age 9, 10 yr lag - - - - 

lag9.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.10$gam) #
summary(lag9.10$gam) #nonlinear pollock abun
summary(lag9.10$mer) #
AIC_9lag10 <- AIC(lag9.10$mer) #
plot(lag9.10$gam)

#age 9, no F - - - - 
lag9.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag9dat )
gam.check(lag9.noF$gam) #
summary(lag9.noF$gam) #nonlinear pollock abun
summary(lag9.noF$mer) #
AIC_9lagnoF <- AIC(lag9.noF$mer) #
plot(lag9.noF$gam)

AIC_9lag0
AIC_9lag1
AIC_9lag4
AIC_9lag6
AIC_9lag8
AIC_9lag10 #

AIC_9lagnoF #all very similar





#age 10------
lag10dat <- lagged_dat[which(lagged_dat$AGE==10),]

#age 10, no lag - - - - 
lag10.0 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(mean_annual_F3plus.x, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.0$gam) #
summary(lag10.0$gam) #nonlinear F, pelagic
summary(lag10.0$mer) #
AIC_10lag0 <- AIC(lag10.0$mer) #
plot(lag10.0$gam)

#age 10, 1 yr lag - - - - 

lag10.1 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag1_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.1$gam) #
summary(lag10.1$gam) #nonlinear pelagic maybe
summary(lag10.1$mer) #
AIC_10lag1 <- AIC(lag10.1$mer) #
plot(lag10.1$gam)

#age 10, 4 yr lag - - - - 

lag10.4 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag4_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.4$gam) #
summary(lag10.4$gam) #nonlinear pelagic
summary(lag10.4$mer) #
AIC_10lag4 <- AIC(lag10.4$mer) #
plot(lag10.4$gam)

#age 10, 6 yr lag - - - - 

lag10.6 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag6_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.6$gam) #
summary(lag10.6$gam) #nonlinear pelagic
summary(lag10.6$mer) #
AIC_10lag6 <- AIC(lag10.6$mer) #
plot(lag10.6$gam)



#age 10, 8 yr lag - - - - 

lag10.8 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(lag8_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.8$gam) #
summary(lag10.8$gam) #nonlinear F and pelagic
summary(lag10.8$mer) #
AIC_10lag8 <- AIC(lag10.8$mer) #
plot(lag10.8$gam)

#age 10, 10 yr lag - - - - 

lag10.10 <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                   s(lag10_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                   s(apex_pred_biomass, k=4) + 
                   s(forage_fish_biomass, k=4) + 
                   s(pelagic_forager_biomass, k=4),
                 random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.10$gam) #
summary(lag10.10$gam) #nonlinear pelagic
summary(lag10.10$mer) #
AIC_10lag10 <- AIC(lag10.10$mer) #
plot(lag10.10$gam)

#age 10, no F - - - - 
lag10.noF <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                    #s(pollock_abun_bil_at_age, k=4) +
                    s(apex_pred_biomass, k=4) + 
                    s(forage_fish_biomass, k=4) + 
                    s(pelagic_forager_biomass, k=4),
                  random=~(1|YEAR/HAUL), data=lag10dat )
gam.check(lag10.noF$gam) #
summary(lag10.noF$gam) #nonlinear pelagic
summary(lag10.noF$mer) #
AIC_10lagnoF <- AIC(lag10.noF$mer) #
plot(lag10.noF$gam)

AIC_10lag0
AIC_10lag1
AIC_10lag4
AIC_10lag6
AIC_10lag8
AIC_10lag10 #

AIC_10lagnoF #all about the same







