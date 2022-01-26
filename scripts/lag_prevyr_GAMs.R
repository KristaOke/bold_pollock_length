#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models WITH lags to PREVIOUS year and age

#Created by Krista, Jan 20, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)

#data
wd <- getwd()
lagged2prev <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_prevyr_pollock_length.csv", sep=""), row.names=1)

lagged2prev <- lagged2prev[which(lagged2prev$AGE<11),]
lagged2prev$YEAR <- as.factor(lagged2prev$YEAR)

#GAMs-----

library(mgcv)
library(gamm4)
library(car)

#age 1 doesn't work because no previous age!

#age 2------
lag2pre <- lagged2prev[which(lagged2prev$AGE==2),]

lag2.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag2pre )
gam.check(lag2.p$gam) #not bad
summary(lag2.p$gam) #apex and forage nonlinear
summary(lag2.p$mer) #
AIC_2lagp <- AIC(lag2.p$mer) #
plot(lag2.p$gam)

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 3------
lag3pre <- lagged2prev[which(lagged2prev$AGE==3),]

lag3.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag3pre )
gam.check(lag3.p$gam) #not bad
summary(lag3.p$gam) #sst, julian, forage nonlinear
summary(lag3.p$mer) #
AIC_3lagp <- AIC(lag3.p$mer) #
plot(lag3.p$gam)

#Oh not a ton of variety in F
ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 4------
lag4pre <- lagged2prev[which(lagged2prev$AGE==4),]

lag4.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag4pre )
gam.check(lag4.p$gam) #
summary(lag4.p$gam) #sst, julian, forage nonlinear
summary(lag4.p$mer) #
AIC_4lagp <- AIC(lag4.p$mer) #
plot(lag4.p$gam)

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 5------
lag5pre <- lagged2prev[which(lagged2prev$AGE==5),]

lag5.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag5pre )
gam.check(lag5.p$gam) #fine
summary(lag5.p$gam) #sst, forage nonlinear
summary(lag5.p$mer) #
AIC_5lagp <- AIC(lag5.p$mer) #
plot(lag5.p$gam)

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 6------
lag6pre <- lagged2prev[which(lagged2prev$AGE==6),]

lag6.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag6pre )
gam.check(lag6.p$gam) # fine
summary(lag6.p$gam) #nonlinear sst, julian, prevyr_prevage_F, forage
summary(lag6.p$mer) #
AIC_6lagp <- AIC(lag6.p$mer) #
plot(lag6.p$gam)


ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 7------
lag7pre <- lagged2prev[which(lagged2prev$AGE==7),]

lag7.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag7pre )
gam.check(lag7.p$gam) #
summary(lag7.p$gam) #julian, prevyr_prevage_F, apex, forage nonlinear
summary(lag7.p$mer) #
AIC_7lagp <- AIC(lag7.p$mer) #
plot(lag7.p$gam)

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 8------
lag8pre <- lagged2prev[which(lagged2prev$AGE==8),]

lag8.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag8pre )
gam.check(lag8.p$gam) #fine
summary(lag8.p$gam) #nonlinear julian, prevyr_prevage, pelagic
summary(lag8.p$mer) #
AIC_8lagp <- AIC(lag8.p$mer) #
plot(lag8.p$gam)

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


#age 9------
lag9pre <- lagged2prev[which(lagged2prev$AGE==9),]

lag9.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag9pre )
gam.check(lag9.p$gam) #ok less good
summary(lag9.p$gam) #nonlinear prevyr_prevage, pollock_abun
summary(lag9.p$mer) #
AIC_9lagp <- AIC(lag9.p$mer) #
plot(lag9.p$gam)

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 10------
lag10pre <- lagged2prev[which(lagged2prev$AGE==10),]

lag10.p <- gamm4(LENGTH ~  s(south.sst.amj, k=4) + t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  s(prevyr_prevage_F, k=4) + #s(pollock_abun_bil_at_age, k=4) +
                  s(apex_pred_biomass, k=4) + 
                  s(forage_fish_biomass, k=4) + 
                  s(pelagic_forager_biomass, k=4),
                random=~(1|YEAR/HAUL), data=lag10pre )
gam.check(lag10.p$gam) #fine
summary(lag10.p$gam) #nonlinear pelagic
summary(lag10.p$mer) #
AIC_10lagp <- AIC(lag10.p$mer) #
plot(lag10.p$gam)


ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10pre, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")




