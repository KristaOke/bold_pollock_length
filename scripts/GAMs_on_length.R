#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#GAMs on length

#Created by: Krista, Jan 3, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes:
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)

#get data saved in load_explore_covar_data
analysis_dat <- read.csv(file=paste(wd,"/data/analysis_ready_data_pollock_length.csv", sep=""), row.names=1)

analysis_dat <- analysis_dat[which(analysis_dat$AGE<11),]

ggplot(analysis_dat, aes(YEAR, mean_annual_size_global)) + 
  geom_point() + facet_wrap(~AGE, scales="free") + geom_smooth()

#let's look again at the covariates we selected

ggplot(analysis_dat, aes(YEAR, pelagic_forager_biomass)) +
  geom_point() + geom_smooth()

ggplot(analysis_dat, aes(pelagic_forager_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")



ggplot(analysis_dat, aes(YEAR, benthic_forager_biomass)) +
  geom_point()+ geom_smooth()

ggplot(analysis_dat, aes(benthic_forager_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(analysis_dat, aes(YEAR, forage_fish_biomass)) +
  geom_point()+ geom_smooth()

ggplot(analysis_dat, aes(forage_fish_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(analysis_dat, aes(YEAR, epifauna_biomass)) +
  geom_point()+ geom_smooth()

ggplot(analysis_dat, aes(epifauna_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")



ggplot(analysis_dat, aes(YEAR, apex_pred_biomass)) +
  geom_point()+ geom_smooth()

ggplot(analysis_dat, aes(apex_pred_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(analysis_dat, aes(YEAR, pollock_abun_bil_at_age)) +
  geom_point() + facet_wrap(~AGE, scales="free")+ geom_smooth()

ggplot(analysis_dat, aes(pollock_abun_bil_at_age, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(analysis_dat, aes(YEAR, south.sst.amj)) +
  geom_point()+ geom_smooth()

ggplot(analysis_dat, aes(south.sst.amj, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(analysis_dat, aes(YEAR, mean_annual_F3plus)) +
  geom_point()+ geom_smooth() +facet_wrap(~AGE)

ggplot(analysis_dat, aes(mean_annual_F3plus, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(analysis_dat, aes(south.sst.amj, mean_annual_F3plus, col=as.factor(YEAR))) + 
  geom_point() + facet_wrap(~AGE, scales = "free")


#GAMs-----

library(mgcv)
library(gamm4)
library(car)

#age 1------
age1dat <- analysis_dat[which(analysis_dat$AGE==1),]
mod1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
             s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age1dat)
gam.check(mod1)
plot(mod1) #all look very linear EXCEPT forage fish, pelagic forager
summary(mod1)

mod1.1 <- gam(mean_annual_size_global ~ south.sst.amj*mean_annual_F3plus + south.sst.amj*pollock_abun_bil_at_age +
              apex_pred_biomass + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age1dat)
gam.check(mod1.1)
plot(mod1.1) 
summary(mod1.1)
Anova(mod1.1, type="III")
anova(mod1.1) #nothing significant?

mod1.2 <- gam(mean_annual_size_global ~ south.sst.amj*mean_annual_F3plus + pollock_abun_bil_at_age +
                apex_pred_biomass + 
                s(forage_fish_biomass, k=4) + 
                s(pelagic_forager_biomass, k=4), 
              data=age1dat)
gam.check(mod1.2)
plot(mod1.2) 
summary(mod1.2)
Anova(mod1.2, type="III")
anova(mod1.2)


mod1.3 <- gam(mean_annual_size_global ~ south.sst.amj + mean_annual_F3plus + pollock_abun_bil_at_age +
                apex_pred_biomass + 
                s(forage_fish_biomass, k=4) + 
                s(pelagic_forager_biomass, k=4), 
              data=age1dat)
gam.check(mod1.3)
plot(mod1.3) 
summary(mod1.3)
Anova(mod1.3, type="III")
anova(mod1.3)

#age 2------
age2dat <- analysis_dat[which(analysis_dat$AGE==2),]
mod2 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + s(mean_annual_F3plus)+ s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age2dat)
gam.check(mod2) #residuals not great but not super horrific
plot(mod2) #forage fish, F3plus, and apex nonlinear, likely also pollock abun
summary(mod2)

mod2.1 <- gam(mean_annual_size_global ~ south.sst.amj + s(mean_annual_F3plus)+ pollock_abun_bil_at_age +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age2dat)
gam.check(mod2.1) #
plot(mod2.1) #
summary(mod2.1)

mod2.x <- gam(mean_annual_size_global ~ south.sst.amj + t2(mean_annual_F3plus, south.sst.amj), 
              data=age2dat)
gam.check(mod2.x) #
plot(mod2.x) #
summary(mod2.x)
library(gratia)
draw(mod2.x, select = 1)

#age 3------
age3dat <- analysis_dat[which(analysis_dat$AGE==3),]
mod3 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age3dat)
gam.check(mod3) #response v fitted a little odd
plot(mod3) #only F3plus seems nonlinear but looks a little over fit, trying w k=4
summary(mod3)

mod3.1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + s(mean_annual_F3plus, k=4) + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              forage_fish_biomass + 
              pelagic_forager_biomass, 
            data=age3dat)
gam.check(mod3.1) #response v fitted a little odd
plot(mod3.1) #
summary(mod3.1)

mod3.x <- gam(mean_annual_size_global ~ south.sst.amj + s(mean_annual_F3plus, south.sst.amj), 
            data=age3dat)
gam.check(mod3.x) #response v fitted a little odd
plot(mod3.x) #
summary(mod3.x)

draw(mod3.x, select=1)

#age 4------
age4dat <- analysis_dat[which(analysis_dat$AGE==4),]
mod4 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age4dat)
gam.check(mod4) #
plot(mod4) #
summary(mod4) #nonlinear: sst, f3plus, forage

mod4.1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus, k=4) + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              s(forage_fish_biomass, k=4) + 
              pelagic_forager_biomass, 
            data=age4dat)
gam.check(mod4.1) #
plot(mod4.1) #
summary(mod4.1)

mod4.x <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus, south.sst.amj), 
              data=age4dat)
gam.check(mod4.x) #
plot(mod4.x) #
summary(mod4.x)

draw(mod4.x, select=2)

#age 5------
age5dat <- analysis_dat[which(analysis_dat$AGE==5),]
mod5 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age5dat)
gam.check(mod5) #residuals not amazing
plot(mod5) #
summary(mod5)

mod5.1 <- gam(mean_annual_size_global ~ south.sst.amj + s(mean_annual_F3plus) + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              s(forage_fish_biomass, k=4) + 
              pelagic_forager_biomass, 
            data=age5dat)
gam.check(mod5.1) #
plot(mod5.1) #
summary(mod5.1)


#age 6------
age6dat <- analysis_dat[which(analysis_dat$AGE==6),]
mod6 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age6dat)
gam.check(mod6)
plot(mod6) #
summary(mod6)

mod6.1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              s(forage_fish_biomass, k=4) + 
             pelagic_forager_biomass, 
            data=age6dat)
gam.check(mod6.1)
plot(mod6.1) #
summary(mod6.1)




#age 7------
age7dat <- analysis_dat[which(analysis_dat$AGE==7),]
mod7 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus, k=4) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) +
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age7dat)
gam.check(mod7)
plot(mod7) #
summary(mod7)

mod7.1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + mean_annual_F3plus + pollock_abun_bil_at_age +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              pelagic_forager_biomass, 
            data=age7dat)
gam.check(mod7.1)
plot(mod7.1) #
summary(mod7.1)




#age 8------
age8dat <- analysis_dat[which(analysis_dat$AGE==8),]
mod8 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age8dat)
gam.check(mod8)
plot(mod8) #none very nonlinear
summary(mod8) 

mod8.1 <- gam(mean_annual_size_global ~ south.sst.amj + mean_annual_F3plus + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              forage_fish_biomass + 
              pelagic_forager_biomass, 
            data=age8dat)
#gam.check(mod8.1)
#plot(mod8.1) #none very nonlinear
summary(mod8.1) 



#age 9------
age9dat <- analysis_dat[which(analysis_dat$AGE==9),]
mod9 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age9dat)
gam.check(mod9) #residuals not great
plot(mod9) #none are nonlinear at all
summary(mod9)

mod9.1 <- lm(mean_annual_size_global ~ south.sst.amj + mean_annual_F3plus + pollock_abun_bil_at_age +
              apex_pred_biomass + 
              forage_fish_biomass + 
              pelagic_forager_biomass, 
            data=age9dat)
# gam.check(mod9) #residuals not great
# plot(mod9) #none are nonlinear at all
summary(mod9.1)




#age 10------
age10dat <- analysis_dat[which(analysis_dat$AGE==10),]
mod10 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(mean_annual_F3plus) + #s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + 
              s(forage_fish_biomass, k=4) + 
              s(pelagic_forager_biomass, k=4), 
            data=age10dat)
gam.check(mod10) #residuals are super bad
plot(mod10) 
summary(mod10)

mod10.1 <- gam(mean_annual_size_global ~ south.sst.amj + s(mean_annual_F3plus) + #s(pollock_abun_bil_at_age, k=4) +
               apex_pred_biomass + 
               s(forage_fish_biomass, k=4) + 
               s(pelagic_forager_biomass, k=4), 
             data=age10dat)
gam.check(mod10.1) #residuals are super bad
plot(mod10.1) 
summary(mod10)



