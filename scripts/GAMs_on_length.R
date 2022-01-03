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

ggplot(analysis_dat, aes(YEAR, F)) +
  geom_point()+ geom_smooth() +facet_wrap(~AGE)

ggplot(analysis_dat, aes(F, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

#GAMs-----

library(mgcv)
library(gamm4)
library(car)

#age 1------
age1dat <- analysis_dat[which(analysis_dat$AGE==1),]
mod1 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + s(F) + s(pollock_abun_bil_at_age, k=4) +
             s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age1dat)
gam.check(mod1)
plot(mod1) #all look very linear EXCEPT forage fish, pelagic forager
summary(mod1)

mod1.1 <- gam(mean_annual_size_global ~ south.sst.amj*F + south.sst.amj*pollock_abun_bil_at_age +
              apex_pred_biomass + epifauna_biomass +
              s(forage_fish_biomass, k=4) + benthic_forager_biomass +
              s(pelagic_forager_biomass, k=4), 
            data=age1dat)
gam.check(mod1.1)
plot(mod1.1) 
summary(mod1.1)
Anova(mod1.1, type="III")
anova(mod1.1)

mod1.2 <- gam(mean_annual_size_global ~ south.sst.amj*F + pollock_abun_bil_at_age +
                apex_pred_biomass + epifauna_biomass +
                s(forage_fish_biomass, k=4) + benthic_forager_biomass +
                s(pelagic_forager_biomass, k=4), 
              data=age1dat)
gam.check(mod1.2)
plot(mod1.2) 
summary(mod1.2)
Anova(mod1.2, type="III")
anova(mod1.2)


#age 2------
age2dat <- analysis_dat[which(analysis_dat$AGE==2),]
mod2 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4) + s(F)+ s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age2dat)
gam.check(mod2) #residuals not great!!
plot(mod2) #forage fish and pelagic nonlinear, likely also pollock abun
summary(mod2)



#age 3------
age3dat <- analysis_dat[which(analysis_dat$AGE==3),]
mod3 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age3dat)
gam.check(mod3) #response v fitted a little odd
plot(mod3) #all except sst seem nonlinear
summary(mod3)




#age 4------
age4dat <- analysis_dat[which(analysis_dat$AGE==4),]
mod4 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age4dat)
gam.check(mod4) #residuals look not great, qq too
plot(mod4) #
summary(mod4) #only linear are pelagic and epifauna




#age 5------
age5dat <- analysis_dat[which(analysis_dat$AGE==5),]
mod5 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age5dat)
gam.check(mod5) #residuals not amazing
plot(mod5) #
summary(mod5)




#age 6------
age6dat <- analysis_dat[which(analysis_dat$AGE==6),]
mod6 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age6dat)
gam.check(mod6)
plot(mod6) #sst, apex, epifauna are nonlinear
summary(mod6)




#age 7------
age7dat <- analysis_dat[which(analysis_dat$AGE==7),]
mod7 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age7dat)
gam.check(mod7)
plot(mod7) #none very nonlinear
summary(mod7)




#age 8------
age8dat <- analysis_dat[which(analysis_dat$AGE==8),]
mod8 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age8dat)
gam.check(mod8)
plot(mod8) #none very nonlinear
summary(mod8) 




#age 9------
age9dat <- analysis_dat[which(analysis_dat$AGE==9),]
mod9 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age9dat)
gam.check(mod9) #residuals not great
plot(mod9) #none are nonlinear at all
summary(mod9)






#age 10------
age10dat <- analysis_dat[which(analysis_dat$AGE==10),]
mod10 <- gam(mean_annual_size_global ~ s(south.sst.amj, k=4)+ s(F) + #s(pollock_abun_bil_at_age, k=4) +
              s(apex_pred_biomass, k=4) + s(epifauna_biomass, k=4) +
              s(forage_fish_biomass, k=4) + s(benthic_forager_biomass, k=4)+
              s(pelagic_forager_biomass, k=4), 
            data=age10dat)
gam.check(mod10)
plot(mod10) #only epifauna very nonlinear
summary(mod10)





