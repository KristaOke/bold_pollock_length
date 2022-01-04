#========================================================================================================
# Load explore covariate data
#
# By Krista, Nov2021
#========================================================================================================
#Notes:
#========================================================================================================

library(tidyverse)
library(ggplot2)

#load fisheries mort data calculated from estimated N and catch from EBS assessment (2020???)
#this will need to be updated with real numbers!
Fdat <- read.csv("./data/calc_F_from_N_and_C.csv")
View(Fdat)

#wide format need to rotate to long for analysis and/or plotting

#ok this is awkward but seems to be working fine and can't figure out another way
piv1_9 <- Fdat %>%
  pivot_longer(!year, names_to = c(".value", "age"),
               names_pattern = "(.)(.)")

piv10_15 <- Fdat %>%
  pivot_longer(!year, names_to = c(".value", "age"),
               names_pattern = "(.)(..)")

Fdatlong <- rbind(piv1_9, piv10_15)
Fdatlong <- na.omit(Fdatlong)

#calc overall F in year-----

Foverall <- Fdatlong %>% group_by(year) %>%
  summarize(mean_annual_C=mean(C, na.rm=TRUE), mean_annual_N=mean(N, na.rm=TRUE))

Foverall$mean_annual_F <- Foverall$mean_annual_C/Foverall$mean_annual_N

ggplot(Foverall, aes(year, mean_annual_F)) + geom_point() + geom_line()


#calc 3plus overall F in year-----

F3plus <- Fdatlong[which(Fdatlong$age>2),] %>% group_by(year) %>%
  summarize(mean_annual_C3plus=mean(C, na.rm=TRUE), mean_annual_N3plus=mean(N, na.rm=TRUE))

F3plus$mean_annual_F3plus <- F3plus$mean_annual_C3plus/F3plus$mean_annual_N3plus

ggplot(F3plus, aes(year, mean_annual_F3plus)) + geom_point() + geom_line()

F3sub <- F3plus[,-c(2:3)]

#climate data from previous project====

climdat <- read.csv("./data/climate data.csv")
names(climdat)

ggplot(climdat, aes(year, south.sst.ndjfm)) + geom_point()


#ecosystem indicator data===============

#this data is all downloaded from EBS ESR report cards available online at 
# https://apps-afsc.fisheries.noaa.gov/refm/reem/ecoweb/index.php?ID=1
#contacts are in second row of original data which is skipped here for
#compatibility with R

#pelagic forager biomass (fish 1000t)
pel_dat <- read.csv("./data/Pelagic.csv", skip=4, header=TRUE, na.strings="null")
View(pel_dat)
colnames(pel_dat)[colnames(pel_dat) == 'Value'] <- 'pelagic_forager_biomass'

#benthic forager biomass (fish 1000t)
bnth_dat <- read.csv("./data/Benthic.csv", skip=4, header=TRUE, na.strings="null")
View(bnth_dat)
colnames(bnth_dat)[colnames(bnth_dat) == 'Value'] <- 'benthic_forager_biomass'

#Aggregate Forage Fish Biomass
forg_dat <- read.csv("./data/Forage.csv", skip=4, header=TRUE, na.strings="null")
View(forg_dat)
colnames(forg_dat)[colnames(forg_dat) == 'Value'] <- 'forage_fish_biomass'

# Motile epifauna biomass (fish and inverts 1000t)
epif_dat <- read.csv("./data/Epifauna.csv", skip=4, header=TRUE, na.strings="null")
View(epif_dat)
colnames(epif_dat)[colnames(epif_dat) == 'Value'] <- 'epifauna_biomass'


#Apex predator biomass (fish 1000t)
#THIS INCLUDES P COD AND ARROWTOOTH
apex_dat <- read.csv("./data/Apex.csv", skip=4, header=TRUE, na.strings="null")
View(apex_dat)
colnames(apex_dat)[colnames(apex_dat) == 'Value'] <- 'apex_pred_biomass'


#Euphausiid density (no. m3), 2004-2020
euph_dat <- read.csv("./data/Euphs.csv", skip=4, header=TRUE, na.strings="null")
View(euph_dat)
colnames(euph_dat)[colnames(euph_dat) == 'Value'] <- 'euph_density'

#and esr clim covas

#Cold pool extent
coldpool_dat <- read.csv("./data/ColdPool.csv", skip=4, header=TRUE, na.strings="null")
View(coldpool_dat)
colnames(coldpool_dat)[colnames(coldpool_dat) == 'Value'] <- 'cold_pool_extent'

#Cold pool extent
coldpool_dat <- read.csv("./data/ColdPool.csv", skip=4, header=TRUE, na.strings="null")
View(coldpool_dat)
colnames(coldpool_dat)[colnames(coldpool_dat) == 'Value'] <- 'cold_pool_extent'

#North Pacific Index (Nov-Mar average)
npi_dat <- read.csv("./data/NPI.csv", skip=4, header=TRUE, na.strings="null")
View(npi_dat)
colnames(npi_dat)[colnames(npi_dat) == 'Index'] <- 'NPI'

#Sea ice extent
seaice_dat <- read.csv("./data/IceExtent.csv", skip=4, header=TRUE, na.strings="null")
View(seaice_dat)
colnames(seaice_dat)[colnames(seaice_dat) == 'Index'] <- 'sea_ice_extent'

esr_dat <- left_join(pel_dat, bnth_dat, by='Year') %>%
  left_join(., forg_dat, by='Year') %>%
  left_join(., epif_dat, by='Year') %>%
  left_join(., apex_dat, by='Year') %>%
  left_join(., euph_dat, by='Year') %>%
left_join(., coldpool_dat, by='Year') %>%
  left_join(., npi_dat, by='Year') %>%
  left_join(., seaice_dat, by='Year') 

View(esr_dat)

#predator abundance data=================

arrowtooth_dat <- read.csv("./data/est_arrowtooth_biomass_2020assessment_tab6-13.csv")
View(arrowtooth_dat)
names(arrowtooth_dat)
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'Total_biomass'] <- 
  'total_arrowtooth_biomass'
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'Biomass_lower_CI'] <- 
  'arrowtooth_biomass_lower_CI'
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'Biomass_upper_CI'] <- 
  'arrowtooth_biomass_upper_CI'
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'FSB'] <- 
  'arrowtooth_FSB'
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'Lower_CI'] <- 
  'arrowtooth_lower_CI'
colnames(arrowtooth_dat)[colnames(arrowtooth_dat) == 'Upper_CI'] <- 
  'arrowtooth_upper_CI'

pcod_dat <- read.csv("./data/estimated_biomass_Pcod_tab2-36_2020assessment_ensbAB.csv")
View(pcod_dat)
names(pcod_dat)
colnames(pcod_dat)[colnames(pcod_dat) == 'age0plus'] <- 
  'age0plus_pcod'
colnames(pcod_dat)[colnames(pcod_dat) == 'spawn'] <- 
  'spawn_pcod'
colnames(pcod_dat)[colnames(pcod_dat) == 'SB_SD'] <- 
  'SB_SD_pcod'


#pollock abundance data========

polabun_dat <- read.csv("./data/estimated_billions_pollock_at_age_from_assmodel_table1-28_2021assessment.csv")
View(polabun_dat)

colnames(polabun_dat)[colnames(polabun_dat) == 'age1'] <- 
  'age1pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age2'] <- 
  'age2pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age3'] <- 
  'age3pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age4'] <- 
  'age4pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age5'] <- 
  'age5pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age6'] <- 
  'age6pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age7'] <- 
  'age7pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age8'] <- 
  'age8pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age9'] <- 
  'age9pollock_abund'
colnames(polabun_dat)[colnames(polabun_dat) == 'age10plus'] <- 
  'age10pluspollock_abund'

#3-plus from 2020 assessment

pol3plus_dat <- read.csv("./data/estimated_3plus_biomass_2020assessment_tab28.csv")
View(pol3plus_dat)

#joins======

#join to pollock data loaded in load_explore_data.R

Fdatlong$age <- as.integer(Fdatlong$age)

mean_wF <- left_join(yrly_means2, Fdatlong, by=c("YEAR"="year", "AGE"="age"))

ggplot(mean_wF[which(mean_wF$AGE<16),], aes(F, mean_annual_size_global, col=as.factor(YEAR))) +
 geom_point() + 
  facet_wrap(~AGE, scales="free")


ggplot(mean_wF[which(mean_wF$AGE<16),], aes(YEAR,  F)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF[which(mean_wF$AGE<16),], aes(YEAR, mean_annual_size_global)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

mean_wF_esr <- left_join(mean_wF, esr_dat, by=c("YEAR"="Year"))

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, pelagic_forager_biomass)) +
  geom_point()

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, benthic_forager_biomass)) +
  geom_point()

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, forage_fish_biomass)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, epifauna_biomass)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, apex_pred_biomass)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, euph_density)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, cold_pool_extent)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, NPI)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, sea_ice_extent)) +
  geom_point() 

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(pelagic_forager_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(benthic_forager_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(forage_fish_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(epifauna_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(apex_pred_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(euph_density, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(cold_pool_extent, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(NPI, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(sea_ice_extent, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

#preds data join

preds_dat <- left_join(arrowtooth_dat, pcod_dat, by="year")

mean_all <- left_join(mean_wF_esr, preds_dat, by=c("YEAR"="year"))

ggplot(mean_all[which(mean_all$AGE<16),], aes(YEAR, total_arrowtooth_biomass)) +
  geom_point() 

ggplot(mean_all[which(mean_all$AGE<16),], aes(YEAR, age0plus_pcod)) +
  geom_point() 


ggplot(mean_all[which(mean_all$AGE<16),], aes(total_arrowtooth_biomass, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_all[which(mean_all$AGE<16),], aes(age0plus_pcod, mean_annual_size_global, col=as.factor(YEAR))) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

#add in pollock data

#mean_all <- left_join(mean_all, polabun_dat, by=c("YEAR"="Year"))
#this joins all age abundances to each row, would it be better
#to join by age? Would need to pivot polabun_dat long and add age

pivp <- polabun_dat %>%
  pivot_longer(!Year)
   
pivp$age <- NA
pivp$age[which(pivp$name=="age1pollock_abund")] <- "1"
pivp$age[which(pivp$name=="age2pollock_abund")] <- "2"
pivp$age[which(pivp$name=="age3pollock_abund")] <- "3"
pivp$age[which(pivp$name=="age4pollock_abund")] <- "4"
pivp$age[which(pivp$name=="age5pollock_abund")] <- "5"
pivp$age[which(pivp$name=="age6pollock_abund")] <- "6"
pivp$age[which(pivp$name=="age7pollock_abund")] <- "7"
pivp$age[which(pivp$name=="age8pollock_abund")] <- "8"
pivp$age[which(pivp$name=="age9pollock_abund")] <- "9"
pivp$age[which(pivp$name=="age10pluspollock_abund")] <- "10plus"
pivp$age <- as.factor(pivp$age)
            
colnames(pivp)[colnames(pivp) == 'value'] <- 
  'pollock_abun_bil_at_age'              

pivp <- pivp[,-2]

mean_all$AGE <- as.factor(as.character(mean_all$AGE))

mean_all <- left_join(mean_all, pivp, by=c("YEAR"="Year", "AGE"="age"))

mean_all <- left_join(mean_all, pol3plus_dat, by=c("YEAR"="Year"))



#join to clim data

analysis_df <- left_join(mean_all, climdat, by=c("YEAR"="year"))


#check collinearity======
library(corrplot)

#look at just covars (NOT by age)
#this EXCLUDES F data which is only by age
covars <- left_join(climdat, preds_dat) %>%
  left_join(., esr_dat, by=c('year'= 'Year')) %>%
  left_join(., pol3plus_dat, by=c('year'= 'Year')) 


names(covars)
pairs(covars[,c(2:25)]) #climate covars
cormat2 <- cor(covars[,c(2:25)], use="complete.obs")
corrplot.mixed(cormat2, upper = 'ellipse',lower='number')
#lots high, what about only those of interest?

cormat3 <- cor(covars[,c(3, 41:43)], use="complete.obs")
corrplot.mixed(cormat3, upper = 'ellipse',lower='number')
#south.sst.amj, CPI, sea ice extent all very strongly cor (>.74)
#NPI not cor with much
#Use SST, not CPI or sea ice

#nonclim covars
pairs(covars[,c(26, 32:33, 35:40, 44)]) #nonclimate covars
cormat1 <- cor(covars[,c(26, 32:33, 35:40, 44)], use="complete.obs")
corrplot.mixed(cormat1, upper = 'ellipse',lower='number')
#wow some quite high correlations!

#drop euphs, short and cor w nearly everything
cormat4 <- cor(covars[,c(26, 32:33, 35:39, 44)], use="complete.obs")
corrplot.mixed(cormat4, upper = 'ellipse',lower='number')
pairs(covars[,c(26, 32:33, 35:39, 44)])

#forage fish is next shortest, are close calls still close if it is dropped?
cormat5 <- cor(covars[,c(26, 32:33, 35:36, 38:39, 44)], use="complete.obs")
corrplot.mixed(cormat5, upper = 'ellipse',lower='number')

#drop age0plus pcod and arrowtooth, add back in clim
cormat6 <- cor(covars[,c(3, 33, 35:36, 38:39, 44)], use="complete.obs")
corrplot.mixed(cormat6, upper = 'ellipse',lower='number')

#add forage fish back in
cormat7 <- cor(covars[,c(3, 33, 35:39, 44)], use="complete.obs")
corrplot.mixed(cormat7, upper = 'ellipse',lower='number')

#with abund at age?
cormat8 <- cor(analysis_df[,c(8:12, 24, 26, 30)], use="complete.obs")
corrplot.mixed(cormat8, upper = 'ellipse',lower='number')
#looks fine except already identified issues

#join with F3sub without messing up indexing above
analysis_df <- left_join(analysis_df, F3sub, by=c("YEAR"="year"))


wd <- getwd()
write.csv(analysis_df, file=paste(wd,"/data/analysis_ready_data_pollock_length.csv", sep=""))
