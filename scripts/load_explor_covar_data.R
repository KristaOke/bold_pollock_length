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

esr_dat <- left_join(pel_dat, bnth_dat, by='Year') %>%
  left_join(., forg_dat, by='Year') %>%
  left_join(., epif_dat, by='Year') %>%
  left_join(., apex_dat, by='Year') %>%
  left_join(., euph_dat, by='Year') 

View(esr_dat)

#predator abundance data=================

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
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, benthic_forager_biomass)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, forage_fish_biomass)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, epifauna_biomass)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, apex_pred_biomass)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")

ggplot(mean_wF_esr[which(mean_wF_esr$AGE<16),], aes(YEAR, euph_biomass)) +
  geom_point() + 
  facet_wrap(~AGE, scales="free")
