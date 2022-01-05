#========================================================================================================
# Load explore data
#
# By Krista, Nov2021
#========================================================================================================
#Notes:
#========================================================================================================

library(gratia)
library(tidyverse)
library(mgcv)
library(lubridate)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")


#========================================================================================================
#read in data
dat2 <- read.csv(("./data/survey data/pollock_survey_specimen_data_confirmation.csv"))


dat2$CRUISE2 <- dat2$CRUISE
dat2 <- dat2 %>% separate(CRUISE2, c("YEAR", "Cruisenum"), sep=4)
dat2$YEAR <- as.numeric(dat2$YEAR)


ggplot(dat2, aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")

#means
yrly_means2 <- dat2 %>% group_by(YEAR, AGE) %>% summarise(mean_annual_size_global=mean(LENGTH, na.rm=TRUE), n=n())

ggplot(dat2, aes(LENGTH, WEIGHT, col=as.factor(AGE))) + geom_point() + facet_wrap(~YEAR)

#look more at indiv level data

indiv_dat <- dat2[which(dat2$AGE<11),]
  
  
  ggplot(indiv_dat, aes(YEAR, LENGTH)) + 
  geom_point()+  geom_smooth() + facet_wrap(~as.factor(AGE), scales = "free")

  ggplot(indiv_dat, aes(YEAR, LENGTH)) + 
    geom_violin(aes(group=YEAR)) + facet_wrap(~as.factor(AGE), scales = "free")
  
  ggplot(indiv_dat[which(indiv_dat$AGE==1),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==2),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==3),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==4),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==5),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==6),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==7),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==8),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==9),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
  ggplot(indiv_dat[which(indiv_dat$AGE==10),], aes(YEAR, LENGTH, col=as.factor(SEX))) + 
    geom_violin(aes(group=YEAR)) 
table(indiv_dat$SEX, indiv_dat$YEAR)  

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
 # annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE), data=indiv_dat[which(indiv_dat$YEAR<1990),]) +  facet_wrap(~YEAR) 
  #things are very sparse in the 70s and 80s don't look like a grid either

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE), data=indiv_dat[which(indiv_dat$YEAR>1990),]) +  facet_wrap(~YEAR) 
#samples very pick up ~2006

#coloured by length
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, col=LENGTH), data=indiv_dat[which(indiv_dat$YEAR<1990),]) +  facet_wrap(~YEAR) 

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-180, -155), ylim = c(53, 65), expand = TRUE) +
  # annotation_scale(location = "bl", width_hint = 0.5) +
  geom_point(aes(LONGITUDE, LATITUDE, col=LENGTH), data=indiv_dat[which(indiv_dat$YEAR>1990),]) +  facet_wrap(~YEAR) 


ggplot(indiv_dat, aes(LENGTH)) + geom_histogram() + facet_wrap(~YEAR)

table(indiv_dat$YEAR, indiv_dat$AGE) #proportion at age seems to change over time, likely good we model at age
  