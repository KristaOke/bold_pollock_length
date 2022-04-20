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


#data

lagged_dat <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_data_pollock_length.csv", sep=""), row.names=1)

lagged_dat <- lagged_dat[which(lagged_dat$AGE<11),]
lagged_dat$YEAR <- as.factor(lagged_dat$YEAR)


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


#GAMs-----

library(mgcv)
library(gamm4)
library(car)
