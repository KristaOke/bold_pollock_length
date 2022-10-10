#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models WITH lags to most likely parents

#Created by Krista, Sept, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")

#data
wd <- getwd()
lagged2prev <- read.csv(file=paste(wd,"/data/analysis_ready_lagged_prevyr_pollock_length.csv", sep=""), row.names=1)

lagged2prev <- lagged2prev[which(lagged2prev$AGE<16),]
lagged2prev$YEAR <- as.factor(lagged2prev$YEAR)
lagged2prev$AGE <- as.factor(lagged2prev$AGE)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-178, -155), ylim = c(53, 65), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  # geom_point(aes(LONGITUDE, LATITUDE, colour=mean_station_bottemp), data=all_analysis_dat) +   
  # scale_colour_gradient2(low="blue", high="red", guide="colorbar") + 
  geom_point(aes(LONGITUDE, LATITUDE, 
                 col=as.factor(STRATUM)), data=lagged2prev) + theme_bw() 

#drop stations in northern bering
lagged2prev <- lagged2prev[which(lagged2prev$STRATUM!="81"&
                                   lagged2prev$STRATUM!="71" &
                                   lagged2prev$STRATUM!="70"),]

#----


lagged2prev$cohort <- as.numeric(as.character(lagged2prev$YEAR)) - as.numeric(as.character(lagged2prev$AGE))
lagged2prev$cohort <- as.factor(lagged2prev$cohort)


#running models on data that isn't scaled is looking not good!

lagged2prevsub <- lagged2prev[,c("LATITUDE", "LONGITUDE", "YEAR", "HAUL",
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
                                 "lag10_F", "prevyr_prevage_F", "cohort")]

#mutate is turning cohort and only cohort into a string of NAs?
lagged2prevsub$cohort <- as.character(lagged2prevsub$cohort)

#z-score variables of interest BY AGE

scaledprevtbl <- lagged2prevsub %>% group_by(AGE) %>%
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
         prevyr_prevage_F_scaled = scale(prevyr_prevage_F),
         julian_scaled=scale(julian),
         south.sst.amj.scaled=scale(south.sst.amj))

scaled_prev <- as.data.frame(scaledprevtbl)

scaled_prev$cohort <- as.factor(scaled_prev$cohort)

#----

#scaled_prev$min_parent_yr <- as.numeric(as.character(scaled_prev$cohort)) - 3

#get data
mass_at_age <- read.csv(file=paste(wd,"/data/2021_assessment_table1-17_EBS_pollock_mass_kg_at_age.csv", sep="")) #from survey

mil_at_age <- read.csv(file=paste(wd,"/data/estimated_billions_pollock_at_age_from_assmodel_table1-28_2021assessment.csv", 
                                  sep=""))
p_m <- read.csv(file=paste(wd,"/data/2021-09-30_table_p23_2021assessment_Pmat_M.csv", 
                                  sep=""))

#rotate long
long_mass <- mass_at_age %>%
  pivot_longer(cols = starts_with("age"), names_to = "Age", values_to = "mass_at_age")

long_mass$Age <- gsub("age", "", long_mass$Age)

long_mil <- mil_at_age %>%
  pivot_longer(cols = starts_with("age"), names_to = "Age", values_to = "mil_at_age")

long_mil$Age <- gsub("age", "", long_mil$Age)

#join data by year
mass_mil <- left_join(long_mass, long_mil)

p_m$Age <- as.character(p_m$Age)

spawners_dat <- left_join(mass_mil, p_m)

spawners_dat$N_at_age <- spawners_dat$mil_at_age*1000000

spawners_dat$SPa <- spawners_dat$N_at_age*spawners_dat$Pmat*spawners_dat$mass_at_age
  
  ggplot(spawners_dat, aes(as.numeric(as.character(Age)), SPa)) + geom_point() + facet_wrap(~Year)

#get proportion of total up to age 10 in each age class
  
  yrly_sum <- spawners_dat %>% group_by(Year) %>%
    summarize(annual_sum_N = sum(N_at_age, na.rm=TRUE))
    
  spawners_dat <- left_join(spawners_dat, yrly_sum)

  spawners_dat$prop_N_age <- spawners_dat$N_at_age/spawners_dat$annual_sum_N  

  #repeat with SPa instead of N
  
  yrly_sum_SPa <- spawners_dat %>% group_by(Year) %>%
    summarize(annual_sum_SPa = sum(SPa, na.rm=TRUE))
  
  spawners_dat <- left_join(spawners_dat, yrly_sum_SPa)
  
  spawners_dat$prop_SPa_age <- spawners_dat$SPa/spawners_dat$annual_sum_SPa  
  
  ggplot(spawners_dat, aes(as.numeric(as.character(Age)), prop_SPa_age)) + geom_point() + facet_wrap(~Year)
  
  
#retain age classes with proportion of SPa GREATER THAN 0.10
  
  ages_10percent_SPa <- spawners_dat[which(spawners_dat$prop_SPa_age > 0.10),]
  
  ggplot(ages_10percent_SPa, aes(as.numeric(as.character(Age)), prop_SPa_age)) + geom_point() + facet_wrap(~Year)

  #let's colour the cohorts and look at how many years they are likely to contribute
  #colours don't mean anything just want nice contrast to track cohorts visually

  ages_10percent_SPa$cohort <- as.numeric(as.character(ages_10percent_SPa$Year)) - 
    as.numeric(as.character(ages_10percent_SPa$Age))  
  ages_10percent_SPa$cohort <- as.factor(ages_10percent_SPa$cohort)
  
  ggplot(ages_10percent_SPa, aes(as.numeric(as.character(Age)), Year, col=as.factor(cohort))) + geom_point() +
    scale_colour_manual(values = c("red", "blue", "green", "black", "pink", "brown", "yellow", "grey", "orange",
                                   "dark blue", "dark red", "dark grey", "dark green", #repeat
                                   "red", "blue", "green", "black", "pink", "brown", "yellow", "grey", "orange",
                                   "dark blue", "dark red", "dark grey", "dark green", #repeat
                                   "red", "blue", "green", "black", "pink", "brown", "yellow", "grey", "orange",
                                   "dark blue", "dark red", "dark grey", "dark green"))
  
#now we know who likely parents are, need to link to fishing on those years/cohorts
  
  #fishing data
  
  wd <- getwd()
  Fdatlong <- read.csv(file=paste(wd,"/data/Fdatlong.csv", sep=""), row.names=1)
  
  #get cohort from Fdat
  Fdatlong$cohort <- Fdatlong$year - Fdatlong$age
  
#look at Fs cohorts were exposed to?
  ggplot(Fdatlong, aes(year, F)) + geom_point() + facet_wrap(~cohort)
  

#ok we have F for years/cohorts - now need a weighted avg  
  
  ages_10percent_SPa$Age <- as.numeric(as.character(ages_10percent_SPa$Age))
  ages_10percent_SPa$cohort <- as.numeric(as.character(ages_10percent_SPa$cohort))
  
  #what we need is 
  #for each cohort
  #F for years LESS THAN EQUAL TO AGE
  #BUT only ages 3+ (?)
  #think we want MEAN of above
  #then weight mean of mean F for each cohort weighted by proportion SPa?
  
  #seems like a loop is needed
  
  ages_10percent_SPa$mean_to_date_cohort_mature_F <- NA
  
  i<-1
  for(i in 1:length(ages_10percent_SPa$Year)){ #for each line of ages_10percent_SPa figure out previous Fs above age 2
    temp_cohort <- ages_10percent_SPa$cohort[i]
    temp_age <- ages_10percent_SPa$Age[i]
    cohort_Fdat <- Fdatlong[which(Fdatlong$cohort==temp_cohort),]
    temp_window <- cohort_Fdat[which(cohort_Fdat$age>2 & cohort_Fdat$age<=temp_age),]
    mean_to_date_mature_F <- mean(temp_window$F, na.rm=TRUE)
    ages_10percent_SPa$mean_to_date_cohort_mature_F[i] <- mean_to_date_mature_F
  }
  #know we know what F they've experienced, so let's get weighted avg of those Fs based on propotions
  
  #join first then
 mean_parent_Fs <- ages_10percent_SPa %>% group_by(Year) %>%
   summarize(weighted_parent_mean_F  = weighted.mean(x=mean_to_date_cohort_mature_F, w=prop_SPa_age, na.rm=TRUE))
 #ok who to group by here is very important
 #currently because I'm grouping by year (here and above) I will have the mean F the parents present in a year
 #have experienced - that still needs to be linked back to the offspring
   
  ggplot(mean_parent_Fs, aes(Year, weighted_parent_mean_F)) + geom_point() + geom_line() #gut check this
  
  ggplot(mean_parent_Fs, aes(as.numeric(as.character(Year)), weighted_parent_mean_F)) + geom_point() + 
    geom_line() + xlab("Year") + ylab("Weighted mean parent F")#gut check this
  
  
  #then link offspring cohort to year???? ie 2000 parent weighted F linked to fish born in 2000
  
   
  mean_parent_Fs$Year <- as.factor(mean_parent_Fs$Year)
  
  #join COHORT in scaled data to YEAR in mean parent Fs 
  
  datwparents <- left_join(scaled_prev, mean_parent_Fs, by=c("cohort"="Year"))
  
  #z-score variables of interest BY AGE
  
  partbl <- datwparents %>% group_by(AGE) %>%
    mutate(mean_weight_parentF_scaled=scale(weighted_parent_mean_F))
  
  datwparents <- as.data.frame(partbl)
  
  wd <- getwd()
  write.csv(datwparents, file=paste(wd,"/data/analysis_ready_data_w_mean_parent_Fs.csv", sep=""))
  
  
  