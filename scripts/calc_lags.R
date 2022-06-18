#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Calculate lags in fishing

#Created by Krista, Jan 5, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: need to lag the overall fishing rate  so models can include recent/realtime fishing (removal) vs 
#fishing on parent generation (evol)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#let's start w same data I am using for indiv lvl models but w different name so nothing gets messed up

dat2lag <- read.csv(file=paste(wd,"/data/analysis_ready_individual_data_pollock_length.csv", sep=""), row.names=1)

#dat2lag <- dat2lag[which(dat2lag$AGE<11),]
#indiv_lvl_dat$YEAR <- as.factor(indiv_lvl_dat$YEAR)
#DON'T make year a factor we need to do some math

#also using F3sub which is loaded in load_explor_covar_data.R
View(F3sub)

F2lag <- F3sub



#think a loop will be required here
#lag is to previous year, but global, ie not specific to age or regions etc
#little funky but seems to do the trick

F2lag$lag1_F <-NA
F2lag$lag2_F <-NA
F2lag$lag3_F <-NA
F2lag$lag4_F <-NA
F2lag$lag5_F <-NA
F2lag$lag6_F <-NA
F2lag$lag7_F <-NA
F2lag$lag8_F <-NA
F2lag$lag9_F <-NA
F2lag$lag10_F <-NA

yrs <- unique(F2lag$year)
i<-1 
for(i in 1:length(yrs)){
  temp.year <- yrs[i]
  prev.year <- yrs[i-1]
  twoyrsago <- yrs[i-2]
  threeyrsago <- yrs[i-3]
  fouryrsago <- yrs[i-4]
  fiveyrsago <- yrs[i-5]
  sixyrsago <- yrs[i-6]
  sevenyrsago <- yrs[i-7]
  eightyrsago <- yrs[i-8]
  nineyrsago <- yrs[i-9]
  tenyrsago <- yrs[i-10]
  elevenyrsago <- yrs[i-11]
  twelveyrsago <- yrs[i-12]
if(temp.year>1964){  F2lag$lag1_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==prev.year)]}
  if(temp.year>1965)  {F2lag$lag2_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==twoyrsago)]}
  if(temp.year>1966){F2lag$lag3_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==threeyrsago)]}
  if(temp.year>1967){F2lag$lag4_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==fouryrsago)]}
  if(temp.year>1968){F2lag$lag5_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==fiveyrsago)]}
  if(temp.year>1969){F2lag$lag6_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==sixyrsago)]}
  if(temp.year>1970){F2lag$lag7_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==sevenyrsago)]}
  if(temp.year>1971){F2lag$lag8_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==eightyrsago)]}
  if(temp.year>1972){F2lag$lag9_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==nineyrsago)]}
  if(temp.year>1973){F2lag$lag10_F[i] <- F2lag$mean_annual_F3plus[which(F2lag$year==tenyrsago)]}
}


lagged_dat <- left_join(dat2lag, F2lag, by=c("YEAR"="year"))

wd <- getwd()
write.csv(lagged_dat, file=paste(wd,"/data/analysis_ready_lagged_data_pollock_length.csv", sep=""))


#calc last year last age lag-----------------------------------------------------------------------

dat2lagsub <- dat2lag

dat2lagsub$lastyr_lastage_F <-NA

yrs <- unique(dat2lagsub$YEAR)
counter <- 1
k<-2 
for(k in 2:length(yrs)){
  temp.year <- yrs[k]
  prev.year <- yrs[k-1]
  temp.dat <- dat2lagsub[which(dat2lagsub$YEAR==temp.year),]
  ages <- unique(temp.dat$AGE)
  g<-2
  for (g in 2:length(ages)) {
  prev.age <- ages[g-1]
  if(temp.year>1964){ dat2lagsub$lastyr_lastage_F[counter] <- dat2lagsub$F[which(dat2lagsub$YEAR==prev.year &
                                                                    dat2lagsub$AGE==prev.age )]}
  counter <- counter + 1
 }
} #not working yet!!


#perhaps easier to do this by joining to a staggered year column??

#copied from other script
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

Fdatlong$age <- as.integer(Fdatlong$age)

Fdatlong_prevjoin <- Fdatlong

#create lagged year column and use to join

Fdatlong_prevjoin$prevyr_prevage_F <- Fdatlong_prevjoin$F

lagged_dat4join <- lagged_dat

lagged_dat4join$YEAR <- as.numeric(as.character(lagged_dat4join$YEAR))

lagged_dat4join$prevyr <- lagged_dat4join$YEAR - 1
lagged_dat4join$prevage <- as.numeric(as.character(lagged_dat4join$AGE)) - 1

lagged2prev <- left_join(lagged_dat4join, Fdatlong_prevjoin[,c(1:2,6)], by=c("prevyr"="year", "prevage"="age"))
#yes working correctly

#save
wd <- getwd()
write.csv(lagged2prev, file=paste(wd,"/data/analysis_ready_lagged_prevyr_pollock_length.csv", sep=""))


