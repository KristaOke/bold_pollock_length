#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Calculate lags in fishing

#Created by Krista, Jan 5, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: need to lag the overall fishing rate  so models can include recent/realtime fishing (removal) vs 
#fishing on parent generation (evol)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#let's start w same data I am using for indiv lvl models but w different name so nothing gets messed up

dat2lag <- read.csv(file=paste(wd,"/data/analysis_ready_individual_data_pollock_length.csv", sep=""), row.names=1)

dat2lag <- dat2lag[which(dat2lag$AGE<11),]
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




