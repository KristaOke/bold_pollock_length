#============================================================================================================
#Models re-run on growth increments instead of length-at-age

#Krista, Feb 2023
#============================================================================================================
#Notes:
#============================================================================================================


library(lme4)
library(mgcv)
library(gamm4)
library(cowplot)

#===

#loaded in weight_age_exploration.R
names(lagdat)

w2means <- lagdat %>% group_by(YEAR, AGE) %>% summarize(mean_annual_weight_at_age=mean(WEIGHT, na.rm=TRUE))

#loop?

w2means$prev_weight_at_age <- NA
w2means$prev_weight_at_age <- as.numeric(w2means$prev_weight_at_age)

table(w2means$AGE, w2means$YEAR)
#data in all yrs up to age 14

w2means <- w2means[which(w2means$AGE<14),]

counter <- 1
i<-1
for(i in 1:length(w2means$YEAR)){
  prev_WAA <- NA
  yr_i <- w2means$YEAR[i]
  age_i <- w2means$AGE[i]
  
  prev_yr <- yr_i - 1
  prev_age <- age_i - 1
  
  if(prev_age>0 & prev_yr>1998) { 
    prev_WAA <- w2means$mean_annual_weight_at_age[which(w2means$YEAR==prev_yr & w2means$AGE==prev_age)]
  }
  
  w2means$prev_weight_at_age[i] <- prev_WAA
  
  
  counter <- counter + 1
}

w2means$growth_increment <- w2means$mean_annual_weight_at_age - w2means$prev_weight_at_age

#all looks good, now match back to metadata