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

#get data-----

wd <- getwd()
datwparents <- read.csv(file=paste(wd,"/data/analysis_ready_data_w_mean_parent_Fs.csv", sep=""), row.names=1)

datwparents$cohort <- as.factor(datwparents$cohort)
datwparents$YEAR <- as.factor(datwparents$YEAR)


w2means <- datwparents %>% group_by(YEAR, AGE) %>% summarize(mean_annual_length_at_age=mean(LENGTH, na.rm=TRUE),
                                                             n=n())
w2means$YEAR <- as.numeric(as.character(w2means$YEAR))

#loop?

w2means$prev_length_at_age <- NA
w2means$prev_length_at_age <- as.numeric(w2means$prev_length_at_age)

table(w2means$AGE, w2means$YEAR)
#not enough data in age 14 in 2018 and age 15 in 2019

w2means <- w2means[which(w2means$AGE>0),]

counter <- 1
i<-1
for(i in 1:length(w2means$YEAR)){
  prev_LAA <- NA
  yr_i <- w2means$YEAR[i]
  age_i <- w2means$AGE[i]
  
  prev_yr <- yr_i - 1
  prev_age <- age_i - 1
  
  if(prev_age>0 & prev_yr>1984) { 
    prev_LAA <- w2means$mean_annual_length_at_age[which(w2means$YEAR==prev_yr & w2means$AGE==prev_age)]
  }
  
  w2means$prev_length_at_age[i] <- prev_LAA
  
  
  counter <- counter + 1
}

w2means$growth_increment <- w2means$mean_annual_length_at_age - w2means$prev_length_at_age

#all looks good, now match back to metadata


metadat <- datwparents[,c(1:3, 10, 34:38, 49:53)] #select just cols w metadata, not individual data
metadat <- metadat[!duplicated(metadat),]

w2means$YEAR <- as.factor(w2means$YEAR)

meanjoin <- left_join(w2means, metadat)

#take a look
ggplot(meanjoin, aes(as.numeric(as.character(YEAR)), growth_increment, col=as.factor(AGE))) +
  geom_point() + geom_smooth() + facet_wrap(~AGE, scales="free")

