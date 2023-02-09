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





#Now repeat models=========================================================================================


#age 2------
mean2 <- meanjoin[which(meanjoin$AGE==2),]

gi2 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,  REML=FALSE,  data=mean2 )
gam.check(gi2) #not bad
summary(gi2) #
summary(gi2) #
plot(gi2)




#age 3------
mean3 <- meanjoin[which(meanjoin$AGE==3),]

gi3 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled, REML=FALSE,  data=mean3 )
gam.check(gi3) #not bad
summary(gi3) #
summary(gi3) #
plot(gi3)



#age 4------
mean4 <- meanjoin[which(meanjoin$AGE==4),]

gi4 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,  REML=FALSE,  data=mean4 )
gam.check(gi4) #not bad
summary(gi4) #
summary(gi4) #
plot(gi4)





#age 5------
mean5 <- meanjoin[which(meanjoin$AGE==5),]

gi5 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,   REML=FALSE,  data=mean5 )
gam.check(gi5) #not bad
summary(gi5) #
summary(gi5) #
plot(gi5)




#age 6------
mean6 <- meanjoin[which(meanjoin$AGE==6),]

gi6 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled, REML=FALSE,  data=mean6 )
gam.check(gi6) #not bad
summary(gi6) #
summary(gi6) #
plot(gi6)






#age 7------
mean7 <- meanjoin[which(meanjoin$AGE==7),]

gi7 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,   REML=FALSE,  data=mean7 )
gam.check(gi7) #not bad
summary(gi7) #
summary(gi7) #
plot(gi7)


#age 8------
mean8 <- meanjoin[which(meanjoin$AGE==8),]

gi8 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,   REML=FALSE,  data=mean8 )
gam.check(gi8) #not bad
summary(gi8) #
summary(gi8) #
plot(gi8)




#age 9------
mean9 <- meanjoin[which(meanjoin$AGE==9),]

gi9 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled,  REML=FALSE,  data=mean9 )
gam.check(gi9) #not bad
summary(gi9) #
summary(gi9) #
plot(gi9)






#age 10------
mean10 <- meanjoin[which(meanjoin$AGE==10),]

gi10 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled,   REML=FALSE,  data=mean10 )
gam.check(gi10) #not bad
summary(gi10) #
summary(gi10) #
plot(gi10)




#age 11------
mean11 <- meanjoin[which(meanjoin$AGE==11),]

gi11 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                mean_weight_parentF_scaled + 
                prevyr_prevage_F_scaled +
                pollock_survey_abun_mil_at_age_scaled +
                apex_pred_biom_scaled + 
                forage_fish_biom_scaled + 
                pelagic_forager_biom_scaled,  REML=FALSE,  data=mean11 )
gam.check(gi11) #not bad
summary(gi11) #
summary(gi11) #
plot(gi11)




#age 12------
mean12 <- meanjoin[which(meanjoin$AGE==12),]

gi12 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                mean_weight_parentF_scaled + 
                prevyr_prevage_F_scaled +
                pollock_survey_abun_mil_at_age_scaled +
                apex_pred_biom_scaled + 
                forage_fish_biom_scaled + 
                pelagic_forager_biom_scaled,   REML=FALSE,  data=mean12 )
gam.check(gi12) #not bad
summary(gi12) #
summary(gi12) #
plot(gi12)





#age 13------
mean13 <- meanjoin[which(meanjoin$AGE==13),]

gi13 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                mean_weight_parentF_scaled + 
                prevyr_prevage_F_scaled +
                pollock_survey_abun_mil_at_age_scaled +
                apex_pred_biom_scaled + 
                forage_fish_biom_scaled + 
                pelagic_forager_biom_scaled,   REML=FALSE,  data=mean13 )
gam.check(gi13) #not bad
summary(gi13) #
summary(gi13) #
plot(gi13)






#age 14------
mean14 <- meanjoin[which(meanjoin$AGE==14),]

gi14 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                mean_weight_parentF_scaled + 
                prevyr_prevage_F_scaled +
                pollock_survey_abun_mil_at_age_scaled +
                apex_pred_biom_scaled + 
                forage_fish_biom_scaled + 
                pelagic_forager_biom_scaled,  REML=FALSE,  data=mean14 )
gam.check(gi14$gam) #not bad
summary(gi14$gam) #
summary(gi14$mer) #
plot(gi14$gam)







#age 15------
mean15 <- meanjoin[which(meanjoin$AGE==15),]

gi15 <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                mean_weight_parentF_scaled + 
                prevyr_prevage_F_scaled +
                pollock_survey_abun_mil_at_age_scaled +
                apex_pred_biom_scaled + 
                forage_fish_biom_scaled + 
                pelagic_forager_biom_scaled,  REML=FALSE,  data=mean15 )
gam.check(gi15$gam) #not bad
summary(gi15$gam) #
summary(gi15$mer) #
plot(gi15$gam)



#with REML========


#age 2------

gi2reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,  REML=TRUE,  data=mean2 )
gam.check(gi2reml) #not bad
summary(gi2reml) #
summary(gi2reml) #
plot(gi2reml)




#age 3------

gi3reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled, REML=TRUE,  data=mean3 )
gam.check(gi3reml) #not bad
summary(gi3reml) #
summary(gi3reml) #
plot(gi3reml)



#age 4------

gi4reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,  REML=TRUE,  data=mean4 )
gam.check(gi4reml) #not bad
summary(gi4reml) #
summary(gi4reml) #
plot(gi4reml)





#age 5------

gi5reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,   REML=TRUE,  data=mean5 )
gam.check(gi5reml) #not bad
summary(gi5reml) #
summary(gi5reml) #
plot(gi5reml)




#age 6------

gi6reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled, REML=TRUE,  data=mean6 )
gam.check(gi6reml) #not bad
summary(gi6reml) #
summary(gi6reml) #
plot(gi6reml)






#age 7------

gi7reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,   REML=TRUE,  data=mean7 )
gam.check(gi7reml) #not bad
summary(gi7reml) #
summary(gi7reml) #
plot(gi7reml)


#age 8------

gi8reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,   REML=TRUE,  data=mean8 )
gam.check(gi8reml) #not bad
summary(gi8reml) #
summary(gi8reml) #
plot(gi8reml)




#age 9------

gi9reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
             mean_weight_parentF_scaled + 
             prevyr_prevage_F_scaled +
             pollock_survey_abun_mil_at_age_scaled +
             apex_pred_biom_scaled + 
             forage_fish_biom_scaled + 
             pelagic_forager_biom_scaled,  REML=TRUE,  data=mean9 )
gam.check(gi9reml) #not bad
summary(gi9reml) #
summary(gi9reml) #
plot(gi9reml)






#age 10------

gi10reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,   REML=TRUE,  data=mean10 )
gam.check(gi10reml) #not bad
summary(gi10reml) #
summary(gi10reml) #
plot(gi10reml)




#age 11------

gi11reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,  REML=TRUE,  data=mean11 )
gam.check(gi11reml) #not bad
summary(gi11reml) #
summary(gi11reml) #
plot(gi11reml)




#age 12------

gi12reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,   REML=TRUE,  data=mean12 )
gam.check(gi12reml) #not bad
summary(gi12reml) #
summary(gi12reml) #
plot(gi12reml)





#age 13------

gi13reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,   REML=TRUE,  data=mean13 )
gam.check(gi13reml) #not bad
summary(gi13reml) #
summary(gi13reml) #
plot(gi13reml)






#age 14------

gi14reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,  REML=TRUE,  data=mean14 )
gam.check(gi14reml$gam) #not bad
summary(gi14reml$gam) #
summary(gi14reml$mer) #
plot(gi14reml$gam)







#age 15------

gi15reml <- gam(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
              mean_weight_parentF_scaled + 
              prevyr_prevage_F_scaled +
              pollock_survey_abun_mil_at_age_scaled +
              apex_pred_biom_scaled + 
              forage_fish_biom_scaled + 
              pelagic_forager_biom_scaled,  REML=TRUE,  data=mean15 )
gam.check(gi15reml$gam) #not bad
summary(gi15reml$gam) #
summary(gi15reml$mer) #
plot(gi15reml$gam)

