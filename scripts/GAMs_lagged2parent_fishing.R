#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#individual-level length models lagged to the fishing experienced by parents

#Created by Krista, Oct 4, 2022
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Notes: 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library( "ggspatial" )
library("sf")
library(tidyverse)

#get data-----

wd <- getwd()
datwparents <- read.csv(file=paste(wd,"/data/analysis_ready_data_w_mean_parent_Fs.csv", sep=""), row.names=1)

datwparents$cohort <- as.factor(datwparents$cohort)
datwparents$YEAR <- as.factor(datwparents$YEAR)

ggplot(datwparents, aes(weighted_parent_mean_F, length_scaled)) + geom_point() + facet_wrap(~AGE, scales = "free") + geom_smooth(method="lm")

#need to scale parent F too
ggplot(datwparents, aes(mean_weight_parentF_scaled, length_scaled)) + geom_point() + facet_wrap(~AGE, scales = "free") + geom_smooth(method="lm")

ggplot(datwparents[which(datwparents$AGE>0),], aes(YEAR, mean_weight_parentF_scaled, col=south.sst.amj.scaled)) + 
  geom_point()  + scale_colour_gradient(low = "blue", high = "red")
#do the interactions make sense!?!?

#-------compare F metrics-------

#look at corr among F metrics

library(corrplot)


names(datwparents)
pairs(datwparents[,c(33, 49, 54)]) #F covars
cormatt <- cor(datwparents[,c(33, 49, 54)], use="complete.obs")
corrplot.mixed(cormatt, upper = 'ellipse',lower='number') #NOT very correlated! None more than 0.14

ggplot(datwparents, aes(mean_weight_parentF_scaled, prevyr_prevage_F_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #maybe a little correlated for oldest age classes? Looks ok tho

ggplot(datwparents, aes(mean_weight_parentF_scaled, mean_ann_F3plus_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #looks fine

ggplot(datwparents, aes(mean_ann_F3plus_scaled, prevyr_prevage_F_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #LOOKS correlated under age 11?

cormatyoung <- cor(datwparents[which(datwparents$AGE<10),c(33, 49, 54)], use="complete.obs")
corrplot.mixed(cormatyoung, upper = 'ellipse',lower='number') #still not very correlated? None more than 0.23

#how correlated is F and SST?!

cormattemp <- cor(datwparents[,c(33, 49, 51, 54)], use="complete.obs")
corrplot.mixed(cormattemp, upper = 'ellipse',lower='number') #still not very correlated? None more than 0.23
#not very

ggplot(datwparents, aes(mean_weight_parentF_scaled, south.sst.amj.scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #hmm uh oh -ve for young and +ve for old?

ggplot(datwparents, aes(YEAR, mean_weight_parentF_scaled, col=south.sst.amj.scaled)) + geom_point() + facet_wrap(~AGE, scales="free") +
  geom_smooth(method="lm") #

ggplot(datwparents, aes(prevyr_prevage_F_scaled, south.sst.amj.scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #high Fs during cold temps

ggplot(datwparents, aes(mean_ann_F3plus_scaled, south.sst.amj.scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #high Fs during cold temps


#GAMs w prev yr prev age AND mean parent F-----

library(mgcv)
library(gamm4)
library(car)

#age 1------
lag1par <- datwparents[which(datwparents$AGE==1),]

#prev yr prev age does NOT work for age 1 so drop
lag1.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                    mean_weight_parentF_scaled + 
                   # prevyr_prevage_F_scaled +
                  pollock_survey_abun_mil_at_age_scaled +
                  apex_pred_biom_scaled + 
                  forage_fish_biom_scaled + 
                  pelagic_forager_biom_scaled +
                  s(cohort, bs="re"),
                random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag1par )
gam.check(lag1.bothF$gam) #not bad
summary(lag1.bothF$gam) #
summary(lag1.bothF$mer) #
AIC_1lagbothF <- AIC(lag1.bothF$mer) #
plot(lag1.bothF$gam)

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 2------
lag2par <- datwparents[which(datwparents$AGE==2),]

lag2.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag2par )
gam.check(lag2.bothF$gam) #not bad
summary(lag2.bothF$gam) #
summary(lag2.bothF$mer) #
AIC_2lagbothF <- AIC(lag2.bothF$mer) #
plot(lag2.bothF$gam)

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 3------
lag3par <- datwparents[which(datwparents$AGE==3),]

lag3.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag3par )
gam.check(lag3.bothF$gam) #not bad
summary(lag3.bothF$gam) #
summary(lag3.bothF$mer) #
AIC_3lagbothF <- AIC(lag3.bothF$mer) #
plot(lag3.bothF$gam)

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")






#age 4------
lag4par <- datwparents[which(datwparents$AGE==4),]

lag4.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag4par )
gam.check(lag4.bothF$gam) #not bad
summary(lag4.bothF$gam) #
summary(lag4.bothF$mer) #
AIC_4lagbothF <- AIC(lag4.bothF$mer) #
plot(lag4.bothF$gam)

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 5------
lag5par <- datwparents[which(datwparents$AGE==5),]

lag5.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag5par )
gam.check(lag5.bothF$gam) #not bad
summary(lag5.bothF$gam) #
summary(lag5.bothF$mer) #
AIC_5lagbothF <- AIC(lag5.bothF$mer) #
plot(lag5.bothF$gam)

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")










#age 6------
lag6par <- datwparents[which(datwparents$AGE==6),]

lag6.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag6par )
gam.check(lag6.bothF$gam) #not bad
summary(lag6.bothF$gam) #
summary(lag6.bothF$mer) #
AIC_6lagbothF <- AIC(lag6.bothF$mer) #
plot(lag6.bothF$gam)

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 7------
lag7par <- datwparents[which(datwparents$AGE==7),]

lag7.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag7par )
gam.check(lag7.bothF$gam) #not bad
summary(lag7.bothF$gam) #
summary(lag7.bothF$mer) #
AIC_7lagbothF <- AIC(lag7.bothF$mer) #
plot(lag7.bothF$gam)

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 8------
lag8par <- datwparents[which(datwparents$AGE==8),]

lag8.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag8par )
gam.check(lag8.bothF$gam) #not bad
summary(lag8.bothF$gam) #
summary(lag8.bothF$mer) #
AIC_8lagbothF <- AIC(lag8.bothF$mer) #
plot(lag8.bothF$gam)

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")







#age 9------
lag9par <- datwparents[which(datwparents$AGE==9),]

lag9.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag9par )
gam.check(lag9.bothF$gam) #not bad
summary(lag9.bothF$gam) #
summary(lag9.bothF$mer) #
AIC_9lagbothF <- AIC(lag9.bothF$mer) #
plot(lag9.bothF$gam)

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")









#age 10------
lag10par <- datwparents[which(datwparents$AGE==10),]

lag10.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag10par )
gam.check(lag10.bothF$gam) #not bad
summary(lag10.bothF$gam) #
summary(lag10.bothF$mer) #
AIC_10lagbothF <- AIC(lag10.bothF$mer) #
plot(lag10.bothF$gam)

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 11------
lag11par <- datwparents[which(datwparents$AGE==11),]

lag11.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag11par )
gam.check(lag11.bothF$gam) #not bad
summary(lag11.bothF$gam) #
summary(lag11.bothF$mer) #
AIC_11lagbothF <- AIC(lag11.bothF$mer) #
plot(lag11.bothF$gam)

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 12------
lag12par <- datwparents[which(datwparents$AGE==12),]

lag12.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag12par )
gam.check(lag12.bothF$gam) #not bad
summary(lag12.bothF$gam) #
summary(lag12.bothF$mer) #
AIC_12lagbothF <- AIC(lag12.bothF$mer) #
plot(lag12.bothF$gam)

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 13------
lag13par <- datwparents[which(datwparents$AGE==13),]

lag13.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag13par )
gam.check(lag13.bothF$gam) #not bad
summary(lag13.bothF$gam) #
summary(lag13.bothF$mer) #
AIC_13lagbothF <- AIC(lag13.bothF$mer) #
plot(lag13.bothF$gam)

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")






#age 14------
lag14par <- datwparents[which(datwparents$AGE==14),]

lag14.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag14par )
gam.check(lag14.bothF$gam) #not bad
summary(lag14.bothF$gam) #
summary(lag14.bothF$mer) #
AIC_14lagbothF <- AIC(lag14.bothF$mer) #
plot(lag14.bothF$gam)

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 15------
lag15par <- datwparents[which(datwparents$AGE==15),]

lag15.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag15par )
gam.check(lag15.bothF$gam) #not bad
summary(lag15.bothF$gam) #
summary(lag15.bothF$mer) #
AIC_15lagbothF <- AIC(lag15.bothF$mer) #
plot(lag15.bothF$gam)

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")



#with temp x F interactions----------------------------------------------------------------------------------------

#not sure below makes any sense so commenting it out for now

# #age 1------
# 
# #prev yr prev age does NOT work for age 1 so drop
# lag1.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                      mean_weight_parentF_scaled + mean_weight_parentF_scaled:south.sst.amj.scaled + 
#                       # prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag1par )
# gam.check(lag1.intF$gam) #not bad
# summary(lag1.intF$gam) #
# summary(lag1.intF$mer) #
# AIC_1lagintF <- AIC(lag1.intF$mer) #
# plot(lag1.intF$gam)
# 
# AIC_1lagintF
# AIC_1lagbothF #basically same, int not sig
# 
# lag1.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                      mean_weight_parentF_scaled + #mean_weight_parentF_scaled:south.sst.amj.scaled + 
#                      # prevyr_prevage_F_scaled +
#                      pollock_survey_abun_mil_at_age_scaled +
#                      apex_pred_biom_scaled + 
#                      forage_fish_biom_scaled + 
#                      pelagic_forager_biom_scaled +
#                      s(cohort, bs="re"),
#                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag1par )
# gam.check(lag1.bothF$gam) #not bad
# summary(lag1.bothF$gam)
# 
# 
# 
# #age 2------
# 
# lag2.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag2par ) #interactions aren't significant
# gam.check(lag2.intF$gam) #not bad
# summary(lag2.intF$gam) #
# summary(lag2.intF$mer) #
# AIC_2lagintF <- AIC(lag2.intF$mer) #
# plot(lag2.intF$gam)
# 
# AIC(lag2.bothF$mer, lag2.intF$mer) #int maybe slightly better (delta 4) but maybe not given extra df (2)
# 
# 
# 
# 
# 
# 
# #age 3------
# 
# lag3.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag3par ) #interactions not sig
# gam.check(lag3.intF$gam) #not bad
# summary(lag3.intF$gam) #
# summary(lag3.intF$mer) #
# AIC_3lagintF <- AIC(lag3.intF$mer) #
# plot(lag3.intF$gam)
# 
# AIC(lag3.bothF$mer, lag3.intF$mer) #same
# 
# 
# 
# 
# #age 4------
# 
# lag4.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag4par ) #interactions ARE significant
# gam.check(lag4.intF$gam) #not bad
# summary(lag4.intF$gam) #
# summary(lag4.intF$mer) #
# AIC_4lagintF <- AIC(lag4.intF$mer) #
# plot(lag4.intF$gam)
# 
# AIC(lag4.bothF$mer, lag4.intF$mer) #9 lower w interactions
# 
# 
# 
# 
# 
# 
# #age 5------
# 
# lag5.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag5par ) #interactions are sig
# gam.check(lag5.intF$gam) #not bad
# summary(lag5.intF$gam) #
# summary(lag5.intF$mer) #
# AIC_5lagintF <- AIC(lag5.intF$mer) #
# plot(lag5.intF$gam)
# 
# AIC(lag5.bothF$mer, lag5.intF$mer) #int is 28 lower
# 
# 
# 
# 
# 
# 
# 
# #age 6------
# 
# lag6.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag6par ) #interactions not sig
# gam.check(lag6.intF$gam) #not bad
# summary(lag6.intF$gam) #
# summary(lag6.intF$mer) #
# AIC_6lagintF <- AIC(lag6.intF$mer) #
# plot(lag6.intF$gam)
# 
# AIC(lag6.bothF$mer, lag6.intF$mer) #int 10 less
# 
# 
# 
# 
# 
# 
# #age 7------
# 
# lag7.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag7par ) #one int sig
# gam.check(lag7.intF$gam) #not bad
# summary(lag7.intF$gam) #
# summary(lag7.intF$mer) #
# AIC_7lagintF <- AIC(lag7.intF$mer) #
# plot(lag7.intF$gam)
# 
# AIC(lag7.bothF$mer, lag7.intF$mer) #only 2 different
# 
# 
# 
# 
# #age 8------
# 
# lag8.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag8par ) #one int sig
# gam.check(lag8.intF$gam) #not bad
# summary(lag8.intF$gam) #
# summary(lag8.intF$mer) #
# AIC_8lagintF <- AIC(lag8.intF$mer) #
# plot(lag8.intF$gam)
# 
# AIC(lag8.bothF$mer, lag8.intF$mer) #same
# 
# 
# 
# 
# #age 9------
# 
# lag9.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                       mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                       prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                       pollock_survey_abun_mil_at_age_scaled +
#                       apex_pred_biom_scaled + 
#                       forage_fish_biom_scaled + 
#                       pelagic_forager_biom_scaled +
#                       s(cohort, bs="re"),
#                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag9par ) #one int sig
# gam.check(lag9.intF$gam) #not bad
# summary(lag9.intF$gam) #
# summary(lag9.intF$mer) #
# AIC_9lagintF <- AIC(lag9.intF$mer) #
# plot(lag9.intF$gam)
# 
# AIC(lag9.bothF$mer, lag9.intF$mer) #same
# 
# 
# 
# 
# 
# 
# 
# #age 10------
# 
# lag10.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag10par ) #int not sig
# gam.check(lag10.intF$gam) #not bad
# summary(lag10.intF$gam) #
# summary(lag10.intF$mer) #
# AIC_10lagintF <- AIC(lag10.intF$mer) #
# plot(lag10.intF$gam)
# 
# AIC(lag10.bothF$mer, lag10.intF$mer) #int 3 higher
# 
# 
# 
# 
# 
# 
# 
# #age 11------
# 
# lag11.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag11par ) #int not sig
# gam.check(lag11.intF$gam) #not bad
# summary(lag11.intF$gam) #
# summary(lag11.intF$mer) #
# AIC_11lagintF <- AIC(lag11.intF$mer) #
# plot(lag11.intF$gam)
# 
# AIC(lag11.bothF$mer, lag11.intF$mer) #int 4 higher
# 
# 
# #age 12------
# 
# lag12.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag12par ) #int not sig
# gam.check(lag12.intF$gam) #not bad
# summary(lag12.intF$gam) #
# summary(lag12.intF$mer) #
# AIC_12lagintF <- AIC(lag12.intF$mer) #
# plot(lag12.intF$gam)
# 
# AIC(lag12.bothF$mer, lag12.intF$mer) #int 2 higher
# 
# 
# 
# #age 13------
# 
# lag13.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag13par ) #int not sig
# gam.check(lag13.intF$gam) #not bad
# summary(lag13.intF$gam) #
# summary(lag13.intF$mer) #
# AIC_13lagintF <- AIC(lag13.intF$mer) #
# plot(lag13.intF$gam)
# 
# AIC(lag13.bothF$mer, lag13.intF$mer) #int 2 higher
# 
# 
# 
# #age 14------
# 
# lag14.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag14par ) #one int sig
# gam.check(lag14.intF$gam) #not bad
# summary(lag14.intF$gam) #
# summary(lag14.intF$mer) #
# AIC_14lagintF <- AIC(lag14.intF$mer) #
# plot(lag14.intF$gam)
# 
# AIC(lag14.bothF$mer, lag14.intF$mer) #int 4 lower
# 
# 
# 
# 
# #age 15------
# 
# lag15.intF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
#                        mean_weight_parentF_scaled:south.sst.amj.scaled + mean_weight_parentF_scaled +
#                        prevyr_prevage_F_scaled:south.sst.amj.scaled + prevyr_prevage_F_scaled +
#                        pollock_survey_abun_mil_at_age_scaled +
#                        apex_pred_biom_scaled + 
#                        forage_fish_biom_scaled + 
#                        pelagic_forager_biom_scaled +
#                        s(cohort, bs="re"),
#                      random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag15par ) #int not sig
# gam.check(lag15.intF$gam) #not bad
# summary(lag15.intF$gam) #
# summary(lag15.intF$mer) #
# AIC_15lagintF <- AIC(lag15.intF$mer) #
# plot(lag15.intF$gam)
# 
# AIC(lag15.bothF$mer, lag15.intF$mer) #int 2 higher
