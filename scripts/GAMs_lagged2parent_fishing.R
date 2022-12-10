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
library(gratia)
library(patchwork)

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
#do the interactions make sense!?!? No, removed

#-------compare F metrics-------

#look at corr among F metrics

library(corrplot)


names(datwparents)
pairs(datwparents[,c(33, 49, 53)]) #F covars
cormatt <- cor(datwparents[,c(33, 49, 53)], use="complete.obs")
corrplot.mixed(cormatt, upper = 'ellipse',lower='number') #NOT very correlated! None more than 0.14

ggplot(datwparents, aes(mean_weight_parentF_scaled, prevyr_prevage_F_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #maybe a little correlated for oldest age classes? Looks ok tho

ggplot(datwparents, aes(mean_weight_parentF_scaled, mean_ann_F3plus_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #looks fine

ggplot(datwparents, aes(mean_ann_F3plus_scaled, prevyr_prevage_F_scaled)) + geom_point() + facet_wrap(~AGE) +
  geom_smooth(method="lm") #LOOKS correlated under age 11?

cormatyoung <- cor(datwparents[which(datwparents$AGE<10),c(33, 49, 53)], use="complete.obs")
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

#look at new covar
ggplot(datwparents, aes(mean_weight_parentF_scaled, length_scaled)) + geom_point() + facet_wrap(~AGE, scales="free") +
  geom_smooth(method="lm") #

mean_lengths <- datwparents %>% group_by(YEAR, AGE) %>%
  summarise(mean_ann_length=mean(LENGTH, na.rm=TRUE))

mean_len_join <- left_join(datwparents, mean_lengths)

mean_len_join$period <- NA
mean_len_join$period [which(as.numeric(as.character(mean_len_join$YEAR))<1988)] <- "Before 1987"
mean_len_join$period [which(as.numeric(as.character(mean_len_join$YEAR))>1987)] <- "After 1987"


ggplot(mean_len_join[which(mean_len_join$AGE!=0),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point() + facet_wrap(~AGE, scales="free_y", ncol=3) +
  geom_smooth(method="lm") + #scale_colour_gradient(low = "blue", high = "red") + 
  ylab("Annual mean length (mm)") + xlab("Mean weighted fishing intensity on parents") + theme_bw()

ggplot(mean_len_join[which(mean_len_join$AGE!=0),], aes(mean_weight_parentF_scaled, mean_ann_length, col=as.numeric(as.character(YEAR)))) + geom_point() + facet_wrap(~AGE, scales="free_y", ncol=3) +
  geom_smooth(method="lm") + #scale_colour_gradient(low = "blue", high = "red") + 
  ylab("Annual mean length (mm)") + xlab("Mean weighted fishing intensity on parents") + theme_bw()



#would be good to plot these on more consistent y axis across ages
library(cowplot)

p1 <- ggplot(mean_len_join[which(mean_len_join$AGE==1 | mean_len_join$AGE==2 |
                                   mean_len_join$AGE==3),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point(aes(col=period)) + 
  facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  ylab("") + xlab("") + theme_bw() +xlim(c(-2,4)) + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

p2 <- ggplot(mean_len_join[which(mean_len_join$AGE==4 | mean_len_join$AGE==5 |
                                   mean_len_join$AGE==6),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point(aes(col=period)) + 
  facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  ylab("") + xlab("") + theme_bw() +xlim(c(-2,4))+ scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

p3 <- ggplot(mean_len_join[which(mean_len_join$AGE==7 | mean_len_join$AGE==8 |
                                   mean_len_join$AGE==9),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point(aes(col=period)) + 
  facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  ylab("Annual mean length (mm)") + xlab("") + theme_bw() +xlim(c(-2,4))+ scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

p4 <- ggplot(mean_len_join[which(mean_len_join$AGE==10 | mean_len_join$AGE==11 |
                                   mean_len_join$AGE==12),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point(aes(col=period)) + 
  facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  ylab("") + xlab("") + theme_bw() +xlim(c(-2,4))+ scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

p5 <- ggplot(mean_len_join[which(mean_len_join$AGE==13 | mean_len_join$AGE==14 |
                                   mean_len_join$AGE==15),], aes(mean_weight_parentF_scaled, mean_ann_length)) + geom_point(aes(col=period)) + 
  facet_wrap(~AGE, ncol=3) + theme_bw() +
  geom_smooth(method="lm") +  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))+
  ylab("") + xlab("Mean weighted fishing intensity on parents") +xlim(c(-2,4))+ scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")


plot_grid(p1, p2, p3, p4, p5, ncol=1)



ggplot(mean_len_join[which(mean_len_join$AGE!=0),], aes(prevyr_prevage_F_scaled, mean_ann_length, col=as.numeric(as.character(YEAR)))) +  geom_point() + facet_wrap(~AGE, scales="free_y", ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("Annual mean length (mm)") #+ xlab("Mean weighted fishing intensity on parents") 

ggplot(mean_len_join[which(mean_len_join$AGE!=0),], aes(as.numeric(as.character(YEAR)), prevyr_prevage_F_scaled)) + geom_point() + facet_wrap(~AGE, ncol=3) +
  geom_line() + #scale_colour_gradient(low = "blue", high = "red") + 
  xlab("Year") + ylab("Fishing intensity on cohort in previous year") + theme_bw() + 
  geom_vline(xintercept=1987, 
             color = "red", size=0.25)

ggplot(mean_len_join[which(mean_len_join$AGE!=0),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point() + facet_wrap(~AGE, scales="free_y", ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("Annual mean length (mm)") + xlab("Fishing intensity on cohort in previous year") 
#cool remake like above


v1 <- ggplot(mean_len_join[which(mean_len_join$AGE==1|
                                   mean_len_join$AGE==2|
                                   mean_len_join$AGE==3),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point(aes(col=period)) + facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("") + xlab("") + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

v2 <- ggplot(mean_len_join[which(mean_len_join$AGE==4|
                                   mean_len_join$AGE==5|
                                   mean_len_join$AGE==6),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point(aes(col=period)) + facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("") + xlab("") + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

v3 <- ggplot(mean_len_join[which(mean_len_join$AGE==7|
                                   mean_len_join$AGE==8|
                                   mean_len_join$AGE==9),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point(aes(col=period)) + facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("Annual mean length (mm)") + xlab("") + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

v4 <- ggplot(mean_len_join[which(mean_len_join$AGE==10|
                                   mean_len_join$AGE==11|
                                   mean_len_join$AGE==12),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point(aes(col=period)) + facet_wrap(~AGE, ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("") + xlab("") + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

v5 <- ggplot(mean_len_join[which(mean_len_join$AGE==13|
                                   mean_len_join$AGE==14|
                                   mean_len_join$AGE==15),], aes(prevyr_prevage_F_scaled, mean_ann_length)) +  geom_point(aes(col=period)) + facet_wrap(~AGE,  ncol=3) +
  geom_smooth(method="lm") +theme_bw()+
  ylab("") + xlab("Fishing intensity on cohort in previous year") + scale_colour_manual(values=c("black", "red")) +theme(legend.position = "none")

plot_grid(v1, v2, v3, v4, v5, ncol=1)


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



#again w REML----------------------------------------------------------------------------------------

#
#age 1------

#prev yr prev age does NOT work for age 1 so drop
lag1.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      # prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag1par )
gam.check(lag1.bothF_REML$gam) #not bad
summary(lag1.bothF_REML$gam) #
summary(lag1.bothF_REML$mer) #
plot(lag1.bothF_REML$gam)


#age 2------

lag2.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag2par )
gam.check(lag2.bothF_REML$gam) #not bad
summary(lag2.bothF_REML$gam) #
summary(lag2.bothF_REML$mer) #
plot(lag2.bothF_REML$gam)




#age 3------

lag3.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag3par )
gam.check(lag3.bothF_REML$gam) #not bad
summary(lag3.bothF_REML$gam) #
summary(lag3.bothF_REML$mer) #
plot(lag3.bothF_REML$gam)




#age 4------

lag4.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag4par )
gam.check(lag4.bothF_REML$gam) #not bad
summary(lag4.bothF_REML$gam) #
summary(lag4.bothF_REML$mer) #
plot(lag4.bothF_REML$gam)






#age 5------

lag5.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag5par )
gam.check(lag5.bothF_REML$gam) #not bad
summary(lag5.bothF_REML$gam) #
summary(lag5.bothF_REML$mer) #
plot(lag5.bothF_REML$gam)






#age 6------

lag6.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag6par )
gam.check(lag6.bothF_REML$gam) #not bad
summary(lag6.bothF_REML$gam) #
summary(lag6.bothF_REML$mer) #
plot(lag6.bothF_REML$gam)







#age 7------

lag7.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag7par )
gam.check(lag7.bothF_REML$gam) #not bad
summary(lag7.bothF_REML$gam) #
summary(lag7.bothF_REML$mer) #
plot(lag7.bothF_REML$gam)




#age 8------

lag8.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag8par )
gam.check(lag8.bothF_REML$gam) #not bad
summary(lag8.bothF_REML$gam) #
summary(lag8.bothF_REML$mer) #
plot(lag8.bothF_REML$gam)






#age 9------

lag9.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag9par )
gam.check(lag9.bothF_REML$gam) #not bad
summary(lag9.bothF_REML$gam) #
summary(lag9.bothF_REML$mer) #
plot(lag9.bothF_REML$gam)




#age 10------

lag10.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag10par )
gam.check(lag10.bothF_REML$gam) #not bad
summary(lag10.bothF_REML$gam) #
summary(lag10.bothF_REML$mer) #
plot(lag10.bothF_REML$gam)







#age 11------

lag11.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag11par )
gam.check(lag11.bothF_REML$gam) #not bad
summary(lag11.bothF_REML$gam) #
summary(lag11.bothF_REML$mer) #
plot(lag11.bothF_REML$gam)


#age 12------

lag12.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag12par )
gam.check(lag12.bothF_REML$gam) #not bad
summary(lag12.bothF_REML$gam) #
summary(lag12.bothF_REML$mer) #
plot(lag12.bothF_REML$gam)




#age 13------

lag13.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag13par )
gam.check(lag13.bothF_REML$gam) #not bad
summary(lag13.bothF_REML$gam) #
summary(lag13.bothF_REML$mer) #
plot(lag13.bothF_REML$gam)




#age 14------


lag14.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag14par )
gam.check(lag14.bothF_REML$gam) #not bad
summary(lag14.bothF_REML$gam) #
summary(lag14.bothF_REML$mer) #
plot(lag14.bothF_REML$gam)




#age 15------

lag15.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag15par )
gam.check(lag15.bothF_REML$gam) #not bad
summary(lag15.bothF_REML$gam) #
summary(lag15.bothF_REML$mer) # #
plot(lag15.bothF_REML$gam)

#SST smooths plot-----------------------

par(mfrow=c(5,3), mai = c(1, 0.1, 0.1, 0.1))


visreg(lag1.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag1par, ylab="" )
visreg(lag2.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag2par, ylab="")
visreg(lag3.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag3par, ylab="")
visreg(lag4.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag4par, ylab="")
visreg(lag5.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag5par, ylab="")
visreg(lag6.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag6par, ylab="")
visreg(lag7.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag7par, ylab="")
visreg(lag8.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag8par, ylab="")
visreg(lag9.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag9par, ylab="")
visreg(lag10.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag10par, ylab="")
visreg(lag11.bothF_REML$gam,  xvar="south.sst.amj.scaled", data=lag11par, ylab="")
visreg(lag12.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag12par, ylab="")
visreg(lag13.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag13par, ylab="")
visreg(lag14.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag14par, ylab="")
visreg(lag15.bothF_REML$gam, xvar="south.sst.amj.scaled", data=lag15par, ylab="")

plot1 <- draw(lag1.bothF_REML$gam, select=1, title="", data=lag1par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot2 <- draw(lag2.bothF_REML$gam, select=1, data=lag2par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot3 <- draw(lag3.bothF_REML$gam, select=1, title="", data=lag3par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot4 <- draw(lag4.bothF_REML$gam, select=1, data=lag4par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot5 <- draw(lag5.bothF_REML$gam, select=1, title="", data=lag5par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot6 <- draw(lag6.bothF_REML$gam, select=1, data=lag6par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot7 <- draw(lag7.bothF_REML$gam, select=1, title="", data=lag7par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot8 <- draw(lag8.bothF_REML$gam, select=1, data=lag8par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot9 <- draw(lag9.bothF_REML$gam, select=1, title="", data=lag9par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot10 <- draw(lag10.bothF_REML$gam, select=1, data=lag10par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot11 <- draw(lag11.bothF_REML$gam, select=1, data=lag11par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot12 <- draw(lag12.bothF_REML$gam, select=1, data=lag12par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot13 <- draw(lag13.bothF_REML$gam, select=1, data=lag13par, gg=TRUE) + theme_bw() + 
  theme(title = element_blank())
plot14 <- draw(lag14.bothF_REML$gam, select=1, data=lag14par, gg=TRUE)+ theme_bw() + 
  theme(title = element_blank()) 
plot15 <- draw(lag15.bothF_REML$gam, select=1, data=lag15par, gg=TRUE) + theme_bw() + theme(title=element_blank())

plot1 + plot2 + plot3 + plot4 + plot5 +
  plot6 + plot7 + plot8 + plot9 + plot10 +
  plot11 + plot12 + plot13 + plot14 + plot15 +
  plot_layout(nrow=5, ncol=3)

#Back simulations----------------------------------------------------------------------------

#maybe start by getting SDs, confiriming that scale did the same as Curry did
#then create a loop that will make 2 new datasets without response variable, 
#add 1 SD to one and subtract 1 SD from other, use predict (how to deal with random?) 

names(lag1par)

back_dat <- data.frame(matrix(ncol=11, nrow=8))
namesv <- c("LONGITUDE", "LATITUDE", "julian_scaled", 
            "south.sst.amj.scaled", 
  "mean_weight_parentF_scaled", 
  "prevyr_prevage_F_scaled",
  "pollock_survey_abun_mil_at_age_scaled",
  "apex_pred_biom_scaled",
  "forage_fish_biom_scaled", 
  "pelagic_forager_biom_scaled", "cohort")
colnames(back_dat) <- namesv

back_dat[1,3:10] <- c(1, 0, 0, 0, 0, 0, 0, 0 )
back_dat[2,3:10] <- c(0, 1, 0, 0, 0, 0, 0, 0 )
back_dat[3,3:10] <- c(0, 0, 1, 0, 0, 0, 0, 0 )
back_dat[4,3:10] <- c(0, 0, 0, 1, 0, 0, 0, 0 )
back_dat[5,3:10] <- c(0, 0, 0, 0, 1, 0, 0, 0 )
back_dat[6,3:10] <- c(0, 0, 0, 0, 0, 1, 0, 0 )
back_dat[7,3:10] <- c(0, 0, 0, 0, 0, 0, 1, 0 )
back_dat[8,3:10] <- c(0, 0, 0, 0, 0, 0, 0, 1 )

lat_median <- median(datwparents$LATITUDE)
long_median <- median(datwparents$LONGITUDE)
cohort_median <- median(as.numeric(as.character(datwparents$cohort)))

back_dat$LATITUDE <- lat_median
back_dat$LONGITUDE <- long_median
back_dat$cohort <- cohort_median

back_dat$covar <- namesv[3:10]

#for each age
age1pred <- predict.gam(lag1.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age1pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age1 <- age1pred$fit
back_dat$pred_SE_age1 <- age1pred$se.fit

#

age2pred <- predict.gam(lag2.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age2pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age2 <- age2pred$fit
back_dat$pred_SE_age2 <- age2pred$se.fit

#

age3pred <- predict.gam(lag3.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age3pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age3 <- age3pred$fit
back_dat$pred_SE_age3 <- age3pred$se.fit

#

age4pred <- predict.gam(lag4.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age4pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age4 <- age4pred$fit
back_dat$pred_SE_age4 <- age4pred$se.fit

#

age5pred <- predict.gam(lag5.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age5pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age5 <- age5pred$fit
back_dat$pred_SE_age5 <- age5pred$se.fit

#

age6pred <- predict.gam(lag6.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age6pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age6 <- age6pred$fit
back_dat$pred_SE_age6 <- age6pred$se.fit

#

age7pred <- predict.gam(lag7.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age7pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age7 <- age7pred$fit
back_dat$pred_SE_age7 <- age7pred$se.fit

#

age8pred <- predict.gam(lag8.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age8pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age8 <- age8pred$fit
back_dat$pred_SE_age8 <- age8pred$se.fit

#

age9pred <- predict.gam(lag9.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age9pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age9 <- age9pred$fit
back_dat$pred_SE_age9 <- age9pred$se.fit

#

age10pred <- predict.gam(lag10.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age10pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age10 <- age10pred$fit
back_dat$pred_SE_age10 <- age10pred$se.fit

#

age11pred <- predict.gam(lag11.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age11pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age11 <- age11pred$fit
back_dat$pred_SE_age11 <- age11pred$se.fit

#

age12pred <- predict.gam(lag12.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age12pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age12 <- age12pred$fit
back_dat$pred_SE_age12 <- age12pred$se.fit

#

age13pred <- predict.gam(lag13.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age13pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age13 <- age13pred$fit
back_dat$pred_SE_age13 <- age13pred$se.fit

#

age14pred <- predict.gam(lag14.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age14pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age14 <- age14pred$fit
back_dat$pred_SE_age14 <- age14pred$se.fit

#

age15pred <- predict.gam(lag15.bothF_REML$gam, newdata = back_dat, se.fit=TRUE)
length(age15pred$fit)
length(back_dat$LATITUDE) #same length

back_dat$predicted_age15 <- age15pred$fit
back_dat$pred_SE_age15 <- age15pred$se.fit



#pivot long then plot?

sub_pred <- back_dat[,c(1,2,11:12, 13:42)]
sub_predict <- sub_pred[,c("LONGITUDE", "LATITUDE", "cohort", "covar",
  "predicted_age1",
                           "predicted_age2",
                           "predicted_age3",
                           "predicted_age4",
                           "predicted_age5",
                           "predicted_age6",
                           "predicted_age7",
                           "predicted_age8",
                           "predicted_age9",
                           "predicted_age10",
                           "predicted_age11",
                           "predicted_age12",
                           "predicted_age13",
                           "predicted_age14",
                           "predicted_age15")]

sub_se <- sub_pred[,c("LONGITUDE", "LATITUDE", "cohort", "covar",
                           "pred_SE_age1",
                           "pred_SE_age2",
                           "pred_SE_age3",
                           "pred_SE_age4",
                           "pred_SE_age5",
                           "pred_SE_age6",
                           "pred_SE_age7",
                           "pred_SE_age8",
                           "pred_SE_age9",
                           "pred_SE_age10",
                           "pred_SE_age11",
                           "pred_SE_age12",
                           "pred_SE_age13",
                           "pred_SE_age14",
                           "pred_SE_age15")]

back_plot_pred <- sub_predict %>% 
  pivot_longer(!c(LATITUDE, LONGITUDE, cohort, covar), names_to = "response", values_to = "value")
back_plot_pred$age <- gsub("predicted_age", "", back_plot_pred$response)
back_plot_pred$age <- as.numeric(as.character(back_plot_pred$age))

back_plot_se <- sub_se %>% 
  pivot_longer(!c(LATITUDE, LONGITUDE, cohort, covar), names_to = "response", values_to = "SE")
back_plot_se$age <- gsub("pred_SE_age", "", back_plot_se$response)
back_plot_se$age <- as.numeric(as.character(back_plot_se$age))

back_plot_dat <- left_join(back_plot_pred, back_plot_se, by=c("LONGITUDE", "LATITUDE", "cohort", "covar", "age"))
#


p1 <- ggplot(back_plot_dat, aes(value, covar)) + geom_point() + geom_errorbar(aes(xmin=(value-SE),
                                                                                  xmax=(value+SE))) + 
  facet_wrap(~age) + geom_vline(xintercept = 0, colour="red") 
p1

#without julian, not really of interest
p2 <- ggplot(back_plot_dat[which(back_plot_dat$covar!="julian_scaled"),], aes(value, covar)) + geom_point() + geom_errorbar(aes(xmin=(value-SE),
                                                                                  xmax=(value+SE))) + 
  facet_wrap(~age) + geom_vline(xintercept = 0, colour="red") 
p2

#are error bars repeating?

back_plot_dat[which(back_plot_dat$age==1 & back_plot_dat$covar=="south.sst.amj.scaled"),]




#REPEAT GAMs without forage fish which is shortest time series-----------------------------

#age 1------

#prev yr prev age does NOT work for age 1 so drop
long1.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      # prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag1par )
gam.check(long1.bothF$gam) #not bad
summary(long1.bothF$gam) #
summary(long1.bothF$mer) #
AIC_1longbothF <- AIC(long1.bothF$mer) #
plot(long1.bothF$gam)

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag1par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")




#age 2------

long2.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                     # forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag2par )
gam.check(long2.bothF$gam) #not bad
summary(long2.bothF$gam) #
summary(long2.bothF$mer) #
AIC_2longbothF <- AIC(long2.bothF$mer) #
plot(long2.bothF$gam)

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag2par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 3------


long3.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag3par )
gam.check(long3.bothF$gam) #not bad
summary(long3.bothF$gam) #
summary(long3.bothF$mer) #
AIC_3longbothF <- AIC(long3.bothF$mer) #
plot(long3.bothF$gam)

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag3par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")






#age 4------


long4.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                     # forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag4par )
gam.check(long4.bothF$gam) #not bad
summary(long4.bothF$gam) #
summary(long4.bothF$mer) #
AIC_4longbothF <- AIC(long4.bothF$mer) #
plot(long4.bothF$gam)

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag4par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 5------


long5.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                    #  forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag5par )
gam.check(long5.bothF$gam) #not bad
summary(long5.bothF$gam) #
summary(long5.bothF$mer) #
AIC_5longbothF <- AIC(long5.bothF$mer) #
plot(long5.bothF$gam)

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag5par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")










#age 6------


long6.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag6par )
gam.check(long6.bothF$gam) #not bad
summary(long6.bothF$gam) #
summary(long6.bothF$mer) #
AIC_6longbothF <- AIC(long6.bothF$mer) #
plot(long6.bothF$gam)

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag6par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 7------


long7.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag7par )
gam.check(long7.bothF$gam) #not bad
summary(long7.bothF$gam) #
summary(long7.bothF$mer) #
AIC_7longbothF <- AIC(long7.bothF$mer) #
plot(long7.bothF$gam)

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag7par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 8------


long8.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag8par )
gam.check(long8.bothF$gam) #not bad
summary(long8.bothF$gam) #
summary(long8.bothF$mer) #
AIC_8longbothF <- AIC(long8.bothF$mer) #
plot(long8.bothF$gam)

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag8par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")







#age 9------


long9.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                      mean_weight_parentF_scaled + 
                      prevyr_prevage_F_scaled +
                      pollock_survey_abun_mil_at_age_scaled +
                      apex_pred_biom_scaled + 
                      #forage_fish_biom_scaled + 
                      pelagic_forager_biom_scaled +
                      s(cohort, bs="re"),
                    random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag9par )
gam.check(long9.bothF$gam) #not bad
summary(long9.bothF$gam) #
summary(long9.bothF$mer) #
AIC_9longbothF <- AIC(long9.bothF$mer) #
plot(long9.bothF$gam)

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag9par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")









#age 10------


long10.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       #forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag10par )
gam.check(long10.bothF$gam) #not bad
summary(long10.bothF$gam) #
summary(long10.bothF$mer) #
AIC_10longbothF <- AIC(long10.bothF$mer) #
plot(long10.bothF$gam)

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag10par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")








#age 11------


long11.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                      # forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag11par )
gam.check(long11.bothF$gam) #not bad
summary(long11.bothF$gam) #
summary(long11.bothF$mer) #
AIC_11longbothF <- AIC(long11.bothF$mer) #
plot(long11.bothF$gam)

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag11par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag11par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")



#age 12------


long12.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       #forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag12par )
gam.check(long12.bothF$gam) #not bad
summary(long12.bothF$gam) #
summary(long12.bothF$mer) #
AIC_12longbothF <- AIC(long12.bothF$mer) #
plot(long12.bothF$gam)

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag12par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag12par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 13------


long13.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                       #forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag13par )
gam.check(long13.bothF$gam) #not bad
summary(long13.bothF$gam) #
summary(long13.bothF$mer) #
AIC_13longbothF <- AIC(long13.bothF$mer) #
plot(long13.bothF$gam)

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag13par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag13par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")






#age 14------


long14.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                     #  forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag14par )
gam.check(long14.bothF$gam) #not bad
summary(long14.bothF$gam) #
summary(long14.bothF$mer) #
AIC_14longbothF <- AIC(long14.bothF$mer) #
plot(long14.bothF$gam)

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag14par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag14par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")





#age 15------


long15.bothF <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                       mean_weight_parentF_scaled + 
                       prevyr_prevage_F_scaled +
                       pollock_survey_abun_mil_at_age_scaled +
                       apex_pred_biom_scaled + 
                      # forage_fish_biom_scaled + 
                       pelagic_forager_biom_scaled +
                       s(cohort, bs="re"),
                     random=~(1|YEAR/HAUL),  REML=FALSE,  data=lag15par )
gam.check(long15.bothF$gam) #not bad
summary(long15.bothF$gam) #
summary(long15.bothF$mer) #
AIC_15longbothF <- AIC(long15.bothF$mer) #
plot(long15.bothF$gam)

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag15par, aes(prevyr_prevage_F, LENGTH)) + geom_point() + geom_smooth(method="lm")


ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH, colour=as.factor(YEAR))) + geom_point() 

ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth()

ggplot(lag15par, aes(mean_weight_parentF_scaled, LENGTH)) + geom_point() + geom_smooth(method="lm")



#again w REML----------------------------------------------------------------------------------------

#
#age 1------

#prev yr prev age does NOT work for age 1 so drop
long1.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           # prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag1par )
gam.check(long1.bothF_REML$gam) #not bad
summary(long1.bothF_REML$gam) #
summary(long1.bothF_REML$mer) #
plot(long1.bothF_REML$gam)


#age 2------

long2.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag2par )
gam.check(long2.bothF_REML$gam) #not bad
summary(long2.bothF_REML$gam) #
summary(long2.bothF_REML$mer) #
plot(long2.bothF_REML$gam)




#age 3------

long3.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag3par )
gam.check(long3.bothF_REML$gam) #not bad
summary(long3.bothF_REML$gam) #
summary(long3.bothF_REML$mer) #
plot(long3.bothF_REML$gam)




#age 4------

long4.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                         #  forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag4par )
gam.check(long4.bothF_REML$gam) #not bad
summary(long4.bothF_REML$gam) #
summary(long4.bothF_REML$mer) #
plot(long4.bothF_REML$gam)






#age 5------

long5.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                        #   forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag5par )
gam.check(long5.bothF_REML$gam) #not bad
summary(long5.bothF_REML$gam) #
summary(long5.bothF_REML$mer) #
plot(long5.bothF_REML$gam)






#age 6------

long6.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag6par )
gam.check(long6.bothF_REML$gam) #not bad
summary(long6.bothF_REML$gam) #
summary(long6.bothF_REML$mer) #
plot(long6.bothF_REML$gam)







#age 7------

long7.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                         #  forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag7par )
gam.check(long7.bothF_REML$gam) #not bad
summary(long7.bothF_REML$gam) #
summary(long7.bothF_REML$mer) #
plot(long7.bothF_REML$gam)




#age 8------

long8.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag8par )
gam.check(long8.bothF_REML$gam) #not bad
summary(long8.bothF_REML$gam) #
summary(long8.bothF_REML$mer) #
plot(long8.bothF_REML$gam)






#age 9------

long9.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                           mean_weight_parentF_scaled + 
                           prevyr_prevage_F_scaled +
                           pollock_survey_abun_mil_at_age_scaled +
                           apex_pred_biom_scaled + 
                          # forage_fish_biom_scaled + 
                           pelagic_forager_biom_scaled +
                           s(cohort, bs="re"),
                         random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag9par )
gam.check(long9.bothF_REML$gam) #not bad
summary(long9.bothF_REML$gam) #
summary(long9.bothF_REML$mer) #
plot(long9.bothF_REML$gam)




#age 10------

long10.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                           # forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag10par )
gam.check(long10.bothF_REML$gam) #not bad
summary(long10.bothF_REML$gam) #
summary(long10.bothF_REML$mer) #
plot(long10.bothF_REML$gam)







#age 11------

long11.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                         #   forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag11par )
gam.check(long11.bothF_REML$gam) #not bad
summary(long11.bothF_REML$gam) #
summary(long11.bothF_REML$mer) #
plot(long11.bothF_REML$gam)


#age 12------

long12.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                           # forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag12par )
gam.check(long12.bothF_REML$gam) #not bad
summary(long12.bothF_REML$gam) #
summary(long12.bothF_REML$mer) #
plot(long12.bothF_REML$gam)




#age 13------

long13.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                          #  forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag13par )
gam.check(long13.bothF_REML$gam) #not bad
summary(long13.bothF_REML$gam) #
summary(long13.bothF_REML$mer) #
plot(long13.bothF_REML$gam)




#age 14------


long14.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                           # forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag14par )
gam.check(long14.bothF_REML$gam) #not bad
summary(long14.bothF_REML$gam) #
summary(long14.bothF_REML$mer) #
plot(long14.bothF_REML$gam)




#age 15------

long15.bothF_REML <- gamm4(length_scaled ~  s(south.sst.amj.scaled, k=4) + t2(LONGITUDE, LATITUDE) + s(julian_scaled, k = 4) +
                            mean_weight_parentF_scaled + 
                            prevyr_prevage_F_scaled +
                            pollock_survey_abun_mil_at_age_scaled +
                            apex_pred_biom_scaled + 
                           # forage_fish_biom_scaled + 
                            pelagic_forager_biom_scaled +
                            s(cohort, bs="re"),
                          random=~(1|YEAR/HAUL),  REML=TRUE,  data=lag15par )
gam.check(long15.bothF_REML$gam) #not bad
summary(long15.bothF_REML$gam) #
summary(long15.bothF_REML$mer) # #
plot(long15.bothF_REML$gam)













