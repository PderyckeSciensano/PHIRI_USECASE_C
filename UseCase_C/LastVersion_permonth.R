# install.packages("epiR")
# install.packages("logisticRR")
# install.packages("data.table")

library(epiR)
library(logisticRR)
library(data.table)
library(plyr)
library(dplyr)

#indicate here the file location, complying to the common data model specifications
# setwd(dir="")
babies<-read.csv2("usecaseC_synthetic.csv",sep = '|')

#change names of variable for countries who have deprivation score
names(babies)[names(babies) == "SES"] <- "SES_ED"
# for Ireland
names(babies)[names(babies) == "SES_OccM"] <- "SES_ED"

babies<-babies[, c("COUNTRY", "GA", "BW", "NNM", "IM", "MULT_B", "VITAL", "MATAGE_B", "PARITY_B", 
                   "PRES", "PREVCS", "MOD", "INSTRUMENT", "TYPECESAR", "ONSET", "SEX", 
                   "year", "month", "day","SES_ED" ) ]


###################################################################################

######      For data collection per month         #################################

###################################################################################

#####  number of births, stillbirths rate per GA ##########

alive_22<-babies %>% filter(GA >= 22, GA!=99, VITAL==4) %>% group_by(year, month) %>% tally()
names(alive_22)[names(alive_22) == "n"] <- "nlive_22"
alive_24<-babies %>% filter(GA >= 24, GA!=99, VITAL==4) %>% group_by(year, month) %>% tally()
names(alive_24)[names(alive_24) == "n"] <- "nlive_24"
alive_28<-babies %>% filter(GA >= 28, GA!=99, VITAL==4) %>% group_by(year, month) %>% tally()
names(alive_28)[names(alive_28) == "n"] <- "nlive_28"
sb_22<-babies %>% filter(GA >= 22, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, month) %>% tally()
names(sb_22)[names(sb_22) == "n"] <- "sb_22"
sb_24<-babies %>% filter(GA >= 24, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, month) %>% tally()
names(sb_24)[names(sb_24) == "n"] <- "sb_24"
sb_28<-babies %>% filter(GA >= 28, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, month) %>% tally()
names(sb_28)[names(sb_28) == "n"] <- "sb_28"
b1<-merge(alive_22, alive_24, by=c("year", "month"))
b2<-merge(b1, alive_28, by=c("year", "month"))
b3<-merge(sb_22, sb_24, by=c("year", "month"))
b4<-merge(b3, sb_28, by=c("year", "month"))
b5<-merge(b2, b4, by=c("year", "month"), all.x=TRUE)

#####   neonatal mortality 22weeks and 24weeks of gestation and denominators   ##############

nnm1_22<-babies %>% filter(GA >= 22, GA!=99, (NNM==1 | NNM==98)) %>% group_by(year, month) %>% tally()
names(nnm1_22)[names(nnm1_22) == "n"] <- "early_nnd22"
nnm2_22<-babies %>% filter(GA >= 22, GA!=99, (NNM==2 | NNM==98)) %>% group_by(year, month) %>% tally()
names(nnm2_22)[names(nnm2_22) == "n"] <- "late_nnd22"
nnm1_24<-babies %>% filter(GA >= 24, GA!=99, (NNM==1 | NNM==98)) %>% group_by(year, month) %>% tally()
names(nnm1_24)[names(nnm1_24) == "n"] <- "early_nnd24"
nnm2_24<-babies %>% filter(GA >= 24, GA!=99, (NNM==2 | NNM==98)) %>% group_by(year, month) %>% tally()
names(nnm2_24)[names(nnm2_24) == "n"] <- "late_nnd24"
b6<-merge(nnm1_22, nnm2_22, by=c("year", "month"), all.x=TRUE)
b7<-merge(nnm1_24, nnm2_24, by=c("year", "month"), all.x=TRUE)
b8<-merge(b6, b7, by=c("year", "month"))
b9<-merge(b5, b8, by=c("year", "month"), all.x=TRUE)
den_for_nnm22<-babies %>% filter(NNM!=99) %>% group_by(year, month) %>% tally()
names(den_for_nnm22)[names(den_for_nnm22) == "n"] <- "den_for_nnm22"
den_for_nnm24<-babies %>% filter(GA >= 24, NNM!=99) %>% group_by(year, month) %>% tally()
names(den_for_nnm24)[names(den_for_nnm24) == "n"] <- "den_for_nnm24"

######      preterm live singleton birth       #####################################################

prema_all<-babies %>% filter(GA >= 22, GA<37, VITAL==4, MULT_B==1) %>% group_by(year, month) %>% tally()
names(prema_all)[names(prema_all) == "n"] <- "prema_singletons"
very_prema<-babies %>% filter(GA >= 22, GA<32, VITAL==4, MULT_B==1) %>% group_by(year, month) %>% tally()
names(very_prema)[names(very_prema) == "n"] <- "extreme_prema_singletons"
b10<-merge(prema_all, very_prema, by=c("year", "month"), all.x=TRUE)
b11<-merge(b9, b10, by=c("year", "month"), all.x=TRUE)


###############            live singleton with birth weight(<2500 grams)     ##################################

lbw<-babies %>% filter(GA >= 22, GA!=99, VITAL==4, MULT_B==1, BW<2500, BW!=99) %>% group_by(year, month) %>% tally()
names(lbw)[names(lbw) == "n"] <- "lbw"
b12<-merge(b11, lbw, by=c("year", "month"), all.x=TRUE)

###########       mothers <25 years and >=35 years and denominator            ################################

mum_young_all<-babies %>% filter(GA >= 22, GA!=99, MATAGE_B<25) %>% group_by(year, month) %>% tally()
names(mum_young_all)[names(mum_young_all) == "n"] <- "below25y_all"
mum_old_all<-babies %>% filter(GA >= 22, GA!=99,  MATAGE_B>=35) %>% group_by(year, month) %>% tally()
names(mum_old_all)[names(mum_old_all) == "n"] <- "above35y_all"
b13<-merge(mum_young_all, mum_old_all, by=c("year", "month"))
b13b<-merge(b12, b13, by=c("year", "month"))

mum_young_prema<-babies %>% filter(GA >= 22, GA<37, MATAGE_B<25) %>% group_by(year, month) %>% tally()
names(mum_young_prema)[names(mum_young_prema) == "n"] <- "below25y_prema"
mum_old_prema<-babies %>% filter(GA >= 22, GA<37,  MATAGE_B>=35) %>% group_by(year, month) %>% tally()
names(mum_old_prema)[names(mum_old_prema) == "n"] <- "above35y_prema"
mum_young_term<-babies %>% filter(GA >= 22, GA>=37, GA!=99, MATAGE_B<25) %>% group_by(year, month) %>% tally()
names(mum_young_term)[names(mum_young_term) == "n"] <- "below25y_term"
mum_old_term<-babies %>% filter(GA >= 22, GA>=37, GA!=99, MATAGE_B>=35) %>% group_by(year, month) %>% tally()
names(mum_old_term)[names(mum_old_term) == "n"] <- "above35y_term"
b13c<-merge(mum_young_prema, mum_old_prema, by=c("year", "month"), all.y=TRUE)
b13d<-merge(mum_young_term, mum_old_term, by=c("year", "month"), all.y=TRUE)
b13e<-merge(b13c, b13d, by=c("year", "month"), all.y=TRUE)
b14<-merge(b13b, b13e, by=c("year", "month"), all.x=TRUE)

den_for_agemat<-babies %>% filter( MATAGE_B!=99) %>% group_by(year, month) %>% tally()
names(den_for_agemat)[names(den_for_agemat) == "n"] <- "den_for_agemat"


################           primiparity and denominator         #######################################

primi<-babies %>% filter(GA >= 22, GA!=99, (PARITY_B==0 | PARITY_B==98)) %>% group_by(year, month) %>% tally()
names(primi)[names(primi) == "n"] <- "primiparity"
primi_prema<-babies %>% filter(GA >= 22, GA<37, (PARITY_B==0| PARITY_B==98)) %>% group_by(year, month) %>% tally()
names(primi_prema)[names(primi_prema) == "n"] <- "primi_prema"
primi_term<-babies %>% filter(GA >= 22, GA>=37, GA!=99, (PARITY_B==0| PARITY_B==98)) %>% group_by(year, month) %>% tally()
names(primi_term)[names(primi_term) == "n"] <- "primi_term"
b14b<-merge(primi_prema, primi_term, by=c("year", "month"))
b14c<-merge(primi, b14b, by=c("year", "month"))
b14d<-merge(b14, b14c, by=c("year", "month"))

den_for_parity<-babies %>% filter( PARITY_B!=99) %>% group_by(year, month) %>% tally()
names(den_for_parity)[names(den_for_parity) == "n"] <- "den_for_parity"

##############              multiple births and denominator               #####################################

multiple<-babies %>% filter(GA >= 22, GA!=99, MULT_B!=1, MULT_B!=99) %>% group_by(year, month) %>% tally()
names(multiple)[names(multiple) == "n"] <- "mult"
multiple_prema<-babies %>% filter(GA >= 22, GA<37, MULT_B!=1, MULT_B!=99) %>% group_by(year, month) %>% tally()
names(multiple_prema)[names(multiple_prema) == "n"] <- "mult_prema"
multiple_term<-babies %>% filter(GA >= 22, GA>=37, GA!=99, MULT_B!=1, MULT_B!=99) %>% group_by(year, month) %>% tally()
names(multiple_term)[names(multiple_term) == "n"] <- "mult_term"
b15<-merge(multiple, multiple_prema, by=c("year", "month"), all.x=TRUE)
b15b<-merge(b15, multiple_term, by=c("year", "month"), all.x=TRUE)

b16<-merge(b14d, b15b, by=c("year", "month"), all.x=TRUE)

den_for_multiple<-babies %>% filter( MULT_B!=99) %>% group_by(year, month) %>% tally()
names(den_for_multiple)[names(den_for_multiple) == "n"] <- "den_for_multiple"

##################################################################################################################
###########     cesarean, CS prelabor, CS intrapartum, CS with beginning of labor spontaneous
#######     CS with labor induced, and denominators
#######    vaginal deliveries with labor induced and instrumental deliveries, and denominators ##################

cs<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98)) %>% group_by(year, month) %>% tally()
names(cs)[names(cs) == "n"] <- "CS"
cs_pre<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==1 | TYPECESAR==98)) %>% group_by(year, month) %>% tally()
names(cs_pre)[names(cs_pre) == "n"] <- "CS_prelabour"
cs_intra<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2| TYPECESAR==98)) %>% group_by(year, month) %>% tally()
names(cs_intra)[names(cs_intra) == "n"] <- "CS_intrapartum"
cs_planned<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2| TYPECESAR==98), ONSET!=1) %>% group_by(year, month) %>% tally()
names(cs_planned)[names(cs_planned) == "n"] <- "CS_and_onsetnonspont"
cs_afterlabour<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2| TYPECESAR==98), (ONSET==1 | ONSET==98), ONSET!=99) %>% group_by(year, month) %>% tally()
names(cs_afterlabour)[names(cs_afterlabour) == "n"] <- "CS_afterlabour_spont"
induction<-babies %>% filter(GA >= 22, GA!=99, (ONSET==2 | ONSET==98)) %>% group_by(year, month) %>% tally()
names(induction)[names(induction) == "n"] <- "induced_vagdel"
instru<-babies %>% filter(GA >= 22, GA!=99, (MOD==1 | MOD==98), (INSTRUMENT==2 | INSTRUMENT==98)) %>% group_by(year, month) %>% tally()
names(instru)[names(instru) == "n"] <- "instrumental"
b17<-merge(b16, cs, by=c("year", "month"))
b17_2<-merge(b17, cs_pre, by=c("year", "month"))
b18<-merge(b17_2, cs_intra, by=c("year", "month"))
b19<-merge(cs_planned, cs_afterlabour, by=c("year", "month"))
b21<-merge(induction, instru, by=c("year", "month"))
b22<-merge(b18, b19, by=c("year", "month"))
b23<-merge(b22, b21, by=c("year", "month"))

den_for_mod<-babies %>% filter( MOD!=99) %>% group_by(year, month) %>% tally()
names(den_for_mod)[names(den_for_mod) == "n"] <- "den_for_mod"
den_for_timecs<-babies %>% filter(MOD!=99, TYPECESAR!=99) %>% group_by(year, month) %>% tally()
names(den_for_timecs)[names(den_for_timecs) == "n"] <- "den_for_timecs"
den_for_onsetcs<-babies %>% filter( MOD!=99, TYPECESAR!=99, ONSET!=99) %>% group_by(year, month) %>% tally()
names(den_for_onsetcs)[names(den_for_onsetcs) == "n"] <- "den_for_onsetcs"
den_for_onset<-babies %>% filter( ONSET!=99) %>% group_by(year, month) %>% tally()
names(den_for_onset)[names(den_for_onset) == "n"] <- "den_for_onset"
den_for_inst<-babies %>% filter( INSTRUMENT!=99) %>% group_by(year, month) %>% tally()
names(den_for_inst)[names(den_for_inst) == "n"] <- "den_for_inst"

#####################################################################################################

##################     calcul of Small for Gestational Age (SGA)      ###############################

###################################################################################################

percentiles<-babies[ , c("COUNTRY","year","month", "GA","BW", "SEX", "MULT_B","VITAL", "SES_ED")]
percentiles$BW[percentiles$BW==99]<-NA
#exclusion of TOP, stillbirths and multiples
percentiles<-percentiles[percentiles$VITAL==4 & percentiles$GA>=22  & percentiles$GA!=99 & percentiles$MULT_B==1, ]
#girls group
percentilesgirl<-percentiles[percentiles$SEX==2 , ]
#boys group
percentilesboy<-percentiles[percentiles$SEX==1, ]

###############     calcul of SGA and LGA for girls          ############################################

girls40sa<-percentilesgirl[percentilesgirl$GA==40,]
MCgirl<-mean(girls40sa$BW,na.rm=TRUE)
SDgirl<-sd(girls40sa$BW,na.rm=TRUE)
percentilesgirl<-mutate(percentilesgirl, 
                        P50g=(exp(0.578+0.332*(percentilesgirl$GA+0.5)- 0.00354*((percentilesgirl$GA+0.5)*
                                                                                   (percentilesgirl$GA+0.5)))*MCgirl)/3705)
percentilesgirl<-mutate(percentilesgirl, P10g=percentilesgirl$P50g*(1-(1.28*(SDgirl/MCgirl))))
percentilesgirl<-mutate(percentilesgirl, P90g=percentilesgirl$P50g*(1+(1.28*(SDgirl/MCgirl))))

percentilesgirl$SGA[percentilesgirl$BW<percentilesgirl$P10g & percentilesgirl$GA>=24]<-1
percentilesgirl$LGA[percentilesgirl$BW>percentilesgirl$P90g & percentilesgirl$GA>=24]<-1
perc_girl1<-percentilesgirl[percentilesgirl$SGA==1 & is.na(percentilesgirl$SGA)==FALSE, ]
perc_girl<-perc_girl1[, c("year", "month", "GA", "SGA", "SES_ED", "SEX")]
perc_girl_lga1<-percentilesgirl[percentilesgirl$LGA==1 & is.na(percentilesgirl$LGA)==FALSE, ]
perc_girl_lga<-perc_girl_lga1[, c("year", "month", "GA", "LGA", "SES_ED", "SEX")]

#####################        calcul of SGA and LGA for boys     #############################################

boys40sa<-percentilesboy[percentilesboy$GA==40,]
MCboy<-mean(boys40sa$BW,na.rm=TRUE)
SDboy<-sd(boys40sa$BW,na.rm=TRUE)
percentilesboy<-mutate(percentilesboy, 
                       P50g=(exp(0.578+0.332*(percentilesboy$GA+0.5)- 0.00354*((percentilesboy$GA+0.5)*
                                                                                 (percentilesboy$GA+0.5)))*MCboy)/3705)
percentilesboy<-mutate(percentilesboy, P10g=percentilesboy$P50g*(1-(1.28*(SDboy/MCboy))))
percentilesboy<-mutate(percentilesboy, P90g=percentilesboy$P50g*(1+(1.28*(SDboy/MCboy))))

percentilesboy$SGA[percentilesboy$BW<percentilesboy$P10g & percentilesboy$GA>=24]<-1
percentilesboy$SGA[percentilesboy$BW<percentilesboy$P10g & percentilesboy$GA>=24]<-1
percentilesboy$LGA[percentilesboy$BW>percentilesboy$P90g & percentilesboy$GA>=24]<-1
perc_boy1<-percentilesboy[percentilesboy$SGA==1 & is.na(percentilesboy$SGA)==FALSE, ]
perc_boy<-perc_boy1[, c("year", "month", "GA", "SGA", "SES_ED", "SEX")]
perc_boy_lga1<-percentilesboy[percentilesboy$LGA==1 & is.na(percentilesboy$LGA)==FALSE, ]
perc_boy_lga<-perc_boy_lga1[, c("year", "month", "GA", "LGA", "SES_ED", "SEX")]

b18A<- percentiles %>% group_by(year, month) %>% tally()
names(b18A)[names(b18A) == "n"] <- "n_live_singleton_all"
b19A<- percentiles %>% filter(GA>=37, GA!=99) %>% group_by(year, month) %>% tally()
names(b19A)[names(b19A) == "n"] <- "n_live_singleton_term"
b20<-rbind.fill(perc_girl,perc_boy)
den_sga<-percentiles %>% filter(SEX!=99, GA!=99, BW!=99) %>% group_by(year, month) %>% tally()
names(den_sga)[names(den_sga) == "n"] <- "den_sga"
sga_prema<-b20 %>% filter(GA>=22, GA<37, SGA==1) %>% group_by(year, month) %>% tally()
names(sga_prema)[names(sga_prema) == "n"] <- "n_sga_prema"
sga_term<-b20 %>% filter(GA>=37, GA!=99, SGA==1) %>% group_by(year, month) %>% tally()
names(sga_term)[names(sga_term) == "n"] <- "n_sga_term"

####### create the final table
b24<-merge(den_sga, sga_prema, by=c("year", "month"), all.x=TRUE)
b25<-merge(b24, sga_term, by=c("year", "month"), all.x=TRUE)
b21A<-merge(b18A,b19A,by=c("year", "month") )

b27<-merge(b23, b25, by=c("year", "month"), all.x=TRUE)
fin<-merge(b27, b21A, by=c("year", "month"))

denNNM<-merge(den_for_nnm22, den_for_nnm24, by=c("year", "month"))
denmother1<-merge(den_for_agemat, den_for_parity, by=c("year", "month"))
denmother2<-merge(den_for_multiple, den_for_mod, by=c("year", "month"))
dendeliv<-merge(den_for_timecs, den_for_onsetcs, by=c("year", "month"))
dendeliv2<-merge(den_for_onset, den_for_inst, by=c("year", "month"))

den1<-merge(denNNM, denmother1, by=c("year", "month"))
den2<-merge(denmother2, dendeliv, by=c("year", "month"))
den3<-merge(den1, den2, by=c("year", "month"))
den4<-merge(den3, dendeliv2, by=c("year", "month"))

final<-merge(fin, den4, by=c("year", "month"))
write.csv2(final, "indicators_per_month2.csv")


#####################################################################################

########                collect per year SGA LGA                    ################

###################################################################################


b30<-rbind.fill(perc_girl,perc_boy)
sga_per_year<-b30 %>% filter(GA>=22, GA!=99, SGA==1) %>% group_by(year) %>% tally()
names(sga_per_year)[names(sga_per_year) == "n"] <- "n_sga_all"
b31<-rbind.fill(perc_girl_lga,perc_boy_lga)
lga_per_year<-b31 %>% filter(GA>=22, GA!=99, LGA==1) %>% group_by(year) %>% tally()
names(lga_per_year)[names(lga_per_year) == "n"] <- "n_lga_all"

b32<-merge(sga_per_year,lga_per_year, by=c("year"))
write.csv2(b32, "sga_lga_per_year.csv")


############################################################################################

#########              statistics on birth weight                               ############

##########################################################################################


myresults<-matrix(ncol=7,nrow=38)
for(j in 1:19){
  a<-subset(percentilesgirl, GA==j+23)
  myresults[j,1]<-j+23
  myresults[j,2]<-2
  myresults[j,3]<-mean(a$BW, na.rm=TRUE)
  myresults[j,4]<-sd(a$BW, na.rm=TRUE)
  myresults[j,5]<-median(a$BW, na.rm=TRUE)
  myresults[j,6]<-quantile(a$BW, probs=0.25, na.rm=TRUE)
  myresults[j,7]<-quantile(a$BW, probs=0.75, na.rm=TRUE)
}
for(j in 20:38){
  b<-subset(percentilesboy, GA==j+4)
  myresults[j,1]<-j+4
  myresults[j,2]<-1
  myresults[j,3]<-mean(b$BW, na.rm=TRUE)
  myresults[j,4]<-sd(b$BW, na.rm=TRUE)
  myresults[j,5]<-median(b$BW, na.rm=TRUE)
  myresults[j,6]<-quantile(b$BW, probs=0.25, na.rm=TRUE)
  myresults[j,7]<-quantile(b$BW, probs=0.75, na.rm=TRUE)
}

output<-data.frame(myresults)
colnames(output)<-c("GA","Sex","Mean","SD","Median","Q1","Q3")

write.csv2(output, "stat_birthweight.csv")


##############################################################################

#######          collect per year according to SES                ############

ses_alive_22<-babies %>% filter(GA >= 22, GA!=99, VITAL==4) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_22)[names(ses_alive_22) == "n"] <- "nlive_22"
ses_alive_24<-babies %>% filter(GA >= 24, GA!=99, VITAL==4) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_24)[names(ses_alive_24) == "n"] <- "nlive_24"
ses_alive_28<-babies %>% filter(GA >= 28, GA!=99, VITAL==4) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_28)[names(ses_alive_28) == "n"] <- "nlive_28"
ses_sb_22<-babies %>% filter(GA >= 22, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_22)[names(ses_sb_22) == "n"] <- "sb_22"
ses_sb_24<-babies %>% filter(GA >= 24, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_24)[names(ses_sb_24) == "n"] <- "sb_24"
ses_sb_28<-babies %>% filter(GA >= 28, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_28)[names(ses_sb_28) == "n"] <- "sb_28"
ses_b1<-merge(ses_alive_22, ses_alive_24, by=c("year", "SES_ED"))
ses_b2<-merge(ses_b1, ses_alive_28, by=c("year",  "SES_ED"))
ses_b3<-merge(ses_sb_22, ses_sb_24, by=c("year",  "SES_ED"))
ses_b4<-merge(ses_b3, ses_sb_28, by=c("year",  "SES_ED"))
ses_b5<-merge(ses_b2, ses_b4, by=c("year", "SES_ED"), all.x=TRUE)
ses_nnm1_22<-babies %>% filter(GA >= 22, GA!=99, NNM==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm1_22)[names(ses_nnm1_22) == "n"] <- "early_nnd22"
ses_nnm2_22<-babies %>% filter(GA >= 22, GA!=99, NNM==2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm2_22)[names(ses_nnm2_22) == "n"] <- "late_nnd22"
ses_nnm1_24<-babies %>% filter(GA >= 24, GA!=99, NNM==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm1_24)[names(ses_nnm1_24) == "n"] <- "early_nnd24"
ses_nnm2_24<-babies %>% filter(GA >= 24, GA!=99, NNM==2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm2_24)[names(ses_nnm2_24) == "n"] <- "late_nnd24"
ses_b6<-merge(ses_nnm1_22, ses_nnm2_22, by=c("year", "SES_ED"))
ses_b7<-merge(ses_nnm1_24, ses_nnm2_24, by=c("year", "SES_ED"))
ses_b8<-merge(ses_b6, ses_b7, by=c("year", "SES_ED"))
ses_b9<-merge(ses_b5, ses_b8, by=c("year", "SES_ED"), all.x=TRUE)

ses_prema_all<-babies %>% filter(GA >= 22, GA<37, VITAL==4, MULT_B==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_prema_all)[names(ses_prema_all) == "n"] <- "prema_singletons"
ses_very_prema<-babies %>% filter(GA >= 22, GA<32, VITAL==4, MULT_B==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_very_prema)[names(ses_very_prema) == "n"] <- "extreme_prema_singletons"
ses_b10<-merge(ses_prema_all, ses_very_prema, by=c("year", "SES_ED"))
ses_b11<-merge(ses_b9, ses_b10, by=c("year", "SES_ED"))

ses_sga_all<-b20 %>% filter(GA>=22, GA!=99, SGA==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_all)[names(ses_sga_all) == "n"] <- "n_sga_all"
ses_sga_prema<-b20 %>% filter(GA>=22, GA<37, SGA==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_prema)[names(ses_sga_prema) == "n"] <- "n_sga_prema"
ses_sga_term<-b20 %>% filter(GA>=37, GA!=99, SGA==1) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_term)[names(ses_sga_term) == "n"] <- "n_sga_term"
ses_b24<-merge(ses_sga_prema,ses_sga_term, by=c("year", "SES_ED"))
ses_b25<-merge(ses_sga_all,ses_b24, by=c("year", "SES_ED"))

ses_cs<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98)) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs)[names(ses_cs) == "n"] <- "CS"
ses_cs_pre<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==1 | TYPECESAR==98)) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs_pre)[names(ses_cs_pre) == "n"] <- "CS_prelabour"
ses_cs_intra<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2 | TYPECESAR==98)) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs_intra)[names(ses_cs_intra) == "n"] <- "CS_intrapartum"
ses_b17<-merge(ses_b11, ses_cs, by=c("year", "SES_ED"))
ses_b18<-merge(ses_cs_pre, ses_cs_intra, by=c("year", "SES_ED"))
ses_b19<-merge(ses_b17, ses_b18, by=c("year","SES_ED"))

den_sga<- percentiles %>% filter(SEX!=99, GA!=99, BW!=99) %>% group_by(year, SES_ED) %>% tally()
names(den_sga)[names(den_sga) == "n"] <- "n_live_singleton_all"
den_cs<- babies %>% filter(GA >= 22, GA!=99, MOD!=99) %>% group_by(year, SES_ED) %>% tally()
names(den_cs)[names(den_cs) == "n"] <- "n_mod_delivery"

den5<- merge(den_sga,den_cs,by=c("year", "SES_ED"))
final_temp<-merge(ses_b19, ses_b25, by=c("year","SES_ED"), all.x=TRUE)
final_ses<-merge(final_temp, den5, by=c("year","SES_ED"))
write.csv2(final_ses, "indicators_per_ses.csv")


######  same collect but per period of risk(=Mars-December) according to SES    #############################

ses_alive_22<-babies %>% filter(GA >= 22, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_22)[names(ses_alive_22) == "n"] <- "nlive_22"
ses_alive_24<-babies %>% filter(GA >= 24, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_24)[names(ses_alive_24) == "n"] <- "nlive_24"
ses_alive_28<-babies %>% filter(GA >= 28, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_alive_28)[names(ses_alive_28) == "n"] <- "nlive_28"
ses_sb_22<-babies %>% filter(GA >= 22, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_22)[names(ses_sb_22) == "n"] <- "sb_22"
ses_sb_24<-babies %>% filter(GA >= 24, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_24)[names(ses_sb_24) == "n"] <- "sb_24"
ses_sb_28<-babies %>% filter(GA >= 28, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sb_28)[names(ses_sb_28) == "n"] <- "sb_28"
ses_b1<-merge(ses_alive_22, ses_alive_24, by=c("year", "SES_ED"))
ses_b2<-merge(ses_b1, ses_alive_28, by=c("year",  "SES_ED"))
ses_b3<-merge(ses_sb_22, ses_sb_24, by=c("year",  "SES_ED"))
ses_b4<-merge(ses_b3, ses_sb_28, by=c("year",  "SES_ED"))
ses_b5<-merge(ses_b2, ses_b4, by=c("year", "SES_ED"), all.x=TRUE)
ses_nnm1_22<-babies %>% filter(GA >= 22, GA!=99, NNM==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm1_22)[names(ses_nnm1_22) == "n"] <- "early_nnd22"
ses_nnm2_22<-babies %>% filter(GA >= 22, GA!=99, NNM==2, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm2_22)[names(ses_nnm2_22) == "n"] <- "late_nnd22"
ses_nnm1_24<-babies %>% filter(GA >= 24, GA!=99, NNM==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm1_24)[names(ses_nnm1_24) == "n"] <- "early_nnd24"
ses_nnm2_24<-babies %>% filter(GA >= 24, GA!=99, NNM==2, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_nnm2_24)[names(ses_nnm2_24) == "n"] <- "late_nnd24"
ses_b6<-merge(ses_nnm1_22, ses_nnm2_22, by=c("year", "SES_ED"))
ses_b7<-merge(ses_nnm1_24, ses_nnm2_24, by=c("year", "SES_ED"))
ses_b8<-merge(ses_b6, ses_b7, by=c("year", "SES_ED"))
ses_b9<-merge(ses_b5, ses_b8, by=c("year", "SES_ED"), all.x=TRUE)

ses_prema_all<-babies %>% filter(GA >= 22, GA<37, VITAL==4, MULT_B==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_prema_all)[names(ses_prema_all) == "n"] <- "prema_singletons"
ses_very_prema<-babies %>% filter(GA >= 22, GA<32, VITAL==4, MULT_B==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_very_prema)[names(ses_very_prema) == "n"] <- "extreme_prema_singletons"
ses_b10<-merge(ses_prema_all, ses_very_prema, by=c("year", "SES_ED"))
ses_b11<-merge(ses_b9, ses_b10, by=c("year", "SES_ED"))

ses_sga_all<-b20 %>% filter(GA>=22, GA!=99, SGA==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_all)[names(ses_sga_all) == "n"] <- "n_sga_all"
ses_sga_prema<-b20 %>% filter(GA>=22, GA<37, SGA==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_prema)[names(ses_sga_prema) == "n"] <- "n_sga_prema"
ses_sga_term<-b20 %>% filter(GA>=37, GA!=99, SGA==1, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_sga_term)[names(ses_sga_term) == "n"] <- "n_sga_term"
ses_b24<-merge(ses_sga_prema,ses_sga_term, by=c("year", "SES_ED"))
ses_b25<-merge(ses_sga_all,ses_b24, by=c("year", "SES_ED"))

ses_cs<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs)[names(ses_cs) == "n"] <- "CS"
ses_cs_pre<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==1 | TYPECESAR==98), month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs_pre)[names(ses_cs_pre) == "n"] <- "CS_prelabour"
ses_cs_intra<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2 | TYPECESAR==98), month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(ses_cs_intra)[names(ses_cs_intra) == "n"] <- "CS_intrapartum"
ses_b17<-merge(ses_b11, ses_cs, by=c("year", "SES_ED"))
ses_b18<-merge(ses_cs_pre, ses_cs_intra, by=c("year", "SES_ED"))
ses_b19<-merge(ses_b17, ses_b18, by=c("year","SES_ED"))

den_sga<- percentiles %>% filter(SEX!=99, GA!=99, BW!=99, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(den_sga)[names(den_sga) == "n"] <- "n_live_singleton_all"
den_cs<- babies %>% filter(GA >= 22, GA!=99, MOD!=99, month!=1, month!=2) %>% group_by(year, SES_ED) %>% tally()
names(den_cs)[names(den_cs) == "n"] <- "n_mod_delivery"

den5<- merge(den_sga,den_cs,by=c("year", "SES_ED"))
final_temp<-merge(ses_b19, ses_b25, by=c("year","SES_ED"), all.x=TRUE)
final_ses<-merge(final_temp, den5, by=c("year","SES_ED"))
write.csv2(final_ses, "indicators_per_ses_withoutjanfeb.csv")


############################################################################################

######                  collect per year according to sex of baby     ######################

sex_alive_22<-babies %>% filter(GA >= 22, GA!=99, VITAL==4) %>% group_by(year, SEX) %>% tally()
names(sex_alive_22)[names(sex_alive_22) == "n"] <- "nlive_22"
sex_alive_24<-babies %>% filter(GA >= 24, GA!=99, VITAL==4) %>% group_by(year, SEX) %>% tally()
names(sex_alive_24)[names(sex_alive_24) == "n"] <- "nlive_24"
sex_alive_28<-babies %>% filter(GA >= 28, GA!=99, VITAL==4) %>% group_by(year, SEX) %>% tally()
names(sex_alive_28)[names(sex_alive_28) == "n"] <- "nlive_28"
sex_sb_22<-babies %>% filter(GA >= 22, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SEX) %>% tally()
names(sex_sb_22)[names(sex_sb_22) == "n"] <- "sb_22"
sex_sb_24<-babies %>% filter(GA >= 24, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SEX) %>% tally()
names(sex_sb_24)[names(sex_sb_24) == "n"] <- "sb_24"
sex_sb_28<-babies %>% filter(GA >= 28, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99) %>% group_by(year, SEX) %>% tally()
names(sex_sb_28)[names(sex_sb_28) == "n"] <- "sb_28"
sex_b1<-merge(sex_alive_22, sex_alive_24, by=c("year",  "SEX"))
sex_b2<-merge(sex_b1, sex_alive_28, by=c("year",  "SEX"))
sex_b3<-merge(sex_sb_22, sex_sb_24, by=c("year",  "SEX"))
sex_b4<-merge(sex_b3, sex_sb_28, by=c("year",  "SEX"))
sex_b5<-merge(sex_b2, sex_b4, by=c("year",  "SEX"), all.x=TRUE)
sex_nnm1_22<-babies %>% filter(GA >= 22, GA!=99, NNM==1) %>% group_by(year, SEX) %>% tally()
names(sex_nnm1_22)[names(sex_nnm1_22) == "n"] <- "early_nnd22"
sex_nnm2_22<-babies %>% filter(GA >= 22, GA!=99, NNM==2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm2_22)[names(sex_nnm2_22) == "n"] <- "late_nnd22"
sex_nnm1_24<-babies %>% filter(GA >= 24, GA!=99, NNM==1) %>% group_by(year, SEX) %>% tally()
names(sex_nnm1_24)[names(sex_nnm1_24) == "n"] <- "early_nnd24"
sex_nnm2_24<-babies %>% filter(GA >= 24, GA!=99, NNM==2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm2_24)[names(sex_nnm2_24) == "n"] <- "late_nnd24"
sex_b6<-merge(sex_nnm1_22, sex_nnm2_22, by=c("year",  "SEX"))
sex_b7<-merge(sex_nnm1_24, sex_nnm2_24, by=c("year",  "SEX"))
sex_b8<-merge(sex_b6, sex_b7, by=c("year",  "SEX"))
sex_b9<-merge(sex_b5, sex_b8, by=c("year",  "SEX"), all.x=TRUE)

sex_prema_all<-babies %>% filter(GA >= 22, GA<37, VITAL==4, MULT_B==1) %>% group_by(year, SEX) %>% tally()
names(sex_prema_all)[names(sex_prema_all) == "n"] <- "prema_singletons"
sex_very_prema<-babies %>% filter(GA >= 22, GA<32, VITAL==4, MULT_B==1) %>% group_by(year, SEX) %>% tally()
names(sex_very_prema)[names(sex_very_prema) == "n"] <- "extreme_prema_singletons"
sex_b10<-merge(sex_prema_all, sex_very_prema, by=c("year",  "SEX"))
sex_b11<-merge(sex_b9, sex_b10, by=c("year",  "SEX"))

sex_sga_all<-b20 %>% filter(GA>=22, GA!=99, SGA==1) %>% group_by(year, SEX) %>% tally()
names(sex_sga_all)[names(sex_sga_all) == "n"] <- "n_sga_all"
sex_sga_prema<-b20 %>% filter(GA>=22, GA<37, SGA==1) %>% group_by(year, SEX) %>% tally()
names(sex_sga_prema)[names(sex_sga_prema) == "n"] <- "n_sga_prema"
sex_sga_term<-b20 %>% filter(GA>=37, GA!=99, SGA==1) %>% group_by(year, SEX) %>% tally()
names(sex_sga_term)[names(sex_sga_term) == "n"] <- "n_sga_term"
sex_b24<-merge(sex_sga_prema,sex_sga_term, by=c("year",  "SEX"))
sex_b25<-merge(sex_sga_all,sex_b24, by=c("year",  "SEX"))

sex_cs<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98)) %>% group_by(year, SEX) %>% tally()
names(sex_cs)[names(sex_cs) == "n"] <- "CS"
sex_cs_pre<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==1 | TYPECESAR==98)) %>% group_by(year, SEX) %>% tally()
names(sex_cs_pre)[names(sex_cs_pre) == "n"] <- "CS_prelabour"
sex_cs_intra<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2 | TYPECESAR==98)) %>% group_by(year, SEX) %>% tally()
names(sex_cs_intra)[names(sex_cs_intra) == "n"] <- "CS_intrapartum"
sex_b17<-merge(sex_b11, sex_cs, by=c("year",  "SEX"))
sex_b18<-merge(sex_cs_pre, sex_cs_intra, by=c("year",  "SEX"))
sex_b19<-merge(sex_b17, sex_b18, by=c("year",  "SEX"))

den_sga_sex<- percentiles %>% filter(SEX!=99, GA!=99, BW!=99)  %>% group_by(year, SEX) %>% tally()
names(den_sga_sex)[names(den_sga_sex) == "n"] <- "n_live_singleton_all"
den_cs_sex<- babies %>% filter(GA >= 22, GA!=99, MOD!=99) %>% group_by(year, SEX) %>% tally()
names(den_cs_sex)[names(den_cs_sex) == "n"] <- "n_mod_delivery"

den5<- merge(den_sga_sex,den_cs_sex,by=c("year", "SEX"))
final_temp<-merge(sex_b19, sex_b25, by=c("year","SEX"), all.x=TRUE)
final_sex<-merge(final_temp, den5, by=c("year","SEX"))
write.csv2(final_sex, "indicators_per_sex.csv")

#####  same collect per period of risk (march-december) according to sex of baby             ###################

sex_alive_22<-babies %>% filter(GA >= 22, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_alive_22)[names(sex_alive_22) == "n"] <- "nlive_22"
sex_alive_24<-babies %>% filter(GA >= 24, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_alive_24)[names(sex_alive_24) == "n"] <- "nlive_24"
sex_alive_28<-babies %>% filter(GA >= 28, GA!=99, VITAL==4, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_alive_28)[names(sex_alive_28) == "n"] <- "nlive_28"
sex_sb_22<-babies %>% filter(GA >= 22, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sb_22)[names(sex_sb_22) == "n"] <- "sb_22"
sex_sb_24<-babies %>% filter(GA >= 24, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sb_24)[names(sex_sb_24) == "n"] <- "sb_24"
sex_sb_28<-babies %>% filter(GA >= 28, GA!=99, VITAL!=4, VITAL!=1, VITAL!=99, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sb_28)[names(sex_sb_28) == "n"] <- "sb_28"
sex_b1<-merge(sex_alive_22, sex_alive_24, by=c("year",  "SEX"))
sex_b2<-merge(sex_b1, sex_alive_28, by=c("year",  "SEX"))
sex_b3<-merge(sex_sb_22, sex_sb_24, by=c("year",  "SEX"))
sex_b4<-merge(sex_b3, sex_sb_28, by=c("year",  "SEX"))
sex_b5<-merge(sex_b2, sex_b4, by=c("year",  "SEX"), all.x=TRUE)
sex_nnm1_22<-babies %>% filter(GA >= 22, GA!=99, NNM==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm1_22)[names(sex_nnm1_22) == "n"] <- "early_nnd22"
sex_nnm2_22<-babies %>% filter(GA >= 22, GA!=99, NNM==2, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm2_22)[names(sex_nnm2_22) == "n"] <- "late_nnd22"
sex_nnm1_24<-babies %>% filter(GA >= 24, GA!=99, NNM==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm1_24)[names(sex_nnm1_24) == "n"] <- "early_nnd24"
sex_nnm2_24<-babies %>% filter(GA >= 24, GA!=99, NNM==2, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_nnm2_24)[names(sex_nnm2_24) == "n"] <- "late_nnd24"
sex_b6<-merge(sex_nnm1_22, sex_nnm2_22, by=c("year",  "SEX"))
sex_b7<-merge(sex_nnm1_24, sex_nnm2_24, by=c("year",  "SEX"))
sex_b8<-merge(sex_b6, sex_b7, by=c("year",  "SEX"))
sex_b9<-merge(sex_b5, sex_b8, by=c("year",  "SEX"), all.x=TRUE)

sex_prema_all<-babies %>% filter(GA >= 22, GA<37, VITAL==4, MULT_B==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_prema_all)[names(sex_prema_all) == "n"] <- "prema_singletons"
sex_very_prema<-babies %>% filter(GA >= 22, GA<32, VITAL==4, MULT_B==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_very_prema)[names(sex_very_prema) == "n"] <- "extreme_prema_singletons"
sex_b10<-merge(sex_prema_all, sex_very_prema, by=c("year",  "SEX"))
sex_b11<-merge(sex_b9, sex_b10, by=c("year",  "SEX"))

sex_sga_all<-b20 %>% filter(GA>=22, GA!=99, SGA==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sga_all)[names(sex_sga_all) == "n"] <- "n_sga_all"
sex_sga_prema<-b20 %>% filter(GA>=22, GA<37, SGA==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sga_prema)[names(sex_sga_prema) == "n"] <- "n_sga_prema"
sex_sga_term<-b20 %>% filter(GA>=37, GA!=99, SGA==1, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_sga_term)[names(sex_sga_term) == "n"] <- "n_sga_term"
sex_b24<-merge(sex_sga_prema,sex_sga_term, by=c("year",  "SEX"))
sex_b25<-merge(sex_sga_all,sex_b24, by=c("year",  "SEX"))

sex_cs<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_cs)[names(sex_cs) == "n"] <- "CS"
sex_cs_pre<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==1 | TYPECESAR==98), month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_cs_pre)[names(sex_cs_pre) == "n"] <- "CS_prelabour"
sex_cs_intra<-babies %>% filter(GA >= 22, GA!=99, (MOD==2 | MOD==98), (TYPECESAR==2 | TYPECESAR==98), month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(sex_cs_intra)[names(sex_cs_intra) == "n"] <- "CS_intrapartum"
sex_b17<-merge(sex_b11, sex_cs, by=c("year",  "SEX"))
sex_b18<-merge(sex_cs_pre, sex_cs_intra, by=c("year",  "SEX"))
sex_b19<-merge(sex_b17, sex_b18, by=c("year",  "SEX"))

den_sga_sex<- percentiles  %>% filter(SEX!=99, GA!=99, BW!=99, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(den_sga_sex)[names(den_sga_sex) == "n"] <- "n_live_singleton_all"
den_cs_sex<- babies %>% filter(GA >= 22, GA!=99, MOD!=99, month!=1, month!=2) %>% group_by(year, SEX) %>% tally()
names(den_cs_sex)[names(den_cs_sex) == "n"] <- "n_mod_delivery"

den5<- merge(den_sga_sex,den_cs_sex,by=c("year", "SEX"))
final_temp<-merge(sex_b19, sex_b25, by=c("year","SEX"), all.x=TRUE)
final_sex<-merge(final_temp, den5, by=c("year","SEX"))

write.csv2(final_sex, "indicators_per_sex_withoutjanfeb.csv")


#######################################################################################################

######     calcul of Robson classification for all births and for caesarean births   ####################

#####################################################################################################

babies$robson<-88
babies$robson[babies$PARITY_B==0 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$ONSET==1]<-1
babies$robson[babies$PARITY_B==0 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$ONSET==2]<-"2a"
babies$robson[babies$PARITY_B==0 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$ONSET==3]<-"2b"
babies$robson[babies$PARITY_B==1 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$PREVCS==2 & babies$ONSET==1]<-3
babies$robson[babies$PARITY_B==1 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$PREVCS==2 & babies$ONSET==2]<-"4a"
babies$robson[babies$PARITY_B==1 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$PREVCS==2 & babies$ONSET==3]<-"4b"
babies$robson[babies$PARITY_B==1 & babies$PRES==1 & babies$MULT_B==1 & babies$GA>=37 & babies$GA!=99 & babies$PREVCS==1]<-5
babies$robson[babies$PARITY_B==0 & babies$PRES==2 & babies$MULT_B==1]<-6
babies$robson[babies$PARITY_B==1 & babies$PRES==2 & babies$MULT_B==1]<-7
babies$robson[babies$MULT_B==2 | babies$MULT_B==3]<-8
babies$robson[babies$MULT_B==1 & (babies$PRES==3 | babies$PRES==4)]<-9
babies$robson[babies$MULT_B==1 & babies$GA<37 & babies$PRES==1]<-10

bbcs<-babies[babies$VITAL!=1 & babies$MOD==2, ]
bbcs<-bbcs[, c("COUNTRY", "year", "robson")]
bbcs$N<-1
robcs<-aggregate (N ~ COUNTRY+year+robson, bbcs, sum)
robcs$category<-"caesarean birth"

allbabies<-babies [babies$VITAL!=1, ]
allbabies<-allbabies[allbabies$MOD!=99,]
allbabies<-allbabies[,c("COUNTRY", "year", "robson") ]
allbabies$N<-1
roball<-aggregate (N ~ COUNTRY+year+ robson, allbabies, sum)
roball$category<-"allbirths"

tableau<-rbind.fill(robcs,roball)
write.csv2(tableau, "robson.csv")



##############################################################################################################

#######     results of unadjusted and adjusted regression analyses                          ###################

###########################THIS PART HAS TO BE RUN WHEN 2020 DATA are available           #####################

######## Variables VITAL (with stillbirths), GA, MOD are compulsory for each year to run this analysis ##########################

babes<-babies

#babies born alive preterm
babes$term[babes$GA>21 & babes$GA<37 & babes$VITAL==4]<-0
babes$term[babes$GA>=37 & babes$GA<=46 & babes$VITAL==4]<-1

#babies stillborn, TOP excluded
babes$death[(babes$VITAL==2 | babes$VITAL==3) & babes$GA>=22]<-0
babes$death[babes$VITAL==4 & babes$GA>=22]<-1


#We chose to take risk period=March-December (to be comparable)
babes$lockdown[babes$month==3]<-1
babes$lockdown[babes$month==4]<-1
babes$lockdown[babes$month==5]<-1
babes$lockdown[babes$month==6]<-1
babes$lockdown[babes$month==7]<-1
babes$lockdown[babes$month==8]<-1
babes$lockdown[babes$month==9]<-1
babes$lockdown[babes$month==10]<-1
babes$lockdown[babes$month==11]<-1
babes$lockdown[babes$month==12]<-1


################          outcome= preterm rate     ##############################

base<-babes[babes$lockdown==1, ]
base$year1[base$year=="2020"]<-0
base$year1[base$year=="2019"]<-1
A<-table(base$year1,base$term)
RR_A<-epi.2by2(A, method="cohort.count", conf.level=0.95)

base$year2[base$year=="2020"]<-0
base$year2[base$year=="2018"]<-1

Abis<-table(base$year2,base$term)
RR_Abis<-epi.2by2(Abis, method="cohort.count", conf.level=0.95)

base$year3[base$year=="2020"]<-0
base$year3[base$year=="2017"]<-1
Ater<-table(base$year3,base$term)
RR_Ater<-epi.2by2(Ater, method="cohort.count", conf.level=0.95)

base$year4[base$year=="2020"]<-0
base$year4[base$year=="2016"]<-1
Aquart<-table(base$year4,base$term)
RR_Aquart<-epi.2by2(Aquart, method="cohort.count", conf.level=0.95)

base$agemat[base$MATAGE_B<=24]<-1
base$agemat[base$MATAGE_B>=25 & base$MATAGE_B<35]<-0
base$agemat[base$MATAGE_B>=35 & base$MATAGE_B<40]<-2
base$agemat[base$MATAGE_B>=40 & base$MATAGE_B!=99]<-3


# give Adjusted Relative Risk from Logistic Regression
base$prema[base$term==0]<-1
base$prema[base$term==1]<-0
base$year1bis[base$year1==0]<-1
base$year1bis[base$year1==1]<-0

base$agemat<- as.factor(base$agemat)
base$PARITY_B1[base$PARITY_B==0]<-1
base$PARITY_B1[base$PARITY_B==1]<-0
base$PARITY_B1<- as.factor(base$PARITY_B1)

RRa_A<- logisticRR(prema ~ year1bis + agemat + PARITY_B1, data=base)

base$year2bis[base$year2==0]<-1
base$year2bis[base$year2==1]<-0
base$year3bis[base$year3==0]<-1
base$year3bis[base$year3==1]<-0
base$year4bis[base$year4==0]<-1
base$year4bis[base$year4==1]<-0

RRa_Abis<-logisticRR(prema ~ year2bis + agemat + PARITY_B1, data=base)
RRa_Ater<-logisticRR(prema ~ year3bis + agemat + PARITY_B1, data=base)
RRa_Aquart<-logisticRR(prema ~ year4bis + agemat + PARITY_B1, data=base)

################          outcome=stillbirth rate              ##############################

B<-table(base$year1,base$death)
RR_B<-epi.2by2(B, method="cohort.count", conf.level=0.95)
Bbis<-table(base$year2,base$death)
RR_Bbis<-epi.2by2(Bbis, method="cohort.count", conf.level=0.95)
Bter<-table(base$year3,base$death)
RR_Bter<-epi.2by2(Bter, method="cohort.count", conf.level=0.95)
Bquart<-table(base$year4,base$death)
RR_Bquart<-epi.2by2(Bquart, method="cohort.count", conf.level=0.95)

base$mortne[base$death==0]<-1
base$mortne[base$death==1]<-0
RRa_B<-logisticRR(mortne ~ year1bis + agemat + PARITY_B1, data=base)
RRa_Bbis<-logisticRR(mortne ~ year2bis + agemat + PARITY_B1, data=base)
RRa_Bter<-logisticRR(mortne ~ year3bis + agemat + PARITY_B1, data=base)
RRa_Bquart<-logisticRR(mortne ~ year4bis + agemat + PARITY_B1, data=base)


##################               outcome=Caesarean rate    #################################
base$CS[base$MOD==2 ]<-1
base$CS[base$MOD==1 ]<-0
C<-table(base$year1,base$CS)
RR_C<-epi.2by2(C, method="cohort.count", conf.level=0.95)

Cbis<-table(base$year2,base$CS)
RR_Cbis<-epi.2by2(Cbis, method="cohort.count", conf.level=0.95)
Cter<-table(base$year3,base$CS)
RR_Cter<-epi.2by2(Cter, method="cohort.count", conf.level=0.95)
Cquart<-table(base$year4,base$CS)
RR_Cquart<-epi.2by2(Cquart, method="cohort.count", conf.level=0.95)

base$CSbis[base$CS==0]<-1
base$CSbis[base$CS==1]<-0
RRa_C<-logisticRR(CSbis ~ year1bis + agemat + PARITY_B1, data=base)
RRa_Cbis<-logisticRR(CSbis ~ year2bis + agemat + PARITY_B1, data=base)
RRa_Cter<-logisticRR(CSbis ~ year3bis + agemat + PARITY_B1, data=base)
RRa_Cquart<-logisticRR(CSbis ~ year4bis + agemat + PARITY_B1, data=base)

##############   collection of results   #######################################

#######   preterm
prem2020<-A[1, ]
prem2019<-A[2, ]
prem2018<-Abis[2, ]
prem2017<-Ater[2, ]
prem2016<-Aquart[2, ]

PTB_RR<-summary(RR_A)$massoc.detail$RR.strata.wald
PTBbis_RR<-summary(RR_Abis)$massoc.detail$RR.strata.wald
PTBter_RR<-summary(RR_Ater)$massoc.detail$RR.strata.wald
PTBquart_RR<-summary(RR_Aquart)$massoc.detail$RR.strata.wald

matrixPTB<-c(prem2020,prem2019, prem2018, prem2017, prem2016, 
             PTB_RR$est,PTB_RR$lower, PTB_RR$upper, RRa_A$RR, RRa_A$delta.var, 
             PTBbis_RR$est,PTBbis_RR$lower, PTBbis_RR$upper, RRa_Abis$RR, RRa_Abis$delta.var,
             PTBter_RR$est,PTBter_RR$lower, PTBter_RR$upper, RRa_Ater$RR, RRa_Ater$delta.var,
             PTBquart_RR$est,PTBquart_RR$lower, PTBquart_RR$upper, RRa_Aquart$RR, RRa_Aquart$delta.var)


results<-transpose(as.data.frame(matrixPTB))
names(results)<-c("PTB_N1","PTB_term1", "PTB_N2", "PTB_term2", "PTB_N3", "PTB_term3", "PTB_N4", "PTB_term4","PTB_N5", "PTB_term5",
                  "PTB_cRR1", "PTB_RR1_lower", "PTB_RR1_upper", "PTB_aRR1", "PTB_aRR1_var",
                  "PTB_cRR2", "PTB_RR2_lower", "PTB_RR2_upper", "PTB_aRR2", "PTB_aRR2_var",
                  "PTB_cRR3", "PTB_RR3_lower", "PTB_RR3_upper", "PTB_aRR3", "PTB_aRR3_var",
                  "PTB_cRR4", "PTB_RR4_lower", "PTB_RR4_upper", "PTB_aRR4", "PTB_aRR4_var")

results<-mutate (results, PTB_percent1=PTB_N1/(PTB_N1+PTB_term1))
results<-mutate (results, PTB_percent2=PTB_N2/(PTB_N2+PTB_term2))
results<-mutate (results, PTB_percent3=PTB_N3/(PTB_N3+PTB_term3))
results<-mutate (results, PTB_percent4=PTB_N4/(PTB_N4+PTB_term4))
results<-mutate (results, PTB_percent5=PTB_N5/(PTB_N5+PTB_term5))


results<- results[, c("PTB_N1","PTB_percent1", "PTB_N2", "PTB_percent2", "PTB_N3", "PTB_percent3","PTB_N4", "PTB_percent4","PTB_N5", "PTB_percent5",
                      "PTB_cRR1", "PTB_RR1_lower", "PTB_RR1_upper", "PTB_aRR1", "PTB_aRR1_var",
                      "PTB_cRR2", "PTB_RR2_lower", "PTB_RR2_upper", "PTB_aRR2", "PTB_aRR2_var",
                      "PTB_cRR3", "PTB_RR3_lower", "PTB_RR3_upper", "PTB_aRR3", "PTB_aRR3_var",
                      "PTB_cRR4", "PTB_RR4_lower", "PTB_RR4_upper", "PTB_aRR4", "PTB_aRR4_var")]


###########     stillbirth

stillb2020<-B[1, ]
stillb2019<-B[2, ]
stillb2018<-Bbis[2, ]
stillb2017<-Bter[2, ]
stillb2016<-Bquart[2, ]
SB_RR<-summary(RR_B)$massoc.detail$RR.strata.wald
SBbis_RR<-summary(RR_Bbis)$massoc.detail$RR.strata.wald
SBter_RR<-summary(RR_Bter)$massoc.detail$RR.strata.wald
SBquart_RR<-summary(RR_Bquart)$massoc.detail$RR.strata.wald
matrixSB<-c(stillb2020,stillb2019,stillb2018,stillb2017,stillb2016,
            SB_RR$est,SB_RR$lower, SB_RR$upper, RRa_B$RR, RRa_B$delta.var,
            SBbis_RR$est,SBbis_RR$lower, SBbis_RR$upper, RRa_Bbis$RR, RRa_Bbis$delta.var,
            SBter_RR$est,SBter_RR$lower, SBter_RR$upper, RRa_Bter$RR, RRa_Bter$delta.var,
            SBquart_RR$est,SBquart_RR$lower, SBquart_RR$upper, RRa_Bquart$RR, RRa_Bquart$delta.var)
resultsB<-transpose(as.data.frame(matrixSB))
names(resultsB)<-c("SB_N1","SB_alive1", "SB_N2", "SB_alive2", "SB_N3", "SB_alive3","SB_N4", "SB_alive4","SB_N5", "SB_alive5",
                   "SB_cRR", "SB_RR_lower", "SB_RR_upper", "SB_aRR", "SB_aRR_var",
                   "SB_cRR2", "SB_RR2_lower", "SB_RR2_upper", "SB_aRR2", "SB_aRR2_var",
                   "SB_cRR3", "SB_RR3_lower", "SB_RR3_upper", "SB_aRR3", "SB_aRR3_var",
                   "SB_cRR4", "SB_RR4_lower", "SB_RR4_upper", "SB_aRR4", "SB_aRR4_var")

resultsB<-mutate (resultsB, SB_percent1=SB_N1/(SB_N1+SB_alive1))
resultsB<-mutate (resultsB, SB_percent2=SB_N2/(SB_N2+SB_alive2))
resultsB<-mutate (resultsB, SB_percent3=SB_N3/(SB_N3+SB_alive3))
resultsB<-mutate (resultsB, SB_percent4=SB_N4/(SB_N4+SB_alive4))
resultsB<-mutate (resultsB, SB_percent5=SB_N5/(SB_N5+SB_alive5))

resultsB<- resultsB[, c("SB_N1","SB_percent1", "SB_N2", "SB_percent2", "SB_N3", "SB_percent3","SB_N4", "SB_percent4","SB_N5", "SB_percent5",
                      "SB_cRR", "SB_RR_lower", "SB_RR_upper", "SB_aRR", "SB_aRR_var",
                      "SB_cRR2", "SB_RR2_lower", "SB_RR2_upper", "SB_aRR2", "SB_aRR2_var",
                      "SB_cRR3", "SB_RR3_lower", "SB_RR3_upper", "SB_aRR3", "SB_aRR3_var",
                      "SB_cRR4", "SB_RR4_lower", "SB_RR4_upper", "SB_aRR4", "SB_aRR4_var")]

###########    cesarean section

cesar2020<-C[1, ]
cesar2019<-C[2, ]
cesar2018<-Cbis[2, ]
cesar2017<-Cter[2, ]
cesar2016<-Cquart[2, ]
CS_RR<-summary(RR_C)$massoc.detail$RR.strata.wald
CSbis_RR<-summary(RR_Cbis)$massoc.detail$RR.strata.wald
CSter_RR<-summary(RR_Cter)$massoc.detail$RR.strata.wald
CSquart_RR<-summary(RR_Cquart)$massoc.detail$RR.strata.wald
matrixCS<-c(cesar2020,cesar2019,cesar2018,cesar2017,cesar2016,
            CS_RR$est,CS_RR$lower, CS_RR$upper, RRa_C$RR, RRa_C$delta.var,
            CSbis_RR$est,CSbis_RR$lower, CSbis_RR$upper, RRa_Cbis$RR, RRa_Cbis$delta.var,
            CSter_RR$est,CSter_RR$lower, CSter_RR$upper, RRa_Cter$RR, RRa_Cter$delta.var,
            CSquart_RR$est,CSquart_RR$lower, CSquart_RR$upper, RRa_Cquart$RR, RRa_Cquart$delta.var)
resultsC<-transpose(as.data.frame(matrixCS))
names(resultsC)<-c("CS_N1","CS_vag1", "CS_N2", "CS_vag2","CS_N3", "CS_vag3","CS_N4", "CS_vag4","CS_N5", "CS_vag5", 
                   "CS_cRR", "CS_RR_lower", "CS_RR_upper", "CS_aRR", "CS_aRR_var",
                   "CS_cRR2", "CS_RR2_lower", "CS_RR2_upper", "CS_aRR2", "CS_aRR2_var",
                   "CS_cRR3", "CS_RR3_lower", "CS_RR3_upper", "CS_aRR3", "CS_aRR3_var",
                   "CS_cRR4", "CS_RR4_lower", "CS_RR4_upper", "CS_aRR4", "CS_aRR4_var")

resultsC<-mutate (resultsC, CS_percent1=CS_N1/(CS_N1+CS_vag1))
resultsC<-mutate (resultsC, CS_percent2=CS_N2/(CS_N2+CS_vag2))
resultsC<-mutate (resultsC, CS_percent3=CS_N3/(CS_N3+CS_vag3))
resultsC<-mutate (resultsC, CS_percent4=CS_N4/(CS_N4+CS_vag4))
resultsC<-mutate (resultsC, CS_percent5=CS_N5/(CS_N5+CS_vag5))

resultsC<- resultsC[, c("CS_N1","CS_percent1", "CS_N2", "CS_percent2", "CS_N3", "CS_percent3","CS_N4", "CS_percent4","CS_N5", "CS_percent5",
                        "CS_cRR", "CS_RR_lower", "CS_RR_upper", "CS_aRR", "CS_aRR_var",
                        "CS_cRR2", "CS_RR2_lower", "CS_RR2_upper", "CS_aRR2", "CS_aRR2_var",
                        "CS_cRR3", "CS_RR3_lower", "CS_RR3_upper", "CS_aRR3", "CS_aRR3_var",
                        "CS_cRR4", "CS_RR4_lower", "CS_RR4_upper", "CS_aRR4", "CS_aRR4_var")]
tempresults<-merge(results,resultsB)
totresults<-merge(tempresults,resultsC)
write.csv2(totresults, "Output_table_analysis.csv")

