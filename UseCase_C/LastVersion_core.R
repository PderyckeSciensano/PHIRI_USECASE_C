# install.packages("dplyr")
# install.packages("plyr")
# install.packages("dlookr")

library(dplyr)
library(plyr)
library(dlookr)

#indicate here the file location, complying with the common data model specifications, 1 ligne=1 baby
#the scripts assume that your file separator is ';'
# setwd(dir="")
babies<-read.csv2("usecaseC_synthetic.csv",sep = '|')

#change names of variable for countries who have deprivation score
names(babies)[names(babies) == "SES"] <- "SES_ED"
# for Ireland
names(babies)[names(babies) == "SES_OccM"] <- "SES_ED"

##############        keep only variables of interest (no identifiers) ##############################

babies<-babies[, c("COUNTRY", "GA", "BW", "MULT_B", "VITAL", "NNM", "IM", "MATAGE_B", "PARITY_B", 
                   "PRES", "PREVCS", "ONSET", "MOD", "INSTRUMENT", "TYPECESAR", "SEX", 
                   "year", "month", "day", "SES_ED") ]

#############       variables are transformed into a categorical format
babies$MULT_B<-as.factor(babies$MULT_B)
babies$VITAL<-as.factor(babies$VITAL)
babies$NNM<-as.factor(babies$NNM)
babies$IM<-as.factor(babies$IM)
babies$PARITY_B<-as.factor(babies$PARITY_B)
babies$PRES<-as.factor(babies$PRES)
babies$PREVCS<-as.factor(babies$PREVCS)
babies$ONSET<-as.factor(babies$ONSET)
babies$MOD<-as.factor(babies$MOD)
babies$INSTRUMENT<-as.factor(babies$INSTRUMENT)
babies$TYPECESAR<-as.factor(babies$TYPECESAR)
babies$SEX<-as.factor(babies$SEX)
babies$SES_ED<-as.factor(babies$SES_ED)

#######      diagnose is an R package that assesses the quality of the variables (mix, max, missing, distribution of variable)
# diagnose(babies)
# diagnose_category(babies)
# diagnose_numeric(babies)
# diagnose_outlier(babies)
#diagnose_web_report(babies, output_format="html")

################### preparation of dataset ######################################################################

########    these commands identify outliers for GA, BW and maternal age (see common data model) and recodes them to missing
babies$GA[babies$GA>46]<-99
#replace BW>7kgs in missing
babies$BW[babies$BW>7000]<-99
# replace maternal age  below 12 and up 59 in missing
babies$MATAGE_B[babies$MATAGE_B<12]<-99
babies$MATAGE_B[babies$MATAGE_B>59]<-99

#this section create classes for birth weight and maternal age (see common data model)
babies$classBW[ babies$BW<500]<-1
babies$classBW[ babies$BW>=500 & babies$BW<=999]<-2
babies$classBW[ babies$BW>=1000 & babies$BW<=1499]<-3
babies$classBW[ babies$BW>=1500 & babies$BW<=2499]<-4
babies$classBW[ babies$BW>=2500 & babies$BW<=4499]<-5
babies$classBW[ babies$BW>=4500]<-6
babies$classBW[ babies$BW==99]<-99

babies$classage[ babies$MATAGE_B<20]<-1
babies$classage[ babies$MATAGE_B>=20 & babies$MATAGE_B<=24]<-2
babies$classage[ babies$MATAGE_B>=25 & babies$MATAGE_B<=29]<-3
babies$classage[ babies$MATAGE_B>=30 & babies$MATAGE_B<=34]<-4
babies$classage[ babies$MATAGE_B>=35 & babies$MATAGE_B<=39]<-5
babies$classage[ babies$MATAGE_B>=40 & babies$MATAGE_B<=44]<-6
babies$classage[ babies$MATAGE_B>=45]<-7
babies$classage[ babies$MATAGE_B==99]<-99

#to identify TOP to be able to exclude it from later analysis
mortne<-babies[babies$VITAL==1 | babies$VITAL==2 |babies$VITAL==3 , ]
mortne$TOP<-0
mortne$TOP[mortne$VITAL==1]<-1


################ creation of aggregates tables #################################################################

#this section creates the aggregate table for the C1a indicator (stillbirth by gestational age (GA) and multiplicity)
mortne<-mortne[, c("COUNTRY","year", "GA", "classBW", "MULT_B", "TOP")]
c1temp<-mortne[, c("COUNTRY","year", "GA", "MULT_B", "TOP")]
C1a<-aggregate(1:nrow(c1temp),c1temp,length)
names(C1a)[names(C1a) == "x"] <- "N"
if(class(C1a$N) == "list"){C1a$N<-as.integer()}
if(nrow(C1a)>0){C1a$indicator<-"C1a"}

#this section creates the aggregate table for the C1b indicator (stillbirth by birth weight(BW) classes and multiplicity)
c1temp2<-mortne[, c("COUNTRY","year", "classBW", "MULT_B","TOP")]
C1b<-aggregate(1:nrow(c1temp2),c1temp2,length)
names(C1b)[names(C1b) == "x"] <- "N"
if(class(C1b$N) == "list"){C1b$N<-as.integer()}
if(nrow(C1b)>0){C1b$indicator<-"C1b"}

#this section creates the aggregate table for the C2a indicator (neonatal death by GA and multiplicity)
neonatmt<-babies[babies$NNM==1 | babies$NNM==2 |babies$NNM==3 | babies$NNM==98, ]
neonatmt<-neonatmt[, c("COUNTRY","year", "NNM", "GA", "classBW", "MULT_B")]
c2temp<-neonatmt[, c("COUNTRY","year", "GA", "MULT_B", "NNM")]
C2a<-aggregate(1:nrow(c2temp),c2temp,length)
names(C2a)[names(C2a) == "x"] <- "N"
if(class(C2a$N) == "list"){C2a$N<-as.integer()}
if(nrow(C2a)>0){C2a$indicator<-"C2a"}

#this section creates the aggregate table for the C2b indicator (neonatal death by BW classes and multiplicity)
c2temp2<-neonatmt[, c("COUNTRY","year", "classBW", "MULT_B", "NNM")]
C2b<-aggregate(1:nrow(c2temp2),c2temp2,length)
names(C2b)[names(C2b) == "x"] <- "N"
if(class(C2b$N) == "list"){C2b$N<-as.integer()}
if(nrow(C2b)>0){C2b$indicator<-"C2b"}

# this section creates the aggregate table for the C3a indicator (infant death by GA and multiplicity)
infantmt<-babies[babies$IM==1 | babies$IM==98, ]
infantmt<-infantmt[, c("COUNTRY","year", "GA", "classBW", "MULT_B")]
c3temp<-infantmt[, c("COUNTRY","year", "GA", "MULT_B")]
C3a<-aggregate(1:nrow(c3temp),c3temp,length)
names(C3a)[names(C3a) == "x"] <- "N"
if(class(C3a$N) == "list"){C3a$N<-as.integer()}
if(nrow(C3a)>0){C3a$indicator<-"C3a"}

# this section creates the aggregate table for the C3b indicator (infant death by BW classes and multiplicity)
c3temp2<-infantmt[, c("COUNTRY","year", "classBW", "MULT_B")]
C3b<-aggregate(1:nrow(c3temp2),c3temp2,length)
names(C3b)[names(C3b) == "x"] <- "N"
if(class(C3b$N) == "list"){C3b$N<-as.integer()}
if(nrow(C3b)>0){C3b$indicator<-"C3b"}

#create C4 indicator (live births by GA and classes of BW and multiplicity)
live<-babies[babies$VITAL==4 , ]
live<-live[, c("COUNTRY","year", "GA", "classBW", "MULT_B")]
C4<-aggregate(1:nrow(live),live,length)
names(C4)[names(C4) == "x"] <- "N"
if(class(C4$N) == "list"){C4$N<-as.integer()}
if(nrow(C4)>0){C4$indicator<-"C4"}

#this section creates the aggregate table for the C5 indicator (live births by GA and multiplicity)
live2<-live[, c("COUNTRY","year", "GA", "MULT_B")]
C5<-aggregate(1:nrow(live2),live2,length)
names(C5)[names(C5) == "x"] <- "N"
if(class(C5$N) == "list"){C5$N<-as.integer()}
if(nrow(C5)>0){C5$indicator<-"C5"}

#this section creates the aggregate table for the C7 indicator (multiple births)
c7b<-babies[, c("COUNTRY","year","MULT_B")]
C7<-aggregate(1:nrow(c7b),c7b,length)
names(C7)[names(C7) == "x"] <- "N"
if(class(C7$N) == "list"){C7$N<-as.integer()}
if(nrow(C7)>0){C7$indicator<-"C7"}

#this section creates the aggregate table for the C8 indicator (maternal age in 5 year groups) by multiplicity
c8b<-babies[, c("COUNTRY","year","classage", "MULT_B")]
C8<-aggregate(1:nrow(c8b),c8b,length)
names(C8)[names(C8) == "x"] <- "N"
if(class(C8$N) == "list"){C8$N<-as.integer()}
if(nrow(C8)>0){C8$indicator<-"C8"}

# this section creates the aggregate table for the C9 indicator (parity: nulliparous vs multiparous) by multiplicity
c9b<-babies[, c("COUNTRY","year", "PARITY_B", "MULT_B")]
C9<-aggregate(1:nrow(c9b),c9b,length)
names(C9)[names(C9) == "x"] <- "N"
if(class(C9$N) == "list"){C9$N<-as.integer()}
if(nrow(C9)>0){C9$indicator<-"C9"}

#this section creates the aggregate table for the C10 indicator (mode of delivery in 4 categories)
c10b<-babies[, c("COUNTRY","year", "GA", "MULT_B", "MOD","INSTRUMENT", "TYPECESAR")]
C10<-aggregate(1:nrow(c10b),c10b,length)
names(C10)[names(C10) == "x"] <- "N"
if(class(C10$N) == "list"){C10$N<-as.integer()}
if(nrow(C10)>0){C10$indicator<-"C10"}

#this section creates the aggregate table for mode of delivery by parity of the mother
c10_primib<-babies[, c("COUNTRY","year", "MOD", "PARITY_B")]
C10_primi<-aggregate(1:nrow(c10_primib),c10_primib,length)
names(C10_primi)[names(C10_primi) == "x"] <- "N"
if(class(C10_primi$N) == "list"){C10_primi$N<-as.integer()}
if(nrow(C10_primi)>0){C10_primi$indicator<-"C10_parity"}

#this section creates the aggregate table for mode of delivery by previous ceasarean section
c10_prevcsb<-babies[, c("COUNTRY","year", "MOD", "PREVCS")]
C10_prevcs<-aggregate(1:nrow(c10_prevcsb),c10_prevcsb,length)
names(C10_prevcs)[names(C10_prevcs) == "x"] <- "N"
if(class(C10_prevcs$N) == "list"){C10_prevcs$N<-as.integer()}
if(nrow(C10_prevcs)>0){C10_prevcs$indicator<-"C10_prevcs"}

#this section creates the aggregate table for mode of delivery by multiplicity
c10_multib<-babies[, c("COUNTRY","year", "MOD","MULT_B")]
C10_multi<-aggregate(1:nrow(c10_multib),c10_multib,length)
names(C10_multi)[names(C10_multi) == "x"] <- "N"
if(class(C10_multi$N) == "list"){C10_multi$N<-as.integer()}
if(nrow(C10_multi)>0){C10_multi$indicator<-"C10_multi"}

# this section creates the aggregate table for mode of delivery by presentation of the baby
c10_presb<-babies[, c("COUNTRY","year", "MOD","PRES")]
C10_pres<-aggregate(1:nrow(c10_presb),c10_presb,length)
names(C10_pres)[names(C10_pres) == "x"] <- "N"
if(class(C10_pres$N) == "list"){C10_pres$N<-as.integer()}
if(nrow(C10_pres)>0){C10_pres$indicator<-"C10_pres"}

# this section creates the aggregate table for the indicator of socio-economic stats (either maternal education, occupation or deprivation depending on source)
ses1<-babies[, c("COUNTRY","year", "SES_ED")]
SES1<-aggregate(1:nrow(ses1),ses1,length)
names(SES1)[names(SES1) == "x"] <- "N"
if(class(SES1$N) == "list"){SES1$N<-as.integer()}
if(nrow(SES1)>0){SES1$indicator<-"SES_ED"}

# this section creates the aggregate table for the sex of the baby 
babysex<-babies[, c("COUNTRY","year", "SEX")]
BBSEX<-aggregate(1:nrow(babysex),babysex,length)
names(BBSEX)[names(BBSEX) == "x"] <- "N"
if(class(BBSEX$N) == "list"){BBSEX$N<-as.integer()}
if(nrow(BBSEX)>0){BBSEX$indicator<-"Baby_sex"}



#creates one database that appends all the aggregate data tables that were created above 
CORE<-rbind.fill(C1a, C1b, C2a, C2b,C3a, C3b, C4, C5, C7, C8, C9, C10, C10_primi, C10_prevcs, 
                 C10_multi, C10_pres, SES1, BBSEX )

#creates the output file of the database with all the aggregate data tables 
write.csv2(CORE, "Core_indicators.csv")
# 




