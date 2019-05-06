##files##
Larvaepupae2018<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Larvae pupae analysis/larvalcounts_2018_11_19.csv")

Larvaepupae2018$Larvalstage<-factor(Larvaepupae2018$Larvalstage, levels=c("Hatch", "One", "OneTwo", "Two", "TwoThree", "Three", "ThreeFour", "Four", "FourPupae", "Pupae"))

#packages##
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(scales)

#Date#
Larvaepupae2018$Date<-as.Date(Larvaepupae2018$Date, format="%d-%b-%y")
Larvaepupae2018$Day<-as.Date(Larvaepupae2018$Day, format="%d-%b-%y")
Larvaepupae2018$Year<-as.factor(Larvaepupae2018$Year)

##getinunitsofLITERS## 350ml * 5 = 1.75L
Larvaepupae2018<-mutate(Larvaepupae2018, Totalliter = Total/1.75)

##summary by date## 
summaryLarvaepupcounts2018 <-Larvaepupae2018 %>% group_by(Site, Year, SiteYear,Date, Day, Larvalstage, Larvalstage2, Emergence) %>% dplyr::summarise(samplemean=mean(Total, na.rm=TRUE),samplemeanliter=mean(Totalliter, na.rm=TRUE),samplevar=var(Total, na.rm=TRUE), samplevarliter=var(Totalliter, na.rm=TRUE), samplelnmean=mean(log(1+Total), na.rm=TRUE),samplelnvar=var(log(1+Total), na.rm=TRUE),Totalarea=(mean(Littoralarea_liters, na.rm=TRUE)),Samplearea=(mean(SampleL, na.rm=TRUE)),n=n())


##add on a column for N (big area/little area) and finitepopulation correction##
summaryLarvaepupcounts2018_2<-as.data.frame(summaryLarvaepupcounts2018) %>% mutate(N=Totalarea/Samplearea, finitepopcorrection=((N-n)/N))

##Calculate total population, variance of total popululation, SE of total population, confidence intervals, upper and lower##
summaryLarvaepupcounts2018_3<-summaryLarvaepupcounts2018_2 %>% mutate(Totalpop=samplemean*N, variancepop=(((N^2)*samplevar)/n)*finitepopcorrection, VMR=(variancepop/Totalpop), SEpop=((N*(sqrt(samplevar))/sqrt(n))*sqrt(finitepopcorrection)), conf95=1.96*SEpop, upper95=Totalpop+conf95, lower95=Totalpop-conf95, lnTotalpop=samplelnmean*N, lnSEpop=((N*(sqrt(samplelnvar))/sqrt(n))*sqrt(finitepopcorrection)))



##subset the dates after mosquitoes finished hatching but before they started emerging##
Larvaepupcounts2018beforeemergence<-subset(summaryLarvaepupcounts2018_3, Emergence=="Before")

write.csv(Larvaepupcounts2018beforeemergence, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Popestimates_12_19_18.csv")

Larvaepupcounts2018beforeemergence<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Popestimates_12_19_18.csv")


##subset each pond individually##
East2018<-subset(Larvaepupcounts2018beforeemergence, Site=="East")
NoOil2018<-subset(Larvaepupcounts2018beforeemergence, Site=="NoOil")
Oil2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Oil")
Golf2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Golf")
Waterfall2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Waterfall")
Vulgaris2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Vulgaris")
Vsmall2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Vulgaris small")
Ice2018<-subset(Larvaepupcounts2018beforeemergence, Site=="Ice")


##per capita mortality for each pond##
Eastmodel<-lm(log(Totalpop)~Day0, data=East2018)
NoOilmodel<-lm(log(Totalpop)~Day0, data=NoOil2018)
Oilmodel<-lm(log(Totalpop)~Day0, data=Oil2018)
Golfmodel<-lm(log(Totalpop)~Day0, data=Golf2018)
Waterfallmodel<-lm(log(Totalpop)~Day0, data=Waterfall2018)
Vulgarismodel<-lm(log(Totalpop)~Day0, data=Vulgaris2018)
Vsmallmodel<-lm(log(Totalpop)~Day0, data=Vsmall2018)
Icemodel<-lm(log(Totalpop)~Day0, data=Ice2018)

summary(Eastmodel)
summary(NoOilmodel)
summary(Oilmodel)
summary(Golfmodel)
summary(Waterfallmodel)
summary(Vulgarismodel)
summary(Vsmallmodel)
summary(Icemodel)


##per capita mortality for each pond density##
Eastmodel2<-lm(samplelnmean~Day0, data=East2018)
NoOilmodel2<-lm(samplelnmean~Day0, data=NoOil2018)
Oilmodel2<-lm(samplelnmean~Day0, data=Oil2018)
Golfmodel2<-lm(samplelnmean~Day0, data=Golf2018)
Waterfallmodel2<-lm(samplelnmean~Day0, data=Waterfall2018)
Vulgarismodel2<-lm(samplelnmean~Day0, data=Vulgaris2018)
Vsmallmodel2<-lm(samplelnmean~Day0, data=Vsmall2018)
Icemodel2<-lm(samplelnmean~Day0, data=Ice2018)

summary(Eastmodel2)
summary(NoOilmodel2)
summary(Oilmodel2)
summary(Golfmodel2)
summary(Waterfallmodel2)
summary(Vulgarismodel2)
summary(Vsmallmodel2)
summary(Icemodel2)


##ancova to test for difference between slopes##
ancovamodel<-lm(samplelnmean~Day0*Site, data=Larvaepupcounts2018beforeemergence)
ancovamodel2<-lm(log(Totalpop)~Day0*Site, data=Larvaepupcounts2018beforeemergence)


