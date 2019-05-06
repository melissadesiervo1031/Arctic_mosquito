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
summary(Vmsallmodel)
summary(Icemodel)

mortcoefstotalpop<-Larvaepupcounts2018beforeemergence%>% group_by(Site) %>% do({fitponds = lm(log(Totalpop)~Day0, data=.)
+ data.frame(slope=coef(fitponds)[2])})



##per capita mortality for each pond##
mortcoefstotalpop<-Larvaepupcounts2018beforeemergence%>% group_by(Site) %>% do({fitponds = lm(log(Totalpop)~Date, data=.)
+ data.frame(slope=coef(fitponds)[2]),stderror=coef(fitponds)[3])})

mortcoefsdensity<-Larvaepupcounts2018beforeemergence%>% group_by(Site) %>% do({fitponds = lm(log(samplemean)~Date, data=.)
+ data.frame(slope=coef(fitponds)[2])})

##graph by each pond seperately##
ggplot(Larvaepupcounts2018beforeemergence, aes(Date, Totalpop)) + geom_point() +facet_wrap(~Site, scales = "free_y", nrow = 2, strip.position = "bottom")


##N0 for 2018 ponds##
N02018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="One"|Larvalstage =="OneTwo")
summaryN02018 <- summarySE(N02018ponds, measurevar="logtotalliter", groupvars=c("Year", "Site", "SiteYear"))
summaryN02018 <-N02018ponds %>% group_by(Year, Site, SiteYear) %>% dplyr::summarise(lnLarvaepupaeliter=mean(logtotalliter, na.rm=TRUE), Stdevliter=sd(logtotalliter, na.rm=TRUE), Sterrorliter=(sd(logtotalliter, na.rm=TRUE)/sqrt(n())))
larvalstage <- rep("N0",8)
summaryN02018 <-cbind(as.data.frame(summaryN02018), larvalstage)

##third instars##
threefour2018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="Three"|Larvalstage =="ThreeFour")
NoOilthreefour<-subset(Larvaepupaeallyears , Year == 2018 & Site =="NoOil" & Larvalstage =="TwoThree"|Larvalstage =="Four")
threefour2018ponds<-rbind(threefour2018ponds,NoOilthreefour)

summarythreefour2018 <- summarySE(threefour2018ponds, measurevar="logtotalliter", groupvars=c("Year", "Site", "SiteYear"))
summarythreefour2018<-threefour2018ponds %>% group_by(Year, Site, SiteYear) %>% dplyr::summarise(lnLarvaepupaeliter=mean(logtotalliter, na.rm=TRUE), Stdevliter=sd(logtotalliter, na.rm=TRUE), Sterrorliter=(sd(logtotalliter, na.rm=TRUE)/sqrt(n())))

larvalstage <- rep("threefour",8)

summarythreefour2018 <-cbind(as.data.frame(summarythreefour2018) , larvalstage)


##total summary of N0###
summaryabundance<-rbind(summaryN02018 , summarythreefour2018 )


##anova differences in abundance##
N02018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="One"|Larvalstage =="OneTwo")
larvalstage2 <- rep("N0",204)
N02018ponds2<-cbind(N02018ponds, larvalstage2)

threefour2018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="Three"|Larvalstage =="ThreeFour")
NoOilthreefour<-subset(Larvaepupaeallyears , Year == 2018 & Site =="NoOil" & Larvalstage =="TwoThree"|Larvalstage =="Four")
threefour2018ponds<-rbind(threefour2018ponds,NoOilthreefour)
larvalstage2 <- rep("threefour",155)
threefour2018ponds2<-cbind(threefour2018ponds, larvalstage2)

earlylatepondsdf<-rbind(N02018ponds2, threefour2018ponds2)

hist(earlylatepondsdf$logtotalliter)


model123<-lm(logtotalliter~Site*larvalstage2, data=earlylatepondsdf)

##seperate for each larval stage##
Justearlyinstars<-subset(earlylatepondsdf, larvalstage2=="N0")
earlymodel123<-lm(logtotalliter~Site, data=Justearlyinstars)

Justlateinstars<-subset(earlylatepondsdf, larvalstage2=="threefour")
latemodel123<-lm(logtotalliter~Site, data=Justlateinstars)

#graph##
##combine together for graph##
summaryearlylate2018<-rbind(summaryN02018, summarythreefour2018)

abundancebypond<-ggplot(summaryearlylate2018, aes(x=Site, y=logtotalliter, fill=larvalstage))+ geom_bar(position=position_dodge(), stat="identity")+geom_errorbar(aes(ymin=logtotalliter+se, ymax=logtotalliter-se), colour="black", width=0.08,position=position_dodge(0.85)) +xlab("")+ylab("ln(N / liter)") +mytheme2+theme(legend.title=element_blank())+ theme(legend.position=c(0.1, 0.87))+scale_fill_manual(values=c("gray80", "gray24"),labels=c("Early instars", "Late instars"))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Nlarvaebypond2.png", width = 7, height = 4, units = 'in', res = 800)
abundancebypond
dev.off()