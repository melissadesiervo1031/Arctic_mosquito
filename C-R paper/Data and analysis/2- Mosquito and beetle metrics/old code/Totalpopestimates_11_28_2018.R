##files##
Larvaepupae2018<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Larvae pupae analysis/larvalcounts_2018_11_28.csv")

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

##summary by date## 
summaryLarvaepupcounts2018 <-Larvaepupae2018 %>% group_by(Site, Year, SiteYear,Date, Day, Larvalstage, Larvalstage2, Emergence) %>% dplyr::summarise(samplemean=mean(Total, na.rm=TRUE),samplelnmean1=log(samplemean), samplelnmean2=mean(lnTotal, na.rm=TRUE), samplevar=var(Total, na.rm=TRUE), samplelnvar=var(lnTotal, na.rm=TRUE), Totalarea=(mean(Littoralarea_liters, na.rm=TRUE)),Samplearea=(mean(SampleL, na.rm=TRUE)),n=n() )

##add on a column for N (big area/little area) and finitepopulation correction##
summaryLarvaepupcounts2018_2<-as.data.frame(summaryLarvaepupcounts2018) %>% mutate(N=Totalarea/Samplearea, finitepopcorrection=((N-n)/N))

##Calculate total population, variance of total popululation, SE of total population, confidence intervals, upper and lower##
summaryLarvaepupcounts2018_3<-summaryLarvaepupcounts2018_2 %>% mutate(Totalpop=samplemean*N, variancepop=(((N^2)*samplevar)/n)*finitepopcorrection, VMR=(variancepop/Totalpop), SEpop=((N*(sqrt(samplevar))/sqrt(n))*sqrt(finitepopcorrection)), conf95=1.96*SEpop, upper95=Totalpop+conf95, lower95=Totalpop-conf95, lnTotalpop=samplelnmean2*N, lnSEpop=((N*(sqrt(samplelnvar))/sqrt(n))*sqrt(finitepopcorrection)))

##subset the dates after mosquitoes finished hatching but before they started emerging##
Larvaepupcounts2018beforeemergence<-subset(summaryLarvaepupcounts2018_3, Emergence=="Before")

##graph all ponds together##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

Totalpopulationsizeallponds<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, Totalpop)) +geom_point(aes(colour = Site))+geom_smooth(aes(colour = Site), method='lm', size=0.5, se=FALSE)+facet_wrap(~Site, scales="free")+geom_errorbar(aes(ymin=Totalpop-SEpop, ymax=Totalpop+SEpop))+ylab("Total population size")+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Totalpopulationsizeallponds.png", width = 8, height = 7, units = 'in', res = 800)
Totalpopulationsizeallponds
dev.off()

##graph all ponds together on LOG SCALE##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

logTotalpopulationsizeallponds<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, lnTotalpop)) +geom_point(aes(colour = Site))+geom_smooth(aes(colour = Site), method='lm', size=0.5, se=FALSE)+facet_wrap(~Site, scales="free")+geom_errorbar(aes(ymin=lnTotalpop-lnSEpop, ymax=lnTotalpop+lnSEpop))+ylab("ln(Total population size)")+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/logTotalpopulationsizeallponds.png", width = 8, height = 7, units = 'in', res = 800)
logTotalpopulationsizeallponds
dev.off()




##per capita mortality for each pond##
mortcoefs<-Larvaepupcounts2018beforeemergence%>% group_by(Site) %>% do({fitponds = lm(lnTotalpop~Date, data=.)
+ data.frame(slope=coef(fitponds)[2])})

##N0 for each pond##
N0ponds2018<-Larvaepupcounts2018beforeemergence %>% group_by(Site) %>% slice(which.min(Date))

##combine N0 and mortality rates##
N0mort2018ponds<-cbind(as.data.frame(mortcoefs),N0ponds2018$Totalpop )

N0mort2018ponds2<-cbind(N0mort2018ponds,N0ponds2018$lnTotalpop )

N0mortality<-ggplot(N0mort2018ponds, aes(x=N0ponds2018$Totalpop, y=(-1*slope), colour=Site))+ geom_point(size=2)+geom_text(aes(label=Site),vjust = -0.5)+xlab("N0")+ylab("Per capita mortality")+ggtitle("")+ mytheme + theme(legend.position="none")
N0mortalityln<-ggplot(N0mort2018ponds, aes(x=N0ponds2018$lnTotalpop, y=(-1*slope), colour=Site))+ geom_point(size=1)+geom_text(aes(label=Site),vjust = -0.5, size=2)+xlab("ln(N0)")+ylab("Per capita mortality")+ggtitle("")+ mytheme + theme(legend.position="none")

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/N0mortalityln.png", width = 4, height = 3, units = 'in', res = 800)
N0mortalityln
dev.off()

m1<-lm(slope~N0ponds2018$lnTotalpop, data=N0mort2018ponds2)
