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
summaryLarvaepupcounts2018 <-Larvaepupae2018 %>% group_by(Site, Year, SiteYear,Date, Day, Larvalstage, Larvalstage2, Emergence) %>% dplyr::summarise(samplemeanbeetle=mean(Beetle, na.rm=TRUE), samplemean=mean(Total, na.rm=TRUE),samplelnmean1=log(samplemean), samplelnmean2=mean(lnTotal, na.rm=TRUE), samplevar=var(Total, na.rm=TRUE),samplevarbeetles=var(Beetle, na.rm=TRUE), samplelnvar=var(lnTotal, na.rm=TRUE), Totalarea=(mean(Littoralarea_liters, na.rm=TRUE)),Samplearea=(mean(SampleL, na.rm=TRUE)),n=n() )

##add on a column for N (big area/little area) and finitepopulation correction##
summaryLarvaepupcounts2018_2<-as.data.frame(summaryLarvaepupcounts2018) %>% mutate(N=Totalarea/Samplearea, finitepopcorrection=((N-n)/N))

##Calculate total population, variance of total popululation, SE of total population, confidence intervals, upper and lower##
summaryLarvaepupcounts2018_3<-summaryLarvaepupcounts2018_2 %>% mutate(Totalpop=samplemean*N, variancepop=(((N^2)*samplevar)/n)*finitepopcorrection, VMR=(variancepop/Totalpop), SEpop=((N*(sqrt(samplevar))/sqrt(n))*sqrt(finitepopcorrection)), lnTotalpop=log(1+Totalpop), lnupperSE=log(Totalpop+SEpop),lnlowerSE=log(Totalpop-SEpop),Totalpopbeetles=samplemeanbeetle*N,SEpopbeetles=((N*(sqrt(samplevarbeetles))/sqrt(n))*sqrt(finitepopcorrection)),lnTotalpopbeetles=log(1+(samplemeanbeetle*N)), lnupperSEbeetles=log(1+(Totalpopbeetles+SEpopbeetles)),lnlowerSEbeetles=log((1+Totalpopbeetles-SEpopbeetles)) )

##subset the dates after mosquitoes finished hatching but before they started emerging##
Larvaepupcounts2018beforeemergence<-subset(summaryLarvaepupcounts2018_3, Emergence=="Before")


##graph all ponds together on LOG SCALE##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

logTotalpopulationsizeallponds<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, lnTotalpop)) +geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 

Larvaepupcounts2018beforeemergence$Site<-factor(Larvaepupcounts2018beforeemergence$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))

##Beetle abundance log scale##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

logTotalpopulationsizeallpondsbeetles<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, lnTotalpopbeetles)) +geom_point()+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSEbeetles, ymax=lnupperSEbeetles))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 

Larvaepupcounts2018beforeemergence$Site<-factor(Larvaepupcounts2018beforeemergence$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))




##graph all ponds together on reulgar SCALE##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

Totalpopulationsizeallponds<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, Totalpop)) +geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=Totalpop-SEpop, ymax=Totalpop+SEpop))+ylab("Total population size")+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 


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



#put ponds in the right order#
Larvaepupcounts2018beforeemergence$Site<-factor(Larvaepupcounts2018beforeemergence$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))

##fix the title issue by dividign into two and merging##
Larvaepupcounts2018beforeemergence$Site<-factor(Larvaepupcounts2018beforeemergence$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))


Larvaepupcounts2018beforeemergenceEGINO<-subset(Larvaepupcounts2018beforeemergence, Site=="East"| Site=="NoOil"| Site=="Oil"| Site=="Golf")
Larvaepupcounts2018beforeemergenceOVVSW<-subset(Larvaepupcounts2018beforeemergence, Site=="Waterfall"|Site=="Vulgaris"| Site=="Vulgaris small"| Site=="Ice")

mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))


logTotalpopulationsizeallpondsEGINO<-ggplot(Larvaepupcounts2018beforeemergenceEGINO, aes(Date, lnTotalpop)) +ylim(10.5,15.25)+geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(axis.text.x=element_blank())+theme(axis.ticks.x=element_blank())+theme(plot.margin=unit(c(1,1,0,1), "cm")) 
logTotalpopulationsizeallpondsEGINO2<-logTotalpopulationsizeallpondsEGINO+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-01"), 15, label=Site, group=NULL))

logTotalpopulationsizeallpondsOVVSW<-ggplot(Larvaepupcounts2018beforeemergenceOVVSW, aes(Date, lnTotalpop)) +geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(0,1,1,1), "cm"))+ scale_y_continuous(limits=c(10.5,17.25), breaks=pretty_breaks(n=6))
logTotalpopulationsizeallpondsOVVSW2<-logTotalpopulationsizeallpondsOVVSW+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-02"), 17, label=Site, group=NULL))


library(gtable)
g111 <- ggplotGrob(logTotalpopulationsizeallpondsEGINO2)
g222 <- ggplotGrob(logTotalpopulationsizeallpondsOVVSW2)

g111222<-rbind(g111, g222)

grid.arrange(g111, g222 , ncol = 1, heights = c(1, 1.2))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/logTotalpopulationsizeallponds22.png", width = 8, height = 4, units = 'in', res = 800)
grid.arrange(g111, g222 , ncol = 1, heights = c(1, 1.2))
dev.off()


##graph all ponds together on reulgar SCALE##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

Totalpopulationsizeallponds<-ggplot(Larvaepupcounts2018beforeemergence, aes(Date, Totalpop)) +geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=Totalpop-SEpop, ymax=Totalpop+SEpop))+ylab("Total population size")+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) 



#put ponds in the right order#
Larvaepupcounts2018beforeemergence$Site<-factor(Larvaepupcounts2018beforeemergence$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/logTotalpopulationsizeallponds.png", width = 8, height = 4, units = 'in', res = 800)
logTotalpopulationsizeallponds
dev.off()
