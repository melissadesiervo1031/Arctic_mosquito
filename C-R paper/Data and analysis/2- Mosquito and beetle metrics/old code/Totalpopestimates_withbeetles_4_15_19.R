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

##extract the Max date and average for beetles##

summarybypond <-Larvaepupcounts2018beforeemergence %>% group_by(Site, Year, SiteYear) %>% dplyr::summarise(maxbeetleTotalpop= max(Totalpopbeetles), lnmaxbeetleTotalpop= max(lnTotalpopbeetles))

##export and convert to long form##

write.csv(Larvaepupcounts2018beforeemergence, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/larvaepupaecountsbeforeemergence_4_12_19.csv")

longformcountsbeforeemergence<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/longform_4_12_19.csv")
longformcountsbeforemaxbeetles<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/longform_maxbeetles_4_12_19.csv")


##graph mosquitoes with beetles##
longformcountsbeforemaxbeetles$Site<-factor(longformcountsbeforemaxbeetles$Site, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))
longformcountsbeforemaxbeetles$Date<-as.Date(longformcountsbeforemaxbeetles$Date, format="%m/%d/%Y")

longformcountsbeforemaxbeetlesEGINO<-subset(longformcountsbeforemaxbeetles, Site=="East"| Site=="NoOil"| Site=="Oil"| Site=="Golf")
longformcountsbeforemaxbeetlesOVVSW<-subset(longformcountsbeforemaxbeetles, Site=="Waterfall"|Site=="Vulgaris"| Site=="Vulgaris small"| Site=="Ice")

mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

blogTotalpopulationsizeallpondsEGINO<-ggplot(longformcountsbeforemaxbeetlesEGINO, aes(Date, lnTotalpop, group=Type)) +geom_point(aes(shape=Type))+ylim(7.5,15.25)+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(axis.text.x=element_blank())+theme(axis.ticks.x=element_blank())+theme(plot.margin=unit(c(1,1,0,1), "cm"))+scale_shape_manual(values=c(2,16)) 
blogTotalpopulationsizeallpondsEGINO2<-blogTotalpopulationsizeallpondsEGINO+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-01"), 15, label=Site, group=NULL))

blogTotalpopulationsizeallpondsOVVSW<-ggplot(longformcountsbeforemaxbeetlesOVVSW, aes(Date, lnTotalpop, group=Type)) +geom_point(aes(shape=Type))+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(0,1,1,1), "cm"))+ scale_y_continuous(limits=c(7.5,17.25), breaks=pretty_breaks(n=6))+scale_shape_manual(values=c(2,16))
blogTotalpopulationsizeallpondsOVVSW2<-blogTotalpopulationsizeallpondsOVVSW+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-02"), 17, label=Site, group=NULL))


library(gtable)
g111 <- ggplotGrob(blogTotalpopulationsizeallpondsEGINO2)
g222 <- ggplotGrob(blogTotalpopulationsizeallpondsOVVSW2)

grid.arrange(g111, g222 , ncol = 1, heights = c(1, 1.2))


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/logTotalpopulationsizeallpondswithbeetles.png", width = 8, height = 4, units = 'in', res = 800)
grid.arrange(g111, g222 , ncol = 1, heights = c(1, 1.2))
dev.off()


##graph just beetle abundance##
longformcountsbeforeemergence<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/longform_4_12_19.csv")

longformcountsjustbeetles<-subset(longformcountsbeforeemergence, Type=="beetle")

jlongformcountsbeetlesEGINO<-subset(longformcountsbeforemaxbeetles, Site=="East"| Site=="NoOil"| Site=="Oil"| Site=="Golf")
jlongformcountsbeetlesOVVSW<-subset(longformcountsbeforemaxbeetles, Site=="Waterfall"|Site=="Vulgaris"| Site=="Vulgaris small"| Site=="Ice")


jblogTotalpopulationsizeallpondsEGINO<-ggplot(longformcountsjustbeetles, aes(Date, lnTotalpop, group=Type)) +geom_point(aes(shape=Type))+ylim(7.5,15.25)+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(axis.text.x=element_blank())+theme(axis.ticks.x=element_blank())+theme(plot.margin=unit(c(1,1,0,1), "cm"))+scale_shape_manual(values=c(2,16)) 
jblogTotalpopulationsizeallpondsEGINO2<-jblogTotalpopulationsizeallpondsEGINO+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-01"), 15, label=Site, group=NULL))

jblogTotalpopulationsizeallpondsOVVSW<-ggplot(longformcountsbeforemaxbeetlesOVVSW, aes(Date, lnTotalpop, group=Type)) +geom_point(aes(shape=Type))+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(0,1,1,1), "cm"))+ scale_y_continuous(limits=c(7.5,17.25), breaks=pretty_breaks(n=6))+scale_shape_manual(values=c(2,16))
blogTotalpopulationsizeallpondsOVVSW2<-blogTotalpopulationsizeallpondsOVVSW+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-02"), 17, label=Site, group=NULL))


##


beetlogTotalpopulationsizeallpondsEGINO<-ggplot(Larvaepupcounts2018beforeemergenceEGINO, aes(Date)) +ylim(10.5,15.25)+geom_point(aes(y=lnTotalpop))+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(axis.text.x=element_blank())+theme(axis.ticks.x=element_blank())+theme(plot.margin=unit(c(1,1,0,1), "cm")) 
beetlogTotalpopulationsizeallpondsEGINO2<-logTotalpopulationsizeallpondsEGINO+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-01"), 15, label=Site, group=NULL))

beetlogTotalpopulationsizeallpondsOVVSW<-ggplot(Larvaepupcounts2018beforeemergenceOVVSW, aes(Date, lnTotalpop)) +geom_point()+geom_smooth(color="black",method='lm', size=0.5, se=FALSE)+facet_wrap(~Site,ncol=4)+geom_errorbar(aes(ymin=lnlowerSE, ymax=lnupperSE))+ylab(expression(ln(N/Pond)))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(0,1,1,1), "cm"))+ scale_y_continuous(limits=c(10.5,17.25), breaks=pretty_breaks(n=6))
beetlogTotalpopulationsizeallpondsOVVSW2<-logTotalpopulationsizeallpondsOVVSW+ theme(strip.text.x = element_blank())+ geom_text(aes(as.Date("2018-06-02"), 17, label=Site, group=NULL))

