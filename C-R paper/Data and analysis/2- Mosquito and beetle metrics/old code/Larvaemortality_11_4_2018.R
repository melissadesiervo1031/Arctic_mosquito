##files##
Larvaepupaeallyears<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Larvae pupae analysis/larvalcounts_allyears_2018.csv")

Larvaepupaeallyears$Larvalstage<-factor(Larvaepupaeallyears$Larvalstage, levels=c("Hatch", "One", "OneTwo", "Two", "TwoThree", "Three", "ThreeFour", "Four", "FourPupae", "Pupae"))

#packages##
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
library(scales)

#Date#
Larvaepupaeallyears$Date<-as.Date(Larvaepupaeallyears$Date, format="%d-%b-%y")
Larvaepupaeallyears$Day<-as.Date(Larvaepupaeallyears$Day, format="%d-%b-%y")
Larvaepupaeallyears$Year<-as.factor(Larvaepupaeallyears$Year)

##add on column for log##

Larvaepupaeallyears <-mutate(Larvaepupaeallyears, logtotal2 = log(1+Total))
Larvaepupaebeetlesallyears <-mutate(Larvaepupaeallyears, beetlelogtotal = log(1+Beetle))

##getinunitsofLITERS## 350ml * 5 = 1.75L
Larvaepupaeallyears<-mutate(Larvaepupaeallyears, Totalliter = Total/1.75)
Larvaepupaeallyears<-mutate(Larvaepupaeallyears, logtotalliter = log(1+Totalliter))


##meansbydateNolog##
Larvaepupaeallyearsmeans<-Larvaepupaeallyears %>% group_by(Site, Year, Date, Day) %>% dplyr::summarise(Larvaepupae=mean(Total, na.rm=TRUE), Stdev=sd(Total, na.rm=TRUE), Sterror=(sd(Total, na.rm=TRUE)/sqrt(n())))

##meansbydateln##
Larvaepupaeallyearslnmeans<-Larvaepupaeallyears %>% group_by(Site, Year, Date, Day) %>% dplyr::summarise(lnLarvaepupae=mean(logTotal, na.rm=TRUE), Stdev=sd(logTotal, na.rm=TRUE), Sterror=(sd(logTotal, na.rm=TRUE)/sqrt(n())))

##meanslnliters##
Larvaepupaeallyearslnmeansliter<-Larvaepupaeallyears %>% group_by(Site, Year, SiteYear,Date, Day, Larvalstage) %>% dplyr::summarise(lnLarvaepupaeliter=mean(logtotalliter, na.rm=TRUE), Stdevliter=sd(logtotalliter, na.rm=TRUE), Sterrorliter=(sd(logtotalliter, na.rm=TRUE)/sqrt(n())))

##summary by date## 
summaryLarvaepupcounts2018 <- summarySE(Larvaepupaeallyears, measurevar="logtotalliter", groupvars=c("Year", "Site", "SiteYear", "Date", "Day",  "Larvalstage"))

#subsetbyyear##
summarylarvpup2011<-subset(summaryLarvaepupcounts2018, Year == 2011)
summarylarvpup2012<-subset(summaryLarvaepupcounts2018, Year == 2012)
summarylarvpup2017<-subset(summaryLarvaepupcounts2018, Year == 2017)
summarylarvpup2018<-subset(summaryLarvaepupcounts2018, Year == 2018)


##N0 for 2018 ponds##
N02018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="One"|Larvalstage =="OneTwo")
summaryN02018 <- summarySE(N02018ponds, measurevar="logtotalliter", groupvars=c("Year", "Site", "SiteYear"))
larvalstage <- rep("N0",length(summaryN02018))
summaryN02018 <-cbind(summaryN02018, larvalstage)

##third instars##
threefour2018ponds<-subset(Larvaepupaeallyears , Year == 2018 & Larvalstage =="Three"|Larvalstage =="ThreeFour")
NoOilthreefour<-subset(Larvaepupaeallyears , Year == 2018 & Site =="NoOil" & Larvalstage =="TwoThree"|Larvalstage =="Four")
threefour2018ponds<-rbind(threefour2018ponds,NoOilthreefour)

summarythreefour2018 <- summarySE(threefour2018ponds, measurevar="logtotalliter", groupvars=c("Year", "Site", "SiteYear"))
larvalstage <- rep("threefour",length(summarythreefour2018))

summarythreefour2018 <-cbind(summarythreefour2018 , larvalstage)

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


#graph##
##combine together for graph##
summaryearlylate2018<-rbind(summaryN02018, summarythreefour2018)

abundancebypond<-ggplot(summaryearlylate2018, aes(x=Site, y=logtotalliter, fill=larvalstage))+ geom_bar(position=position_dodge(), stat="identity")+geom_errorbar(aes(ymin=logtotalliter+se, ymax=logtotalliter-se), colour="black", width=0.08,position=position_dodge(0.85)) +xlab("")+ylab("ln(N / liter)") +mytheme2+theme(legend.title=element_blank())+ theme(legend.position=c(0.1, 0.87))+scale_fill_manual(values=c("gray80", "gray24"),labels=c("Early instars", "Late instars"))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Nlarvaebypond2.png", width = 7, height = 4, units = 'in', res = 800)
abundancebypond
dev.off()