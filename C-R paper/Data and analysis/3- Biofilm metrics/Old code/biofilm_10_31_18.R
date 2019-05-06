Biofilmdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Biofilm_adjusted_10_22_18.csv",header = TRUE)  

##repeated measures anova with all three grourps (pre, cage, no cage)##

summary(aov(AFDM ~ Treatment + Error(Pond/Treatment), data=Biofilmdata))

##make graph ##
summarybytreatment<-Biofilmdata %>% group_by(Treatment) %>% dplyr::summarise(mean=mean(AFDM, na.rm=TRUE), Stdev=sd(AFDM, na.rm=TRUE), Sterror=(sd(AFDM, na.rm=TRUE)/sqrt(n())))

Biofilmdata$Treatment <- factor(Biofilmdata$Treatment, levels = c("Pre", "Cage", "No cage"))
summarybytreatment$Treatment <- factor(summarybytreatment$Treatment, levels = c("Pre", "Cage", "No cage"))

mytheme<- theme_bw()+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
AFDMtotalgraph2<-ggplot(summarybytreatment, aes(x=Treatment, y=mean))+ geom_bar(stat="identity", width=0.5)+geom_errorbar(aes(ymin=mean-Sterror, ymax=mean+Sterror), colour="black", width=0.08)+xlab("")+ylab(expression(~ µg  ~C / ~ 360 ~ cm^{2} / ~ "week"))+mytheme+ theme(legend.position="none")+ scale_x_discrete(labels = c('Early season','Exclosure','No Exclosure'))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/AFDMbiofilmperweek.png", width = 4, height = 4, units = 'in', res = 800)
AFDMtotalgraph2
dev.off()

##same data but with lines between for ponds##

AFDMbysite<-ggplot(data=Biofilmdata, aes(x=Treatment, y=AFDM, group=Pond, color=Pond)) +geom_line()+geom_point()+xlab("")+ylab(expression(~ µg  ~C / ~ 360 ~ cm^{2} / ~ "week"))+mytheme+ theme(legend.position="right")+ theme(legend.title=element_blank())+ scale_x_discrete(labels = c('Early season','Exclosure','No Exclosure'))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/AFDMbysite.png", width = 6, height = 4, units = 'in', res = 800)
AFDMbysite
dev.off()

##same things but only for cage vs. no cage##


##paired t test for caged vs. no cage##
cagenocage<-subset(Biofilmdata, Treatment == "Cage"|Treatment == "No cage" )


Cage<-subset(Biofilmdata, Treatment == "Cage")
NoCage<-subset(Biofilmdata, Treatment == "No cage")
t.test(Cage$AFDM, NoCage$AFDM, paired = TRUE, alternative = "greater")

head(summarybytreatment)

summarybytreatment2<-subset(summarybytreatment, Treatment == "Cage"|Treatment == "No cage" )
summarybytreatment2$Treatment <- factor(summarybytreatment2$Treatment, levels = c("Cage", "No Cage"))

mytheme<- theme_bw()+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
AFDMcagenocage2<-ggplot(summarybytreatment2, aes(x=Treatment, y=mean))+ geom_bar(stat="identity", width=0.5)+geom_errorbar(aes(ymin=mean-Sterror, ymax=mean+Sterror), colour="black", width=0.08)+xlab("")+ylab(expression(~ µg  ~C / ~ 360 ~ cm^{2} / ~ "week"))+mytheme+ theme(legend.position="none")+ scale_x_discrete(labels = c('Exclosure','No Exclosure'))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/cagenocage.png", width = 3, height = 3, units = 'in', res = 800)
AFDMcagenocage2
dev.off()

##same thing but with lines##

cagenocage$Treatment <- factor(cagenocage$Treatment, levels = c("Cage", "No Cage"))

AFDMbysitecagenocage<-ggplot(data=cagenocage, aes(x=Treatment, y=AFDM, group=Pond, color=Pond)) +geom_line()+geom_point()+xlab("")+ylab(expression(~ µg  ~C / ~ 360 ~ cm^{2} / ~ "week"))+mytheme+ theme(legend.position="right")+ theme(legend.title=element_blank())+ scale_x_discrete(labels = c('Exclosure','No Exclosure'))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/AFDMbysitecagenocage.png", width = 5, height = 3.5, units = 'in', res = 800)
AFDMbysitecagenocage
dev.off()


##look at data within ponds##
Biofilmdatabytreatment<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/TreatmentAFDM_10_22_18.csv",header = TRUE)  

summarybytreatment111<-Biofilmdatabytreatment %>% group_by(Pond, Treatment) %>% dplyr::summarise(mean=mean(AFDMtreatment, na.rm=TRUE), Stdev=sd(AFDMtreatment, na.rm=TRUE), Sterror=(sd(AFDMtreatment, na.rm=TRUE)/sqrt(n())))

summarybytreatment111$Treatment <- factor(summarybytreatment111$Treatment, levels = c("Pre", "Cage", "No cage"))
Biofilmbypond<-ggplot(summarybytreatment111, aes(x=Pond, y=mean, fill=Treatment))+ geom_bar(position=position_dodge(), stat="identity")+geom_errorbar(aes(ymin=mean-Sterror, ymax=mean+Sterror), colour="black", width=0.08,position=position_dodge(0.85)) +xlab("")+ylab(expression("Total Biofilm Production" (~ µg  ~C / ~ 360 ~ cm^{2})))+mytheme+theme(legend.title=element_blank())+ scale_fill_discrete(labels=c("Early season (13 days)", "Full season exclosure (33 days)", "Full season no exclosure (33 days)"))+ theme(legend.position=c(0.185, 0.87))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Biofilmbypond2.png", width = 7, height = 4, units = 'in', res = 800)
Biofilmbypond
dev.off()

###biofilm treatment reorganized##
Biofilmdatabytreatment2<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Biofilmtreatment_10_24_18.csv",header = TRUE)  

pondcalculations<-Biofilmdatabytreatment2 %>% group_by(Pond) %>% dplyr::summarise(meanCageminnocage=mean(Cageminnocage, na.rm=TRUE), StdevCageminnocage=sd(Cageminnocage, na.rm=TRUE), SterrorCageminnocage=(sd(Cageminnocage, na.rm=TRUE)/sqrt(n())), meanCageminpre=mean(Cageminpre, na.rm=TRUE), StdevCageminpre=sd(Cageminpre, na.rm=TRUE), SterrorCageminpre=(sd(Cageminpre, na.rm=TRUE)/sqrt(n())), meanNoCageminpre=mean(NoCageminpre, na.rm=TRUE), StdevNoCageminpre=sd(NoCageminpre, na.rm=TRUE), SterrorNoCageminpre=(sd(NoCageminpre, na.rm=TRUE)/sqrt(n())))


##
##Biofilm with exclusion##
mytheme2<- theme_bw()+theme(axis.line.x= element_blank())+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
cageminpregraph<-ggplot(pondcalculations, aes(x=Pond, y=meanCageminpre))+ geom_bar(aes(fill=Pond), stat="identity", width=0.5)+geom_errorbar(aes(ymin=meanCageminpre-SterrorCageminpre, ymax=meanCageminpre+SterrorCageminpre), colour="black", width=0.08) +xlab("")+ylab(expression("Change in Biofilm production with exclusion" (~ µg  ~C / ~ 360 ~ cm^{2})))+ geom_hline(yintercept = 0) +mytheme2+ theme(legend.position="none")
hist(Biofilmdatabytreatment2$Cageminpre)

cageminpremodel<-lm(Cageminpre~Pond, data=Biofilmdatabytreatment2)
anova(cageminpremodel)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/cageminpregraph.png", width = 7, height = 4, units = 'in', res = 800)
cageminpregraph
dev.off()

##Biofilm without exclusion##
mytheme2<- theme_bw()+theme(axis.line.x= element_blank())+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
nocageminpregraph<-ggplot(pondcalculations, aes(x=Pond, y=meanNoCageminpre))+ geom_bar(aes(fill=Pond), stat="identity", width=0.5)+geom_errorbar(aes(ymin=meanNoCageminpre-SterrorNoCageminpre, ymax=meanNoCageminpre+SterrorNoCageminpre), colour="black", width=0.08) +xlab("")+ylab(expression("Change in Biofilm production without exclusion" (~ µg  ~C / ~ 360 ~ cm^{2})))+ geom_hline(yintercept = 0) +mytheme2+ theme(legend.position="none")

hist(Biofilmdatabytreatment2$NoCageminpre)
hist(log(Biofilmdatabytreatment2$NoCageminpre))

Nocageminpremodel<-lm(log(NoCageminpre)~Pond, data=Biofilmdatabytreatment2)
anova(Nocageminpremodel)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Nocageminpregraph.png", width = 7, height = 4, units = 'in', res = 800)
nocageminpregraph
dev.off()

##grazing pressure##
cageminnocageraph<-ggplot(pondcalculations, aes(x=Pond, y=meanCageminnocage))+ geom_bar(aes(fill=Pond), stat="identity", width=0.5)+geom_errorbar(aes(ymin=meanCageminnocage-SterrorCageminnocage, ymax=meanCageminnocage+SterrorCageminnocage), colour="black", width=0.08) +xlab("")+ylab(expression("Biofilm production exclosure - no exclosure" (~ µg  ~C / ~ 360 ~ cm^{2})))+ geom_hline(yintercept = 0) +mytheme2+ theme(legend.position="none")

hist(Biofilmdatabytreatment2$Cageminnocage)
cageminnocagemodel<-lm(Cageminnocage~Pond, data=Biofilmdatabytreatment2)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/cageminnocageraph.png", width = 7, height = 4, units = 'in', res = 800)
cageminnocageraph
dev.off()

           
##seperate anovas for the sources of variation between ponds and dates##

SSwaterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/SSbiofilmmultivariateponddata_10_24_18.csv",header = TRUE)  
tempdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Tempallponds_10_24_18.csv",header = TRUE)  


##anovas for a few variables##

#Area#
#Perim#

Depthmodel<-lm(Depth~Pond*Instar, data=SSwaterdata)
DOCmodel<-lm(DOC~Pond*Instar, data=SSwaterdata)
NH4model<-lm(NH4~Pond*Instar, data=SSwaterdata)
N03N02model<-lm(N03N02~Pond*Instar, data=SSwaterdata)
TotalPmodel<-lm(LogTotalP~Pond*Instar, data=SSwaterdata)
pHmodel<-lm(pH~Pond*Instar, data=SSwaterdata)
conductmodel<-lm(logConduc~Pond*Instar, data=SSwaterdata)
Domodel<-lm(DoPerc~Pond*Instar, data=SSwaterdata)
FPOMmodel<-lm(FPOM_adjust~Pond*Instar, data=SSwaterdata)

anova(Depthmodel)
anova(DOCmodel)
anova(NH4model)
anova(N03N02model)
anova(TotalPmodel)
anova(pHmodel)
anova(conductmodel)
anova(Domodel)
anova(FPOMmodel)

#temperature##
tempearlylate<-subset(tempdata, Period=="Early"|Period=="Late")
tempbyday<-tempearlylate %>% group_by(Pond, Date, Period) %>% dplyr::summarise(temp=mean(Temp, na.rm=TRUE))

tempmodel<-lm(temp~Pond*Period, data=tempbyday)


##correllation coefficients##

SSwaterdataind<-SSwaterdata[, -c(1, 2, 3, 4)]

cor(SSwaterdataind, use="complete.obs", method="pearson") 

help(cor)

##biofilm by pond predictors##
biofilmpredictors<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/biofilmpredictors_10_25_18.csv",header = TRUE)  

#Total N needs to be log transformed##
hist(log(biofilmpredictors$TotalN))
hist(log(biofilmpredictors$CageAFDM))

#models##
Null<-lm(log(CageAFDM)~1, data=biofilmpredictors)
Nmodel1<-lm(log(CageAFDM)~log(TotalN), data=biofilmpredictors)
Pmodel1<-lm(log(CageAFDM)~LogTotalP, data=biofilmpredictors)
NPmodel1<-lm(log(CageAFDM)~LogTotalP+log(TotalN), data=biofilmpredictors)
NPmodelint1<-lm(log(CageAFDM)~LogTotalP*log(TotalN), data=biofilmpredictors)

rawaic<-AIC(Null, Nmodel1, Pmodel1, NPmodel1, NPmodelint1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)

plot(log(CageAFDM)~log(TotalN), data=biofilmpredictors)
plot(log(CageAFDM)~LogTotalP, data=biofilmpredictors)

##
Null<-lm(log(CageAFDM)~1, data=biofilmpredictors)
FPOMmodel1<-lm(log(CageAFDM)~FPOM, data=biofilmpredictors)
DOCmodel1<-lm(log(CageAFDM)~DOC , data=biofilmpredictors)
FPOMDOCmodel1<-lm(log(CageAFDM)~FPOM+DOC , data=biofilmpredictors)
FPOMDOCmodelint1<-lm(log(CageAFDM)~FPOM*DOC , data=biofilmpredictors)

rawaic<-AIC(Null, FPOMmodel1, DOCmodel1, FPOMDOCmodel1, FPOMDOCmodelint1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)

#models##
Null<-lm(Cageminpre~1, data=biofilmpredictors)
Nmodel1<-lm(Cageminpre~log(TotalN), data=biofilmpredictors)
Pmodel1<-lm(Cageminpre~LogTotalP, data=biofilmpredictors)
NPmodel1<-lm(Cageminpre~LogTotalP+log(TotalN), data=biofilmpredictors)
NPmodelint1<-lm(Cageminpre~LogTotalP*log(TotalN), data=biofilmpredictors)

rawaic<-AIC(Null, Nmodel1, Pmodel1, NPmodel1, NPmodelint1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)

plot(log(Cageminpre)~log(TotalN), data=biofilmpredictors)
plot(log(Cageminpre)~LogTotalP, data=biofilmpredictors)

##
Null<-lm(Cageminpre~1, data=biofilmpredictors)
FPOMmodel1<-lm(Cageminpre~FPOM, data=biofilmpredictors)
DOCmodel1<-lm(Cageminpre~DOC , data=biofilmpredictors)
FPOMDOCmodel1<-lm(Cageminpre~FPOM+DOC , data=biofilmpredictors)
FPOMDOCmodelint1<-lm(Cageminpre~FPOM*DOC , data=biofilmpredictors)

rawaic<-AIC(Null, FPOMmodel1, DOCmodel1, FPOMDOCmodel1, FPOMDOCmodelint1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)

##temp##
##
Null<-lm(Cageminpre~1, data=biofilmpredictors)
Tempmodel1<-lm(Cageminpre~Temp, data=biofilmpredictors)
rawaic<-AIC(Null, Tempmodel1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)

##other##
##
Null<-lm(Cageminpre~1, data=biofilmpredictors)
DOmodel1<-lm(Cageminpre~DO, data=biofilmpredictors)
pHmodel1<-lm(Cageminpre~pH, data=biofilmpredictors)
Conducmodel1<-lm(Cageminpre~Conduc, data=biofilmpredictors)
ConducpHmodel1<-lm(Cageminpre~Conduc+pH, data=biofilmpredictors)


rawaic<-AIC(Null, DOmodel1, pHmodel1, Conducmodel1, ConducpHmodel1)
nR<-dim(biofilmpredictors)[1]  #Sample size 
aictable(rawaic,nR)


###biofilm with mosquito population predictors## 

##diff cage no cage vs. # of larvae##
mosabundance<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Filters and FPOM/Mosquitabundancebypond_2018.csv", header = TRUE)  # read csv file 
biofilmpredictorslarvabund<-cbind(biofilmpredictors, mosabundance$Log34instars)

grazingpressuregraph<-ggplot(biofilmpredictorslarvabund, aes(x=mosabundance$Log34instars, y=Cageminnocage, colour=Pond))+ geom_point(size=2)+geom_text(aes(label=Pond),vjust = -0.5)+xlab("log(N late instars)")+ylab("Grazing pressure cage-no cage (ug/360 cm)")+ggtitle("")+ mytheme + theme(legend.position="none")+xlim(1.5,4)+ylim(-2,20)

grazingpressuremodel<-lm(Cageminnocage~mosabundance$Log34instars, data=biofilmpredictorslarvabund)
summary(grazingpressuremodel)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/grazingpressuregraph.png", width = 6, height = 5, units = 'in', res = 800)
grazingpressuregraph
dev.off()


##Biofilm production (cage min pre) vs. size##
emersize2018<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Emertrap_size_9_7_18.csv",header = TRUE)  
summarysizebypond<-emersize2018 %>% group_by(Pond) %>% dplyr::summarise(meanwing=mean(Winglength2, na.rm=TRUE))

biofilmpredictorswing<-cbind(biofilmpredictors, summarysizebypond$meanwing)

biofilmbysize<-ggplot(biofilmpredictorswing, aes(x=summarysizebypond$meanwing, y=Cageminpre, colour=Pond))+ geom_point(size=2)+geom_text(aes(label=Pond),vjust = -0.5)+xlab("Average wing length (mm)")+ylab("Biofilm production cage-pre (ug/360 cm)")+ggtitle("")+ mytheme + theme(legend.position="none")+ xlim(3.5,5.75)+ylim(-5,40)

biofilmsizemodel<-lm(Cageminpre~summarysizebypond$meanwing, data=biofilmpredictorswing)
anova(biofilmsizemodel)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/biofilmbysize.png", width = 6, height = 5, units = 'in', res = 800)
biofilmbysize
dev.off()

##Biofilm production (cage min pre) vs. Number of emerging##
totalemer2018<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/totalemergence_2018_10_30_18.csv",header = TRUE)  

biofilmpredictorswingnumber<-cbind(biofilmpredictorswing, totalemer2018$Totalpertrap)

hist(log(totalemer2018$Totalpertrap))

biofilmbynumberemerg<-ggplot(biofilmpredictorswingnumber, aes(x=log(totalemer2018$Totalpertrap), y=Cageminpre, colour=Pond))+ geom_point(size=2)+geom_text(aes(label=Pond),vjust = -0.5)+xlab("log(N emerging adults)/trap")+ylab("Biofilm production cage-pre (ug/360 cm)")+ggtitle("")+ mytheme + theme(legend.position="none")+ylim(-5,40)+xlim(2,7)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/biofilmbynumberemerg.png", width = 6, height = 5, units = 'in', res = 800)
biofilmbynumberemerg
dev.off()

biofilmnumbermodel<-lm(Cageminpre~totalemer2018$Totalpertrap, data=biofilmpredictorswingnumber)
anova(biofilmnumbermodel)
