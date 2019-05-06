Biofilmdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Biofilm_adjusted_10_22_18.csv",header = TRUE)  

##repeated measures anova with all three grourps (pre, cage, no cage)##

summary(aov(AFDM ~ Treatment + Error(Pond/Treatment), data=Biofilmdata))



##make graph ##
summarybytreatment<-Biofilmdata %>% group_by(Treatment) %>% dplyr::summarise(mean=mean(AFDM, na.rm=TRUE), Stdev=sd(AFDM, na.rm=TRUE), Sterror=(sd(AFDM, na.rm=TRUE)/sqrt(n())))

Biofilmdata$Treatment <- factor(Biofilmdata$Treatment, levels = c("Pre", "Cage", "No Cage"))
summarybytreatment$Treatment <- factor(summarybytreatment$Treatment, levels = c("Pre", "Cage", "No Cage"))

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

summary(aov(AFDM ~ Treatment + Error(Pond/Treatment), data=cagenocage))

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
