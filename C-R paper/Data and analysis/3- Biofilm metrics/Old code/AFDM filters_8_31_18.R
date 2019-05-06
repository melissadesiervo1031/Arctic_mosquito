##Food quality experiment results 2018##

Filtersbiofilm<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Filters and FPOM/Filters_practice_8_31_18.csv", header = TRUE)  # read csv file 

str(Filtersbiofilm)

##total all ponds##
summarybytreatment<-Filtersbiofilm %>% group_by(Treatment) %>% dplyr::summarise(mean=mean(AFDM_adjust, na.rm=TRUE), Stdev=sd(AFDM_adjust, na.rm=TRUE), Sterror=(sd(AFDM_adjust, na.rm=TRUE)/sqrt(n())))

mytheme<- theme_bw()+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
AFDMtotalgraph<-ggplot(summarybytreatment, aes(x=Treatment, y=mean))+ geom_bar(stat="identity", width=0.5)+geom_errorbar(aes(ymin=mean-Sterror, ymax=mean+Sterror), colour="black", width=0.08)+xlab("")+ylab("AFDM (ug)")+mytheme+ theme(legend.position="none")

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/AFDMbiofilmtotal.png", width = 4, height = 4, units = 'in', res = 800)
AFDMtotalgraph
dev.off()

##aov##
anova1<-aov(AFDM_adjust~Treatment, data=Filtersbiofilm)
summary(anova1)

##plot by pond##

mytheme<- theme_bw()+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
AFDMbypond <- ggplot(Filtersbiofilm, aes(Treatment, AFDM_adjust)) + geom_point(aes(color=Pond), stat="identity")+ facet_wrap(vars(Pond))+xlab("")+ylab("AFDM (ug)")+mytheme+ theme(legend.position="none")


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/AFDMbiofilm.png", width = 7, height = 7, units = 'in', res = 800)
AFDMbypond
dev.off()

##aov##
anova2<-aov(AFDM_adjust~Treatment*Pond, data=Filtersbiofilm)
summary(anova2)

#loadin a differently organized filed..same data#

Filtersbiofilm2<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Filters and FPOM/Biofilm_filters_9_3_18.csv", header = TRUE)  # read csv file 

Filtersbiofilm2<-Filtersbiofilm2 %>% mutate(cageminuspre = AFDMCage - AFDMPre)%>% mutate(cageminusnocage = AFDMCage - AFDMNoCage)

summarybypond<-Filtersbiofilm2 %>% group_by(Pond) %>% dplyr::summarise(Stockearly=mean(AFDMPre, na.rm=TRUE), StockearlyStdev=sd(AFDMPre, na.rm=TRUE), StockearlySterror=(sd(AFDMPre, na.rm=TRUE)/sqrt(n())), Stocklate=mean(AFDMNoCage, na.rm=TRUE), StocklateStdev=sd(AFDMNoCage, na.rm=TRUE), StocklateSterror=(sd(AFDMNoCage, na.rm=TRUE)/sqrt(n())), Cageminpre=mean(cageminuspre, na.rm=TRUE), CageminpreStdev=sd(cageminuspre, na.rm=TRUE), CageminpreSterror=(sd(cageminuspre, na.rm=TRUE)/sqrt(n())), Cageminnocage=mean(cageminusnocage, na.rm=TRUE), CageminnocageStdev=sd(cageminusnocage, na.rm=TRUE), CageminnocageSterror=(sd(cageminusnocage, na.rm=TRUE)/sqrt(n())))

write.csv(summarybypond, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Filters and FPOM/Biofilm_filters_summary.csv")  # read csv file 

##load data in different organization##
Biofilmstandingstock<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Filters and FPOM/Biofilm_filters_summary_stock.csv", header = TRUE)  # read csv file 

Biofilmearlylate<-ggplot(Biofilmstandingstock, aes(x=Pond, y=Stock, fill=EarlyLate))+ geom_bar(position=position_dodge(), stat="identity")+geom_errorbar(aes(ymin=Stock-sterror, ymax=Stock+sterror), colour="black", width=0.08,position=position_dodge(0.85)) +xlab("")+ylab("Biofilm production (ug/cm)")+mytheme+theme(legend.title=element_blank())
png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Biofilmearlylate.png", width = 7, height = 4, units = 'in', res = 800)
Biofilmearlylate


##import data on mosquito abundance (and eventually size##)
dev.off()


##Biofilm with exclusion##
mytheme2<- theme_bw()+theme(axis.line.x= element_blank())+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
biofilmwithexclusion<-ggplot(summarybypond, aes(x=Pond, y=Cageminpre))+ geom_bar(aes(fill=Pond), stat="identity", width=0.5)+geom_errorbar(aes(ymin=Cageminpre-CageminpreSterror, ymax=Cageminpre+CageminpreSterror), colour="black", width=0.08) +xlab("")+ylab("Biofilm production with exclusion (ug/cm)")+ geom_hline(yintercept = 0) +mytheme2+ theme(legend.position="none")

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/biofilmwithexclusion.png", width = 6, height = 4, units = 'in', res = 800)
biofilmwithexclusion
dev.off()

##grazing pressure##
mytheme2<- theme_bw()+theme(axis.line.x= element_blank())+theme(panel.grid.minor = element_blank())+theme(panel.grid.major =  element_blank())+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=7, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))
grazingpressure<-ggplot(summarybypond, aes(x=Pond, y=Cageminnocage))+ geom_bar(aes(fill=Pond), stat="identity", width=0.5)+geom_errorbar(aes(ymin=Cageminnocage-CageminnocageSterror, ymax=Cageminnocage+CageminnocageSterror), colour="black", width=0.08) +xlab("")+ylab("Grazing pressure (ug/cm)")+ geom_hline(yintercept = 0) +mytheme2+ theme(legend.position="none")

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/grazingpressure.png", width = 6, height = 4, units = 'in', res = 800)
grazingpressure
dev.off()
