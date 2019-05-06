PLFAall<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/PLFA_3_5_19.csv")

#summarize across ponds since they are all so similar##
summaryPLFAbiomarker<-PLFAall %>% group_by(Pond2,Type)%>% dplyr::summarise(totalamount=sum(Amount, na.rm=TRUE), percent=(sum(Amount))/mean(Totalmicrobialbiomass)*100)

summaryPLFAtotal<-PLFAall %>% group_by(Pond,Pond2)%>% dplyr::summarise(totalmicrobialamount=sum(Amount, na.rm=TRUE))

#pie charts need to fix to make percentages##
piecharts<-ggplot(summaryPLFAbiomarker, aes(x="", y=percent, fill=Type))+geom_bar(stat="identity",width = 1)+coord_polar("y")+facet_wrap(~Pond2, ncol=4)+ geom_text(aes(label = paste0(round(totalamount))), position = position_stack(vjust = 0.5))+ scale_fill_brewer(palette="Set2")+theme(axis.text.x=element_blank()) + theme(legend.title=element_blank())


##subset for just algae and bacteria#

summaryPLFAautohet<-subset(summaryPLFAbiomarker, Type=="Algae, cyanobacteria, diatoms"|Type=="Bacteria")


write.csv(summaryPLFAautohet, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/PLFAbiomarkers_3_5_19.csv")

##read in##
PLFAsummary18wauto<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/PLFAbiomarkers_3_5_19_2.csv")
PLFAsummary18wauto2<-PLFAsummary18wauto%>% group_by(Pond) %>% dplyr::summarise(meanheterotrophs=mean(Heterotrophic), sdheterotrophs=sd(Heterotrophic), seheterotrophs=sd(Heterotrophic)/sqrt(3), meanAutotrophs=mean(Autotrophic), sdautotrophs=sd(Autotrophic), seautotrophs=sd(Autotrophic)/sqrt(3))

autotrophicvheterotrophic<-ggplot(PLFAsummary18wauto, aes(Autotrophic, Heterotrophic)) +geom_point()+geom_text(aes(label=Pond2),position=position_nudge(y=-20), size=2) +xlab("Biomass of fatty acids from autotrophs (nmol/g)")+ylab("Biomass of fatty acids from heterotrophs (nmol/g)")+xlim(50, 1350)+ylim(50,1350)+ mytheme +geom_abline(slope=1, linetype="dashed")
autotrophicvheterotrophic2<-ggplot(PLFAsummary18wauto2, aes(meanAutotrophs, meanheterotrophs)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-20), size=2) +xlab("Biomass of fatty acids from autotrophs (nmol/g)")+ylab("Biomass of fatty acids from heterotrophs (nmol/g)")+xlim(50, 1250)+ylim(50,1250)+ mytheme +geom_abline(slope=1, linetype="dashed")+ geom_errorbar(aes(ymin = meanheterotrophs-seheterotrophs,ymax = meanheterotrophs+seheterotrophs)) + geom_errorbarh(aes(xmin = meanAutotrophs-seautotrophs,xmax = meanAutotrophs+seautotrophs))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/autovsheterotrophs.png", width = 5, height = 4, units = 'in', res = 800)
autotrophicvheterotrophic2
dev.off()


##does total microbial biomass vary significantly among ponds##
hist(summaryPLFAtotal$totalmicrobialamount)
totallm<-lm(totalmicrobialamount~Pond, data=summaryPLFAtotal)

###changed categorization of the 18:1w7c from algae to bacteria##
#summarize across ponds since they are all so similar##
summaryPLFAbiomarker2<-PLFAall %>% group_by(Pond2,Type2)%>% dplyr::summarise(totalamount=sum(Amount, na.rm=TRUE), percent=(sum(Amount))/mean(Totalmicrobialbiomass)*100)
summaryPLFAautohet2<-subset(summaryPLFAbiomarker2,Type2=="Algae, cyanobacteria, diatoms"|Type2=="Bacteria")
write.csv(summaryPLFAautohet2, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/PLFAbiomarkers_3_5_192.csv")

##read in##
PLFAsummary4<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/PLFAbiomarkers_3_7_19_2.csv")
PLFAsummary5<-PLFAsummary4%>% group_by(Pond) %>% dplyr::summarise(meanheterotrophs=mean(Heterotrophic), sdheterotrophs=sd(Heterotrophic), seheterotrophs=sd(Heterotrophic)/sqrt(3), meanAutotrophs=mean(Autotrophic), sdautotrophs=sd(Autotrophic), seautotrophs=sd(Autotrophic)/sqrt(3))

autotrophicvheterotrophic2<-ggplot(PLFAsummary5, aes(meanAutotrophs, meanheterotrophs)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-20), size=2) +xlab("Biomass of fatty acids from autotrophs (nmol/g)")+ylab("Biomass of fatty acids from heterotrophs (nmol/g)")+xlim(50, 1250)+ylim(50,1250)+ mytheme +geom_abline(slope=1, linetype="dashed")+ geom_errorbar(aes(ymin = meanheterotrophs-seheterotrophs,ymax = meanheterotrophs+seheterotrophs)) + geom_errorbarh(aes(xmin = meanAutotrophs-seautotrophs,xmax = meanAutotrophs+seautotrophs))


###try merging the two categorizations of 18:1w7c##

