Biofilmabiotic<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypondwithbiofilm_3_11_19.csv")


##test relationships between PP and nutrients, temp##

PPNH4<-lm(Algae1~NH4, data=Biofilmabiotic)
PPN03<-lm(Algae1~N03N02 , data=Biofilmabiotic)
PPtotalP<-lm(Algae1~logTotalP, data=Biofilmabiotic)
PPTemp<-lm(Algae1~TempHobo, data=Biofilmabiotic)

PP2NH4<-lm(Algae2~NH4, data=Biofilmabiotic)
PP2N03<-lm(Algae2~N03N02 , data=Biofilmabiotic)
PP2totalP<-lm(Algae2~logTotalP, data=Biofilmabiotic)
PP2Temp<-lm(Algae2~TempHobo, data=Biofilmabiotic)


##test relationships between Heterotrophs and DOC, FPOM, temp##

BacteriaDOC<-lm(Bacteria1~DOC, data=Biofilmabiotic)
BacteriaFPOM<-lm(Bacteria1~FPOM, data=Biofilmabiotic)
BacteriaTemp<-lm(Bacteria1~TempHobo, data=Biofilmabiotic)

Bacteria2DOC<-lm(Bacteria2~DOC, data=Biofilmabiotic)
Bacteria2FPOM<-lm(Bacteria2~FPOM, data=Biofilmabiotic)
Bacteria2Temp<-lm(Bacteria2~TempHobo, data=Biofilmabiotic)

##ratio with DOC##
Ratio1DOC<-lm(RatioAtoB1~DOC, data=Biofilmabiotic)
Ratio2DOC<-lm(RatioAtoB2~DOC, data=Biofilmabiotic)

summary(Ratio1DOC)
summary(Ratio2DOC)

##summaries##
summary(PPNH4) ##NS##
summary(PP2NH4) ##NS##

summary(PPN03) ##NS##
summary(PP2N03) ##NS##

summary(PPtotalP) ##Significant##
summary(PP2totalP)  ##marginal significant#

summary(PPTemp) ##NS##
summary(PP2Temp) ##NS##

summary(BacteriaDOC) ##marginal significant#
summary(Bacteria2DOC) ##marginal significant#

summary(BacteriaFPOM) ##not significant#
summary(Bacteria2FPOM) ##not significant#

summary(BacteriaTemp) ##not significant#
summary(Bacteria2Temp) ##not significant#


PPPhos<-ggplot(Biofilmabiotic, aes(log(TotalP*1000), Algae2)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-21), size=2)+ylim(150,1150)+xlim(1.4,4.2)+ylab("Biomass of Fatty acids derived \n from primary producers (nmol/g)")+xlab("ln(Total P(µg / L))") + mytheme +geom_smooth(color="black", method='lm', size=0.5, se=FALSE, linetype="dashed")+theme(axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0))) 

BacteriaDOC<-ggplot(Biofilmabiotic, aes(DOC, Bacteria2)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-16), size=2)+ylab("Biomass of Fatty acids derived \n from heterotrophs (nmol/g)")+ylim(25,650)+xlim(14,45)+xlab("DOC (mg / L)") + mytheme +geom_smooth(color="black", method='lm', size=0.5, se=FALSE, linetype="dashed")+theme(axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0))) 

library(gtable)
gPP <- ggplotGrob(PPPhos)
gBacteria <- ggplotGrob(BacteriaDOC)

grid.arrange(gPP, gBacteria , ncol = 2)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/biofilmabiotics2.png", width = 5.5, height = 3, units = 'in', res = 800)
grid.arrange(gPP, gBacteria , ncol = 2)
dev.off()