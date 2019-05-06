
##seperate anovas for the sources of variation between ponds and dates##

SSwaterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/SSbiofilmmultivariateponddata_12_17_18.csv",header = TRUE)  


##correllations at the pond level##

#averages for YSI data across the whole period##
summarybypond111<-SSwaterdata %>% group_by(Pond) %>% dplyr::summarise(meanDepth=mean(Depth, na.rm=TRUE), meanDOC=mean(DOC, na.rm=TRUE), meanNH4=mean(NH4, na.rm=TRUE), meanN03N02=mean(N03N02, na.rm=TRUE), meanTotalP=mean(TotalP, na.rm=TRUE), meanlogTotalP=mean(LogTotalP, na.rm=TRUE),meanConduc=mean(Conduc, na.rm=TRUE),meanlogConduc=mean(logConduc, na.rm=TRUE), meanTemp=mean(Temp, na.rm=TRUE), meanDoPerc=mean(DoPerc, na.rm=TRUE), meanpH=mean(pH, na.rm=TRUE), meanFPOM_adjust=mean(FPOM_adjust, na.rm=TRUE))

##other variables (perim area hobotemps)
summarybypondarea<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypondperim_3_4_19.csv",header = TRUE)  

mergeddata<- merge(summarybypondarea,summarybypond111,by="Pond")

subsetpredicators<- subset(mergeddata, select = c(Perimeter_avg, Area_avg, Area_loss,meanDepth,  meanTempHobo, medianThermalsum, meanDOC, meanNH4, meanN03N02, meanlogTotalP,  meanlogConduc, meanDoPerc, meanpH, meanFPOM_adjust))

round(cor(subsetpredicators, use = "pairwise.complete.obs"),2)

plot(subsetpredicators$meanTempHobo, subsetpredicators$meanlogTotalP)

###
write.csv(summarybypond111, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypond_12_18_18.csv")

##
summarybypondarea<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypondperimarea_12_18_18.csv",header = TRUE)  

##corellation at pond level##
subsetpredicators2<- subset(summarybypondarea, select = c(Area_1, Perimeter_1, meanDepth, meanDOC, meanNH4, meanN03N02, meanlogTotalP, meanpH, meanlogConduc, meanDoPerc, meanFPOM_adjust))

round(cor(subsetpredicators2, use = "pairwise.complete.obs"),2)

##MANOVA -- Do all pond variables vary w/ respect to pond and date## 
alldeps<-cbind(SSwaterdata$FPOM_adjust, SSwaterdata$DOC, SSwaterdata$NH4, SSwaterdata$N03N02, SSwaterdata$LogTotalP, SSwaterdata$logConduc, SSwaterdata$DoPerc, SSwaterdata$pH, SSwaterdata$Depth)

manRes1 <- manova(cbind(FPOM_adjust,DOC, NH4, N03N02, LogTotalP, logConduc, DoPerc, pH, Depth) ~ Pond*Instar+Error(Pond|Sampling.station), data = SSwaterdata, na.action=na.omit)
summary(manRes1, tol=0)


##anovas for a few variables##

#bonferroni adjustment....a0.05=0.0056, a0.01=0.0011, a0.001=0.00011116##

Depthmodel<-lmer(Depth~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
DOCmodel<-lmer(DOC~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
NH4model<-lmer(NH4~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
N03N02model<-lmer(N03N02~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
TotalPmodel<-lmer(LogTotalP~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
pHmodel<-lmer(pH~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
conductmodel<-lmer(logConduc~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
Domodel<-lmer(DoPerc~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)
FPOMmodel<-lmer(FPOM_adjust~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata, na.action=na.omit)

summary(Depthmodel)
anova(Depthmodel)
p.Depth <- anova(Depthmodel)$coefficients[, 6]

summary(DOCmodel)
anova(DOCmodel)

summary(NH4model)
anova(NH4model)

summary(N03N02model)
anova(N03N02model)

summary(TotalPmodel)
anova(TotalPmodel)

summary(pHmodel)
anova(pHmodel)

summary(conductmodel)
anova(conductmodel)

summary(Domodel)
anova(Domodel)

summary(FPOMmodel)
anova(FPOMmodel)




