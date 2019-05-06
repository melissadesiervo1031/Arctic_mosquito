
##seperate anovas for the sources of variation between ponds and dates##

SSwaterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/SSbiofilmmultivariateponddata_12_17_18.csv",header = TRUE)  


##anovas for a few variables##

Depthmodel<-lmer(Depth~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
DOCmodel<-lmer(DOC~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
NH4model<-lmer(NH4~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
N03N02model<-lmer(N03N02~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
TotalPmodel<-lmer(LogTotalP~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
pHmodel<-lmer(pH~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
conductmodel<-lmer(logConduc~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
Domodel<-lmer(DoPerc~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
tempmodel<-lmer(Temp~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)
FPOMmodel<-lmer(FPOM_adjust~Pond*Instar+(1|Pond:Sampling.station), data=SSwaterdata)

summary(Depthmodel)
anova(Depthmodel)

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

summary(tempmodel)
anova(tempmodel)

summary(FPOMmodel)
anova(FPOMmodel)

##correllations at the block level##
subsetpredicators<- subset(SSwaterdata, select = c(FPOM_adjust, DOC, N03N02, LogTotalP, Temp, logConduc, DoPerc, pH, Depth))
subsetpredicators<- subset(SSwaterdata, select = c(Depth, DOC, NH4, N03N02, LogTotalP, pH, logConduc, DoPerc, FPOM_adjust))

round(cor(subsetpredicators, use = "pairwise.complete.obs"),2)

##correllations at the pond level##
summarybypond111<-SSwaterdata %>% group_by(Pond) %>% dplyr::summarise(meanDepth=mean(Depth, na.rm=TRUE), meanDOC=mean(DOC, na.rm=TRUE), meanNH4=mean(NH4, na.rm=TRUE), meanN03N02=mean(N03N02, na.rm=TRUE), meanTotalP=mean(TotalP, na.rm=TRUE), meanlogTotalP=mean(LogTotalP, na.rm=TRUE),meanConduc=mean(Conduc, na.rm=TRUE),meanlogConduc=mean(logConduc, na.rm=TRUE), meanTemp=mean(Temp, na.rm=TRUE), meanDoPerc=mean(DoPerc, na.rm=TRUE), meanpH=mean(pH, na.rm=TRUE), meanFPOM_adjust=mean(FPOM_adjust, na.rm=TRUE))

round(summarybypond111,2)


###
write.csv(summarybypond111, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypond_12_18_18.csv")

##
summarybypondarea<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybypondperimarea_12_18_18.csv",header = TRUE)  

##corellation at pond level##
subsetpredicators2<- subset(summarybypondarea, select = c(Area_1, Perimeter_1, meanDepth, meanDOC, meanNH4, meanN03N02, meanlogTotalP, meanpH, meanlogConduc, meanDoPerc, meanFPOM_adjust))

round(cor(subsetpredicators2, use = "pairwise.complete.obs"),2)


##subset just the 2nd and 3rd sampling date...then average across for each sampling station ##
subset2ndpupae<- subset(SSwaterdata, Instar=="2nd3rd"|Instar=="Pupae")

summarybysamplingstation<-SSwaterdata %>% group_by(Pond, Sampling.station) %>% dplyr::summarise(meanDepth=mean(Depth, na.rm=TRUE), meanDOC=mean(DOC, na.rm=TRUE), meanNH4=mean(NH4, na.rm=TRUE), meanN03N02=mean(N03N02, na.rm=TRUE), meanTotalP=mean(TotalP, na.rm=TRUE), meanlogTotalP=mean(LogTotalP, na.rm=TRUE),meanConduc=mean(Conduc, na.rm=TRUE),meanlogConduc=mean(logConduc, na.rm=TRUE), meanTemp=mean(Temp, na.rm=TRUE), meanDoPerc=mean(DoPerc, na.rm=TRUE), meanpH=mean(pH, na.rm=TRUE), meanFPOM_adjust=mean(FPOM_adjust, na.rm=TRUE))

write.csv(summarybysamplingstation, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstation_12_18_18.csv")

##upload with biofilm and predictors##
biofilmpondata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstationbiofilm_12_18_18.csv",header = TRUE)  


biofilmsummaryGP<-biofilmpondata %>% group_by(Pond) %>% dplyr::summarise(meanGrazingpressure=mean(Grazingpressure, na.rm=TRUE),StdevGP=sd(Grazingpressure, na.rm=TRUE), SterrorGP=(sd(Grazingpressure, na.rm=TRUE)/sqrt(n())))
biofilmsummaryProd<-biofilmpondata %>% group_by(Pond) %>% dplyr::summarise(meanProd=mean(BiofilmProd, na.rm=TRUE),StdevProd=sd(BiofilmProd, na.rm=TRUE), SterrorProd=(sd(BiofilmProd, na.rm=TRUE)/sqrt(n())))


#grazing pressure model##
Grazingpressuremodel<-lmer(Grazingpressure~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), data=biofilmpondata)


#Productivity model##
BiofilmProdmodel<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), data=biofilmpondata)
BiofilmProdmodel2<-lmer(BiofilmProd~meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), data=biofilmpondata)

##
BiofilmProdpond<-lm(BiofilmProd~Pond, data=biofilmpondata)
Grazingpressurepond<-lm(Grazingpressure~Pond, data=biofilmpondata)
