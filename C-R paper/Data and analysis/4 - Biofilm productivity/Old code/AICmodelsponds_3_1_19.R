##upload with biofilm and predictors##
biofilmpondata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstationbiofilm_3_1_19.csv",header = TRUE)  


library(lme4)
library(dplyr)

#null#
Nullmod<-lmer(BiofilmProd~ (1|Pond), REML=F, data=biofilmpondata)

##univariate models## ##nmote I had to to REM=f to be able to rank with AIC#
Perim<-lmer(BiofilmProd~Perim_1 + (1|Pond), REML=F, data=biofilmpondata)
Area<-lmer(BiofilmProd~Area_1+(1|Pond), REML=F, data=biofilmpondata)
Area_loss<-lmer(BiofilmProd~Area_loss_per+(1|Pond), REML=F, data=biofilmpondata)
Depth<-lmer(BiofilmProd~meanDepth+(1|Pond), REML=F, data=biofilmpondata)
DOC<-lmer(BiofilmProd~meanDOC+(1|Pond), REML=F, data=biofilmpondata)
NH4<-lmer(BiofilmProd~meanNH4+(1|Pond), REML=F, data=biofilmpondata)
N03N02<-lmer(BiofilmProd~meanN03N02+(1|Pond), REML=F, data=biofilmpondata)
totalP<-lmer(BiofilmProd~meanlogTotalP+(1|Pond), REML=F, data=biofilmpondata)
Conduct<-lmer(BiofilmProd~meanlogConduc+(1|Pond), REML=F, data=biofilmpondata)
DOperc<-lmer(BiofilmProd~meanDoPerc+(1|Pond), REML=F, data=biofilmpondata)
pH<-lmer(BiofilmProd~meanpH+(1|Pond), REML=F, data=biofilmpondata)
tempYSI<-lmer(BiofilmProd~meanTemp+(1|Pond), REML=F, data=biofilmpondata)
temphobo<-lmer(BiofilmProd~meanTemp2+(1|Pond), REML=F, data=biofilmpondata)
TSavghobo<-lmer(BiofilmProd~Thermalsumavg+(1|Pond), REML=F, data=biofilmpondata)
FPOM<-lmer(BiofilmProd~meanFPOM_adjust+(1|Pond), REML=F, data=biofilmpondata)

#Productivity model##
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, data=biofilmpondata)

##run the AIC without scaling the parameters##
##compare univariate models##
rawaic<-AIC(Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP,DOperc, Conduct,pH,temphobo, TSavghobo, FPOM,full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)



##do the same unscaled data##
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp2+Thermalsumavg+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=biofilmpondata)

##test correllations between##
biofilmpondata<-biofilmpondata[!is.na(biofilmpondata$BiofilmProd), ]


pairs(~Perim_1+Area_1+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp2+Thermalsumavg+meanFPOM_adjust, data=biofilmpondata)

subsetpredicators<- subset(biofilmpondata, select = c(Perim_1,Area_1,Area_loss_per,meanDepth,meanDOC,meanNH4,meanN03N02,meanlogTotalP,meanlogConduc,meanDoPerc,meanpH,meanTemp2,Thermalsumavg,meanFPOM_adjust))
round(cor(subsetpredicators, use = "pairwise.complete.obs"),2)

##don't include models with correllated predictors##
##make a correllation matrix and true/false for ones that are >0.5 correllation##
smat <- abs(cor(subsetpredicators)) <= .5
smat[!lower.tri(smat)] <- NA
i <- as.vector(smat == FALSE & !is.na(smat))

dredgedmodelsallupto4noscale <- dredge(full, subset=smat, m.lim=c(1,4))
bestmodelsetnoscale<-subset(dredgedmodelsallupto4noscale, delta <2) 
avgmodelnoscale<-model.avg(bestmodelsetnoscale)

##using the 2nd perimter and area measurement##
subsetpredicators2<- subset(biofilmpondata, select = c(Perim_2,Area_2,Area_loss_per,meanDepth,meanDOC,meanNH4,meanN03N02,meanlogTotalP,meanlogConduc,meanDoPerc,meanpH,meanTemp2,Thermalsumavg,meanFPOM_adjust))
round(cor(subsetpredicators2, use = "pairwise.complete.obs"),2)

##don't include models with correllated predictors##
##make a correllation matrix and true/false for ones that are >0.5 correllation##
smat2 <- abs(cor(subsetpredicators2)) <= .5
smat2[!lower.tri(smat2)] <- NA
i <- as.vector(smat == FALSE & !is.na(smat))

biofilmpondata<-biofilmpondata[!is.na(biofilmpondata$BiofilmProd), ]

full2<-lmer(BiofilmProd~Perim_2+Area_2+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp2+Thermalsumavg+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=biofilmpondata)


dredgedmodelsallupto4noscale2 <- dredge(full2, subset=smat2, m.lim=c(1,4))
bestmodelsetnoscale2<-subset(dredgedmodelsallupto4noscale2, delta <2) 
avgmodelnoscale2<-model.avg(bestmodelsetnoscale2)

##using the avg perimter and area measurement##
subsetpredicators3<- subset(biofilmpondata, select = c(Perim_avg,Area_avg,Area_loss_per,meanDepth,meanDOC,meanNH4,meanN03N02,meanlogTotalP,meanlogConduc,meanDoPerc,meanpH,meanTemp2,Thermalsumavg,meanFPOM_adjust))
round(cor(subsetpredicators3, use = "pairwise.complete.obs"),2)

##don't include models with correllated predictors##
##make a correllation matrix and true/false for ones that are >0.5 correllation##
smat3 <- abs(cor(subsetpredicators3)) <= .5
smat3[!lower.tri(smat3)] <- NA

biofilmpondata<-biofilmpondata[!is.na(biofilmpondata$BiofilmProd), ]

full3<-lmer(BiofilmProd~Perim_avg+Area_avg+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp2+Thermalsumavg+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=biofilmpondata)


dredgedmodelsallupto4noscale3 <- dredge(full3, subset=smat3, m.lim=c(1,4))
bestmodelsetnoscale3<-subset(dredgedmodelsallupto4noscale3, delta <2) 
avgmodelnoscale3<-model.avg(bestmodelsetnoscale3)
