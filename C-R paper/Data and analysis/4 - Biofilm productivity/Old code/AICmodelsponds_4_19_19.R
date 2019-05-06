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

##same thing on the scaled data##
scaledpredictors3<-scale(subsetpredicators3)
biofilmdepnames<-subset(biofilmpondata, select=c("Pond", "BiofilmProd"))
scaledbiofilmponddata<-cbind(biofilmdepnames, scaledpredictors3)
scaledbiofilmponddata<-scaledbiofilmponddata[!is.na(scaledbiofilmponddata$BiofilmProd), ]

scaledfull3<-lmer(BiofilmProd~Perim_avg+Area_avg+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp2+Thermalsumavg+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)

dredgedmodelsallupto4scale3 <- dredge(scaledfull3, subset=smat3, m.lim=c(1,4))
bestmodelsetscaled3<-subset(dredgedmodelsallupto4scale3, delta <2) 
avgmodelscaled3<-model.avg(bestmodelsetscaled3)


##get the adjusted ys##
predicty1<-predict(model.avg(bestmodelsetnoscale3, fit=TRUE))
predictyscaled<-predict(model.avg(bestmodelsetscaled3, fit=TRUE))

yandresids<-as.data.frame(cbind(predictyscaled,scaledbiofilmponddata$BiofilmProd))
names(yandresids) <- c("predictedyscaled", "BiofilmProdactual")
yandresids<-mutate(yandresids, resids = BiofilmProdactual-predictyscaled)

Perimadjustedy<-as.data.frame(( 0.008321317*(biofilmpondata$Perim_avg-mean(biofilmpondata$Perim_avg))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))
Depthadjustedy<-as.data.frame((0.047136*(biofilmpondata$meanDepth-mean(biofilmpondata$meanDepth))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))
Doadjustedy<-as.data.frame(( -0.015821527*(biofilmpondata$meanDoPerc-mean(biofilmpondata$meanDoPerc))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))

library(tidyr)
alladjustedys<-cbind(Perimadjustedy, Depthadjustedy)
alladjustedys<-cbind(alladjustedys, Doadjustedy)
names(alladjustedys) <- c("Perimadjustedy", "Depthadjustedy","Doadjustedy")
alladjustedys<-cbind(alladjustedys, subsetpredicators3)


#plot them#
Perimplot<-ggplot(alladjustedys, aes(x=Perim_avg, y=Perimadjustedy)) + geom_point(size=1)+xlab(bquote(atop("Pond perimeter" (m^{2}))))+ylab(bquote(atop("Adjusted Biofilm productivity" , ( ~ µg  ~C %.% ~ 360 ~ cm^{-2} %.% ~day^{-1}))))+ggtitle("")+ylim(-0.5, 2.25)+ stat_smooth(method="lm", se=FALSE, color = "black")+ mytheme
Depthplot<-ggplot(alladjustedys, aes(x=meanDepth, y=Depthadjustedy)) + geom_point(size=1)+xlab(bquote(atop("Depth (cm)")))+ylab("")+ylim(-0.5, 2.25)+ggtitle("")+ mytheme+ stat_smooth(method="lm", se=FALSE, color = "black")+theme(axis.text.y=element_blank())
DOplot<-ggplot(alladjustedys, aes(x=meanDoPerc, y=Doadjustedy)) + geom_point(size=1)+xlab(bquote(atop("DO %")))+ylab("")+ylim(-0.5, 2.25)+ggtitle("")+ mytheme+ stat_smooth(method="lm", se=FALSE, color = "black")+theme(axis.text.y=element_blank())

Alladjustedygraphs<-grid.arrange(Perimplot, Depthplot,DOplot, nrow = 1)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Adjustygraph3_4_19.png", width = 7, height = 3, units = 'in', res = 800)
Alladjustedygraphs<-grid.arrange(Perimplot, Depthplot,DOplot, nrow = 1, widths = c(1.2,1,1))
dev.off()

##calculate an r squared manually##
total_variance=var(yandresids$BiofilmProdactual)
residual_variance = var(yandresids$resids)

r_squared=1-(residual_variance / total_variance)
