##upload with biofilm and predictors##
biofilmpondata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstationbiofilm_12_21_18.csv",header = TRUE)  


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
temp<-lmer(BiofilmProd~meanTemp+(1|Pond), REML=F, data=biofilmpondata)
FPOM<-lmer(BiofilmProd~meanFPOM_adjust+(1|Pond), REML=F, data=biofilmpondata)

#Productivity model##
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, data=biofilmpondata)

##run the AIC without scaling the parameters##
##compare univariate models##
rawaic<-AIC(Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP,DOperc, Conduct,pH,temp,FPOM,full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)


##scale all the predictors##
biofilmpredictors<-subset(biofilmpondata, select = c("Perim_1", "Area_1", "Area_loss_per", "meanDepth", "meanDOC", "meanNH4", "meanN03N02", "meanlogTotalP", "meanlogConduc", "meanTemp", "meanDoPerc", "meanpH", "meanFPOM_adjust"))
scaledpredictors<-scale(biofilmpredictors)
biofilmdepnames<-subset(biofilmpondata, select=c("Pond", "BiofilmProd"))
scaledbiofilmponddata<-cbind(biofilmdepnames, scaledpredictors)
scaledbiofilmponddata<-scaledbiofilmponddata[!is.na(scaledbiofilmponddata$BiofilmProd), ]

#null#
Nullmod<-lmer(BiofilmProd~ (1|Pond), REML=F, data=scaledbiofilmponddata)

##univariate models with scaled paramters##
sPerim<-lmer(BiofilmProd~Perim_1 + (1|Pond),  REML=F, data=scaledbiofilmponddata)
sArea<-lmer(BiofilmProd~Area_1+(1|Pond), REML=F, data=scaledbiofilmponddata)
sArea_loss<-lmer(BiofilmProd~Area_loss_per+(1|Pond), REML=F, data=scaledbiofilmponddata)
sDepth<-lmer(BiofilmProd~meanDepth+(1|Pond), REML=F, data=scaledbiofilmponddata)
sDOC<-lmer(BiofilmProd~meanDOC+(1|Pond), REML=F, data=scaledbiofilmponddata)
sNH4<-lmer(BiofilmProd~meanNH4+(1|Pond), REML=F, data=scaledbiofilmponddata)
sN03N02<-lmer(BiofilmProd~meanN03N02+(1|Pond), REML=F, data=scaledbiofilmponddata)
stotalP<-lmer(BiofilmProd~meanlogTotalP+(1|Pond), REML=F, data=scaledbiofilmponddata)
sConduct<-lmer(BiofilmProd~meanlogConduc+(1|Pond), REML=F, data=scaledbiofilmponddata)
sDOperc<-lmer(BiofilmProd~meanDoPerc+(1|Pond), REML=F, data=scaledbiofilmponddata)
spH<-lmer(BiofilmProd~meanpH+(1|Pond), REML=F, data=scaledbiofilmponddata)
stemp<-lmer(BiofilmProd~meanTemp+(1|Pond), REML=F, data=scaledbiofilmponddata)
sFPOM<-lmer(BiofilmProd~meanFPOM_adjust+(1|Pond), REML=F, data=scaledbiofilmponddata)

#Productivity model##
sfull<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, data=scaledbiofilmponddata)

##run the AIC  scaling the parameters##
##compare univariate models##
rawaic<-AIC(Nullmod,sPerim,sArea,sArea_loss,sDepth,sDOC,sNH4,sN03N02,stotalP,sDOperc, sConduct,spH,stemp,sFPOM,sfull)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)


###
sfull<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)


##tests all possible 1 parameter models ##
dredgedmodelsone <- dredge(sfull, m.lim=c(1,1))


##tests all possible models with either Area, Perim, Conudctivity or Depth as the predictors##
dredgedmodelstwoparam <- dredge(sfull, subset = Area_1|Perim_1|meanlogConduc|meanDepth, m.lim=c(1,2))
subset(dredgedmodelstwoparam, delta <2) 
summary(model.avg(dredgedmodels, subset = delta < 2))


##tests all possible models with either Area, Perim, Conudctivity or Depth as the predictors##
dredgedmodelsthreeparam <- dredge(sfull, subset = Area_1|Perim_1|meanlogConduc|meanDepth, m.lim=c(3,3))

##tests all possible models with either Area, Perim, Conudctivity or Depth as the predictors##
dredgedmodelsfourparam <- dredge(sfull, subset = Area_1|Perim_1|meanlogConduc|meanDepth, m.lim=c(4,4))

##all of these are worse than delta 2 AIC of the other ones##

#best models within delta 2 AIC of each other##
sPerim<-lmer(BiofilmProd~Perim_1 + (1|Pond),  REML=F, data=scaledbiofilmponddata)
sPerimdepth<-lmer(BiofilmProd~Perim_1+meanDepth+(1|Pond), REML=F, data=scaledbiofilmponddata)
sPerimArealoss<-lmer(BiofilmProd~Perim_1+Area_loss_per+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)
sPerimDO<-lmer(BiofilmProd~Perim_1+meanDoPerc+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)
sPerimarealossdepth<-lmer(BiofilmProd~Perim_1+Area_loss+meanDepth+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)
sfull<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)
snull<-lmer(BiofilmProd~(1|Pond), REML=F, na.action = "na.fail",data=scaledbiofilmponddata)

rawaic<-AIC(sPerim,sPerimdepth, sPerimArealoss, sPerimDO, sPerimarealossdepth, sfull, snull)
nR<-dim(scaledbiofilmponddata)[1]  #Sample size 
aictable(rawaic,nR)

##make sure I get the same answer ##
dredgedmodelsallupto4 <- dredge(sfull, fixed="Perim_1", m.lim=c(1,4))
bestmodelset<-subset(dredgedmodelsallupto4, delta <2) 

avgmodel<-model.avg(bestmodelset)
summary(avgmodel)


##do the same thing for unscaled data##
biofilmpondata<-biofilmpondata[!is.na(biofilmpondata$BiofilmProd), ]
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss_per+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), REML=F, na.action = "na.fail",data=biofilmpondata)

dredgedmodelsallupto4noscale <- dredge(full, fixed="Perim_1", m.lim=c(1,4))
bestmodelsetnoscale<-subset(dredgedmodelsallupto4noscale, delta <2) 
avgmodelnoscale<-model.avg(bestmodelsetnoscale)


##adjustedys##

predictyscaled<-predict(model.avg(bestmodelset, fit=TRUE))

predicty<-predict(model.avg(bestmodelsetnoscale, fit=TRUE))

yandresids<-as.data.frame(cbind(predictyscaled,scaledbiofilmponddata$BiofilmProd))
names(yandresids) <- c("predictedyscaled", "BiofilmProdactual")
yandresids<-mutate(yandresids, resids = predictedyscaled-BiofilmProdactual)

##calculate an r squared manually##


#get coefficients from model##
Perimadjustedy<-as.data.frame((0.007723*(biofilmpondata$Perim_1-mean(biofilmpondata$Perim_1))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))
Arealossadjustedy<-as.data.frame((-0.621128*(biofilmpondata$Area_loss_per-mean(biofilmpondata$Area_loss_per))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))
Depthadjustedy<-as.data.frame((0.047589*(biofilmpondata$meanDepth-mean(biofilmpondata$meanDepth))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))
Doadjustedy<-as.data.frame((-0.014428*(biofilmpondata$meanDoPerc-mean(biofilmpondata$meanDoPerc))-yandresids$resids)+mean(biofilmpondata$BiofilmProd))

library(tidyr)
alladjustedys<-cbind(Perimadjustedy, Arealossadjustedy)
alladjustedys<-cbind(alladjustedys, Depthadjustedy)
alladjustedys<-cbind(alladjustedys, Doadjustedy)
names(alladjustedys) <- c("Perimadjustedy", "Arealossadjustedy","Depthadjustedy","Doadjustedy")

plot(biofilmpondata$Perim_1, alladjustedys$Perimadjustedy)
plot(biofilmpondata$Area_loss_per, alladjustedys$Arealossadjustedy)
plot(biofilmpondata$meanDepth, alladjustedys$Depthadjustedy)
plot(biofilmpondata$meanDoPerc, alladjustedys$Doadjustedy)

xvariables<-biofilmpondata %>% select(Perim_1, Area_loss_per, meanDepth, meanDoPerc)

alladjustedys<-cbind(alladjustedys, xvariables)


##plot adjusted ys##
Perimplot<-ggplot(alladjustedys, aes(x=Perim_1, y=Perimadjustedy)) + geom_point(size=1)+xlab(bquote(atop("Pond perimeter" (m^{2}))))+ylab(bquote(atop("Adjusted Biofilm productivity" , ( ~ µg  ~C %*% ~ 360 ~ cm^{2} ^{-1} %*% ~day^{-1}))))+ggtitle("")+ylim(-0.5, 2.25)+ stat_smooth(method="lm", se=FALSE, color = "black")+ mytheme
Depthplot<-ggplot(alladjustedys, aes(x=meanDepth, y=Depthadjustedy)) + geom_point(size=1)+xlab(bquote(atop("Depth (cm)")))+ylab("")+ylim(-0.5, 2.25)+ggtitle("")+ mytheme+ stat_smooth(method="lm", se=FALSE, color = "black")
Arealossplot<-ggplot(alladjustedys, aes(x=(Area_loss_per*100), y=Arealossadjustedy)) + geom_point(size=1)+xlab(bquote(atop("% Area loss")))+ylim(-0.5, 2.25)+ylab("")+ggtitle("")+ mytheme+ stat_smooth(method="lm", se=FALSE, color = "black")
DOplot<-ggplot(alladjustedys, aes(x=meanDoPerc, y=Doadjustedy)) + geom_point(size=1)+xlab(bquote(atop("DO %")))+ylab("")+ylim(-0.5, 2.25)+ggtitle("")+ mytheme+ stat_smooth(method="lm", se=FALSE, color = "black")


Alladjustedygraphs<-multiplot(Perimplot, Depthplot, Arealossplot,DOplot, cols=4)

Alladjustedygraphs<-grid.arrange(Perimplot, Depthplot, Arealossplot,DOplot, nrow = 1)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Adjustygraph2.png", width = 10, height = 3, units = 'in', res = 800)
grid.arrange(Perimplot, Depthplot, Arealossplot,DOplot, nrow = 1)
dev.off()