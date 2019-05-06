##upload with biofilm and predictors##
biofilmpondata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstationbiofilm_12_21_18.csv",header = TRUE)  


library(lme4)


#null#
Nullmod<-lmer(BiofilmProd~ (1|Pond), data=biofilmpondata)

##univariate models##
Perim<-lmer(BiofilmProd~Perim_1 + (1|Pond), data=biofilmpondata)
Area<-lmer(BiofilmProd~Area_1+(1|Pond), data=biofilmpondata)
Area_loss<-lmer(BiofilmProd~Area_loss+(1|Pond), data=biofilmpondata)
Depth<-lmer(BiofilmProd~meanDepth+(1|Pond), data=biofilmpondata)
DOC<-lmer(BiofilmProd~meanDOC+(1|Pond), data=biofilmpondata)
NH4<-lmer(BiofilmProd~meanNH4+(1|Pond), data=biofilmpondata)
N03N02<-lmer(BiofilmProd~meanN03N02+(1|Pond), data=biofilmpondata)
totalP<-lmer(BiofilmProd~meanlogTotalP+(1|Pond), data=biofilmpondata)
Conduct<-lmer(BiofilmProd~meanlogConduc+(1|Pond), data=biofilmpondata)
DOperc<-lmer(BiofilmProd~meanDoPerc+(1|Pond), data=biofilmpondata)
pH<-lmer(BiofilmProd~meanpH+(1|Pond), data=biofilmpondata)
temp<-lmer(BiofilmProd~meanTemp+(1|Pond), data=biofilmpondata)
FPOM<-lmer(BiofilmProd~meanFPOM_adjust+(1|Pond), data=biofilmpondata)

#Productivity model##
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), data=biofilmpondata)


##run the AIC without scaling the parameters##




##scale all the predictors##
biofilmpredictors<-subset(biofilmpondata, select = c("Perim_1", "Area_1", "Area_loss", "meanDepth", "meanDOC", "meanNH4", "meanN03N02", "meanlogTotalP", "meanlogConduc", "meanTemp", "meanDoPerc", "meanpH", "meanFPOM_adjust"))
scaledpredictors<-scale(biofilmpredictors)
biofilmdepnames<-subset(biofilmpondata, select=c("Pond", "BiofilmProd"))
scaledbiofilmponddata<-cbind(biofilmdepnames, scaledpredictors)
scaledbiofilmponddata<-scaledbiofilmponddata %>% drop_na(BiofilmProd)

#null#
Nullmod<-lmer(BiofilmProd~ (1|Pond), data=scaledbiofilmponddata)

##univariate models##
Perim<-lmer(BiofilmProd~Perim_1 + (1|Pond), data=scaledbiofilmponddata)
Area<-lmer(BiofilmProd~Area_1+(1|Pond), data=scaledbiofilmponddata)
Area_loss<-lmer(BiofilmProd~Area_loss+(1|Pond), data=scaledbiofilmponddata)
Depth<-lmer(BiofilmProd~meanDepth+(1|Pond), data=scaledbiofilmponddata)
DOC<-lmer(BiofilmProd~meanDOC+(1|Pond), data=scaledbiofilmponddata)
NH4<-lmer(BiofilmProd~meanNH4+(1|Pond), data=scaledbiofilmponddata)
N03N02<-lmer(BiofilmProd~meanN03N02+(1|Pond), data=scaledbiofilmponddata)
totalP<-lmer(BiofilmProd~meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
Conduct<-lmer(BiofilmProd~meanlogConduc+(1|Pond), data=scaledbiofilmponddata)
DOperc<-lmer(BiofilmProd~meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
pH<-lmer(BiofilmProd~meanpH+(1|Pond), data=scaledbiofilmponddata)
temp<-lmer(BiofilmProd~meanTemp+(1|Pond), data=scaledbiofilmponddata)
FPOM<-lmer(BiofilmProd~meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

#Productivity model##
full<-lmer(BiofilmProd~Perim_1+Area_1+Area_loss+meanDepth+meanDOC+meanNH4+meanN03N02+meanlogTotalP+meanlogConduc+meanDoPerc+meanpH+meanTemp+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

fm1 <- lmer(BiofilmProd ~ ., +(1|Pond), data=scaledbiofilmponddata)
dd <- dredge(full)
subset(dd, delta < 4)


ms1 <- dredge(full, fixed = "Pond", subset = (Perim_1 | !Area_1) && (Area_1 | !Perim_1), beta = "partial")


##compare univariate models##
rawaic<-AIC(Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP,DOperc, Conduct,pH,temp,FPOM,full)
nR<-dim(scaledbiofilmponddata)[1]  #Sample size 
aictable(rawaic,nR)


##two way models with just the ones better than the null##
PerimArea_loss<-lmer(BiofilmProd~Perim_1+Area_loss+(1|Pond), data=scaledbiofilmponddata)
PerimDepth<-lmer(BiofilmProd~Perim_1+meanDepth+(1|Pond), data=scaledbiofilmponddata)
PerimDOC<-lmer(BiofilmProd~Perim_1+meanDOC+(1|Pond), data=scaledbiofilmponddata)
PerimNH4<-lmer(BiofilmProd~Perim_1+meanNH4+(1|Pond), data=scaledbiofilmponddata)
PerimN03N02<-lmer(BiofilmProd~Perim_1+meanN03N02+(1|Pond), data=scaledbiofilmponddata)
PerimtotalP<-lmer(BiofilmProd~Perim_1+meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
PerimConduct<-lmer(BiofilmProd~Perim_1+meanlogConduc+(1|Pond), data=scaledbiofilmponddata)
PerimDOperc<-lmer(BiofilmProd~Perim_1+meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
PerimpH<-lmer(BiofilmProd~Perim_1+meanpH+(1|Pond), data=scaledbiofilmponddata)
Perimtemp<-lmer(BiofilmProd~Perim_1+meanTemp+(1|Pond), data=scaledbiofilmponddata)
PerimFPOM<-lmer(BiofilmProd~Perim_1+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

AreaDepth<-lmer(BiofilmProd~Area_1+meanDepth+(1|Pond), data=scaledbiofilmponddata)
AreaDOC<-lmer(BiofilmProd~Area_1+meanDOC+(1|Pond), data=scaledbiofilmponddata)
AreaNH4<-lmer(BiofilmProd~Area_1+meanNH4+(1|Pond), data=scaledbiofilmponddata)
AreaN03N02<-lmer(BiofilmProd~Area_1+meanN03N02+(1|Pond), data=scaledbiofilmponddata)
AreatotalP<-lmer(BiofilmProd~Area_1+meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
AreaConduct<-lmer(BiofilmProd~Area_1+meanlogConduc+(1|Pond), data=scaledbiofilmponddata)
AreaDOperc<-lmer(BiofilmProd~Area_1+meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
AreapH<-lmer(BiofilmProd~Area_1+meanpH+(1|Pond), data=scaledbiofilmponddata)
Areatemp<-lmer(BiofilmProd~Area_1+meanTemp+(1|Pond), data=scaledbiofilmponddata)
AreaFPOM<-lmer(BiofilmProd~Area_1+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

ConducDepth<-lmer(BiofilmProd~meanlogConduc+meanDepth+(1|Pond), data=scaledbiofilmponddata)
ConducDOC<-lmer(BiofilmProd~meanlogConduc+meanDOC+(1|Pond), data=scaledbiofilmponddata)
ConducNH4<-lmer(BiofilmProd~meanlogConduc+meanNH4+(1|Pond), data=scaledbiofilmponddata)
ConducN03N02<-lmer(BiofilmProd~meanlogConduc+meanN03N02+(1|Pond), data=scaledbiofilmponddata)
ConductotalP<-lmer(BiofilmProd~meanlogConduc+meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
ConducDOperc<-lmer(BiofilmProd~meanlogConduc+meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
ConducpH<-lmer(BiofilmProd~meanlogConduc+meanpH+(1|Pond), data=scaledbiofilmponddata)
Conductemp<-lmer(BiofilmProd~meanlogConduc+meanTemp+(1|Pond), data=scaledbiofilmponddata)
ConducFPOM<-lmer(BiofilmProd~meanlogConduc+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

DepthDOC<-lmer(BiofilmProd~meanDepth+meanDOC+(1|Pond), data=scaledbiofilmponddata)
DepthNH4<-lmer(BiofilmProd~meanDepth+meanNH4+(1|Pond), data=scaledbiofilmponddata)
DepthN03N02<-lmer(BiofilmProd~meanDepth+meanN03N02+(1|Pond), data=scaledbiofilmponddata)
DepthtotalP<-lmer(BiofilmProd~meanDepth+meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
DepthDOperc<-lmer(BiofilmProd~meanDepth+meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
DepthpH<-lmer(BiofilmProd~meanDepth+meanpH+(1|Pond), data=scaledbiofilmponddata)
Depthtemp<-lmer(BiofilmProd~meanDepth+meanTemp+(1|Pond), data=scaledbiofilmponddata)
DepthFPOM<-lmer(BiofilmProd~meanDepth+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)


##three models with just the ones better than the null##
PerimArea_lossDepth<-lmer(BiofilmProd~Perim_1+Area_loss+Depth(1|Pond), data=scaledbiofilmponddata)
PerimDOC<-lmer(BiofilmProd~Perim_1+meanDOC+(1|Pond), data=scaledbiofilmponddata)
PerimNH4<-lmer(BiofilmProd~Perim_1+meanNH4+(1|Pond), data=scaledbiofilmponddata)
PerimN03N02<-lmer(BiofilmProd~Perim_1+meanN03N02+(1|Pond), data=scaledbiofilmponddata)
PerimtotalP<-lmer(BiofilmProd~Perim_1+meanlogTotalP+(1|Pond), data=scaledbiofilmponddata)
PerimConduct<-lmer(BiofilmProd~Perim_1+meanlogConduc+(1|Pond), data=scaledbiofilmponddata)
PerimDOperc<-lmer(BiofilmProd~Perim_1+meanDoPerc+(1|Pond), data=scaledbiofilmponddata)
PerimpH<-lmer(BiofilmProd~Perim_1+meanpH+(1|Pond), data=scaledbiofilmponddata)
Perimtemp<-lmer(BiofilmProd~Perim_1+meanTemp+(1|Pond), data=scaledbiofilmponddata)
PerimFPOM<-lmer(BiofilmProd~Perim_1+meanFPOM_adjust+(1|Pond), data=scaledbiofilmponddata)

##more than 3##

##compare all models##
rawaic<-AIC(PerimArea_loss,PerimDepth,PerimDOC,PerimNH4,PerimN03N02,PerimtotalP,PerimConduct,PerimDOperc,PerimpH,Perimtemp,PerimFPOM,
            AreaDepth,AreaDOC,AreaNH4,AreaN03N02,AreatotalP,AreaConduct,AreaDOperc,AreapH,Areatemp,AreaFPOM,
            ConducDepth,ConducDOC,ConducNH4,ConducN03N02,ConductotalP,ConducDOperc,ConducpH,Conductemp,ConducFPOM,
            DepthDOC,DepthNH4,DepthN03N02,DepthtotalP,DepthDOperc,DepthpH,Depthtemp,DepthFPOM, 
            Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP,DOperc, Conduct,pH,temp,FPOM,full)
nR<-dim(scaledbiofilmponddata)[1]  #Sample size 
aictable(rawaic,nR)

###with the 3 ways###


##more than 3##


#summary of best modell#
summary(NH4N03N02Conduc)

##r2 for best model##
r.squaredGLMM(NH4N03N02Conduc)

##check VIF of the best model##
vif.mer(NH4N03N02Conduc)

##playing around with the mumin package##

##First need to standardize predictors##


require(MuMIn)
options(na.action="na.fail")

out.put<-model.sel(Nullmod,NH4,N03N02, Conduct,
                  NH4Perim, NH4N03N02,NH4P,NH4conduc,NH4ph, 
                  N03N02Perim,N03N02P,N03N02conduc, N03N02ph, 
                  Conducph,
                  NH4N03N02Conduc,NH4N03N02pH, NH4N03N02Perimeter, NH4N03N02P,
                  N03N02ConducpH, N03N02ConducPerimeter, N03N02ConducP,
                  NH4ConducpH, NH4ConductPerimeter, NH4ConductP,
                  full)

bestmodels<-subset(out.put, delta <2) 
importance(bestmodels)

# Model average using all candidate models, always use revised.var = TRUE 
MA.ests<-model.avg(bestmodels, revised.var = TRUE) 
MA.ests
confint(MA.ests)
summary(MA.ests)
