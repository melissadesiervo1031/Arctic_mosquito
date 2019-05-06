##upload with biofilm and predictors##
biofilmpondata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/summarybysamplingstationbiofilm_12_21_18.csv",header = TRUE)  

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


##compare univariate models##
rawaic<-AIC(Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP,DOperc, Conduct,pH,temp,FPOM,full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)


##two way models with just the ones better than the null##
NH4Perim<-lmer(BiofilmProd~Perim_1+meanNH4+(1|Pond), data=biofilmpondata)
NH4Area<-lmer(BiofilmProd~Area_1+meanNH4+(1|Pond), data=biofilmpondata)
NH4Area_loss<-lmer(BiofilmProd~Area_loss+meanNH4+(1|Pond), data=biofilmpondata)
NH4depth<-lmer(BiofilmProd~meanDepth+meanNH4+(1|Pond), data=biofilmpondata)
NH4DOC<-lmer(BiofilmProd~meanDOC+meanNH4+(1|Pond), data=biofilmpondata)
NH4N03N02<-lmer(BiofilmProd~meanNH4+meanN03N02 + (1|Pond), data=biofilmpondata)
NH4P<-lmer(BiofilmProd~meanNH4+meanlogTotalP+(1|Pond), data=biofilmpondata)
NH4conduc<-lmer(BiofilmProd~meanNH4+meanlogConduc + (1|Pond), data=biofilmpondata)
NH4D0perc<-lmer(BiofilmProd~meanNH4+meanDoPerc+(1|Pond), data=biofilmpondata)
NH4ph<-lmer(BiofilmProd~meanNH4+meanpH+(1|Pond), data=biofilmpondata)
NH4temp<-lmer(BiofilmProd~meanNH4+meanTemp+(1|Pond), data=biofilmpondata)
NH4FPOM<-lmer(BiofilmProd~meanNH4+meanFPOM_adjust+(1|Pond), data=biofilmpondata)

N03N02Perim<-lmer(BiofilmProd~Perim_1+meanN03N02+(1|Pond), data=biofilmpondata)
N03N02Area<-lmer(BiofilmProd~Area_1+meanN03N02+(1|Pond), data=biofilmpondata)
N03N02Area_loss<-lmer(BiofilmProd~Area_loss+meanN03N02+(1|Pond), data=biofilmpondata)
N03N02depth<-lmer(BiofilmProd~meanDepth+meanN03N02+(1|Pond), data=biofilmpondata)
N03N02DOC<-lmer(BiofilmProd~meanDOC+meanN03N02+(1|Pond), data=biofilmpondata)
N03N02P<-lmer(BiofilmProd~meanN03N02+meanlogTotalP+(1|Pond), data=biofilmpondata)
N03N02conduc<-lmer(BiofilmProd~meanN03N02+meanlogConduc + (1|Pond), data=biofilmpondata)
N03N02D0perc<-lmer(BiofilmProd~meanN03N02+meanDoPerc+(1|Pond), data=biofilmpondata)
N03N02ph<-lmer(BiofilmProd~meanN03N02+meanpH+(1|Pond), data=biofilmpondata)
N03N02temp<-lmer(BiofilmProd~meanN03N02+meanTemp+(1|Pond), data=biofilmpondata)
N03N02FPOM<-lmer(BiofilmProd~meanNH4+meanFPOM_adjust+(1|Pond), data=biofilmpondata)

ConducPerim<-lmer(BiofilmProd~Perim_1+meanlogConduc+(1|Pond), data=biofilmpondata)
ConducArea<-lmer(BiofilmProd~Area_1+meanlogConduc+(1|Pond), data=biofilmpondata)
ConducArea_loss<-lmer(BiofilmProd~Area_loss+meanlogConduc+(1|Pond), data=biofilmpondata)
Conducdepth<-lmer(BiofilmProd~meanDepth+meanlogConduc+(1|Pond), data=biofilmpondata)
ConducDOC<-lmer(BiofilmProd~meanDOC+meanlogConduc+(1|Pond), data=biofilmpondata)
ConducP<-lmer(BiofilmProd~meanlogConduc+meanlogTotalP+(1|Pond), data=biofilmpondata)
ConducD0perc<-lmer(BiofilmProd~meanlogConduc+meanDoPerc+(1|Pond), data=biofilmpondata)
Conducph<-lmer(BiofilmProd~meanlogConduc+meanpH+(1|Pond), data=biofilmpondata)
Conductemp<-lmer(BiofilmProd~meanlogConduc+meanTemp+(1|Pond), data=biofilmpondata)
ConducFPOM<-lmer(BiofilmProd~meanNH4+meanFPOM_adjust+(1|Pond), data=biofilmpondata)

##three models with just the ones better than the null##
NH4N03N02Conduc<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanlogConduc+ (1|Pond), data=biofilmpondata)
NH4N03N02pH<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanpH+ (1|Pond), data=biofilmpondata)
NH4N03N02Perimeter<-lmer(BiofilmProd~meanNH4+meanN03N02 +Perim_1+ (1|Pond), data=biofilmpondata)
NH4N03N02P<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanlogTotalP+ (1|Pond), data=biofilmpondata)

NH4ConducpH<-lmer(BiofilmProd~meanNH4+meanlogConduc +meanpH+ (1|Pond), data=biofilmpondata)
NH4ConductP<-lmer(BiofilmProd~meanNH4+meanlogConduc +meanlogTotalP+ (1|Pond), data=biofilmpondata)
NH4ConductPerimeter<-lmer(BiofilmProd~meanNH4+meanlogConduc +Perim_1+ (1|Pond), data=biofilmpondata)

N03N02ConducpH<-lmer(BiofilmProd~meanN03N02 +meanlogConduc +meanpH +(1|Pond), data=biofilmpondata)
N03N02ConducP<-lmer(BiofilmProd~meanN03N02 +meanlogConduc  +meanlogTotalP +(1|Pond), data=biofilmpondata)
N03N02ConducPerimeter<-lmer(BiofilmProd~meanN03N02 +meanlogConduc  +Perim_1 +(1|Pond), data=biofilmpondata)

##more than 3##
NH4N03N02ConduphPperim<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanlogConduc+meanpH +meanlogTotalP+ Perim_1+ (1|Pond), data=biofilmpondata)
NH4N03N02ConduphP<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanlogConduc+meanpH +meanlogTotalP+ (1|Pond), data=biofilmpondata)
NH4N03N02Conduph<-lmer(BiofilmProd~meanNH4+meanN03N02 +meanlogConduc+meanpH + (1|Pond), data=biofilmpondata)



##compare all models##
rawaic<-AIC(Nullmod,Perim,Area,Area_loss,Depth,DOC,NH4,N03N02,totalP, Conduct,pH,temp,FPOM,DOperc,
            NH4Perim,NH4Area,NH4Area_loss,NH4depth,NH4DOC, NH4N03N02,NH4P,NH4conduc, NH4D0perc,NH4ph,NH4temp,NH4FPOM, 
            N03N02Perim,N03N02Area,N03N02Area_loss,N03N02depth,N03N02DOC, N03N02P,N03N02conduc, N03N02D0perc,N03N02ph,N03N02temp,N03N02FPOM, 
            ConducPerim,ConducArea,ConducArea_loss,Conducdepth,ConducDOC, ConducP,ConducD0perc,Conducph,Conductemp,ConducFPOM, 
            NH4N03N02Conduc,
            full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)

rawaic<-AIC(Nullmod,NH4,N03N02, Conduct,
            NH4Perim,NH4Area,NH4Area_loss,NH4depth,NH4DOC, NH4N03N02,NH4P,NH4conduc, NH4D0perc,NH4ph,NH4temp,NH4FPOM, 
            N03N02Perim,N03N02Area,N03N02Area_loss,N03N02depth,N03N02DOC, N03N02P,N03N02conduc, N03N02D0perc,N03N02ph,N03N02temp,N03N02FPOM, 
            ConducPerim,ConducArea,ConducArea_loss,Conducdepth,ConducDOC, ConducP,ConducD0perc,Conducph,Conductemp,ConducFPOM, 
            full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)

###with the 3 ways###
rawaic<-AIC(Nullmod,NH4,N03N02, Conduct,
            NH4Perim, NH4N03N02,NH4P,NH4conduc,NH4ph, 
            N03N02Perim,N03N02P,N03N02conduc, N03N02ph, 
            Conducph,
            NH4N03N02Conduc,NH4N03N02pH, NH4N03N02Perimeter, NH4N03N02P,
            N03N02ConducpH, N03N02ConducPerimeter, N03N02ConducP,
            NH4ConducpH, NH4ConductPerimeter, NH4ConductP,
            full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)

##more than 3##
rawaic<-AIC(Nullmod,NH4,N03N02, Conduct,
            NH4Perim, NH4N03N02,NH4P,NH4conduc,NH4ph, 
            N03N02Perim,N03N02P,N03N02conduc, N03N02ph, 
            Conducph,
            NH4N03N02Conduc,NH4N03N02pH, NH4N03N02Perimeter, NH4N03N02P,
            N03N02ConducpH, N03N02ConducPerimeter, N03N02ConducP,
            NH4ConducpH, NH4ConductPerimeter, NH4ConductP,
            full)
nR<-dim(biofilmpondata)[1]  #Sample size 
aictable(rawaic,nR)


#summary of best modell#
summary(NH4N03N02Conduc)

##r2 for best model##
r.squaredGLMM(NH4N03N02Conduc)

##check VIF of the best model##
vif.mer(NH4N03N02Conduc)


