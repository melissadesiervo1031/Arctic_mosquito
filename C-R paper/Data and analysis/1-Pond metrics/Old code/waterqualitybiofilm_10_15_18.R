
##read in file##
waterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/multivariateponddata2_10_15_18.csv",header = TRUE)  

##subset data from ind. variables#
waterind<-waterdata[, c(1, 2)]
waterdep<-waterdata[, -c(1, 2)]
waterdepnodensity<-waterdep[, -c(14, 15)]
waterdepnodensitynoperimarea<-waterdepnodensity[, -c(12, 13)]


##see how data are correllated##
pairs(waterdep)
cor(waterdep, use="complete.obs")
## a lot of variables are correllated##


##subset by instar stage##
firstinstars<-subset(waterdata, Instar =="1stinstar")
secondthirdinstars<-subset(waterdata, Instar =="2nd3rdinstar")
pupaeinstars<-subset(waterdata, Instar =="Mostlypupae")

#packages#
install.packages("vegan")
library(vegan)


##all ponds and time points together##
NMDS2 <- metaMDS(waterdep,dist = "bray",autotransform=T, k=3, trymax=250, maxit=500, trace=TRUE) ##number of axes predetermined by stress plots##
ordiplot(NMDS2, display = "sites", type="none")
points(NMDS2, "sites", pch=19, col = "red", select = waterind$Pond == "East")
points(NMDS2, "sites", pch=19, col = "orange", select = waterind$Pond == "NoOil")
points(NMDS2, "sites", pch=19, col = "yellow", select = waterind$Pond == "Oil")
points(NMDS2, "sites", pch=19, col = "green", select = waterind$Pond == "Golf")
points(NMDS2, "sites", pch=19, col = "blue violet", select = waterind$Pond == "Waterfall")
points(NMDS2, "sites", pch=19, col = "purple", select = waterind$Pond == "Vulgaris")
points(NMDS2, "sites", pch=19, col = "black", select = waterind$Pond == "Vulgarissmall")
points(NMDS2, "sites", pch=19, col = "gray", select = waterind$Pond == "Ice")

fit2<-envfit(NMDS2~ Depth+FPOM+DOC+NH4+N03N02+TotalP+Conduc+DO+pH+ORP+Thermalsum1+Perim+Coursearea+Mozdensity+Colymdensity,na.rm=TRUE, waterdep)
plot(fit2,col = "black", cex=1.1, axis=TRUE)
fit2

##fill in missing values with the mean from that column##
for(i in 1:ncol(waterdepnodensitynoperimarea)){
  waterdepnodensitynoperimarea[is.na(waterdepnodensitynoperimarea[,i]), i] <- mean(waterdepnodensitynoperimarea[,i], na.rm = TRUE)
}

##try PCA##
pca <- prcomp(waterdepnodensitynoperimarea, center = TRUE,scale. = TRUE)
summary(pca)
str(pca)

#
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

#plot the pca##
ggbiplot(pca)
ggbiplot(pca, labels=waterind$Pond)


##seperate PCA for 1st instar ##
firstinstars<-subset(waterdata, Instar =="1st")
pupaeinstars<-subset(waterdata, Instar =="Pupae")

firstinstarsind<-firstinstars[, c(1, 2)]
firstinstarsdep<-firstinstars[, -c(1, 2)]
firstinstarsdepnodensity<-firstinstarsdep[, -c(14, 15)]
firstinstarsdepnodensitynoperimarea<-firstinstarsdepnodensity[, -c(12, 13)]

##fill in missing values with the mean from that column##
for(i in 1:ncol(firstinstarsdepnodensitynoperimarea)){
  firstinstarsdepnodensitynoperimarea[is.na(firstinstarsdepnodensitynoperimarea[,i]), i] <- mean(firstinstarsdepnodensitynoperimarea[,i], na.rm = TRUE)
}

##pca first instars##
pcafirst <- prcomp(firstinstarsdepnodensitynoperimarea, center = TRUE,scale. = TRUE)
summary(pcafirst)
str(pcafirst)

#plot the pca##
ggbiplot(pcafirst)
ggbiplot(pcafirst, labels=firstinstarsind$Pond)


##seperate PCA for pupae ##
pupaeinstars<-subset(waterdata, Instar =="Pupae")

pupaeinstarsind<-pupaeinstars[, c(1, 2)]
pupaeinstarsdep<-pupaeinstars[, -c(1, 2)]
pupaeinstarsdepnodensity<-pupaeinstarsdep[, -c(14, 15)]
pupaeinstarsdepnodensitynoperimarea<-pupaeinstarsdepnodensity[, -c(12, 13)]

##fill in missing values with the mean from that column##
for(i in 1:ncol(pupaeinstarsdepnodensitynoperimarea)){
  pupaeinstarsdepnodensitynoperimarea[is.na(pupaeinstarsdepnodensitynoperimarea[,i]), i] <- mean(firstinstarsdepnodensitynoperimarea[,i], na.rm = TRUE)
}

##pca first instars##
pcapupae <- prcomp(pupaeinstarsdepnodensitynoperimarea, center = TRUE,scale. = TRUE)
summary(pcapupae)
str(pcapupae)

#plot the pca##
ggbiplot(pcapupae)
ggbiplot(pcapupae, labels=firstinstarsind$Pond)


##incorporate biofilm data to see what predicts biofilm biomass##

biofilmwaterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/biofilmmultivariateponddata_10_15_18.csv",header = TRUE)  

##look at distributions##
hist(biofilmwaterdata$Depth ) #ok#
hist(biofilmwaterdata$FPOM) #ok#
hist(biofilmwaterdata$DOC) #ok#
hist(biofilmwaterdata$NH4) #ok#
hist(biofilmwaterdata$N03N02) #ok#
hist(biofilmwaterdata$TotalP) #left skewed...transform#
hist(log(biofilmwaterdata$TotalP)) #better##
hist(biofilmwaterdata$Conduc) #left skewed...transform#
hist(log(biofilmwaterdata$Conduc)) #better##
hist(biofilmwaterdata$DO) #ok#
hist(biofilmwaterdata$pH) #ok#
hist(biofilmwaterdata$ORP) #ok#
hist(biofilmwaterdata$Thermalsum1) #a little skewed but probably okay#
hist(biofilmwaterdata$Perim) #ok#
hist(biofilmwaterdata$Coursearea) #a little skewed but probably okay#
hist(biofilmwaterdata$Mozdensity) #ok#
hist(biofilmwaterdata$Colymdensity) ##very skewed..transform##
hist(log(biofilmwaterdata$Colymdensity)) ##better but maybe don't use##
hist(biofilmwaterdata$Biofilmnocage) #ok but maybe transform to keep consistent with other#
hist(log(biofilmwaterdata$Biofilmnocage)) #better#
hist(biofilmwaterdata$Biofilmcage) #skewed#
hist(log(biofilmwaterdata$Biofilmcage)) #better#
#