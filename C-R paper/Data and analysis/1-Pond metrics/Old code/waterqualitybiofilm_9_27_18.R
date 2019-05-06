
##read in file##
waterdata<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/YSIwaterquality_9_26_18.csv",header = TRUE)  

##subset by instar stage##
firstinstars<-subset(waterdata, Larvalstage =="1stinstar")
secondthirdinstars<-subset(waterdata, Larvalstage =="2nd3rdinstar")
pupaeinstars<-subset(waterdata, Larvalstage =="Mostlypupae")

##pull out data for markers##
waterdataind<-waterdata[, c(1, 2,3,18,19)]
firstinstarind<-firstinstars[, c(1, 2,3,18,19)]
secondthirdinstarsind<-secondthirdinstars[, c(1, 2,3,18,19)]
pupaeinstarsind<-pupaeinstars[, c(1, 2,3,18,19)]

##pull out data for the multivariate analysis##
waterdatadep<-waterdata[, -c(1, 2,3,4,6,9,18,19)]
firstinstardep<-firstinstars[, -c(1, 2,3,4,6,9,18,19)]
secondthirdinstarsdep<-secondthirdinstars[, -c(1, 2,3,4,6,9,18,19)]
pupaeinstarsdep<-pupaeinstars[, -c(1, 2,3,4,6,9,18,19)]

#packages#
install.packages("vegan")
library(vegan)


##just the first instar data##
NMDS1 <- metaMDS(firstinstardep,dist = "bray",autotransform=T, k=3, trymax=250, maxit=500, trace=TRUE) ##number of axes predetermined by stress plots##
ordiplot(NMDS1, display = "sites", type="none")
points(NMDS1, "sites", pch=19, col = "red", select = firstinstarind$Pond == "East")
points(NMDS1, "sites", pch=19, col = "orange", select = firstinstarind$Pond == "NoOil")
points(NMDS1, "sites", pch=19, col = "yellow", select = firstinstarind$Pond == "Oil")
points(NMDS1, "sites", pch=19, col = "green", select = firstinstarind$Pond == "Golf")
points(NMDS1, "sites", pch=19, col = "blue violet", select = firstinstarind$Pond == "Waterfall")
points(NMDS1, "sites", pch=19, col = "purple", select = firstinstarind$Pond == "Vulgaris")
points(NMDS1, "sites", pch=19, col = "black", select = firstinstarind$Pond == "Vulgarissmall")
points(NMDS1, "sites", pch=19, col = "gray", select = firstinstarind$Pond == "Ice")
fit1<-envfit(NMDS1~Thermalsum0+Conductivity+DO+pH+ORP+Depth+DOC+NH4+N03N02+TotalP+FPOM,na.rm=TRUE, firstinstardep)
plot(fit1,col = "black", cex=1.1, axis=TRUE)
fit1


##all ponds and time points together##
NMDS2 <- metaMDS(waterdatadep,dist = "bray",autotransform=T, k=3, trymax=250, maxit=500, trace=TRUE) ##number of axes predetermined by stress plots##
ordiplot(NMDS2, display = "sites", type="none")
points(NMDS2, "sites", pch=19, col = "red", select = waterdataind$Pond == "East")
points(NMDS2, "sites", pch=19, col = "orange", select = waterdataind$Pond == "NoOil")
points(NMDS2, "sites", pch=19, col = "yellow", select = waterdataind$Pond == "Oil")
points(NMDS2, "sites", pch=19, col = "green", select = waterdataind$Pond == "Golf")
points(NMDS2, "sites", pch=19, col = "blue violet", select = waterdataind$Pond == "Waterfall")
points(NMDS2, "sites", pch=19, col = "purple", select = waterdataind$Pond == "Vulgaris")
points(NMDS2, "sites", pch=19, col = "black", select = waterdataind$Pond == "Vulgarissmall")
points(NMDS2, "sites", pch=19, col = "gray", select = waterdataind$Pond == "Ice")
fit2<-envfit(NMDS2~Thermalsum0+Conductivity+DO+pH+ORP+Depth+DOC+NH4+N03N02+TotalP+FPOM,na.rm=TRUE, waterdatadep)
plot(fit2,col = "black", cex=1.1, axis=TRUE)
fit2




