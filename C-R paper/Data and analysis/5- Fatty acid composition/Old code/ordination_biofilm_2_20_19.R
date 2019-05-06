

###upload files##
fattyacidraw<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/ordinationfattyacids_2_20_19.csv",header = TRUE, row.names = 1)
ponddata<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/ordinationponddata_2_20_19.csv",header = TRUE, row.names = 1)



#packages##.
library(vegan)

####NMDS for fatty acid data###
NMDS <- metaMDS(fattyacidraw,dist = "bray",autotransform=T, k=3, trymax=250, maxit=500, trace=TRUE) ##number of axes predetermined by stress plots##
NMDS ##stress values##  Stress values >0.2 are generally poor and potentially uninterpretable, whereas values <0.1 are good and <0.05 are excellent, leaving little danger of misinterpretation.  Stress values between 0.1 and 0.2 are useable but some of the distances will be misleading.
NMDS$points ##X,Y values if you want to make figure elsewhere##
par(mfrow=c(1,1))

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/ordinationbiofilm.png", width = 5 ,height = 5, units = 'in', res = 800)
ordiplot(NMDS, display = "sites", type="none" ) 
points(NMDS, "sites", pch = 3, col = "black", select = ponddata$Pond == "East")
points(NMDS, "sites", pch = 4, col = "black", select = ponddata$Pond == "NoOil")
points(NMDS, "sites", pch = 19, col = "black", select = ponddata$Pond == "Oil")
points(NMDS, "sites", pch = 24, col = "black", select = ponddata$Pond == "Golf")
points(NMDS, "sites", pch = 13, col = "black", select = ponddata$Pond == "Waterfall")
points(NMDS, "sites", pch = 1, col = "black", select = ponddata$Pond == "Vulgaris")
points(NMDS, "sites", pch = 18, col = "black", select = ponddata$Pond == "Vulgaris small")
points(NMDS, "sites", pch = 8, col = "black", select = ponddata$Pond == "Ice")
legend("topleft", inset=.06, bty="n", c("East", "NoOil", "Oil", "Golf", "Waterfall", "Vulgaris", "Vsmall", "Ice"),col = c("black"), text.col = "black", pch = c(3,4,19,24,13,1,18,8), horiz = FALSE, cex=0.7, ncol=2)
dev.off()

ordiplot(NMDS, display = "sites", type="text" ) 
