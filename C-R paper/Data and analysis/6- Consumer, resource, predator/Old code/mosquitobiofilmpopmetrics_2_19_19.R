
popmetrics<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Populationmetrics_2_19_19.csv")

##add on a lnN column#

popmetrics_2<-popmetrics %>% mutate(lnN0abundance=log(N0), lnNT1abundance=log(NT1abundance), lnDensityliter=log(DensityLiter), lnNT1dens=log(NT1dens))


##linear regressions for all variables##
lnNfecunditym1 <- lm(Fecundity ~ lnN0abundance, data=popmetrics_2) 
lnNmortalitym2 <- lm(mortalityN0 ~ lnN0abundance, data=popmetrics_2) 
lnNRm3 <- lm(Rabundance ~ lnN0abundance, data=popmetrics_2) 

##samewith density##
denslnNfecunditym1 <- lm(Fecundity ~ lnDensityliter, data=popmetrics_2) 
denslnNmortalitym2 <- lm(mortalitydensity ~ lnDensityliter, data=popmetrics_2) 
denslnNRm3 <- lm(Rdens ~ lnDensityliter, data=popmetrics_2) 

fecunditym1 <- lm(Fecundity ~ Productivity, data=popmetrics_2) 
Productivitym2 <- lm(mortalitydensity ~ Productivity, data=popmetrics_2) 
ProductivityRm3 <- lm(Rdens ~ Productivity, data=popmetrics_2) 

##two way anova##
twowayanova <- lm(mortalitydensity ~ lnDensityliter*Productivity, data=popmetrics_2) 

##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=8, colour = "black"))+theme(axis.text.y=element_text(size=8, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

lnNfecundity<-ggplot(popmetrics_2, aes(lnN0, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-2), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlab("ln(N0)") + mytheme 
lnNmortality<-ggplot(popmetrics_2, aes(lnN0, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+xlab("ln(N0)") + mytheme 
lnNR<-ggplot(popmetrics_2, aes(lnN0, R)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.05), size=2)+ylab("R")+xlab("ln(N0)") + mytheme + geom_hline(yintercept=0, linetype="dashed") 
   
Productivityfecundity<-ggplot(popmetrics_2, aes(Productivity, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-2), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlab(expression("Biofilm Productivity" (~ µg  ~C / ~ 360 ~ cm^{2}/~ day))) + mytheme 
Productivitymortality<-ggplot(popmetrics_2, aes(Productivity, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+xlab(expression("Biofilm Productivity" (~ µg  ~C / ~ 360 ~ cm^{2}/~ day)))  + mytheme 
ProductivityR<-ggplot(popmetrics_2, aes(Productivity, R)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.05), size=2)+ylab("R")+xlab(expression("Biofilm Productivity" (~ µg  ~C / ~ 360 ~ cm^{2}/~ day)))  + mytheme + geom_hline(yintercept=0, linetype="dashed") 


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/lnNfecundity.png", width = 5 ,height = 5, units = 'in', res = 800)
lnNfecundity
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/lnNmortality.png", width = 5 ,height = 5, units = 'in', res = 800)
lnNmortality
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/lnNR.png", width = 5 ,height = 5, units = 'in', res = 800)
lnNR
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Productivityfecundity.png", width = 5 ,height = 5, units = 'in', res = 800)
Productivityfecundity
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Productivitymortality.png", width = 5 ,height = 5, units = 'in', res = 800)
Productivitymortality
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/ProductivityR.png", width = 5 ,height = 5, units = 'in', res = 800)
ProductivityR
dev.off()

##for multi panel figures abundance##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=8, colour = "black"))+theme(axis.text.y=element_text(size=8, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

lnNfecundity1<-ggplot(popmetrics_2, aes(lnN0abundance, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlim(12.5,17)+xlab("") + mytheme +theme(axis.text.x=element_blank())
lnNmortality2<-ggplot(popmetrics_2, aes(lnN0abundance, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+ylim(0,0.17)+xlab("") +xlim(12.5,17)+ mytheme +theme(axis.text.x=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)
lnNR3<-ggplot(popmetrics_2, aes(lnN0abundance, Rabundance)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("R")+xlab(expression(ln(N[0]))) + mytheme +ylim(-1.5, 1.5)+xlim(12.5,17)+ geom_hline(yintercept=0, linetype="dashed")+geom_smooth(color="black", method='lm', size=0.5, se=FALSE) 

Productivityfecundity4<-ggplot(popmetrics_2, aes(Productivity, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("")+ylim(0,100)+xlim(-0.2,1.8)+xlab("") + mytheme +theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())
Productivitymortality5<-ggplot(popmetrics_2, aes(Productivity, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylim(0,0.17)+ylab("")+xlim(-0.2,1.8)+xlab("")  + mytheme +theme(axis.text.x=element_blank()) +theme(axis.text.y=element_blank())
ProductivityR6<-ggplot(popmetrics_2, aes(Productivity, Rabundance)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+ylim(-1.5, 1.5)+xlim(-0.2,1.8)+xlab(expression("Biofilm Productivity" (~ µg  ~C / ~ 360 ~ cm^{2}/~ day)))  + mytheme + geom_hline(yintercept=0, linetype="dashed") +theme(axis.text.y=element_blank())

#get them aligned##
g1 <- ggplotGrob(lnNfecundity1)
g2 <- ggplotGrob(lnNmortality2)
g3 <- ggplotGrob(lnNR3)
g4 <- ggplotGrob(Productivityfecundity4)
g5 <- ggplotGrob(Productivitymortality5)
g6 <- ggplotGrob(ProductivityR6)

g123 <- rbind(g1, g2, g3, size = "first")
g456<- rbind(g4, g5, g6, size = "first")
g123456<-cbind(g123, g456, size = "first")

grid.newpage()
grid.draw(g123456)

##for multi panel figures density##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=8, colour = "black"))+theme(axis.text.y=element_text(size=8, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5))) 

denslnNfecundity1<-ggplot(popmetrics_2, aes(lnDensityliter, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlab("") + mytheme +xlim(2,5.25)+theme(axis.text.x=element_blank())
denslnNmortality2<-ggplot(popmetrics_2, aes(lnDensityliter, mortalitydensity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+xlab("")+ylim(-0.07,0.11) +xlim(2,5.25)+ mytheme +theme(axis.text.x=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE, linetype="dotted")+ annotate("text", x = 2.5, y = 0.10, label = "0.033 ± 0.016", size=2.5)
denslnNR3<-ggplot(popmetrics_2, aes(lnDensityliter,  Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("R")+xlab(expression(ln(N/liter))) + mytheme +ylim(-1.5, 1.5)+xlim(2,5.25)+ geom_hline(yintercept=0, linetype="dashed")+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)+ annotate("text", x = 2.5, y = 1.3, label = "-0.611 ± 0.247", size=2.5) 

densProductivityfecundity4<-ggplot(popmetrics_2, aes(Productivity, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("")+ylim(0,100)+xlim(-0.2,1.8)+xlab("") + mytheme +theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())
densProductivitymortality5<-ggplot(popmetrics_2, aes(Productivity, mortalitydensity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("")+xlim(-0.2,1.8)+ylim(-0.07,0.11)+xlab("")  + mytheme +theme(axis.text.x=element_blank()) +theme(axis.text.y=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)+ annotate("text", x = 0.1, y = 0.1, label = "0.054 ± 0.021", size=2.5)
densProductivityR6<-ggplot(popmetrics_2, aes(Productivity, Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+ylim(-1.5, 1.5)+xlim(-0.2,1.8)+xlab(expression("Biofilm Productivity" ( ~ µg  ~C %.% ~ 360 ~ cm^{-2} %.% ~day^{-1})))  + mytheme + geom_hline(yintercept=0, linetype="dashed") +theme(axis.text.y=element_blank())



#get them aligned##
library(gtable)
g11 <- ggplotGrob(denslnNfecundity1)
g22 <- ggplotGrob(denslnNmortality2)
g33 <- ggplotGrob(denslnNR3)
g44 <- ggplotGrob(densProductivityfecundity4)
g55 <- ggplotGrob(densProductivitymortality5)
g66 <- ggplotGrob(densProductivityR6)

g112233 <- rbind(g11, g22, g33, size = "first")
g445566<- rbind(g44, g55, g66, size = "first")
g112233445566<-cbind(g112233, g445566, size = "first")

grid.newpage()
grid.arrange(g112233, g445566, ncol=2)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/popdynamicsdensity.png", width = 5.5 ,height = 7, units = 'in', res = 800)
grid.arrange(g112233, g445566, ncol=2)
dev.off()

##early instars versus early instars##
Ntwoyears<-ggplot(popmetrics_2, aes(lnN0abundance, lnNT1abundance)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.08), size=2)+xlim(12.6, 16.6)+ylim(12.6,16.6)+xlab(expression(ln(N[0])))+ylab(expression(ln(N[t+1]))) + mytheme +geom_abline(slope=1, linetype="dashed")

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Ntwoyears.png", width = 4 ,height = 4, units = 'in', res = 800)
Ntwoyears
dev.off()

Ntwoyearsdens<-ggplot(popmetrics_2, aes(lnDensityliter, lnNT1dens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.08), size=2)+xlim(2.0, 5.2)+ylim(2.0, 5.2)+xlab(expression(ln(N/liter[0])))+ylab(expression(ln(N/liter[t+1]))) + mytheme +geom_abline(slope=1, linetype="dashed")


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Ntwoyearsdens.png", width = 4 ,height = 4, units = 'in', res = 800)
Ntwoyearsdens
dev.off()

PerimetervR<-ggplot(popmetrics_2, aes(Perimeter, Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.08), size=2)+ylim(-1.5, 1.5)+xlim(40,325)+xlab(expression(Perimeter(m^2)))+ylab("R") + mytheme 

RPerimeter <- lm(Rdens ~ Perimeter, data=popmetrics_2) 

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/PerimetervR.png", width = 4 ,height = 4, units = 'in', res = 800)
PerimetervR
dev.off()

##grazing pressure vs. total abundance##
popmetrics_3<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Populationmetrics_2_20_19.csv")
popmetrics_4<-popmetrics_3 %>% mutate(lnN0abundance=log(N0), lnNT1abundance=log(NT1abundance), lnDensityliter=log(DensityLiter), lnNT1dens=log(NT1dens), lnNmiddle=log(Nmiddle), lnDensitymiddle=log(Densitylitersmiddle))


GPabundance<-ggplot(popmetrics_4, aes(lnNmiddle, Grazing.pressure)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.5), size=2)+xlim(12,17.5)+ylim(-2,22)+xlab(expression(ln(N[0]/Pond)))+ylab(expression("Average Grazing Pressure" ( ~ µg  ~C / ~ 360 ~ cm^{2}))) + mytheme +geom_smooth(color="black", method='lm', size=0.5, se=FALSE)

GPdensity<-ggplot(popmetrics_4, aes(lnDensitymiddle, Grazing.pressure)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.5), size=2)+xlim(1.5,6)+ylim(-2,22)+xlab(expression(ln(N[0]/L)))+ylab(expression("Average Grazing Pressure" ( ~ µg  ~C / ~ 360 ~ cm^{2}))) + mytheme 


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/GPabundance.png", width = 4 ,height = 4, units = 'in', res = 800)
GPabundance
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/GPdensity.png", width = 4 ,height = 4, units = 'in', res = 800)
GPdensity
dev.off()



GPabund <- lm(Grazing.pressure ~ lnN0abundance, data=popmetrics_2) 
GPdensity <- lm(Grazing.pressure ~ DensityLiter, data=popmetrics_2)
