
popmetrics<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Populationmetrics_2_14_19.csv")

##add on a lnN column#

popmetrics_2<-popmetrics %>% mutate(lnN0=log(N0), lnNT1=log(NT1))


##linear regressions for all variables##
lnNfecunditym1 <- lm(Fecundity ~ lnN0, data=popmetrics_2) 
lnNmortalitym2 <- lm(mortalityN0 ~ lnN0, data=popmetrics_2) 
lnNRm3 <- lm(R ~ lnN0, data=popmetrics_2) 

fecunditym1 <- lm(Fecundity ~ Productivity, data=popmetrics_2) 
Productivitym2 <- lm(mortalityN0 ~ Productivity, data=popmetrics_2) 
ProductivityRm3 <- lm(R ~ Productivity, data=popmetrics_2) 

summary(lnNfecunditym1)
summary(lnNmortalitym2)
summary(lnNRm3)
summary(fecunditym1)
summary(Productivitym2)
summary(ProductivityRm3)

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

##for multi panel figures##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=8, colour = "black"))+theme(axis.text.y=element_text(size=8, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

lnNfecundity1<-ggplot(popmetrics_2, aes(lnN0, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlim(12.5,17)+xlab("") + mytheme +theme(axis.text.x=element_blank())
lnNmortality2<-ggplot(popmetrics_2, aes(lnN0, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+ylim(0,0.17)+xlab("") +xlim(12.5,17)+ mytheme +theme(axis.text.x=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)
lnNR3<-ggplot(popmetrics_2, aes(lnN0, R)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("R")+xlab("ln(N0)") + mytheme +ylim(-1.5, 1.5)+xlim(12.5,17)+ geom_hline(yintercept=0, linetype="dashed")+geom_smooth(color="black", method='lm', size=0.5, se=FALSE) 

Productivityfecundity4<-ggplot(popmetrics_2, aes(Productivity, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("")+ylim(0,100)+xlim(-0.2,1.8)+xlab("") + mytheme +theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())
Productivitymortality5<-ggplot(popmetrics_2, aes(Productivity, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylim(0,0.17)+ylab("")+xlim(-0.2,1.8)+xlab("")  + mytheme +theme(axis.text.x=element_blank()) +theme(axis.text.y=element_blank())
ProductivityR6<-ggplot(popmetrics_2, aes(Productivity, R)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+ylim(-1.5, 1.5)+xlim(-0.2,1.8)+xlab(expression("Biofilm Productivity" (~ µg  ~C / ~ 360 ~ cm^{2}/~ day)))  + mytheme + geom_hline(yintercept=0, linetype="dashed") +theme(axis.text.y=element_blank())

#get them aligned##
library(gtable)
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

###
png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/multipanelpopdynamics.png", width = 6 ,height = 8.5, units = 'in', res = 800)
grid.draw(g123456)
dev.off()

##early instars versus early instars##
Ntwoyears<-ggplot(popmetrics_2, aes(lnN0, lnNT1)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("ln(Nt+1)")+xlab("ln(N0)") + mytheme 






perimeterR<-ggplot(popmetrics_2, aes(Perimeter, R)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.05), size=2)+ylab("R")+xlab("Perimeter(m)") + mytheme + geom_hline(yintercept=0, linetype="dashed") 
