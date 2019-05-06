
popmetrics<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Populationmetrics_4_8_19.csv")

##add on a lnN column#

popmetrics_2<-popmetrics %>% mutate(lnN0abundance=log(N0), lnNT1abundance=log(NT1abundance), lnDensityliter=log(DensityLiter), lnNT1dens=log(NT1dens))


##for multi panel figures density##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=8, colour = "black"))+theme(axis.text.y=element_text(size=8, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

lnNfecundity1<-ggplot(popmetrics_2, aes(lnDensityliter, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("\nPotential Fecundity")+xlim(1.75,5.5)+ylim(0,100)+xlab("") + mytheme +theme(axis.text.x=element_blank())
lnNmortality2<-ggplot(popmetrics_2, aes(lnDensityliter, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("\nLarval mortality")+xlim(1.75,5.5)+ylim(0,0.17)+xlab("") + mytheme +theme(axis.text.x=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)
lnNR3<-ggplot(popmetrics_2, aes(lnDensityliter,  Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("\nR")+xlab(bquote(atop(, ln(N[0])))) + mytheme +xlim(1.75,5.5)+ylim(-1.5, 1.5)+ geom_hline(yintercept=0, linetype="dashed")+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)+theme(axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0))) 

Productivityfecundity4<-ggplot(popmetrics_2, aes(Productivity, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("")+ylim(0,100)+xlim(-0.3,1.9)+xlab("") + mytheme +theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())
Productivitymortality5<-ggplot(popmetrics_2, aes(Productivity, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylim(0,0.17)+ylab("")+xlim(-0.3,1.9)+xlab("")  + mytheme +theme(axis.text.x=element_blank()) +theme(axis.text.y=element_blank())
ProductivityR6<-ggplot(popmetrics_2, aes(Productivity, Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+ylim(-1.5, 1.5)+xlim(-0.3,1.9)+xlab(bquote(atop(,"Biofilm productivity " (~ µg  ~C %.% ~ 360 ~ cm^{-2} %.% ~day^{-1}))))  + mytheme + geom_hline(yintercept=0, linetype="dashed") +theme(axis.text.y=element_blank()) 

Predatorfecundity7<-ggplot(popmetrics_2, aes(lnPredatormax, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("")+ylim(0,100)+xlab("") + mytheme +theme(axis.text.x=element_blank())+theme(axis.text.y=element_blank())
Predatormortality8<-ggplot(popmetrics_2, aes(lnPredatormax, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylim(0,0.17)+ylab("")+xlab("")  + mytheme +theme(axis.text.x=element_blank()) +theme(axis.text.y=element_blank())
PredatorR9<-ggplot(popmetrics_2, aes(lnPredatormax, Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+ylim(-1.5, 1.5)+xlab(bquote(atop(,"ln(N predator)"))) + mytheme + geom_hline(yintercept=0, linetype="dashed") +theme(axis.text.y=element_blank()) 


##all together###
#get them aligned##
g1 <- ggplotGrob(lnNfecundity1)
g2 <- ggplotGrob(lnNmortality2)
g3 <- ggplotGrob(lnNR3)
g4 <- ggplotGrob(Productivityfecundity4)
g5 <- ggplotGrob(Productivitymortality5)
g6 <- ggplotGrob(ProductivityR6)
g7 <- ggplotGrob(Predatorfecundity7)
g8 <- ggplotGrob(Predatormortality8)
g9 <- ggplotGrob(PredatorR9)

g123 <- rbind(g1, g2, g3, size = "first")
g456<- rbind(g4, g5, g6, size = "first")
g789<- rbind(g7, g8, g9, size = "first")



g123456789<-cbind(g123, g456, g789,size = "first")

grid.newpage()
grid.arrange(g123456789)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/allpopdynamicsdensity_wpredator.png", width = 7 ,height = 7, units = 'in', res = 800)
grid.arrange(g123456789)
dev.off()




##samewith density##
denslnNfecunditym1 <- lm(Fecundity ~ lnDensityliter, data=popmetrics_2) 
denslnNmortalitym2 <- lm(mortalityN0 ~ lnDensityliter, data=popmetrics_2) 
denslnNRm3 <- lm(Rdens ~ lnDensityliter, data=popmetrics_2) 

fecunditym1 <- lm(Fecundity ~ Productivity, data=popmetrics_2) 
Productivitym2 <- lm(mortalityN0 ~ Productivity, data=popmetrics_2) 
ProductivityRm3 <- lm(Rdens ~ Productivity, data=popmetrics_2) 

##two way anova##
twowayanova <- lm(mortalitydensity ~ lnDensityliter*Productivity, data=popmetrics_2) 



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


GPabundance<-ggplot(popmetrics_4, aes(lnNmiddle, Grazing.pressure)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.5), size=2)+xlim(12,14.5)+ylim(-2,22)+xlab(expression(ln(N[mid]/Pond)))+ylab(expression("Average Grazing Pressure" ( ~ µg  ~C / ~ 360 ~ cm^{2}))) + mytheme 

GPdensity<-ggplot(popmetrics_4, aes(lnDensitymiddle, Grazing.pressure)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.5), size=2)+xlim(1,4)+ylim(-2,22)+xlab(expression(ln(N[mid]/L)))+ylab(expression("Average Grazing Pressure" ( ~ µg  ~C / ~ 360 ~ cm^{2}))) + mytheme 


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/GPabundance.png", width = 4 ,height = 4, units = 'in', res = 800)
GPabundance
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/GPdensity.png", width = 4 ,height = 4, units = 'in', res = 800)
GPdensity
dev.off()



GPabund <- lm(Grazing.pressure ~ lnN0abundance, data=popmetrics_2) 
GPdensity <- lm(Grazing.pressure ~ DensityLiter, data=popmetrics_2)


##total microbial biomass##

microbesfecundity1<-ggplot(popmetrics_2, aes(totalmicrobes, Fecundity)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-4), size=2)+ylab("Potential Fecundity")+ylim(0,100)+xlab("") +xlim(250,2200)+ mytheme +theme(axis.text.x=element_blank())+ylab("") +theme(axis.text.y=element_blank())
microbesmortality2<-ggplot(popmetrics_2, aes(totalmicrobes, mortalityN0)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.005), size=2)+ylab("Larval mortality")+ylim(0,0.17)+xlab("")+xlim(250,2200) + mytheme +theme(axis.text.x=element_blank())+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)+ylab("")+theme(axis.text.y=element_blank())
microbesmortalityNR3<-ggplot(popmetrics_2, aes(totalmicrobes,  Rdens)) +geom_point()+geom_text(aes(label=Pond),position=position_nudge(y=-0.1), size=2)+ylab("")+xlab(bquote(atop(,"Total Biomass of fatty acids (nmol/g)"))) +xlim(250,2200) + mytheme + geom_hline(yintercept=0, linetype="dashed")+theme(axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0)))+geom_smooth(color="black", method='lm', size=0.5, se=FALSE)+theme(axis.text.y=element_blank())+ylim(-1.5, 1.5) 

microbesfecunditym1 <- lm(Fecundity ~ totalmicrobes, data=popmetrics_2) 
microbesmortalitym2 <- lm(mortalityN0 ~ totalmicrobes, data=popmetrics_2) 
microbesRm3 <- lm(Rdens ~ totalmicrobes, data=popmetrics_2) 

#get them aligned##
gg1 <- ggplotGrob(microbesfecundity1)
gg2 <- ggplotGrob(microbesmortality2)
gg3 <- ggplotGrob(microbesmortalityNR3)

gg123 <- rbind(gg1, gg2, gg3, size = "first")

grid.newpage()
grid.arrange(gg123)

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/biomassoffattyacidspop.png", width = 3.5 ,height = 7, units = 'in', res = 800)
grid.arrange(gg123)
dev.off()



