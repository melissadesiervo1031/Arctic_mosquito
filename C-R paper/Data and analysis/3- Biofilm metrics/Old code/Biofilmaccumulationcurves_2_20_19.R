##files##
Biofilmcurves<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Biofilmcurves_12_11_18.csv")

Biofilmcurves$Date<-as.Date(Biofilmcurves$Date, format="%d-%b")

Biofilmcurves$Type1<-factor(Biofilmcurves$Type1, levels=c("Beginning", "Middle", "End"))
Biofilmcurves$Type<-factor(Biofilmcurves$Type, levels=c("NoCage", "Cage"))

Biofilmcurves$Pond<-factor(Biofilmcurves$Pond, levels=c("East", "NoOil","Oil", "Golf", "Waterfall", "Vulgaris", "Vulgaris small", "Ice"))


summarybiofilmcurves<-Biofilmcurves %>% group_by(Pond, Type, Type1, Type2) %>% dplyr::summarise(meanBiofilm=mean(Biofilm, na.rm=TRUE), StdevBiofilm=sd(Biofilm, na.rm=TRUE), SterrorBiofilm=(sd(Biofilm, na.rm=TRUE)/sqrt(n())))


##draw curves for each panel seperately##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

summarybiofilmcurvesEGINO<-subset(summarybiofilmcurves, Pond=="East"| Pond=="NoOil"| Pond=="Oil"| Pond=="Golf")
summarybiofilmcurvesOVVSW<-subset(summarybiofilmcurves, Pond=="Waterfall"| Pond=="Vulgaris"| Pond=="Vulgaris small"| Pond=="Ice")

BiofilmgraphEGINO<-ggplot(summarybiofilmcurvesEGINO, aes(Type1, meanBiofilm, group=Type)) +geom_point()+ylim(0,27)+ geom_line(aes(linetype=Type))+geom_errorbar(aes(ymin=meanBiofilm-SterrorBiofilm, ymax=meanBiofilm+SterrorBiofilm), width=0.2)+facet_wrap(~Pond, ncol=4)+ylab(bquote(atop("Biofilm Biomass" , (~ µg  ~C / ~ 360 ~ cm^{2}))))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(1,1,0,1), "cm"))+theme(axis.text.x=element_blank())+theme(axis.ticks.x=element_blank())
BiofilmgraphEGINO2<-BiofilmgraphEGINO+ theme(strip.text.x = element_blank())+ geom_text(aes(2, 25, label=Pond, group=NULL))

BiofilmgraphOVVSW<-ggplot(summarybiofilmcurvesOVVSW, aes(Type1, meanBiofilm, group=Type)) +geom_point()+ geom_line(aes(linetype=Type))+geom_errorbar(aes(ymin=meanBiofilm-SterrorBiofilm, ymax=meanBiofilm+SterrorBiofilm), width=0.2)+facet_wrap(~Pond, ncol=4)+ylab(bquote(atop("Biofilm Biomass" , (~ µg  ~C / ~ 360 ~ cm^{2}))))+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+theme(plot.margin=unit(c(0,1,1,1), "cm"))+ scale_y_continuous(limits=c(0,57), breaks=pretty_breaks(n=6))
BiofilmgraphOVVSW2<-BiofilmgraphOVVSW+ theme(strip.text.x = element_blank())+ geom_text(aes(2, 55, label=Pond, group=NULL))


grid.arrange(BiofilmgraphEGINO2, BiofilmgraphOVVSW2 , ncol = 1, heights = c(1, 1.2))


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Biofilmcurve22.png", width = 8, height = 4, units = 'in', res = 800)
grid.arrange(BiofilmgraphEGINO2, BiofilmgraphOVVSW2 , ncol = 1, heights = c(1, 1.8))
dev.off()