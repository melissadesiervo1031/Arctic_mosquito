##files##
Biofilmcurves<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Biofilm/Biofilmcurves_11_29_18.csv")

Biofilmcurves$Date<-as.Date(Biofilmcurves$Date, format="%d-%b")

Biofilmcurves$Type1<-factor(Biofilmcurves$Type1, levels=c("Beginning", "Treatment", "Cage", "No Cage"))


##draw curves for each panel seperately##
mytheme<- theme_bw()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=6, colour = "black"))+theme(axis.text.y=element_text(size=6, colour = "black"))+theme(axis.title=element_text(size=9))+theme(plot.title=element_text(size=7) +theme(plot.title = element_text(hjust = 0.5)))

Biofilmgraph<-ggplot(Biofilmcurves, aes(Date, Biofilm, fill=Type, color=Type))+ geom_smooth(method="lm", se=FALSE, linetype="dashed") +geom_point(aes(colour = Type1))+facet_wrap(~Pond, scales="free")+ylab("Biofilm biomasss (ug)")+xlab("") + mytheme+ theme(panel.spacing = unit(0.5, "lines"))+ theme(legend.position="none")+  theme(plot.title = element_text(vjust = 3)) + scale_color_manual(values=c("black", "#00BFC4", "#F8766D", "#F8766D", "black"))
