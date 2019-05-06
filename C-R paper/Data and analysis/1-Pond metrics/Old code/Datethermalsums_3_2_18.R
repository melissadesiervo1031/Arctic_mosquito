#date predictions##

Larvaepupaemass <- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Larvae pupae analysis/Larvaepupaemass20112017.csv")
Larvaepupaemass$Date<-as.Date(Larvaepupaemass$Date, format="%d-%b-%y")
Larvaepupaemass$Day<-as.Date(Larvaepupaemass$Day, format="%d-%b-%y")
Larvaepupaemass$Year<-as.factor(Larvaepupaemass$Year)



##calculate one growth rate per pond by pulling out slope##
Masssummaryfordatepredictions<- Larvaepupaemass %>% group_by(Date, Day, Site, Year, SiteYear)%>%summarise(Meanmasslarvae=mean(Mass, na.rm=TRUE), variancemasslarvae=(sd(Mass, na.rm=TRUE))^2, Stdevmasslarvae=sd(Mass, na.rm=TRUE), Sterror=(sd(Mass, na.rm=TRUE)/sqrt(n())))

growthcoefsdatepredic<-Masssummaryfordatepredictions%>% group_by(Site, Year, SiteYear) %>% do({fitmass = lm(Meanmasslarvae~Date, data=.)
+ data.frame(intercept= coef(fitmass)[1], slope=coef(fitmass)[2])})   

growthcoefsdatepredic %>% filter(complete.cases(slope, intercept))


##use intercept and slope to predict dates for when larvae are 0.2 mg###

##black2011##
as.Date.numeric((0.2-- 990)/0.0655, origin = "1970-01-01")

##black2012##
as.Date.numeric((0.2-- 1467)/ 0.0948, origin = "1970-01-01")

##moz valley 2017##
as.Date.numeric((0.2--1042)/ 0.0602, origin = "1970-01-01")

##Oil 2017##
as.Date.numeric((0.2--1313)/ 0.0759, origin = "1970-01-01")

##Seahorse 2017##
as.Date.numeric((0.2--1162)/ 0.0671, origin = "1970-01-01")

##Target 2017##
as.Date.numeric((0.2--1271)/  0.0735, origin = "1970-01-01")

##Vulgaris 2017##
as.Date.numeric((0.2--1046)/  0.0605, origin = "1970-01-01")


##

Tsumsyears <- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Phenology data analysis/Tsumsearlyinstars3_2_2018.csv")
Tsumsyears$Tsum6.7<-as.Date(Tsumsyears$Tsum6.7, format="%m/%d/%Y")
Tsumsyears$Tsum27.25<-as.Date(Tsumsyears$Tsum27.25, format="%m/%d/%Y")
Tsumsyears$Tsum40.875<-as.Date(Tsumsyears$Tsum40.875, format="%m/%d/%Y")
Tsumsyears$Tsum44.5<-as.Date(Tsumsyears$Tsum44.5, format="%m/%d/%Y")
Tsumsyears$Tsum61<-as.Date(Tsumsyears$Tsum61, format="%m/%d/%Y")
Tsumsyears$Year<-as.factor(Tsumsyears$Year)

#Frequency histograms of dates##
hist(Tsumsyears$Tsum6.7, main="Very early ponds", xlab="Date of early instars (0.2 mg)", "2 days", format = "%d %b")
    

Veryearlyponds<- ggplot(Tsumsyears, aes(Tsum6.7, ..count..)) +geom_histogram(binwidth=15, color = "grey30", fill = "white") + xlab("")+ ylab("Frequency")+ggtitle("Very early ponds") + scale_x_date(breaks = date_breaks("8 days"),labels = date_format("%b-%d"))+mytheme


###
Tsumsyears <- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Phenology data analysis/Tsumsearlyinstars3_2_2018_2.csv")
Tsumsyears$Date<-as.Date(Tsumsyears$Date, format="%m/%d/%Y")
Tsumsyears2<-subset(Tsumsyears2, Date > as.Date("2015-03-02"))
Tsumssince2000<-subset(Tsumsyears2, Year > 1999)

Tsumsyears2$Pondtype <- factor(Tsumsyears2$Pondtype, levels = c("Very early ponds", "Early ponds", "Average ponds", "Later than average ponds", "Late ponds"))
Tsumssince2000$Pondtype <- factor(Tsumssince2000$Pondtype, levels = c("Very early ponds", "Early ponds", "Average ponds", "Later than average ponds", "Late ponds"))

compare_med_date <- Tsumsyears2 %>%group_by(Pondtype) %>%summarise(MedianDate = median(Date))
compare_med_datesince2000 <- Tsumssince2000 %>%group_by(Pondtype) %>%summarise(MedianDate = median(Date))

Allpondstiming<-ggplot(Tsumsyears2, aes(x = Date, fill = Pondtype)) +geom_density(alpha = .5)+ xlab("")+ ylab("Frequency")+ggtitle("Date of early instar development 1974-2017")+geom_vline(data = compare_med_date, aes(xintercept = as.numeric(MedianDate))) + scale_x_date(breaks = date_breaks("14 days"),labels = date_format("%b-%d"))+mytheme

mytheme2<- theme_classic()+ theme(axis.line.x= element_line(colour = "black", size=0.3))+theme(axis.line.y= element_line(colour = "black", size=0.3))+theme(axis.text.x=element_text(size=5, colour = "black"))+theme(axis.text.y=element_text(size=7, colour = "black"))+theme(axis.title=element_text(size=8))+theme(plot.title=element_text(size=8) +theme(plot.title = element_text(hjust = 0.5)))

Allpondstiming2<- ggplot(Tsumsyears2, aes(x = Date, fill = Pondtype)) +geom_histogram(binwidth=15) + xlab("")+ ylab("Frequency")+ggtitle("Date of early instar development 1974-2017") +facet_wrap(~Pondtype, scales='free_x', ncol=2)+geom_vline(data = compare_med_date, aes(xintercept = as.numeric(MedianDate)), linetype = "dashed")+ scale_x_date(breaks = date_breaks("12 days"),labels = date_format("%b-%d"))+mytheme2+ theme(legend.position="none")

Allpondstimingsince2000<- ggplot(Tsumssince2000, aes(x = Date, fill = Pondtype)) +geom_histogram(binwidth=15) + xlab("")+ ylab("Frequency")+ggtitle("Date of early instar development 2000-2017") +facet_wrap(~Pondtype, scales='free_x',ncol=2)+geom_vline(data = compare_med_datesince2000, aes(xintercept = as.numeric(MedianDate)), linetype = "dashed")+ scale_x_date(breaks = date_breaks("12 days"),labels = date_format("%b-%d"))+mytheme2+ theme(legend.position="none")


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/phenologyearlyinstars.png", width = 5, height = 6, units = 'in', res = 800)
Allpondstiming2
dev.off()

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/phenologyearlyinstarssince2000.png", width = 5, height = 6, units = 'in', res = 800)
Allpondstimingsince2000
dev.off()

