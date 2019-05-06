Ponds2018Temp2<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Weather and hobo loggers/Quality controlled pond temp/Allponds2018andairport_Temp.csv", header = TRUE)  # read csv file 

Ponds2018Temp2$Date <- as.Date(Ponds2018Temp$Date, "%m/%d/%Y")
Ponds2018Temp2$DateTime<-as.POSIXct(Ponds2018Temp$DateTime, format = "%m/%d/%Y %H:%M") ##dateformatfordate_time_start##


##tidyr to turn into long form##
library(tidry)
Ponds2018Templong<- Ponds2018Temp2%>% gather(OilTemp, NoOilTemp, EastTemp, GolfTemp, IceTemp, VulgTemp, Vulgsmalltemp, WaterfallTemp, Airporttemp, key = "Pond", value = "TempC")

install.packages("chillR")
library(chillR)

## GDD hourly for all ponds##

Degreehoursall2018<-Ponds2018Templong %>% drop_na() %>% mutate(GDD0=ifelse(TempC>0, TempC-0, 0), GDD1=ifelse(TempC>1, TempC-1, 0), GDD2=ifelse(TempC>2, TempC-2, 0), GDD3=ifelse(TempC>3, TempC-3, 0), GDD4=ifelse(TempC>4, TempC-4, 0), GDD5=ifelse(TempC>5, TempC-5, 0))

##add up each column by date and then divide by 24 to get daily thermal sums##

Degreedaysall2018<-Degreehoursall2018 %>% group_by(Date, Pond) %>%dplyr::summarize(DegreeDay0=(sum(GDD0, na.rm=FALSE))/24, DegreeDay1=(sum(GDD1, na.rm=FALSE))/24, DegreeDay2=(sum(GDD2, na.rm=FALSE))/24, DegreeDay3=(sum(GDD3, na.rm=FALSE))/24, DegreeDay4=(sum(GDD4, na.rm=FALSE))/24, DegreeDay5=(sum(GDD5, na.rm=FALSE))/24) %>% arrange(Pond, Date = as.Date(Date, "%d-%m-%Y"))


##then cumulation to get thermal sums##
Thermalsumsall2018<-Degreedaysall2018  %>% group_by(Pond)%>% arrange(Pond, Date)%>% dplyr::mutate(TS0=cumsum(DegreeDay0),TS1=cumsum(DegreeDay1), TS2=cumsum(DegreeDay2), TS3=cumsum(DegreeDay3), TS4=cumsum(DegreeDay4), TS5=cumsum(DegreeDay5))


##plot thermal sums by date for each pond and airport##

Thermalsumsgraph<-ggplot(Thermalsumsall2018, aes(Date, TS0, color=Pond)) + geom_line(aes(group=Pond)) +scale_y_continuous(limits = c(0, 400))+ scale_x_date(date_breaks = "5 days",labels=date_format("%m-%d"),limits = as.Date(c('2018-05-07','2018-07-04')))+mytheme+ylab("Thermal sum (Base > 0)")+theme(legend.title=element_blank())+ geom_hline(yintercept=300, linetype="dashed") 

png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Thermalsumsandairport.png", width = 8, height = 5, units = 'in', res = 300)
Thermalsumsgraph
dev.off()                                                                                                                                                                                                                                                                                   

##exclude the airport...model for all 8 ponds##

Thermalsumsall2018ponds<-subset(Thermalsumsall2018, Pond=="OilTemp"|  Pond=="NoOilTemp"|  Pond=="EastTemp"|  Pond=="GolfTemp"| Pond=="IceTemp"| Pond=="VulgTemp"|  Pond=="Vulgsmalltemp"|  Pond=="WaterfallTemp")

##average thermal sums across the 8 ponds##
##first go back to the wide format##

Thermalsums02018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS0) %>%  spread(Pond, TS0, fill=NA)%>%rowwise()%>%  mutate(averageTS0=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))
Thermalsums12018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS1) %>%  spread(Pond, TS1, fill=NA)%>%rowwise()%>%  mutate(averageTS1=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))
Thermalsums22018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS2) %>%  spread(Pond, TS2, fill=NA)%>%rowwise()%>%  mutate(averageTS2=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))
Thermalsums32018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS3) %>%  spread(Pond, TS3, fill=NA)%>%rowwise()%>%  mutate(averageTS3=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))
Thermalsums42018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS4) %>%  spread(Pond, TS4, fill=NA)%>%rowwise()%>%  mutate(averageTS4=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))
Thermalsums52018pondswide<-Thermalsumsall2018ponds  %>% select(Date, Pond, TS5) %>%  spread(Pond, TS5, fill=NA)%>%rowwise()%>%  mutate(averageTS5=mean(c(EastTemp,GolfTemp,IceTemp,NoOilTemp,OilTemp, Vulgsmalltemp, WaterfallTemp), na.rm=T))

Thermalsumsavgallponds<-data.frame(Thermalsums02018pondswide$Date, Thermalsums02018pondswide$averageTS0, Thermalsums12018pondswide$averageTS1, Thermalsums22018pondswide$averageTS2, Thermalsums32018pondswide$averageTS3, Thermalsums42018pondswide$averageTS4, Thermalsums52018pondswide$averageTS5)

Thermalsumsavgallponds<-plyr::rename(Thermalsumsavgallponds, c("Thermalsums02018pondswide.Date"="Date", "Thermalsums02018pondswide.averageTS0"="avgTS0", "Thermalsums12018pondswide.averageTS1"="avgTS1", "Thermalsums22018pondswide.averageTS2"="avgTS2", "Thermalsums32018pondswide.averageTS3"="avgTS3", "Thermalsums42018pondswide.averageTS4"="avgTS4", "Thermalsums52018pondswide.averageTS5"="avgTS5"))

##now get the air temp in the same format##

Thermalsumsall2018airport<-subset(Thermalsumsall2018, Pond=="Airporttemp")
Thermalsumsall2018airport2 <- Thermalsumsall2018airport %>% select(Date, TS0, TS1, TS2, TS3, TS4, TS5)

Thermalsumspondandair <- merge(Thermalsumsavgallponds,Thermalsumsall2018airport2,by="Date")

plot(Thermalsumspondandair$TS0, Thermalsumspondandair$avgTS0)
plot(Thermalsumspondandair$TS1, Thermalsumspondandair$avgTS1)
plot(Thermalsumspondandair$TS2, Thermalsumspondandair$avgTS2)

