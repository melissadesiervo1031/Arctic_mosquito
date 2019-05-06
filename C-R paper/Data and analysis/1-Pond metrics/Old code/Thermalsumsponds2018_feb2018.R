Ponds2018Temp<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Weather and hobo loggers/Quality controlled pond temp/Allponds2018_Temp.csv", header = TRUE)  # read csv file 

str(Ponds2018Temp)

Ponds2018Temp$Date <- as.Date(Ponds2018Temp$Date, "%m/%d/%Y")
Ponds2018Temp$DateTime<-as.POSIXct(Ponds2018Temp$DateTime, format = "%m/%d/%Y %H:%M") ##dateformatfordate_time_start##

install.packages("chillR")
library(chillR)

##GDH Oil##
OilTemp2018<-subset(Ponds2018Temp, (!is.na(Ponds2018Temp[,1])) & (!is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,4])))

OilGDD<-OilTemp2018 %>% select (1:4)%>% mutate(OilGDD0=ifelse(OilTemp>0, OilTemp-0, 0), OilGDD1=ifelse(OilTemp>1, OilTemp-1, 0), OilGDD2=ifelse(OilTemp>2, OilTemp-2, 0), OilGDD3=ifelse(OilTemp>3, OilTemp-3, 0), OilGDD4=ifelse(OilTemp>4, OilTemp-4, 0), OilGDD5=ifelse(OilTemp>5, OilTemp-5, 0))

##GDH NoOil##
NoOilTemp2018<-subset(Ponds2018Temp, (!is.na(Ponds2018Temp[,1])) & (!is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,5])))

NoOilGDD<-NoOilTemp2018 %>% select (1:3,5)%>% mutate(NoOilGDD0=ifelse(NoOilTemp>0, NoOilTemp-0, 0), NoOilGDD1=ifelse(NoOilTemp>1, NoOilTemp-1, 0), NoOilGDD2=ifelse(NoOilTemp>2, NoOilTemp-2, 0), NoOilGDD3=ifelse(NoOilTemp>3, NoOilTemp-3, 0), NoOilGDD4=ifelse(NoOilTemp>4, NoOilTemp-4, 0), NoOilGDD5=ifelse(NoOilTemp>5, NoOilTemp-5, 0))


#GDHEast##
EastTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,6])))

EastGDD<-EastTemp2018 %>% select (1:3,6)%>% mutate(EastGDD0=ifelse(EastTemp>0, EastTemp-0, 0), EastGDD1=ifelse(EastTemp>1, EastTemp-1, 0), EastGDD2=ifelse(EastTemp>2, EastTemp-2, 0), EastGDD3=ifelse(EastTemp>3, EastTemp-3, 0), EastGDD4=ifelse(EastTemp>4, EastTemp-4, 0), EastGDD5=ifelse(EastTemp>5, EastTemp-5, 0))


#GDHGolf##
GolfTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,7])))

GolfGDD<-GolfTemp2018 %>% select (1:3,7)%>% mutate(GolfGDD0=ifelse(GolfTemp>0, GolfTemp-0, 0), GolfGDD1=ifelse(GolfTemp>1, GolfTemp-1, 0), GolfGDD2=ifelse(GolfTemp>2, GolfTemp-2, 0), GolfGDD3=ifelse(GolfTemp>3, GolfTemp-3, 0), GolfGDD4=ifelse(GolfTemp>4, GolfTemp-4, 0), GolfGDD5=ifelse(GolfTemp>5, GolfTemp-5, 0))

#GDHIce##
IceTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,8])))
IceGDD<-IceTemp2018 %>% select (1:3,8)%>% mutate(IceGDD0=ifelse(IceTemp>0, IceTemp-0, 0), IceGDD1=ifelse(IceTemp>1, IceTemp-1, 0), IceGDD2=ifelse(IceTemp>2, IceTemp-2, 0), IceGDD3=ifelse(IceTemp>3, IceTemp-3, 0), IceGDD4=ifelse(IceTemp>4, IceTemp-4, 0), IceGDD5=ifelse(IceTemp>5, IceTemp-5, 0))

#GDHVulg##
VulgTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,9])))

VulgGDD<-VulgTemp2018 %>% select (1:3,9)%>% mutate(VulgGDD0=ifelse(VulgTemp>0, VulgTemp-0, 0), VulgGDD1=ifelse(VulgTemp>1, VulgTemp-1, 0), VulgGDD2=ifelse(VulgTemp>2, VulgTemp-2, 0), VulgGDD3=ifelse(VulgTemp>3, VulgTemp-3, 0), VulgGDD4=ifelse(VulgTemp>4, VulgTemp-4, 0), VulgGDD5=ifelse(VulgTemp>5, VulgTemp-5, 0))

#GDHVulgsmall##
VulgsmallTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,10])))

VulgsmallGDD<-VulgsmallTemp2018 %>% select (1:3,10)%>% mutate(VulgsmallGDD0=ifelse(Vulgsmalltemp>0, Vulgsmalltemp-0, 0), VulgsmallGDD1=ifelse(Vulgsmalltemp>1, Vulgsmalltemp-1, 0), VulgsmallGDD2=ifelse(Vulgsmalltemp>2, Vulgsmalltemp-2, 0), VulgsmallGDD3=ifelse(Vulgsmalltemp>3, Vulgsmalltemp-3, 0), VulgsmallGDD4=ifelse(Vulgsmalltemp>4, Vulgsmalltemp-4, 0), VulgsmallGDD5=ifelse(Vulgsmalltemp>5, Vulgsmalltemp-5, 0))


#GDHWaterfall##
WaterfallTemp2018<-subset(Ponds2018Temp, ((!is.na(Ponds2018Temp[,1])) & !is.na(Ponds2018Temp[,3])) & (!is.na(Ponds2018Temp[,10])))

WaterfallGDD<-WaterfallTemp2018 %>% select (1:3,11)%>% mutate(WaterfallGDD0=ifelse(WaterfallTemp>0, WaterfallTemp-0, 0), WaterfallGDD1=ifelse(WaterfallTemp>1, WaterfallTemp-1, 0), WaterfallGDD2=ifelse(WaterfallTemp>2, WaterfallTemp-2, 0), WaterfallGDD3=ifelse(WaterfallTemp>3, WaterfallTemp-3, 0), WaterfallGDD4=ifelse(WaterfallTemp>4, WaterfallTemp-4, 0), WaterfallGDD5=ifelse(WaterfallTemp>5, WaterfallTemp-5, 0))


###merge the data frames together by degree days##
AllpondsGDD<-merge(Ponds2018Temp,OilGDD, by="DateTime", all = T)
AllpondsGDD1<-merge(AllpondGDD,NoOilGDD, by="DateTime", all = T)
AllpondsGDD2<-merge(AllpondsGDD1, EastGDD, by="DateTime", all = T)
AllpondsGDD3<-merge(AllpondsGDD2,GolfGDD , by="DateTime", all = T)
AllpondsGDD4<-merge(AllpondsGDD3, IceGDD, by="DateTime", all = T)
AllpondsGDD5<-merge(AllpondsGDD4, VulgGDD, by="DateTime", all = T)
AllpondsGDD6<-merge(AllpondsGDD5, VulgsmallGDD , by="DateTime", all = T)
AllpondsGDD7<-merge(AllpondsGDD6,WaterfallGDD, by="DateTime", all = T)

##writecsv hourly thermal sums ##
write.csv(AllpondsGDD7, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Pond thermal sums/allponds2018hourlyDD.csv")

##read in edited csv where I got rid of a few columns
allpondshourlyDD<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Pond thermal sums/allponds2018hourlyDD_edited.csv", header = TRUE)  # read csv file 

##add up each column by date and then divide by 24 to get daily thermal sums##

allpondsdailyDD0<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD0=(sum(OilGDD0, na.rm=FALSE))/24, NoOilDD0=(sum(NoOilGDD0, na.rm=FALSE))/24,EastDD0=(sum(EastGDD0, na.rm=FALSE))/24,GolfDD0=(sum(GolfGDD0, na.rm=FALSE))/24,IceDD0=(sum(IceGDD0, na.rm=FALSE))/24,VulgDD0=(sum(VulgGDD0, na.rm=FALSE))/24,VulgsmallDD0=(sum(VulgsmallGDD0, na.rm=FALSE))/24,WaterfallDD0=(sum(WaterfallGDD0, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD0[is.na(allpondsdailyDD0)] <- 0
allpondsdailyDD0$Date <- as.Date(allpondsdailyDD0$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD0<-allpondsdailyDD0  %>% arrange(Date)%>% dplyr::mutate(OilTS0=cumsum(OilDD0),NoOilTS0=cumsum(NoOilDD0),EastTS0=cumsum(EastDD0), GolfTS0=cumsum(GolfDD0), IceTS0=cumsum(IceDD0), VulgTS0=cumsum(VulgDD0), VulgsmallTS0=cumsum(VulgsmallDD0), WaterfallTS0=cumsum(WaterfallDD0))

allpondsdailyDD1<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD1=(sum(OilGDD1, na.rm=FALSE))/24, NoOilDD1=(sum(NoOilGDD1, na.rm=FALSE))/24,EastDD1=(sum(EastGDD1, na.rm=FALSE))/24,GolfDD1=(sum(GolfGDD1, na.rm=FALSE))/24,IceDD1=(sum(IceGDD1, na.rm=FALSE))/24,VulgDD1=(sum(VulgGDD1, na.rm=FALSE))/24,VulgsmallDD1=(sum(VulgsmallGDD1, na.rm=FALSE))/24,WaterfallDD1=(sum(WaterfallGDD1, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD1[is.na(allpondsdailyDD1)] <- 0
allpondsdailyDD1$Date <- as.Date(allpondsdailyDD1$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD1<-allpondsdailyDD1  %>% arrange(Date)%>% dplyr::mutate(OilTS1=cumsum(OilDD1),NoOilTS1=cumsum(NoOilDD1),EastTS1=cumsum(EastDD1), GolfTS1=cumsum(GolfDD1), IceTS1=cumsum(IceDD1), VulgTS1=cumsum(VulgDD1), VulgsmallTS1=cumsum(VulgsmallDD1), WaterfallTS1=cumsum(WaterfallDD1))


allpondsdailyDD2<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD2=(sum(OilGDD2, na.rm=FALSE))/24, NoOilDD2=(sum(NoOilGDD2, na.rm=FALSE))/24,EastDD2=(sum(EastGDD2, na.rm=FALSE))/24,GolfDD2=(sum(GolfGDD2, na.rm=FALSE))/24,IceDD2=(sum(IceGDD2, na.rm=FALSE))/24,VulgDD2=(sum(VulgGDD2, na.rm=FALSE))/24,VulgsmallDD2=(sum(VulgsmallGDD2, na.rm=FALSE))/24,WaterfallDD2=(sum(WaterfallGDD2, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD2[is.na(allpondsdailyDD2)] <- 0
allpondsdailyDD2$Date <- as.Date(allpondsdailyDD2$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD2<-allpondsdailyDD2  %>% arrange(Date)%>% dplyr::mutate(OilTS2=cumsum(OilDD2),NoOilTS2=cumsum(NoOilDD2),EastTS2=cumsum(EastDD2), GolfTS2=cumsum(GolfDD2), IceTS2=cumsum(IceDD2), VulgTS2=cumsum(VulgDD2), VulgsmallTS2=cumsum(VulgsmallDD2), WaterfallTS2=cumsum(WaterfallDD2))

allpondsdailyDD3<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD3=(sum(OilGDD3, na.rm=FALSE))/24, NoOilDD3=(sum(NoOilGDD3, na.rm=FALSE))/24,EastDD3=(sum(EastGDD3, na.rm=FALSE))/24,GolfDD3=(sum(GolfGDD3, na.rm=FALSE))/24,IceDD3=(sum(IceGDD3, na.rm=FALSE))/24,VulgDD3=(sum(VulgGDD3, na.rm=FALSE))/24,VulgsmallDD3=(sum(VulgsmallGDD3, na.rm=FALSE))/24,WaterfallDD3=(sum(WaterfallGDD3, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD3[is.na(allpondsdailyDD3)] <- 0
allpondsdailyDD3$Date <- as.Date(allpondsdailyDD3$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD3<-allpondsdailyDD3  %>% arrange(Date)%>% dplyr::mutate(OilTS3=cumsum(OilDD3),NoOilTS3=cumsum(NoOilDD3),EastTS3=cumsum(EastDD3), GolfTS3=cumsum(GolfDD3), IceTS3=cumsum(IceDD3), VulgTS3=cumsum(VulgDD3), VulgsmallTS3=cumsum(VulgsmallDD3), WaterfallTS3=cumsum(WaterfallDD3))

allpondsdailyDD4<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD4=(sum(OilGDD4, na.rm=FALSE))/24, NoOilDD4=(sum(NoOilGDD4, na.rm=FALSE))/24,EastDD4=(sum(EastGDD4, na.rm=FALSE))/24,GolfDD4=(sum(GolfGDD4, na.rm=FALSE))/24,IceDD4=(sum(IceGDD4, na.rm=FALSE))/24,VulgDD4=(sum(VulgGDD4, na.rm=FALSE))/24,VulgsmallDD4=(sum(VulgsmallGDD4, na.rm=FALSE))/24,WaterfallDD4=(sum(WaterfallGDD4, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD4[is.na(allpondsdailyDD4)] <- 0
allpondsdailyDD4$Date <- as.Date(allpondsdailyDD4$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD4<-allpondsdailyDD4  %>% arrange(Date)%>% dplyr::mutate(OilTS4=cumsum(OilDD4),NoOilTS4=cumsum(NoOilDD4),EastTS4=cumsum(EastDD4), GolfTS4=cumsum(GolfDD4), IceTS4=cumsum(IceDD4), VulgTS4=cumsum(VulgDD4), VulgsmallTS4=cumsum(VulgsmallDD4), WaterfallTS4=cumsum(WaterfallDD4))

allpondsdailyDD5<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(OilDD5=(sum(OilGDD5, na.rm=FALSE))/24, NoOilDD5=(sum(NoOilGDD5, na.rm=FALSE))/24,EastDD5=(sum(EastGDD5, na.rm=FALSE))/24,GolfDD5=(sum(GolfGDD5, na.rm=FALSE))/24,IceDD5=(sum(IceGDD5, na.rm=FALSE))/24,VulgDD5=(sum(VulgGDD5, na.rm=FALSE))/24,VulgsmallDD5=(sum(VulgsmallGDD5, na.rm=FALSE))/24,WaterfallDD5=(sum(WaterfallGDD5, na.rm=FALSE))/24)%>% arrange(Date = as.Date(Date, "%d-%m-%Y"))
allpondsdailyDD5[is.na(allpondsdailyDD5)] <- 0
allpondsdailyDD5$Date <- as.Date(allpondsdailyDD5$Date, "%m/%d/%Y")
allpondsdailythermalsumsDD5<-allpondsdailyDD5  %>% arrange(Date)%>% dplyr::mutate(OilTS5=cumsum(OilDD5),NoOilTS5=cumsum(NoOilDD5),EastTS5=cumsum(EastDD5), GolfTS5=cumsum(GolfDD5), IceTS5=cumsum(IceDD5), VulgTS5=cumsum(VulgDD5), VulgsmallTS5=cumsum(VulgsmallDD5), WaterfallTS5=cumsum(WaterfallDD5))

library(tidyverse)
allpondsdailythermalsumsall<-list(allpondsdailythermalsumsDD0, allpondsdailythermalsumsDD1, allpondsdailythermalsumsDD2, allpondsdailythermalsumsDD3, allpondsdailythermalsumsDD4, allpondsdailythermalsumsDD5) %>% reduce(left_join, by = "Date")

allpondsdailythermalsumsall$Date <- as.Date(allpondsdailythermalsumsall$Date, "%m/%d/%Y")

write.csv(allpondsdailythermalsumsall, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Pond thermal sums/allponds2018thermalsumsdaily.csv")


##plot thermal sums by date for each pond##

allpondsdailythermalsumsDD0$Date <- as.Date(allpondsdailythermalsumsDD0$Date, "%m/%d/%Y")
allpondsdailyDD5[is.na(allpondsdailyDD5)] <- 0

Thermalsums<-ggplot(allpondsdailythermalsumsDD0, aes(Date)) + geom_line(aes(y = OilTS0 , colour = "OilTS0 ")) + geom_line(aes(y = NoOilTS0 , colour = "NoOilTS0"))+ geom_line(aes(y = EastTS0 , colour = "EastTS0 "))+ geom_line(aes(y = IceTS0 , colour = "IceTS0 "))+ geom_line(aes(y =VulgTS0 , colour = "VulgTS0 "))+ geom_line(aes(y = VulgsmallTS0 , colour = "VulgsmallTS0 "))+ geom_line(aes(y = WaterfallTS0 , colour = "WaterfallTS0 ")) + geom_line(aes(y = GolfTS0 , colour = "GolfTS0"))+scale_y_continuous(limits = c(0, 400))+ scale_x_date(date_breaks = "5 days",labels=date_format("%m-%d"),limits = as.Date(c('2018-05-07','2018-07-04')))+mytheme+ylab("Thermal sum (Base > 0)")+theme(legend.title=element_blank())


png("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Figures/Thermalsums2.png", width = 8, height = 5, units = 'in', res = 300)
Thermalsums
dev.off()                                                                                                                                                                                                                                                                                   

##get average daily temps from the hourly data##
allpondsdailytemp<-allpondshourlyDD %>% group_by(Date) %>%dplyr::summarize(Oildailytemp=mean(OilTemp, na.rm=TRUE),NoOildailytemp=mean(NoOilTemp, na.rm=TRUE), Eastdailytemp=mean(EastTemp, na.rm=TRUE), Golfdailytemp=mean(GolfTemp, na.rm=TRUE), Icedailytemp=mean(IceTemp, na.rm=TRUE) , Vulgdailytemp=mean(VulgTemp, na.rm=TRUE), Vulgsmalldailytemp=mean(Vulgsmalltemp, na.rm=TRUE), Waterfallldailytemp=mean(WaterfallTemp, na.rm=TRUE))

write.csv(allpondsdailytemp, "C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Pond thermal sums/allpondsdailytemp.csv")
