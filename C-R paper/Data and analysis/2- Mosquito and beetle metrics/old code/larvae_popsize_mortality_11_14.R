Larvaepupae2018<- read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data modules/Larvae pupae analysis/larvalcounts_2018_11_14.csv")


#Date#
Larvaepupae2018$Date<-as.Date(Larvaepupae2018$Date, format="%d-%b-%y")
Larvaepupae2018$Day<-as.Date(Larvaepupae2018$Day, format="%d-%b-%y")
Larvaepupae2018$Year<-as.factor(Larvaepupae2018$Year)

###
hist(Larvaepupae2018$Total)
