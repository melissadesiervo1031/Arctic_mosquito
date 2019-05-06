
Sweepnet<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data from 2017/20122017sweepnet.csv", header = TRUE)  # read csv file 
eggcount<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data from 2017/20122017eggs.csv", header = TRUE)  # read csv file 

Sweepnet$Date<-as.Date(Sweepnet$Date, format = "%m/%d/%Y") ##dateformat##
eggcount$Date<-as.Date(eggcount$Date, format = "%m/%d/%Y") ##dateformat##

str(Sweepnet)

###Add mosquitoes with 0 eggs as rows to make freq histogram##

Sweepnetnotgravid<-Sweepnet[ -c(4, 6) ]
Sweepnetnotgravid<-subset(Sweepnetnotgravid, NotGravid > 0)
Sweepnetnotgravidind<-do.call("rbind"
        , apply(Sweepnetnotgravid, 1, function(x) 
          data.frame(cbind(Year = x[1], Site = x[2], Date= x[3],   Eggs = rep(0, times = x[4]), Winglength= rep ("NA", times = x[4])))
        )
)

eggcountwithzeroes<-rbind(eggcount, Sweepnetnotgravidind)

eggcountwithzeroes$Eggs<-as.numeric(eggcountwithzeroes$Eggs)

eggcount2017<-subset(eggcountwithzeroes, Year > 2016)
eggcount2017nozero<-subset(eggcount2017, Eggs > 0)
eggcount2012<-subset(eggcountwithzeroes, Year < 2016)
eggcount2012nozero<-subset(eggcount2012, Eggs > 0)


hist(eggcount2017$Eggs)


# put a break at the default axis and position
library(plotrix)
histeggs2017<-hist(eggcount2017$Eggs)##histogramofeggs##
egg.freq2017<-cbind(histeggs2017$breaks[1:9],histeggs2017$counts) ##pullout
colnames(egg.freq2017) <- c("Numeggs","count")
egg.freq2017<-as.data.frame(egg.freq2017)
egg.freq2017$Numeggs<-as.factor(egg.freq2017$Numeggs)

par(mfrow=c(1,2))

eggs2017<-gap.barplot(egg.freq2017$count,gap=c(11,930),xlab="Number of eggs",xtics=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110), ytics=c(1, 5, 10, 940),
            ylab="Frequency",main="2017 egg frequency distribution", col = "black")
abline(v=mean(eggcount2017nozero$Eggs), col = "red", lty= 2, lwd = 2)

histeggs2012<-hist(eggcount2012$Eggs)##histogramofeggs##
egg.freq2012<-cbind(histeggs2012$breaks[1:12],histeggs2012$counts) ##pullout
colnames(egg.freq2012) <- c("Numeggs","count")
egg.freq2012<-as.data.frame(egg.freq2012)
egg.freq2012$Numeggs<-as.factor(egg.freq2012$Numeggs)

eggs2012<-gap.barplot(egg.freq2012$count,gap=c(141,2900),xlab="Number of eggs",xtics=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90,100, 110), ytics=c(1, 50, 100, 2940),
                      ylab="Frequency",main="2012 egg frequency distribution", col = "black")
abline(v=mean(eggcount2012nozero$Eggs), col = "red", lty= 2, lwd = 2)


