
## wing length to size for emergence traps##

emergencesize<-read.csv("C:/Users/Mitzi/Dropbox/Mosquito Science (1)/Data 2011 to 2018/Data from 2018/Emergence traps/Emertrap_size_9_7_18.csv", header = TRUE)  # read csv file 


## add column for egg##

emergencesize_2<-emergencesize %>% mutate(eggs=Winglength2 + I(Winglength2^2.707825))

##subset females only##

emergencesize_2femalesonly<-subset(emergencesize_2, Sex=="F")

## summary##

summaryemergencesize<-emergencesize_2 %>% group_by(Pond) %>% dplyr::summarise(Winglength=mean(Winglength2, na.rm=TRUE), Stdevwing=sd(Winglength2, na.rm=TRUE), Sterrorwing=(sd(Winglength2, na.rm=TRUE)/sqrt(n())), meaneggs=mean(eggs, na.rm=TRUE), Stdeveggs=sd(eggs, na.rm=TRUE), Sterroreggs=(sd(eggs, na.rm=TRUE)/sqrt(n())))

summaryemergencesize<-emergencesize_2 %>% group_by(Pond,Sex) %>% dplyr::summarise(Winglength=mean(Winglength2, na.rm=TRUE), Stdevwing=sd(Winglength2, na.rm=TRUE), Sterrorwing=(sd(Winglength2, na.rm=TRUE)/sqrt(n())), meaneggs=mean(eggs, na.rm=TRUE), Stdeveggs=sd(eggs, na.rm=TRUE), Sterroreggs=(sd(eggs, na.rm=TRUE)/sqrt(n())))

summaryemergencesizefemalesonly<-subset(summaryemergencesizebysex, Sex=="F")


##does wing length vary by pond##
Winglengthall<-aov(Winglength~Pond, data=emergencesize_2)

Winglengthfemale<-aov(Winglength~Pond, data=emergencesize_2femalesonly)


##does egg count vary by pond##

eggsall<-aov(eggs~Pond, data=emergencesize_2)

eggsfemale<-aov(eggs~Pond, data=emergencesize_2femalesonly)

