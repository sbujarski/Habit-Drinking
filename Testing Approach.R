#Testing Approach

#"PATTERN DRINKING"

library(SpPack)
library(ggplot2)
library(irr)

#Singular sample subject testing----
Habitual <- matrix(c(0,	2,	0,	0,	9,	2,	8, #week1
                     2,	3,	0,	1,	10,	3,	5, #week2
                     0,	0,	0,	0,	6,	5,	9, #week3
                     1,	5,	3,	0,	7,	5,	6), nrow=4, byrow=T)#week4


#icc ratings=nxm matrix 
icc(t(Habitual),type="agreement", unit="single")

PerfectHabit <- matrix(c(0,	2,	0,	0,	9,	5,	8, #week1
                         0,	2,	0,	0,	9,	5,	8, #week2
                         0,	2,	0,	0,	9,	5,	8, #week3
                         0,	2,	0,	0,	9,	5,	8), nrow=4, byrow=T)#week4

icc(t(PerfectHabit),type="agreement", model="twoway", unit="single")

Random <- matrix(sample(0:10, 28, replace=T), nrow=4, byrow=T)

icc(t(Random),type="agreement", model="twoway", unit="single")

#Monte-Carlo Simulation Testing
#with random to see distribution
n <- 1000
ICCs.Rand <- rep(NA,n)
for(i in 1:n){
  ICCs.Rand[i] <- icc(t(matrix(sample(0:10, 28, replace=T), nrow=4, byrow=T)), type="agreement", model="twoway", unit="single")$value
}
ICCs.Rand.plot <- SpHist(as.data.frame(ICCs.Rand)$ICCs.Rand)
ICCs.Rand.plot <- ICCs.Rand.plot + ggtitle("Simulated ICC from Random Drinking between 0 and 10")
ggsave(ICCs.Rand.plot, filename="ICCs.Rand.plot.png", height=5, width=7, dpi=200)

#Simulate Weekend binger (otherwise abstinent)
n <- 1000
ICCs.Binge <- rep(NA,n)
for(i in 1:n){
  ICCs.Binge[i] <- icc(t(matrix(c(rep(0,5),sample(4:10,2,replace=T),
                                 rep(0,5),sample(4:10,2,replace=T),
                                 rep(0,5),sample(4:10,2,replace=T),
                                 rep(0,5),sample(4:10,2,replace=T)), nrow=4, byrow=T)),
                      type="agreement", model="twoway", unit="single")$value
}
ICCs.Binge.plot <- SpHist(as.data.frame(ICCs.Binge)$ICCs.Binge)
ICCs.Binge.plot <- ICCs.Binge.plot + ggtitle("Simulated ICC from Exclusive Weekend Binger 4:10")
ICCs.Binge.plot
ggsave(ICCs.Binge.plot, filename="ICCs.Binge.plot.png", height=5, width=7, dpi=200)


#Simulate Realistic
#Weekend between 2-10, otherwise 0-2 drinks per weekday, 50%-0, 25%-1, 25%-2
n <- 1000
ICCs.Realistic <- rep(NA,n)
Total.Drinks.Realistic <- rep(NA,n)
SD.Drinks.Realistic <- rep(NA,n)
for(i in 1:n){
  RealVector <- c(runif(5,0,1),rnorm(2,5,2),
                        runif(5,0,1),rnorm(2,5,2),
                        runif(5,0,1),rnorm(2,5,2),
                        runif(5,0,1),rnorm(2,5,2))
  RealVector <- ifelse(RealVector<=0.5,0,
                       ifelse(RealVector<=0.75,1,
                              ifelse(RealVector<=1,2,RealVector)))
  Realistic <- matrix(RealVector, nrow=4, byrow=T)
  
  ICCs.Realistic[i] <- icc(t(Realistic),
                       type="agreement", model="twoway", unit="single")$value
  
  #testing correlation between Total.Drinks and ICCs.Realistic
  Total.Drinks.Realistic[i] <- sum(RealVector)
  
  #testing correlation between SD.Drinks and ICCs.Realistic
  SD.Drinks.Realistic[i] <- sd(RealVector)
  
}

Realistic <- data.frame(ICCs=ICCs.Realistic, Total.Drinks=Total.Drinks.Realistic, SD.Drinks=SD.Drinks.Realistic)

ICCs.Realistic.plot <- SpHist(Realistic, variable="ICCs")
ICCs.Realistic.plot <- ICCs.Realistic.plot + ggtitle("Simulated ICC from 'Realistic' Drinker")
ICCs.Realistic.plot
ggsave(ICCs.Realistic.plot, filename="ICCs.Realistic.plot.png", height=5, width=7, dpi=200)

SD.Realistic.plot <- SpHist(Realistic, variable="SD.Drinks")
SD.Realistic.plot <- SD.Realistic.plot + ggtitle("Simulated SD Drinks from 'Realistic' Drinker")
SD.Realistic.plot
ggsave(SD.Realistic.plot, filename="SD.Realistic.plot.png", height=5, width=7, dpi=200)

Total.Drinks.Realistic.plot <- SpHist(Realistic, variable="Total.Drinks")
Total.Drinks.Realistic.plot <- Total.Drinks.Realistic.plot + ggtitle("Simulated Total Drinks from 'Realistic' Drinker")
Total.Drinks.Realistic.plot
ggsave(Total.Drinks.Realistic.plot, filename="Total.Drinks.Realistic.plot.png", height=5, width=7, dpi=200)

#SD of drinks over 4 weeks does not capture the same thing as ICCs for pattern drinking
cor.test(Realistic$ICCs, Realistic$SD.Drinks)
#cor = 0.3863243
ICCs.SD.Scatter.plot <- ggplot(Realistic, aes(x=ICCs, y=SD.Drinks)) + geom_point() + stat_smooth(method="lm") + 
  ggtitle("Simulated Realistic ICCs and SD of Drinks") + SpTheme()
ICCs.SD.Scatter.plot
ggsave(ICCs.SD.Scatter.plot, filename="ICCs.SD.Scatter.plot.png", height=5, width=7, dpi=200)

#Total drinks is just as correlated as SD.Drinks
cor.test(Realistic$ICCs, Realistic$Total.Drinks)
#cor = 0.392298
ICCs.Total.Scatter.plot <- ggplot(Realistic, aes(x=ICCs, y=Total.Drinks)) + geom_point() + stat_smooth(method="lm") + 
  ggtitle("Simulated Realistic ICCs and Total Drinks") + SpTheme()
ICCs.Total.Scatter.plot
ggsave(ICCs.Total.Scatter.plot, filename="ICCs.Total.Scatter.plot.png", height=5, width=7, dpi=200)



#Test with real data from previous study
SampleData<-read.csv("SampleData.csv")
SpDesc(SampleData)


#Compute ICC of "Habitualness" for each subject
SampleData$HabitICC <- NA

#Use ATLFB_2 through ATLFB_29
vars <- paste("ATLFB_", 2:29, sep="")
for(i in 1:dim(SampleData)[1]){
  #need to force data to be type double
  ATLFB.byWks <- matrix(as.double(SampleData[i,vars]), nrow=4, byrow=T)
  
  SampleData$HabitICC[i] <- icc(t(ATLFB.byWks), type="agreement", model="twoway", unit="single")$value
}

#Less Habitual than I would have thought
SpDesc(SampleData$HabitICC)
SpHist(SampleData, variable="HabitICC")

#Inspect those who are low ICC
matrix(SampleData$HabitICC< -0.2)
#Rownumber: 42, 50, 52, 92
#42
matrix(as.double(SampleData[42,vars]), nrow=4, byrow=T)
icc(t(matrix(as.double(SampleData[42,vars]), nrow=4, byrow=T)), model="twoway", type="agreement", unit="single")

#50
matrix(as.double(SampleData[50,vars]), nrow=4, byrow=T)
icc(t(matrix(as.double(SampleData[50,vars]), nrow=4, byrow=T)), model="twoway", type="agreement", unit="single")

#52
matrix(as.double(SampleData[52,vars]), nrow=4, byrow=T)
icc(t(matrix(as.double(SampleData[52,vars]), nrow=4, byrow=T)), model="twoway", type="agreement", unit="single")

#92
matrix(as.double(SampleData[92,vars]), nrow=4, byrow=T)
icc(t(matrix(as.double(SampleData[92,vars]), nrow=4, byrow=T)), model="twoway", type="agreement", unit="single")

#Yes all these people are very inconsistent drinkers. 
#Seems to work well


#Testing whether Age or sex is related to HabitICC
summary(lm(HabitICC~Age, data=SampleData))
ggplot(SampleData, aes(x=Age, y=HabitICC)) + geom_point() + stat_smooth(method="lm") + SpTheme()

t.test(HabitICC~Female, data=SampleData)
ggplot(SampleData, aes(x=Female, y=HabitICC)) + geom_point(position = position_jitter(w = 0.05, h = 0)) + stat_smooth(method="lm") + SpTheme()

#Adding in a control variable of total alcohol consumed
vars <- paste("ATLFB_", 2:29, sep="")
SampleData$Total.Drinks <- rowSums(SampleData[,vars])

summary(lm(HabitICC~Total.Drinks, data=SampleData))
ggplot(SampleData, aes(x=Total.Drinks, y=HabitICC)) + geom_point() + stat_smooth(method="lm") + SpTheme()


#Testing correlation with SD
vars <- paste("ATLFB_", 2:29, sep="")
for(i in 1:dim(SampleData)[1]){
  SampleData$SD.Drinks[i] <- sd(SampleData[i,vars])
}

summary(lm(HabitICC~SD.Drinks, data=SampleData))
ggplot(SampleData, aes(x=HabitICC, y=SD.Drinks)) + geom_point() + stat_smooth(method="lm") + SpTheme()






