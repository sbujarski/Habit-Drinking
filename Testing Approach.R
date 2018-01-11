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
SpHist(as.data.frame(ICCs.Rand)$ICCs.Rand)


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
SpHist(as.data.frame(ICCs.Binge)$ICCs.Binge)


#Simulate Realistic
#Weekend between 2-10, otherwise 0-2 drinks per weekday, 50%-0, 25%-1, 25%-2
n <- 1000
ICCs.Realistic <- rep(NA,n)
Total.Drinks.Realistic <- rep(NA,n)
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
}

Realistic <- data.frame(ICCs=ICCs.Realistic, Total.Drinks=Total.Drinks.Realistic)
SpHist(Realistic, variable="ICCs")
SpHist(Realistic, variable="Total.Drinks")

cor.test(Realistic$ICCs, Realistic$Total.Drinks)
ggplot(Realistic, aes(x=Total.Drinks, y=ICCs)) + geom_point() + stat_smooth(method="lm") + SpTheme()



#Test with real data from previous study
SampleData<-read.csv("SampleData.csv")
SpDesc(SampleData)


#Compute ICC of "Habitualness" for each submect
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








