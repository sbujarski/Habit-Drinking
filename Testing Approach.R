#Testing Approach

library(SpPack)
library(ggplot2)
library(irr)

#Sampledata
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

icc(t(PerfectHabit),type="agreement", unit="single")

Random <- matrix(sample(0:10, 28, replace=T), nrow=4, byrow=T)

icc(t(Random),type="agreement", unit="single")

#simulate with random to see distribution
n <- 1000
ICCs.Rand <- rep(NA,n)
for(i in 1:n){
  ICCs.Rand[i] <- icc(t(matrix(sample(0:10, 28, replace=T), nrow=4, byrow=T)),type="agreement", unit="single")$value
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
                      type="agreement", unit="single")$value
}
SpHist(as.data.frame(ICCs.Binge)$ICCs.Binge)


#Simulate Realistic
#Weekend between 2-10, otherwise 0-2 drinks per weekday, 50%-0, 25%-1, 25%-2
n <- 5000
ICCs.Realistic <- rep(NA,n)
for(i in 1:n){
  RealVector <- c(runif(5,0,1),sample(2:10,2,replace=T),
                        runif(5,0,1),sample(2:10,2,replace=T),
                        runif(5,0,1),sample(2:10,2,replace=T),
                        runif(5,0,1),sample(2:10,2,replace=T))
  RealVector <- ifelse(RealVector<=0.5,0,
                       ifelse(RealVector<=0.75,1,
                              ifelse(RealVector<=1,2,RealVector)))
  Realistic <- matrix(RealVector, nrow=4, byrow=T)
  
  ICCs.Realistic[i] <- icc(t(Realistic),
                       type="agreement", unit="single")$value
}
SpHist(as.data.frame(ICCs.Realistic)$ICCs.Realistic)



#Test with real data from previous study
SampleData<-read.csv("SampleData.csv")
SpDesc(SampleData)


#Compute ICC of "Habitualness" for each submect
SampleData$HabitICC <- NA

#Use ATLFB_2 through ATLFB_29
vars <- paste("ATLFB_", 2:29, sep="")
for(i in 1:dim(SampleData)[1]){
  ATLFB.byWks <- matrix(as.double(SampleData[i,vars]), nrow=4, byrow=T)
  
  SampleData$HabitICC[i] <- icc(t(ATLFB.byWks), type="agreement", unit="single")$value
}
SpDesc(SampleData$HabitICC)
SpHist(SampleData, variable="HabitICC")
