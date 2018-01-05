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

#Weekend binger
