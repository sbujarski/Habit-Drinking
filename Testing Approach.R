#Testing Approach

library(SpPack)
library(ggplot2)

#Sampledata
Habitual <- c(0,	2,	0,	0,	9,	2,	8, #week1
              2,	3,	0,	1,	10,	3,	5, #week2
              0,	0,	0,	0,	6,	5,	9, #week3
              1,	5,	3,	0,	7,	5,	6) #week4

SpDesc(Habitual)

Habitual.W1 <- Habitual[1:7]
Habitual.W2 <- Habitual[8:14]
Habitual.W3 <- Habitual[15:21]
Habitual.W4 <- Habitual[22:28]

