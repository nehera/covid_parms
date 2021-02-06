#KmlShape
library(kmlShape)
setwd("~/Desktop")

cases<-read.csv('state cases by day.csv', header=TRUE)

new<-read.csv("Long_Cases.csv")

myCldsL<-cldsLong(new)
plot(myCldsL)

par(ask=FALSE)
kmlShape(myCldsL,4)
par(ask=TRUE)
plot(myCldsL)

