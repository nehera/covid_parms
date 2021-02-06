
setwd("~/Desktop/COVID/data/tidy_data")
obs<-read.csv("all-states-history.csv")
head(obs)

obs_slim<-data.frame(obs$state, obs$positive)

unique1<-data.frame(unique(obs$state))

chart<-data.frame(matrix(NA, ncol=56, nrow=270))

for(i in  1:nrow(unique1)){
  print(i)
  X<-subset(obs_slim, obs_slim$obs.state==unique1[i,])
  b<-270-nrow(X)
  chart[,i]<-c(X$obs.positive, rep(0,b))
  Y<-cbind(X)
}

write.csv(chart,"state cases by day.csv")
write.csv(unique1, "unique1.csv", row.names=F)


cases<-read.csv("state cases by day.csv")
#cases$Days<-rev(cases$Days)

options(scipen=999)
matplot(cases$Days,cases[,2:57], type="l", lty=1, lwd=1.5, 
        xlab="Days", ylab="Cases", ylim=c(0,200300))


library(tidyr)
df.long <- pivot_longer(cases, cols=2:57, names_to = "State", 
                        values_to = "Cases")

write.csv(df.long, "Long_Cases.csv", row.names = FALSE)
