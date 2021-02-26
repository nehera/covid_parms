end <- ymd("2020-12-11") # Vaccines approved for emergency use by FDA 

library(plyr)
library(lubridate)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_positives <- read.csv("state-positives.csv")

states <- as.list(state.abb)

df <- as.data.frame(matrix(nrow = 50, ncol = 3))
colnames(df) <- c("state", "d1", "diff")

extract.dates <- function(state_of_interest) {
  d1 <- ymd(state_positives[which(state_positives$state==state_of_interest),1][1])
  diff <- as.numeric(end-d1)
  return(data.frame(state=state_of_interest,d1=d1,diff=diff))
}

res <- ldply(lapply(states, extract.dates),rbind)

setwd("~/Desktop/covid_parms/data/tidy_data")
write.csv(res, "state-time-to-dec11.csv")
