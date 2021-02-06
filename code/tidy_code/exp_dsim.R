library(readr)
library(dplyr)
setwd("~/Desktop/COVID/data/tidy_data")
state_positives <- read_csv("state-positives.csv")
df <- data.frame()
for (i in state.abb) {
  num <- nrow(subset(state_positives, state==i))
  df <- rbind(df, data.frame(state=i,num=num))
}
dsim_min <- subset(df, num == min(df$num))
WV_date <- state_positives %>%
  subset(state=="WV")
WV_date[228,]  
dsim_max <- subset(df, num == max(df$num))
WA_date <- state_positives %>%
  subset(state=="WA")
WA_date[228,]  