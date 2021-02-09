dsim <- 228 # days simulated

library(readr)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_positives <- read_csv("state-positives.csv")
df <- data.frame()
for (i in state.abb) {
  state <- subset(state_positives, state==i)
  d_max <- nrow(state)
  sim_start <- state$date[1]
  sim_end <- state$date[dsim]
  df <- rbind(df, data.frame(state=i,d_max=d_max,sim_start=sim_start,sim_end=sim_end))
}
# dsim_min <- subset(df, d_max == min(df$d_max))
# WV_date <- state_positives %>%
#   subset(state=="WV")
# WV_date[228,]  
# dsim_max <- subset(df, d_max == max(df$d_max))
# WA_date <- state_positives %>%
#   subset(state=="WA")
# WA_date[228,]  

df2 <- data.frame(id=df$state, min=df$sim_start, max=df$sim_end)

library(forcats)

setwd("~/Desktop/covid_parms/figures/tidy_figures")
pdf(file="state-day-ranges.pdf")

ggplot(df2, aes(x=fct_rev(id))) +
  geom_linerange(aes(ymin=min,ymax=max),linetype=2,color="blue")+
  geom_point(aes(y=min),size=0.5,color="red") +
  geom_point(aes(y=max),size=0.5,color="red") +
  theme_bw() + xlab("State") + ylab("Date") +
  coord_flip()

dev.off()