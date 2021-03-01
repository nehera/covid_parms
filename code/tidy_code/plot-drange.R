set.seed(12995)

library(data.table)
library(plyr)
library(lubridate)
library(ggplot2)
library(forcats)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- fread("state-phases.csv") # would be nice to denote state phase breakpoints on plot
state_positives <- fread("state-positives.csv")
state_dsim <- fread("state-time-to-dec11.csv") # need to imp dsims thru dec 11

state.mid <- as.list(c("MN", "WI", "ND", "MI", "OH", "IN", "IL", "IA", "MO", "SD", "NE", "KS")) # need to make Midwest specific

df <- data.frame()
for (i in state.mid) {
  state <- subset(state_positives, state==i)
  sim_start <- state$date[1]
  sim_end <- "2020-12-11"
  phases <- subset(state_phases, State==i)[,3:5]
  df <- rbind(df, data.frame(id=i,min=sim_start,max=sim_end,p2_0=phases$Phase2_0,p3_0=phases$Phase3_0,p4_0=phases$Phase4_0))
}

df$min <- ymd(as.POSIXct(df$min))
df$max <- ymd(as.POSIXct(df$max))

df$p2_0 <- df$min+df$p2_0
df$p3_0 <- df$min+df$p3_0
df$p4_0 <- df$min+df$p4_0

setwd("~/Desktop/covid_parms/figures/tidy_figures")
pdf(file="state-day-ranges.pdf")

ggplot(df, aes(x=fct_rev(id))) +
  geom_linerange(aes(ymin=min,ymax=max),linetype=2,color="black")+
  geom_point(aes(y=min),size=1,color="red") +
  geom_point(aes(y=max),size=1,color="red") +
  geom_point(aes(y=p2_0),size=1,color="red") +
  geom_point(aes(y=p3_0),size=1,color="red") +
  geom_point(aes(y=p4_0),size=1,color="red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("State") + ylab("Date") +
  coord_flip() + 
  scale_y_date(date_breaks = "1 month", date_labels = "%b")

dev.off()