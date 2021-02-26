set.seed(12995)

state.mid <- c("MN", "WI", "ND", "MI", "OH", "IN", "IL", "IA", "MO", "SD", "NE", "KS")

library(readr)
library(dplyr)
library(tseries)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- read_csv("state-phases.csv")
state_pops <- read_csv("state-pops.csv")
state_positives <- read_csv("state-positives.csv")

df <- data.frame()

for (j in 1:1) {
  state_of_interest <- as.character(state.mid[j])
  phases <- subset(state_phases, State==state_of_interest)
  phase_num <- phases$phase_num
  pop <- as.numeric(subset(state_pops, Abbrev==state_of_interest)[3])
  o_pos <- state_positives %>%
    subset(state==state_of_interest)
  o_pos <- o_pos$positive[1:dsim]
  
  state <- subset(state_positives, state==state_of_interest)
}