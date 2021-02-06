setwd("~/Desktop/COVID/data/tidy_data/")
data <- read.csv("all-states-history.csv") ## updated November 17th, 2020
states <- read.csv("List of States.csv")

library(dplyr)

## check if line 70 rmse calculation accurate in abc_model

out_df <- data.frame(State=as.character(),
                     Epidemic_Duration=as.integer())

for (i in 1:length(states$Abbrev)) {
  state_of_interest <- states$Abbrev[i]
  state_data <- data %>% 
    subset(state==state_of_interest) %>%
    subset(positive != 0)
  duration <- length(state_data$positive)
  out <- cbind(state_of_interest, duration)
  out_df <- rbind(out_df, out)
}

## with state data updated to November 17th, 2020, the maximum duration of epidemic at the state level is 244 days.
## thus, the maximum number of days a simulation can be run is 244 days. 