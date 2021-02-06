setwd("~/Desktop/COVID/data/tidy_data/")
states <- read.csv("List of States.csv")
data <- read.csv("all-states-history.csv") ## updated November 17th, 2020
phases <- read.csv("phases_by_state.csv") 

library(dplyr)
library(ggplot2)
library(zoo) #for calculating rollmean

state_of_interest <- "AK"
state_population <- as.numeric(subset(states, Abbrev == state_of_interest)[3])

state_data <- data %>%
  subset(state == state_of_interest) %>%
  subset(!is.na(positive))

state_positives <- rev(state_data$positive)

day_range <- 1:length(state_positives)

state_phases <- as.numeric(subset(phases, State == state_of_interest)[1,2:4])

plotme <- data.frame(Day = day_range, Positives = state_positives)

p1 <- ggplot(data = plotme, aes(x=Day, y=Positives)) +
  geom_rect(aes(xmin=0,xmax=length(day_range),ymin=-Inf,ymax=Inf),alpha=0.1,fill="green") +
  theme_bw() +
  geom_line()

p2 <- ggplot(data = plotme, aes(x=Day, y=Positives)) +
  geom_rect(aes(xmin=0,xmax=state_phases[1],ymin=-Inf,ymax=Inf),alpha=0.1,fill="green") +
  geom_rect(aes(xmin=state_phases[1],xmax=length(day_range),ymin=-Inf,ymax=Inf),alpha=0.1,fill="orange") +
  theme_bw() +
  geom_line()

p3 <- ggplot(data = plotme, aes(x=Day, y=Positives)) +
  geom_rect(aes(xmin=0,xmax=state_phases[1],ymin=-Inf,ymax=Inf),alpha=0.1,fill="green") +
  geom_rect(aes(xmin=state_phases[1],xmax=state_phases[2],ymin=-Inf,ymax=Inf),alpha=0.1,fill="orange") +
  geom_rect(aes(xmin=state_phases[2],xmax=length(day_range),ymin=-Inf,ymax=Inf),alpha=0.2,fill="red") +
  theme_bw() +
  geom_line()

p4 <- ggplot(data = plotme, aes(x=Day, y=Positives)) +
  geom_rect(aes(xmin=0,xmax=state_phases[1],ymin=-Inf,ymax=Inf),alpha=0.1,fill="green") +
  geom_rect(aes(xmin=state_phases[1],xmax=state_phases[2],ymin=-Inf,ymax=Inf),alpha=0.1,fill="orange") +
  geom_rect(aes(xmin=state_phases[2],xmax=state_phases[3],ymin=-Inf,ymax=Inf),alpha=0.2,fill="red") +
  geom_rect(aes(xmin=state_phases[3],xmax=length(day_range),ymin=-Inf,ymax=Inf),alpha=0.2,fill="yellow") +
  theme_bw() +
  geom_line()

for (i in 1:50) {
  state_of_interest <- states$Abbrev[i]
  state_data <- data %>%
    subset(state == state_of_interest) %>%
    subset(!is.na(positive))
  state_positives <- rev(state_data$positive)
  day_range <- 1:length(state_positives)
  plotme <- data.frame(Day = day_range, Positives = state_positives)
  state_phases <- as.numeric(subset(phases, State == state_of_interest)[1,2:4])
  if (sum(is.na(state_phases)) == 0) {
    p4
  } else if (sum(is.na(state_phases)) == 1) {
    p3
  } else if (sum(is.na(state_phases)) == 2) {
    p2
  } else if (sum(is.na(state_phases)) == 3) {
    p1
  }
}