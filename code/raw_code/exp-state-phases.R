setwd("~/Desktop/COVID/data/tidy_data/")
data <- read.csv("all-states-history.csv") ## updated November 17th, 2020
states <- read.csv("List of States.csv")

library(dplyr)
library(zoo)

AK <- data %>%
  subset(state == "AK") %>%
  subset(!is.na(positive))

days <- 1:length(cases)
case_increase <- rev(AK$positiveIncrease)

plot(days, case_increase, type="l", lty=1, col="red", lwd=1.5, xlim=c(0, 250),
     ylim=c(0,1000), xlab = "Days Since First Case", ylab = "Case Increase", main = "AK")

population <- states[2,3]

incidence <- case_increase / population * 100000 #new cases / 100,000 people
incidence_14day <- rollmean(incidence, 14)

plot(days[14:length(days)], incidence_14day, type="l", lty=1, col="red", lwd=1.5, xlim=c(0, 250),
     ylim=c(0,100), xlab = "Days Since First Case", ylab = "Incidence (14 d mean / 100,000)", main = "AK")


setwd("~/Desktop/COVID/figures/exp_figures/")
pdf("14day_incidence_by_state.pdf")
for (i in 1:50) {
  state_of_interest <- states[i,2]
  population <- states[i,3]
  state_data <- data %>%
    subset(state == state_of_interest) %>%
    subset(!is.na(positive))
  
  case_increase <- rev(state_data$positiveIncrease)
  days <- 1:length(case_increase)
  incidence <- case_increase / population * 100000 #new cases / 100,000 people
  incidence_14day <- rollmean(incidence, 14)
  
  plot(days[14:length(days)], incidence_14day, type="l", lty=1, col="red", lwd=1.5, xlim=c(0, 250),
       ylim=c(0,100), xlab = "Days Since First Case", ylab = "Incidence (14 d mean / 100,000)", main = state_of_interest)
}
dev.off()
