# source packages

library(dplyr)
library(ggplot2)
library(zoo)

# source custom function

setwd("~/Desktop/COVID/code/tidy_code")
source("func_state_positive.R")

# source observed data

setwd("~/Desktop/COVID/data/tidy_data/")
data <- read.csv("all-states-history.csv")
pop_file <- read.csv("List of States.csv")
phases <- read.csv("state_phase_endpoints.csv")

# define model output directory of interest

out_dir <- "/Users/aidanneher/Desktop/COVID/data/tidy_data/runs/2020-12-17 12-05-16"
setwd(out_dir)

# plot state of interest's observed vs. modeled positive case data

# pull state specific data

state_of_interest <- "AZ"
pop_sub <- which(pop_file==state_of_interest, arr.ind=TRUE)
pop_of_int <- pop_file[pop_sub[1],3]

state_positive_func(state_of_interest) # state_positive_obs is output with colnames Date, Positive

state_positive_mod <- readRDS(paste(state_of_interest, ".rds", sep = ""))

sim_num = 10
sim_day <- 228

day_range <- 1:sim_day

rmse_upper_percentile <- 0.1 # set the acceptance threshold for filtering out the bottom 10% of RMSEs

ymax <- 0.03 # set the ymax of the plots = to 100% of the population of interest

actual_vector <- state_positive_obs$Positive[1:sim_day] / pop_of_int # create a vector of observed cases since the state's first day of outbreak

#create vectors of accepted and rejected simulations' positive cases since outbreak
rmse_upper_bound <- quantile(state_positive_mod$RMSE, rmse_upper_percentile)
state_accepted <- subset(state_positive_mod, RMSE <= rmse_upper_bound)
state_rejected <- subset(state_positive_mod, RMSE > rmse_upper_bound)
sim_accepted <- subset(state_accepted, time == 1)
sim_accepted <- sim_accepted$sim_num
sim_rejected <- subset(state_rejected, time == 1)
sim_rejected <- sim_rejected$sim_num

for (i in sim_rejected) {
  predicted <- subset(state_rejected, sim_num == i)
  predicted <- cumsum(predicted$Is)
  plot(day_range, predicted, type="l", lty=1, col="gray", lwd=1.5, xlim=c(0, max(day_range)),
       ylim=c(0,ymax), axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
}

for (j in sim_accepted) {
  predicted <- subset(state_accepted, sim_num == j)
  predicted <- cumsum(predicted$Is)
  plot(day_range, predicted, type="l", lty=1, col="blue", lwd=1.5, xlim=c(0, max(day_range)),
       ylim=c(0,ymax), axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
}

plot(day_range, actual_vector, type="l", lty=1, col="red", lwd=1.5, xlim=c(0, max(day_range)),
     ylim=c(0,ymax), xlab = "Days Since First COVID-19 Case", ylab = "Proportion of Population Positive for COVID-19", main = state_of_interest)

legend("topleft", 
       legend = c("Actual", "Predicted - Rejected", "Predicted - Accepted"), 
       col = c("red", 
               "gray",
               "blue"), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

################################################################################################

  setwd("~/Desktop/COVID/figures/exp_figures/")
  pdf("state_actual_v_predicted.pdf")
  for (i in 1:50) {
    state_actual_v_predicted("2020-11-17 15-09-47", 1:244, 0.1, states$Abbrev[i])
  }
  dev.off()
  
################################################################################################

state_data <- data %>%
  subset(state == state_of_interest) %>%
  subset(!is.na(positive))

state_positives <- rev(state_data$positive)

day_range <- 1:length(state_positives)

state_phases <- as.numeric(subset(phases, State == state_of_interest)[1,2:4])

plotme <- data.frame(Day = day_range, Positives = state_positives)

p1 <- ggplot(data = plotme, aes(x=Date, y=Positives)) +
  geom_rect(aes(xmin=0,xmax=length(sim_day),ymin=-Inf,ymax=Inf),alpha=0.1,fill="green") +
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