set.seed(12995)

state.mid <- c("MN", "WI", "MI", "OH", "IN", "IL", "IA", "MO", "ND", "SD", "NE", "KS")

# define variables
run <- "2021-02-15 20-35-56"
run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

for (j in 1:1) { 
  # pull state-specific data
  state_of_interest <- as.character(state.mid[j])
  setwd(run_dir)
  # rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  g <- ggplot(state_rmse, aes(x=sim_id,y=value)) + geom_point(aes(colour = factor(type)))
  print(g)
}
