set.seed(12995)

state.mid <- c("MN", "WI", "MI", "OH", "IN", "IL", "IA", "MO", "ND", "SD", "NE", "KS")

run <- "2021-02-20 12-44-41"
run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_rmse.pdf")

for (j in 1:1) { 
  # pull state-specific data
  state_of_interest <- as.character(state.mid[j])
  setwd(run_dir)
  rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  g <- ggplot(rmse, aes(x=sim_id,y=value))
  print(g)
}

dev.off()