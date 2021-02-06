set.seed(12995)

# define variables
run <- "2021-02-05 09-58-46"
run_dir <- paste("~/Desktop/COVID/data/tidy_data/runs/", run, sep = "")
sran <- 1:3 # states to simulate, set to 1:50 to sim all 50 states
nsim <- 10000 # number of simulations
dsim <- 228 # days to simulate
upper <- 0.01 # upper percentile of accepted rmse

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("~/Desktop/COVID/data/tidy_data")
state_phases <- read_csv("state-phases.csv")
state_pops <- read_csv("state-pops.csv")
state_positives <- read_csv("state-positives.csv")

setwd("~/Desktop/COVID/figures/exp_figures/")
pdf("state_parms.pdf")

for (j in sran) {
  # pull state-specific data
  state_of_interest <- as.character(state_pops[j,2])
  phases <- subset(state_phases, State==state_of_interest)
  phase_num <- phases$phase_num
  pop <- as.numeric(subset(state_pops, Abbrev==state_of_interest)[3])
  
  setwd(run_dir)
  beta <- read_rds(paste(state_of_interest, "_beta.rds", sep = ""))
  pred <- read_rds(paste(state_of_interest, "_seir.rds", sep = ""))
  rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  up_r <- quantile(rmse$rmse, upper)
  sim_acc <- subset(rmse, rmse <= up_r)$sim_id

  for (i in sim_acc) {
    b <- subset(beta, sim_id == i)
  }

}

dev.off()


#   # pull each state's accepted betas by phase
#   acc_betas <- data.frame()
#   
#   for (k in sim_acc) {
#     state_betas <- readRDS(paste(state_of_interest, "_beta.rds", sep = ""))
#     sim_betas <- subset(state_betas, sim_id == k)
#     rel_betas <- sim_betas[2:(phase_num+1)]
#     acc_betas <- rbind(acc_betas, rel_betas)
#   }
#   
#   names(acc_betas)[1:phase_num] <- c(1:phase_num)
#   
#   acc_betas <- gather(acc_betas, key = "Phase", value = "Beta")
#   
#   b <- ggplot(acc_betas, aes(Phase, Beta, fill=Phase)) + geom_boxplot() +
#     scale_fill_brewer(palette="BuPu") + theme(legend.position = "none") +
#     ylab("Beta (transmission rate)")
#   
#   print(b)
# }
# 
# 