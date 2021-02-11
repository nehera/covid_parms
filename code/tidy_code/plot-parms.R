set.seed(12995)

# define variables
run <- "2021-02-09 10-18-57"
run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")
sran <- c("AK", "FL") # states to plot
nsim <- 10000 # number of simulations
dsim <- 228 # days to simulate
upper <- 0.01 # upper percentile of accepted rmse

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- read_csv("state-phases.csv")
state_pops <- read_csv("state-pops.csv")
state_positives <- read_csv("state-positives.csv")

# setwd("~/Desktop/covid_parms/figures/exp_figures/")
# pdf("state_parms.pdf")

p_n <- c("durE", "durIa", "durIs", "durD", "durR", "p", "q", "x", "y")
p_d <- list()
for (i in 1:length(p_n)){
  p_d[[i]] <- data.frame("state", "value") # running into bug here
}

dE_acc <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(dE_acc) <- c("state", "durE")

for (j in 1:length(sran)) {
  # pull state-specific data
  state_of_interest <- sran[j]
  phases <- subset(state_phases, State==state_of_interest)
  phase_num <- phases$phase_num
  pop <- as.numeric(subset(state_pops, Abbrev==state_of_interest)[3])
  
  setwd(run_dir)
  beta <- read_rds(paste(state_of_interest, "_beta.rds", sep = ""))
  parm <- read_rds(paste(state_of_interest, "_parm.rds", sep = ""))
  pred <- read_rds(paste(state_of_interest, "_seir.rds", sep = ""))
  rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  up_r <- quantile(rmse$rmse, upper)
  sim_acc <- subset(rmse, rmse <= up_r)$sim_id
  
  beta_acc <- beta[beta$sim_id %in% sim_acc,]
  parm_acc <- parm[parm$sim_id %in% sim_acc,]
  
  st_vect <- rep(state_of_interest, length(sim_acc))
  dE_temp <- data.frame("state"=st_vect, "durE"=parm_acc$durE)

  dE_acc <- rbind(dE_acc, dE_temp)
}

# dev.off()


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