set.seed(12995)

# define variables
run <- "2021-02-11 11-42-30"
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

beta_acc_all <- as.data.frame(matrix(nrow = 0, ncol = 3))
colnames(beta_acc_all) <- c("state", "type", "value")
parm_acc_all <- as.data.frame(matrix(nrow = 0, ncol = 10))
colnames(parm_acc_all) <- c("state", "durE", "durIa", "durIs",
                            "durD", "durR", "p", "q", "x", "y")

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
  
  colnames(beta_acc)[1] <- "state"
  colnames(parm_acc)[1] <- "state"
  
  beta_acc$state <- state_of_interest
  parm_acc$state <- state_of_interest
  
  thresh_b1 <- as.numeric(quantile(beta_acc[beta_acc$type == 1,3])[4]+IQR(beta_acc[beta_acc$type == 1,3]))
  thresh_b2 <- as.numeric(quantile(beta_acc[beta_acc$type == 2,3])[4]+IQR(beta_acc[beta_acc$type == 2,3]))
  beta_acc[,3] <- ifelse(beta_acc[beta_acc$type == 1,3] > thresh_b1, NA, beta_acc[,3])
  beta_acc[,3] <- ifelse(beta_acc[beta_acc$type == 2,3] > thresh_b2, NA, beta_acc[,3])
  
  for (k in 2:length(colnames(parm_acc))) {
    thresh_p <- as.numeric(quantile(parm_acc[,k])[4] + IQR(parm_acc[,k]))
    parm_acc[,k] <- ifelse(parm_acc[,k] > thresh_p, NA, parm_acc[,k])
  }
  
  beta_acc_all <- rbind(beta_acc_all, beta_acc)
  parm_acc_all <- rbind(parm_acc_all, parm_acc)
}

beta_acc_all$type <- as.factor(beta_acc_all$type)

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_beta.pdf")

for (w in 1:length(sran)) {
  
  state_of_interest <- sran[w]
  
  # stat_box_data <- function(y) {
  #   return(
  #     data.frame(
  #       y = max(na.omit(parm_acc_all[i]))*0.95,
  #       label = paste('count =', length(na.omit(y)), '\n',
  #                     'mean =', round(mean(na.omit(y)), digits = 3), '\n')
  #     )
  #   )
  # }
  
  g_b <- ggplot(na.omit(beta_acc_all), aes(x=type, y=value, fill = type)) +
    geom_boxplot() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    xlab(state_of_interest) + ylab("Beta") +
    coord_flip() + 
    scale_fill_brewer(palette="BuPu") +
    stat_summary(
      fun.data = stat_box_data,
      geom = "text",
      position = position_nudge(x=0.2))
  print(g_b)
}

dev.off()

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_parm.pdf")

for (i in 2:length(colnames(parm_acc_all))) {
  
  p_of_int <- colnames(parm_acc_all)[i]
  
  stat_box_data <- function(y) {
    return(
      data.frame(
        y = max(na.omit(parm_acc_all[i]))*0.95,
        label = paste('count =', length(na.omit(y)), '\n',
                      'mean =', round(mean(na.omit(y)), digits = 3), '\n')
      )
    )
  }
  
  g_p <- ggplot(na.omit(parm_acc_all), aes_string('state', p_of_int, fill = 'state')) +
    geom_boxplot() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    xlab("State") + ylab(p_of_int) +
    coord_flip() + 
    scale_fill_brewer(palette="BuPu") +
    stat_summary(
      fun.data = stat_box_data,
      geom = "text",
      position = position_nudge(x=0.2))
  print(g_p)
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