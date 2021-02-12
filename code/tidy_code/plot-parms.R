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
  
  beta_acc_all <- rbind(beta_acc_all, beta_acc)
  parm_acc_all <- rbind(parm_acc_all, parm_acc)
}

beta_acc_all$type <- as.factor(beta_acc_all$type)

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_beta.pdf")

for (k in 1:length(sran)) {
  
  state_of_interest <- sran[k]
  
  g <- ggplot(beta_acc_all, aes(x = type, y = value, fill = type)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
    theme_classic() +
    stat_summary(fun = mean, geom="point",colour="darkred", size=2) +
    theme(legend.position="none") +
    scale_fill_brewer(palette="Set1") +
    labs(title = state_of_interest) +
    xlab("Phase") + ylab("Beta")
  
  print(g)
  
}

dev.off()

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_parm.pdf")

for (i in 2:length(colnames(parm_acc_all))) {
  
  p_of_int <- colnames(parm_acc_all)[i]

  p <- ggplot(parm_acc_all, aes_string(x= "state", y = p_of_int, fill = "state")) 
  p <- p +  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 
  p <- p + theme_classic()
  p <- p + stat_summary(fun = mean, geom="point",colour="darkred", size=2) + theme(legend.position="none") +
    scale_fill_brewer(palette="Set1")
  
  print(p)

}

dev.off()