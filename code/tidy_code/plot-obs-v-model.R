set.seed(12995)

# define variables
run <- "2021-02-11 19-42-35"
run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")
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

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_obs_v_pred.pdf")

for (j in 1:3) { 
  # pull state-specific data
  # state_of_interest <- as.character(state_pops[j,2])
  state_of_interest <- as.character(state.abb[j])
  phases <- subset(state_phases, State==state_of_interest)
  phase_num <- phases$phase_num
  pop <- as.numeric(subset(state_pops, Abbrev==state_of_interest)[3])
  o_pos <- state_positives %>%
    subset(state==state_of_interest)
  o_pos <- o_pos$positive[1:dsim]
  o_df <- data.frame(Day = 1:dsim, Positives = o_pos / pop)
  
  setwd(run_dir)
  pred <- read_rds(paste(state_of_interest, "_seir.rds", sep = ""))
  rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  up_r <- quantile(rmse$rmse, upper)
  sim_acc <- subset(rmse, rmse <= up_r)$sim_id

  p <- ggplot() + 
    theme_bw()
  
  if (phase_num==1){
    d <- data.frame(x1=0,x2=dsim,t='a')
  } else {
    d <- data.frame(x1=c(0,as.numeric(phases[3:(phase_num+1)])),
                    x2=c(as.numeric(phases[3:(phase_num+1)]),dsim),
                    t=letters[1:phase_num])
  }
  
  p <- p + geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf, fill=t), color="gray100", alpha=0.1, show.legend = FALSE)
  
  # plot accepted sims
  for (i in sim_acc) {
    p_pos <- subset(pred, sim_id == i)
    p_pos <- cumsum(p_pos$Is)
    p_pos_df <- data.frame(Day = 1:dsim, Positives = p_pos)
    p <- p + geom_line(data = p_pos_df, aes(x = Day, y = Positives), color = "gray78")
  }
  
  # plot actual data
  p <- p + geom_line(data = o_df, aes(x = Day, y = Positives), color = "grey28") +
    ggtitle(state_of_interest) + xlab("Days Post 1st Case In-State") +
    ylab("Positive Cases (Proportion of State Population)") + xlim(0,dsim) + ylim(0,0.05)
  
  print(p)
}

dev.off()