set.seed(12995)

state.mid <- c("MN", "WI", "ND", "MI", "OH", "IN", "IL", "IA", "MO", "SD", "NE", "KS")

# define variables
run <- "2021-02-20 13-56-28"
run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")
nsim <- 10000 # number of simulations
dsim <- 228 # days to simulate
upper <- 0.1 # upper percentile of accepted rmse

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(modi)

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- read_csv("state-phases.csv")
state_pops <- read_csv("state-pops.csv")
state_positives <- read_csv("state-positives.csv")

setwd("~/Desktop/covid_parms/figures/exp_figures/")
pdf("state_obs_v_pred.pdf")

for (j in 1:3) { 
  # pull state-specific data
  state_of_interest <- as.character(state.mid[j])
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
  
  ###
  # state_rmse <- data.frame(matrix(nrow = 0, ncol = 3))
  # colnames(state_rmse) <- c("sim_id", "type", "value")
  # 
  # for (i in rmse$sim_id) { 
  #   rmse_temp <- data.frame(matrix(nrow = 0, ncol = 3))
  #   colnames(rmse_temp) <- c("sim_id", "type", "value")
  #   
  #   m_pos_init <- pred %>% subset(sim_id == i)
  #   m_pos <- cumsum(m_pos_init$Is * pop)
  # 
  #   p_start <- 1
  #   for (k in 1:phase_num) {
  #     if (k==phase_num) {
  #       p_end <- dsim
  #     } else {
  #       p_end <- as.numeric(phases[k+2])
  #     }
  #     
  #     days <- p_start:p_end
  #     len <- length(days)
  # 
  #     rmse_ph <- sqrt(RMSE(m_pos[days], o_pos[days])^2*len*(1-len/dsim)) # transforms traditional rmse into weighted rmse proposed here https://stats.stackexchange.com/questions/230517/weighted-root-mean-square-error
  #     rmse_temp <- rbind(rmse_temp, data.frame(sim_id=i, type=paste("p",k,sep = ""), value=rmse_ph))
  #     
  #     if (k!=phase_num) {
  #       p_start <- p_end+1
  #     }
  #   }
  #   
  #   rmse_ag <- (sum(rmse_temp$value))/phase_num
  #   rmse_temp <- rbind(rmse_temp, data.frame(sim_id=i, type="ag", value=rmse_ag))
  #   state_rmse <- rbind(state_rmse, rmse_temp)
  # }
  ###
  
  # rmse_filt <- subset(state_rmse, type == "ag")
  
  up_r <- quantile(rmse$value, upper)
  sim_acc <- subset(rmse, value <= up_r)$sim_id

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