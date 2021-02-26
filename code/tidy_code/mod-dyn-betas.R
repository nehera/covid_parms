set.seed(12995)

state.mid <- c("MN", "WI", "MI", "OH", "IN", "IL", "IA", "MO", "ND", "SD", "NE", "KS")

# define variables
nsim <- 10000 # number of simulations
dsim <- 228 # days to simulate

# create out directory
start <- Sys.time()
out_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs", start, sep = "/")
out_dir <- gsub(":", "-", out_dir)
dir.create(out_dir, showWarnings = FALSE)

# write run notes
setwd(out_dir)
notes <- paste("This run includes",nsim,"simulated outbreaks per state, and each simulated outbreak lasts",dsim,"days.")
write(notes,"notes.md")

# source packages

library(readr)
library(dplyr)
library(tidyr)
library(mc2d)
library(deSolve)
library(MLmetrics)

# source data

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- read_csv("state-phases.csv")
state_pops <- read_csv("state-pops.csv")
state_positives <- read_csv("state-positives.csv")

# source diff equations

setwd("~/Desktop/covid_parms/code/tidy_code")
source("func-seir.R")

for (j in 1:1) { # j in sran if sran is a vector

  # pull state-specific data
  state_of_interest <- as.character(state.mid[j])
  phases <- subset(state_phases, State==state_of_interest)
  phase_num <- phases$phase_num
  pop <- as.numeric(subset(state_pops, Abbrev==state_of_interest)[3])
  o_pos <- state_positives %>%
    subset(state==state_of_interest)
  o_pos <- o_pos$positive[1:dsim]

  # create dfs for outputs
  state_seir <- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(state_seir) <- c("sim_id", "day", "S", "E", "Ia", "Is", "D", "R")
  state_beta <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(state_beta) <- c("sim_id", "type", "value")
  state_parm <- data.frame(matrix(nrow = 0, ncol = 9))
  colnames(state_parm) <- c("sim_id", "durE", "durIa", "durD", "durR", "p", "q", "x", "y")
  state_rmse <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(state_rmse) <- c("sim_id", "rmse")
  
  for (i in 1:nsim) {
    # priors
    theta2 <- runif(1,1,4)
    theta3 <- runif(1,1,4)
    theta5 <- runif(1,1,10)
    theta6 <- runif(1,1,7.5)
    theta7 <- runif(1,0,0.2)
    theta8 <- runif(1,0.2,0.8)
    # distributions assumed to be pert
    durE <- abs(1/rpert(1, 1, theta2, 5, 4))
    durIa <- abs(1/rpert(1, 1, theta3,5, 4))
    durD <- abs(1/rpert(1, 1, theta5,14,4))
    durR <- abs(1/rpert(1, 1, theta6,10,4))
    p <- abs(rpert(1, 0, theta7,.25,4))
    q <- 1-p
    x <- abs(rpert(1, 0.2, theta8, 0.9, 4))
    y <- 1-x
    
    state_parm <- rbind(state_parm, data.frame(sim_id=i, durE=durE, durIa=durIa, durD=durD, durR=durR, p=p, q=q, x=x, y=y))
    
    # format df for outputs
    out <- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(out) <- c("sim_id", "day", "S", "E", "Ia", "Is", "D", "R")
    
    p_start <- 1
    init <- c(S = 1 - 1 / pop, E=0, Ia=0, Is= 1 / pop, D=0, R=0)
    
    for (k in 1:phase_num) {
      # define beta
      theta1 <- runif(1,0.05,0.55)
      beta <- abs(rpert(1, 0.05, theta1, 0.55, 4))
      if (k==phase_num) {
        p_end <- dsim
      } else {
        p_end <- as.numeric(phases[k+2])
      }
      times <- seq(1, p_end-p_start+1, by = 1)
      parameters <- c(beta = abs(beta), durE=abs(durE), durIa=abs(durIa), q=abs(q),
                      p=abs(p), durD=abs(durD), durR=abs(durR), x=abs(x), y=abs(y))
      options(scipen=999)
      d_out <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
      d_out$time <- c(p_start:p_end)
      sim_id <- rep(i, nrow(d_out))
      p_out <- cbind(sim_id, d_out)
      out <- rbind(out, p_out)
      state_beta <- rbind(state_beta, data.frame(sim_id=i, type=k, value=beta))
      if (k!=phase_num) {
        p_start <- p_end+1
        init <- c(S = p_out$S[length(times)], E = p_out$E[length(times)], Ia = p_out$Ia[length(times)], Is = p_out$Is[length(times)], D = p_out$D[length(times)], R = p_out$R[length(times)])
      }
    }
    state_seir <- rbind(state_seir, out)
    m_pos <- cumsum(out$Is * pop)
    rmse <- RMSE(m_pos, o_pos)
    state_rmse <- rbind(state_rmse,data.frame(sim_id=i,rmse=rmse))
  }
  setwd(out_dir)
  saveRDS(state_seir, paste(state_of_interest, "_seir.rds", sep = ""))
  saveRDS(state_beta, paste(state_of_interest, "_beta.rds", sep = ""))
  saveRDS(state_parm, paste(state_of_interest, "_parm.rds", sep = ""))
  saveRDS(state_rmse, paste(state_of_interest, "_rmse.rds", sep = ""))
}

end <- Sys.time()
mod_dur <- end - start
cat(paste("This model started running at",start,"and finished running at",end,"for a timediff =",mod_dur), file = "notes.md", append = TRUE, sep = "\n\n")
cat(paste("This model included the following warnings:",warnings()), file = "notes.md", append = TRUE, sep = "\n\n")