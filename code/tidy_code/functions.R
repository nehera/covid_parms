# Compartmental Model
seir <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*(Ia+Is)
    dE <- beta*S*(Ia+Is)-durE*E
    dIa <- durE*E-durIa*Ia
    dIs <- durIa*Ia*x-durD*Is*p-durR*Is*q
    dD <- durD*Is*p
    dR <- durIa*Ia*y+durR*Is*q
    return(list(c(dS,dE,dIa,dIs,dD,dR)))
  })
}

# Weighted RMSE
weighted.rmse <- function(observed, modeled, weights) { 
  errors <- weights*(modeled-observed)^2
  w_rmse <- sqrt(sum(errors))
  return(w_rmse)
}

my.sim <- function(state_of_interest) {
  # pull state-specific data
  dsim <- as.numeric(state_dsim[which(state_dsim$state==state_of_interest),3]) # the number of days to sim/state was calculated as the diff in time between Dec 11, 2020 and the state's epidemic start point
  phases <- state_phases[which(state_phases$state==state_of_interest),2:9]
  phase_num <- state_phases$phase_num[which(state_phases$state==state_of_interest)]
  pop <- as.numeric(state_pops[which(state_pops$Abbrev==state_of_interest),3])
  o_pos <- state_positives$positive[which(state_positives$state==state_of_interest)][1:dsim]
  
  # create data.tables for outputs
  state_seir <- as.data.table(matrix(NA, nrow = nsim*dsim, ncol = 8))
  colnames(state_seir) <- c("sim_id", "time", "S", "E", "Ia", "Is", "D", "R")
  state_seir <- state_seir[, lapply(.SD, as.numeric)]
  
  state_beta <- as.data.table(matrix(NA, nrow = phase_num*nsim, ncol = 3))
  colnames(state_beta) <- c("sim_id", "phase", "value")
  state_beta <- state_beta[, lapply(.SD, as.numeric)]
  
  state_parm <- as.data.table(matrix(NA, nrow = nsim, ncol = 9))
  colnames(state_parm) <- c("sim_id", "durE", "durIa", "durD", "durR", "p", "q", "x", "y")
  state_parm <- state_parm[, lapply(.SD, as.numeric)]
  
  state_rmse <- as.data.table(matrix(NA, nrow = nsim, ncol = 2))
  colnames(state_rmse) <- c("sim_id", "value")
  state_rmse <- state_rmse[, lapply(.SD, as.numeric)]
  
  a=0 # accepted iteration id
  i=0 # simulation iteration id
  
  while (a < nsim) {
    i = i+1
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
    
    # format data.tables for outputs
    out <- as.data.table(matrix(NA, nrow = dsim, ncol = 8))
    colnames(out) <- c("sim_id", "time", "S", "E", "Ia", "Is", "D", "R")
    out <- out[, lapply(.SD, as.numeric)]
    
    beta_temp <- as.data.table(matrix(NA, nrow = phase_num, ncol = 3))
    colnames(beta_temp) <- c("sim_id", "phase", "value")
    beta_temp <- beta_temp[, lapply(.SD, as.numeric)]
    
    prop_temp <- as.data.table(matrix(NA, nrow = phase_num, ncol = 2))
    colnames(prop_temp) <- c("phase", "prop")
    prop_temp <- prop_temp[, lapply(.SD, as.numeric)]
    
    p_start <- 1
    init <- c(S = 1 - 1 / pop, E=0, Ia=0, Is= 1 / pop, D=0, R=0)
    
    for (k in 1:phase_num) {
      # define beta
      theta1 <- runif(1,0.05,0.55)
      beta <- abs(rpert(1, 0.05, theta1, 0.55, 4))
      if (k==phase_num) {
        p_end <- dsim
      } else {
        p_end <- as.numeric(phases[[1,k+1]])
      }
      times <- seq(1, p_end-p_start+1, by = 1)
      parameters <- c(beta = abs(beta), durE=abs(durE), durIa=abs(durIa), q=abs(q),
                      p=abs(p), durD=abs(durD), durR=abs(durR), x=abs(x), y=abs(y))
      options(scipen=999)
      
      out[p_start:p_end,] <- as.data.table(cbind(sim_id=rep(i, p_end-p_start+1), 
                                                 ode(y = init, times = times, func = seir, parms = parameters)))
      
      beta_temp[k,] <- data.frame(sim_id=i, phase=k, value=beta)
      prop_temp[k,] <- data.frame(phase=k, prop=length(p_start:p_end)/dsim)
      
      if (k!=phase_num) {
        p_start <- p_end+1
        init <- c(S = out$S[p_end], E = out$E[p_end], Ia = out$Ia[p_end], Is = out$Is[p_end], D = out$D[p_end], R = out$R[p_end])
      }
    }
    
    m_pos <- cumsum(durIa*out$Ia*pop*x)
    # m_pos <- cumsum(out$Is * pop)
    w_t <- prop_temp
    setorder(prop_temp, prop)
    setorder(w_t, prop)
    colnames(w_t) <- c("phase", "weight")
    w_t$weight <- rev(w_t$weight)/(prop_temp$prop*dsim) # df needs to be ordered by prop first
    setorder(prop_temp, phase)
    setorder(w_t, phase)
    w <- rep(w_t$weight, prop_temp$prop*dsim)
    rmse_trial <- weighted.rmse(o_pos, m_pos, w)
    
    if (i == 1) {
      rmse_prev <- rmse_trial
    }
    if (rmse_trial <= rmse_prev) {
      a=a+1
      out$time <- 1:dsim
      state_seir[((a-1)*dsim+1):(a*dsim)] <- out
      state_beta[((a-1)*phase_num+1):(a*phase_num)] <- beta_temp
      state_parm[a] <- data.frame(sim_id=i, durE=durE, durIa=durIa, durD=durD, durR=durR, p=p, q=q, x=x, y=y)
      state_rmse[a] <- data.frame(sim_id=i, value=rmse_trial)
      rmse_prev <- rmse_trial
    } else {
      rmse_prev <- rmse_prev*1.01
    }
  }
  setwd(out_dir)
  saveRDS(as.data.frame(state_seir), paste(state_of_interest, "_seir.rds", sep = ""))
  saveRDS(as.data.frame(state_beta), paste(state_of_interest, "_beta.rds", sep = ""))
  saveRDS(as.data.frame(state_parm), paste(state_of_interest, "_parm.rds", sep = ""))
  saveRDS(as.data.frame(state_rmse), paste(state_of_interest, "_rmse.rds", sep = ""))
}

plot.ovm <- function(state_of_interest, run, upper) {
  set.seed(12995)
  pacman::p_load(readr, dplyr, ggplot2, tidyr, MLmetrics, modi)
  run_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs/", run, sep = "")
  
  # pull state-specific data
  dsim <- as.numeric(state_dsim[which(state_dsim$state==state_of_interest),3]) # the number of days to sim/state was calculated as the diff in time between Dec 11, 2020 and the state's epidemic start point
  phases <- state_phases[which(state_phases$state==state_of_interest),2:9]
  phase_num <- state_phases$phase_num[which(state_phases$state==state_of_interest)]
  pop <- as.numeric(state_pops[which(state_pops$Abbrev==state_of_interest),3])
  o_pos <- state_positives$positive[which(state_positives$state==state_of_interest)][1:dsim]
  
  o_df <- data.frame(Day = 1:dsim, Positives = o_pos / pop)
  
  setwd(run_dir)
  pred <- read_rds(paste(state_of_interest, "_seir.rds", sep = ""))
  rmse <- read_rds(paste(state_of_interest, "_rmse.rds", sep = ""))
  
  up_r <- quantile(rmse$value, upper)
  sim_acc <- subset(rmse, value <= up_r)$sim_id
  
  p <- ggplot() + 
    theme_bw() +
    xlim(0,dsim) +
    ylim(0,0.05)
 
  if (phase_num==1){
    d <- data.frame(x1=0,x2=dsim,t='a')
  } else {
    d <- data.frame(x1=c(0,as.numeric(phases[1,2:phase_num])),
                    x2=c(as.numeric(phases[1,2:phase_num]),dsim),
                    t=letters[1:(phase_num)])
  }
  
  p <- p + geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf, fill=t), color="white", alpha=0.25, show.legend = FALSE)
  
  # plot accepted sims
  for (i in sim_acc) {
    m_pos <- subset(pred, sim_id == i)
    m_pos <- cumsum(durIa*pred$Ia*pop*x) # cumsum(m_pos$Is)
    m_pos_df <- data.frame(Day = 1:dsim, Positives = m_pos)
    p <- p + geom_line(data = m_pos_df, aes(x = Day, y = Positives), color = "gray78")
  }
  
  # plot actual data
  p <- p + geom_line(data = o_df, aes(x = Day, y = Positives), color = "grey28") +
    ggtitle(state_of_interest) + xlab("Days Post 1st Case In-State") +
    ylab("Positive Cases (Proportion of State Population)")
  
  print(p)
}