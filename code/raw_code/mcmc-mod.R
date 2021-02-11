set.seed(12995)

# create output directory
start_time <- Sys.time()
out_dir <- paste("/Users/aidanneher/Desktop/COVID/data/tidy_data/runs", start_time, sep = "/")
out_dir <- gsub(":", "-", out_dir)
dir.create(out_dir, showWarnings = FALSE)

# source packages

library(mc2d)
library(deSolve)
library(MLmetrics)
library(schoolmath)
library(dplyr)

# source data

setwd("~/Desktop/COVID/data/tidy_data")
data <- read.csv("all-states-history.csv")
pop_file <- read.csv("List of States.csv")
phases <- read.csv("state_phase_endpoints.csv")

# source custom functions

setwd("~/Desktop/COVID/code/tidy_code")
source("func_seir.R")
source("func_state_positive.R")

# define variables ### nsim needs to be changed to something arbitrarily large eventually

nsim=10
sim_day <- 228

for(j in 1:nrow(pop_file)){
  
  # pull state-specific data
  state_of_interest <- as.character(pop_file[j,2])
  state_positive_func(state_of_interest) # state_positive_obs is output with colnames Date, Positive
  
  phases_of_interest <- subset(phases, State==state_of_interest)
  
  pop_sub <- which(pop_file==state_of_interest, arr.ind=TRUE)
  pop_of_int <- pop_file[pop_sub[1],3]
  
  # create dataframes for outputs
  
  state_output <- data.frame(matrix(nrow = 0, ncol = 9))
  colnames(state_output) <- c("sim_num", "time", "S", "E", "Ia", "Is", "D", "R", "RMSE")
  
  for(i in 1:nsim){ ### need to create multiple simulation feature
    
    measure = 0
    
    while(measure<1) {
      
      # define priors
      
      theta1<-runif(1,0.2,0.3)
      theta2<-runif(1,0,0.5)
      theta3<-runif(1,0,.75)
      theta4<-runif(1,1,4)
      theta5<-runif(1,1,8)
      theta6<-runif(1,1,7.5)
      theta7<-runif(1,0,0.2)
      theta8<-runif(1,0.2,0.8) ### We need to evaluate whether or not x and y dist are biologically plausible
      
      # distributions assumed to be pert
      beta <- abs(rpert(1, .10, theta1,.3,4))
      durExp <- abs(1/rpert(1, 0,theta2, 3.1, 4))
      durIa <- abs(1/rpert(1,0,theta3,3, 4))
      durIs <- abs(1/rpert(1, 0,theta4,5,4))
      mort <- abs(1/rpert(1, 0, theta5,10,4))
      durR <- abs(1/rpert(1, 0, theta6,10,4))
      p <- abs(rpert(1, 0, theta7,.25,4))
      q <- 1-p
      x <- abs(rpert(1, 0.2, theta8, 0.9, 4)) 
      y <- 1-x
      
      # phase 2 priors
      thetab<-runif(1,.05,0.1)
      beta2<-abs(rpert(1, .029, thetab,.2,4))
      
      # phase 3 priors
      thetac<-runif(1,.09,0.12)
      beta3<-abs(rpert(1, 0.05, thetac,.2,4))
      
      # phase 4 priors
      thetad<-runif(1,.09,.12)
      beta4<-abs(rpert(1, 0.05, thetad, 0.2, 4))
      
      # format dfs for phased outputs
      out <- data.frame(matrix(nrow = 0, ncol = 7))
      colnames(out) <- c("time", "S", "E", "Ia", "Is", "D", "R")
      phase1 <- out
      phase2 <- out
      phase3 <- out
      phase4 <- out
      
      # phase 1
      End1<-phases_of_interest$End1
      init <- c(S = 1 - 1 / pop_of_int, E=0, Ia=0, Is= 1 / pop_of_int, D=0, R=0)
      parameters <- c(beta = abs(beta), durExp=abs(durExp), durIa=abs(durIa), durIs=abs(durIs), q=abs(q), 
                      p=abs(p), mort=abs(mort), durR=abs(durR), x=abs(x), y=abs(y))
      
      if (is.na(End1)) {
        times <- seq(1, sim_day, by = 1)
      } else {
        times <- seq(1, End1, by = 1)
      }
      
      options(scipen=999)
      phase1 <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
      
      # phase test
      phase_num <- phases_of_interest$phase_num
      
      if (phase_num == 1) {
        out <- phase1
      } else {
        
        # phase 2
        End2<-phases_of_interest$End2
        init <- c(S = phase1[End1,2], E = phase1[End1,3], Ia = phase1[End1,4], Is = phase1[End1,5], D = phase1[End1,6], R = phase1[End1,7])
        parameters <- c(beta = abs(beta2), durExp=abs(durExp), durIa=abs(durIa), durIs=abs(durIs), q=abs(q), 
                        p=abs(p), mort=abs(mort), durR=abs(durR), x=abs(x), y=abs(y))
        
        if (is.na(End2)) {
          times <- seq(1, sim_day - End1, by = 1)
        } else {
          times <- seq(1, End2, by = 1)
        }
        
        options(scipen=999)
        phase2 <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
        
        if (phase_num == 2) {
          out <- rbind(phase1, phase2)
        } else {
          
          # phase 3
          End3<-phases_of_interest$End3
          init <- c(S = phase2[End2,2], E = phase2[End2,3], Ia = phase2[End2,4], Is = phase2[End2,5], D = phase2[End2,6], R = phase2[End2,7])
          parameters <- c(beta = abs(beta3), durExp=abs(durExp), durIa=abs(durIa), durIs=abs(durIs), q=abs(q), 
                          p=abs(p), mort=abs(mort), durR=abs(durR), x=abs(x), y=abs(y))
          
          if (is.na(End3)) {
            times <- seq(1, sim_day - End2 - End1, by = 1)
          } else {
            times <- seq(1, End3, by = 1)
          }
          
          options(scipen=999)
          phase3 <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
          
          if (phase_num == 3) {
            out <- rbind(phase1, phase2, phase3)
          } else {
            
            init <- c(S = phase3[End3,2], E = phase3[End3,3], Ia = phase3[End3,4], Is = phase3[End3,5], D = phase3[End3,6], R = phase3[End3,7])
            parameters <- c(beta = abs(beta4), durExp=abs(durExp), durIa=abs(durIa), durIs=abs(durIs), q=abs(q), 
                            p=abs(p), mort=abs(mort), durR=abs(durR), x=abs(x), y=abs(y))
            times <- seq(1, sim_day - End3 - End2 - End1, by = 1)
            options(scipen=999)
            phase4 <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
            
            out <- rbind(phase1, phase2, phase3, phase4)
          }
        }
      }
      
      out$time <- c(1:nrow(out))
      
      modeled <- cumsum(out$Is * pop_of_int)
      rmse_new <- RMSE(state_positive_obs[1:sim_day,2], modeled)
      
      if (i==1) {
        measure = 1
      } else {
        rmse_last <- subset(state_output, sim_num == i-1)[1,9]
        measure <- rmse_last / rmse_new # measure >= 1 indicates that the new rmse is less than the previous
      }
    }
    sim_vect <- rep.int(i, sim_day)
    rmse_vect <- rep.int(rmse_new, sim_day)
    keep <- cbind(sim_vect, out, rmse_vect)
    colnames(keep) <- c("sim_num", "time", "S", "E", "Ia", "Is", "D", "R", "RMSE")
    state_output <- rbind(state_output, keep)
  }
  setwd(out_dir)
  saveRDS(state_output, paste(state_of_interest, ".rds", sep = ""))
}

end_time <- Sys.time()

model_duration <- end_time - start_time
