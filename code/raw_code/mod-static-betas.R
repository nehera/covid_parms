# create output directory
start_time <- Sys.time()
out_dir <- paste("/Users/aidanneher/Desktop/COVID/data/tidy_data/runs", start_time, sep = "/")
out_dir <- gsub(":", "-", out_dir)
dir.create(out_dir, showWarnings = FALSE)

# copy priors.r file used for the model run into out dir for archival purposes
file.copy("~/Desktop/COVID/code/tidy_code/priors.R", paste(out_dir, "/", "priors.R", sep = ""))

# load packages
library(deSolve)
library(MLmetrics)
library(schoolmath)

# define differential equations
sir <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS<--beta*S*(Ia+Is)
    dE<-beta*S*(Ia+Is)-durExp*E
    dIa<-x*durExp*E-durIa*Ia
    dIs<-y*durExp*E-Is*p*mort-Is*q*durR
    dD<-Is*p*mort
    dR<-Is*q*durR+durIa*Ia
    return(list(c(dS,dE,dIa,dIs,dD,dR)))
  })
}

# read in data
setwd("~/Desktop/COVID/data/tidy_data/")
params <- read.csv("params.csv")
states <- read.csv("List of States.csv")
data <- read.csv("all-states-history.csv") ## daily JHU covid case data

# assume proportion with covid that receives a test to be 1, for now
testp = 1

# define the number of simulations per state
sim_num <- 1000

# define the amount of time to simulate
sim_day <- 244

# run the model
for(i in 1:nrow(states)){
  state_of_interest<-as.character(states[i,2])
  Main_data<-subset(data, state==state_of_interest)
  Main_data<-subset(Main_data, positive!=0)
  Main_data$day<-rev(c(1:nrow(Main_data)))
  
  out_df <- data.frame(Sim=as.integer(),
                       Sim_Time=as.integer(),
                       Susceptible=as.numeric(),
                       Exposed=as.numeric(),
                       I_asymptomatic=as.numeric(),
                       I_symptomatic=as.numeric(),
                       Dead=as.numeric(),
                       Recovered=as.numeric(),
                       RMSE=as.numeric())
  
  for(j in 1:sim_num){
    init <- c(S = 1 - 1 / states[i,3], E=0, Ia=0, Is= 1 / states[i,3], D=0, R=0)
    parameters <- c(beta = params[j,2], durExp=params[j,3], durIa=params[j,4], durIs=params[j,5], q=params[j,11], 
                    p=params[j,10], mort=params[j,6], durR=params[j,7], x=params[j,8], y=params[j,9])
    times <- seq(1, sim_day, by = 1)
    options(scipen=999)
    out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters))
    colnames(out)<-c("Sim_Time", "Susceptible", "Exposed", "I_asymptomatic", "I_symptomatic", "Dead","Recovered")
    
    # run rmse analysis
    actual<- rev(tail(Main_data$positive, sim_day))  #positive case data from the first 100 days of the outbreak 
    
    # assign 0 to sim susceptible if float is less than abs(0.000001)
    for (k in 1:nrow(out)){
      if (out$Susceptible[k] <= 0.000001){
        out$Susceptible[k] <- 0
      }
    }
    
    # check if sim susceptible drops into negative
    negative_check <- is.negative(out$Susceptible)
    
    if(sum(negative_check) != 0) {
      RMSE <- NA
    } else {
    
    #I symptomatic model outputs are fractions of the population and specific to a certain time point.
    #To match the actual data, we need to determine the increase in the I_symptomatic compartment and take the cumsum
    predicted_increase <- out$I_symptomatic - out$I_asymptomatic
    predicted<-cumsum(predicted_increase*states[i,3]*testp)
    RMSE <- RMSE(actual, predicted)
    }
    
    # match run time, state, sim num, rmse to length (out) and cbind
    Sim <- rep(j, sim_day)
    RMSE <- rep(RMSE, sim_day)
    out <- cbind(Sim, out, RMSE)
    
    # append out to out_df
    out_df <- rbind(out_df, out)
  }
  
  # write state output to .rds
  setwd(out_dir)
  saveRDS(out_df, file = paste(state_of_interest, ".rds", sep = ""), compress = TRUE)
}

# write params used for model run to outdir as .rds
saveRDS(params, file = "params.rds", compress = TRUE)