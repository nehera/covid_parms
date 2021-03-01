set.seed(12995)

pacman::p_load(parallel, readr, data.table, dplyr, tidyr, mc2d, deSolve)

nsim <- 10000 # number of iterations to accept

state.mid <- as.list(c("MN", "WI", "ND", "MI", "OH", "IN", "IL", "IA", "MO", "SD", "NE", "KS"))

start <- Sys.time()
out_dir <- paste("~/Desktop/covid_parms/data/tidy_data/runs", start, sep = "/")
out_dir <- gsub(":", "-", out_dir)
dir.create(out_dir, showWarnings = FALSE)

setwd(out_dir)
notes <- paste("This run includes",nsim,"simulated outbreaks per state.")
write(notes,"notes.md")

setwd("~/Desktop/covid_parms/code/tidy_code")
source("functions.R")

setwd("~/Desktop/covid_parms/data/tidy_data")
state_phases <- fread("state-phases.csv")
state_pops <- fread("state-pops.csv")
state_positives <- fread("state-positives.csv")
state_dsim <- fread("state-time-to-dec11.csv")

ntasks <- length(state.mid)
ncores <- detectCores()

RNGkind("L'Ecuyer-CMRG") # makes random number generation reproducible when parallel computing

if (ntasks < ncores) {
  mclapply(state.mid, my.sim, mc.cores = ntasks)
} else {
  mclapply(state.mid, my.sim, mc.cores = ncores)
}

setwd(out_dir)
end <- Sys.time()
mod_dur <- end - start
cat(paste("This model started running at",start,"and finished running at",end,"for a timediff =",mod_dur), file = "notes.md", append = TRUE, sep = "\n\n")
cat(paste("This model included the following warnings:",warnings()), file = "notes.md", append = TRUE, sep = "\n\n")