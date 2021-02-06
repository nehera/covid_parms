setwd("~/Desktop/COVID/data/raw_data")
p <- readr::read_csv("state-phases.csv")
p <- p[!(p$State=="DC"),] # rem DC
p[p==0] <- NA # sub NAs
p <- plyr::arrange(p,State) # arrange alph
v <- as.numeric()
for (i in 1:nrow(p)) {
  r <- p[i,]
  s <- sum(is.na(r[2:5]))
  pn <- 5 - s
  v <- c(v, pn)
}
p <- cbind(p, v)
colnames(p)[6] <- "phase_num"
setwd("~/Desktop/COVID/data/tidy_data")
write_csv(p, "~/Desktop/COVID/data/tidy_data/state-phases.csv")