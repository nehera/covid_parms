library(dplyr)
library(tidyverse)
library(readr)

setwd("~/Desktop/COVID/data/raw_data")
data <- read.csv("all-states-history.csv")

filtered <- data %>%
  select(date, state, positive) %>%
  subset(positive > 0) %>%
  map_df(rev)

write_csv(filtered, path = "~/Desktop/COVID/data/tidy_data/all-state-positive.csv")
