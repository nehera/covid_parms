setwd("~/Desktop/COVID")

#load packages
library(rvest)
library(dplyr)
library(lubridate)
library(xml2)
library(stringr)

#read html into r
nyt <- read_html("https://www.nytimes.com/interactive/2020/us/coronavirus-stay-at-home-order.html")

#create list of states
states <- c(state.abb, "DC")

#create empty dataframe
df <- data.frame(State = character(),
                 Place = character(),
                 Order = character(),
                 Start_Date = POSIXct())

for (i in 1:length(states)) {
  
  #pull state
  state <- states[i]
  id <- paste("#item-", state, sep = "")
  
  if (length(html_text(html_nodes(nyt, id))) == 0) {
    new <- data.frame(State = state, Place = NA, Order = NA, Start_Date = NA)
    df <- rbind(df, new)
  } else {
    
    #pull order
    order <- nyt %>%
      html_nodes(paste(id, ".l-order")) %>%
      html_text()
    order <- substr(order, 1,regexpr(",", order)-1)
    
    #pull date
    datetime <- nyt %>% 
      html_nodes(paste(id, ".l-date")) %>%
      html_text() %>%
      str_remove_all(pattern = ", effective ")
    start_date <- datetime %>%
      word(1, 2) %>%
      paste(2020) %>%
      mdy()
    
    #pull place
    if (length(html_text(html_nodes(nyt, paste(id, ".l-place")))) == 0) {
      place <- NA} else {
        place <- nyt %>%
          html_nodes(paste(id, ".l-place")) %>%
          html_text()
        place <- substr(place, 1, regexpr(" About ", place)-1)
      }
    }
    
    #bind state, order, date to df
    new <- data.frame(State = state, Place = place, Order = order, Start_Date = start_date)
    df <- rbind(df, new)
  }

write.csv(df, "stay-home-orders-start.csv", row.names = FALSE)

nyt2 <- read_html("https://www.nytimes.com/interactive/2020/us/states-reopen-map-coronavirus.html")

##need to build program for scraping state re-openings
                           