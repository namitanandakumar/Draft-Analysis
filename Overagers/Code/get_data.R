library(tidyverse)
library(rvest)
library(lubridate)
library(doParallel)
source('scraping_functions.R')

### GRAB DRAFT DATA
drafts <- c()
for (year in 2005:2017){
  temp <- nhl_draft_stats(year) %>%
    left_join(hockeyref_draft_stats(year),
              by=c('draftyear','overall'))
  drafts <- rbind(drafts,temp)
  print(year)
  rm(temp,year)
}

# manually correct blank ages
drafts <- drafts %>%
  mutate(age = replace(age,player=='Trent Vogelhuber',19),
         age = replace(age,player=='Dillon Simpson',18))

### GRAB CAREER PLAYER GAMES
# start parallelizing because this takes a while
no_cores <- detectCores() - 1
# seems like shit gets throttled on 4+ cores
cl <- makeCluster(no_cores, outfile='')
registerDoParallel(cl)

# verbose = TRUE will output the name of the player to see how far along we are
# I usually watch Frasier while this is running
# I think my favorite character is Niles but I also love Roz, you know?
# Daphne's a bit much for me if I'm honest
# also it beeps if there's an error connecting to Hockey Ref
# ............which has happened before
all <- foreach(iter = 1:nrow(drafts),
               .combine = rbind,
               .packages=c('rvest','dplyr','stringr','lubridate')) %dopar%
  hockeyref_player_games(drafts$playerurl[iter],verbose=TRUE) %>%
  left_join(drafts,by='playerurl') %>%
  unique()
# I like to do a different lil beep at the end
beepr::beep(2)
# beep
# anyways run this in chunks or on fewer cores if you're having trouble

# check for discrepancies between draft page games played total and individual games scraped
check <- all %>%
  group_by(draftyear,overall) %>%
  tally() %>%
  right_join(drafts[,c('draftyear','overall','gp','playerurl')],
             by=c('draftyear','overall')) %>%
  replace_na(list(n = 0)) %>%
  mutate(diff = gp - n) %>%
  filter(diff > 0)

# 3 players with missing games we can add manually.
add <- data.frame(date = ymd(c('2007-01-09','2007-02-02','2007-04-07','2006-04-18')),
                  team = c('BOS','CBJ','CBJ','NYI'),
                  num = c(1,1,2,1),
                  draftyear = rep(2005,4),
                  overall = c(100,131,131,144),
                  pointshares = c(-.1,.1,.1,0),
                  stringsAsFactors = FALSE) %>%
  left_join(drafts,by=c('draftyear','overall'))

all <- rbind(all,add)

# get team schedules to join with player game dates
# mostly for survival analysis
games <- foreach(year = 2006:2019,
                 .combine = rbind,
                 .packages=c('rvest','dplyr','lubridate')) %dopar%
  hockeyref_schedule(year)

stopCluster(cl)

rm(add,check,cl,no_cores)

write_csv(drafts,'drafts.csv')
write_csv(all,'playergames.csv')
write_csv(games,'teamgames.csv')
