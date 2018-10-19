library(tidyverse)
library(lubridate)
library(survival)

# was not gone over in the slides but like
# mess around with this if you want
# and check out my prospect survival analysis presentation

### DATA

drafts <- read.csv('drafts.csv',
                   stringsAsFactors = FALSE)
all <- read.csv('playergames.csv',
                stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
games <- read.csv('teamgames.csv',
                  stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))

### FIGURE OUT WHEN PROSPECTS PLAYED NTH GAME

nth_game <- function(df,n){
  df <- df %>%
    filter(num == n) %>%
    mutate(status = 1) %>%
    left_join(games,by=c('team' = 'short',
                         'date' = 'date')) %>%
    right_join(drafts,by=colnames(drafts)) %>%
    # need discrete time for players who haven't reached the benchmark
    # assume they've missed like 5 of their draft team's games this year
    replace_na(list(year = 2018, teamnum = 5, status = 0, num = n)) %>%
    # fuckin lockout
    mutate(time = ifelse(draftyear <= 2012 & year > 2012,
                         (year-draftyear-1)*82+48+teamnum,
                         (year-draftyear)*82+teamnum))
  return(df)
}

df_surv <- nth_game(all,1) %>%
  mutate(na = ifelse(country %in% c('USA','CAN'),1,0),
         pos = ifelse(pos %in% c('G','D'),pos,'F'),
         overage = ifelse(age > 18,1,0))

cox <- coxph(Surv(time, status) ~ ht + wt + I(overall^(0.5)) + overall + I(overall^(2)) +
               pos + overage + na + rnd, data = df_surv)
summary(cox)

