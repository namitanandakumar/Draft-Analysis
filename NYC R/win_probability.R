library(tidyverse)
library(janitor)
library(viridis)
library(ggthemes)
library(xgboost)

# load data
temp <- tempfile()
download.file('http://peter-tanner.com/moneypuck/downloads/shots_2018.zip', temp)
pbp <- read_csv(unz(temp, 'shots_2018.csv'))
unlink(temp)

# clean it up 
pbp_clean <- pbp %>%
  clean_names() %>%
  # keep goals only
  filter(event == 'GOAL') %>%
  select(game_id, period, time, is_home_team, home_team_won, home_team_won,
         home_team = home_team_code, home_goals = home_team_goals,
         away_team = away_team_code, away_goals = away_team_goals) %>%
  # add goal that was just scored to running tally
  mutate(home_goals = (is_home_team == 1) + home_goals,
         away_goals = (is_home_team == 0) + away_goals) %>%
  group_by(game_id) %>%
  # no OT games
  filter(max(period) <= 3) %>%
  ungroup() %>%
  select(-c(is_home_team, period))

#pbp_clean %>% View()

# expand!
pbp_full <- pbp_clean %>%
  group_by(game_id) %>%
  # row for every second of every game
  expand(time = 0:3600) %>%
  left_join(pbp_clean, by=c('game_id', 'time')) %>%
  # fill in score info for times between goals
  fill(home_team_won, home_team, home_goals,
       away_team, away_goals, .direction='down') %>%
  fill(home_team_won, home_team, away_team, .direction='up') %>%
  # every game starts 0-0
  replace_na(list(home_goals = 0, away_goals = 0)) %>%
  # calculate magnitude of lead and indicator for whether leading team won
  mutate(lead = abs(home_goals - away_goals),
         lead_won = ifelse(home_goals > away_goals,
                           1*(home_team_won == 1), 1*(home_team_won == 0)),
         lead_won = replace(lead_won, lead == 0, 0.5)) %>%
  ungroup()

#pbp_full %>% slice(20:30) %>% View()

# graph the empirical #s
pbp_full %>%
  mutate(lead = replace(lead, lead > 4, 4)) %>%
  group_by(lead, time) %>%
  summarize(empirical_prob = mean(lead_won)) %>%
  ggplot(aes(time, empirical_prob, group=as.factor(lead))) +
  geom_line(aes(col=as.factor(lead)), size=1.5, alpha=0.5) +
  geom_smooth(aes(col=as.factor(lead)), size=1.5, se=FALSE) +
  scale_color_viridis_d(begin=0.2, end=0.95) +
  scale_x_continuous(breaks=seq(0,3600,300)) +
  scale_y_continuous(breaks=seq(0,1,0.05),
                     limits=c(0.5,1), labels=scales::percent_format(1)) +
  theme_hc() +
  labs(x='Time (s)', y='% Win', col='Lead')

# use xgboost to enforce monotonicity
cols <- c('lead', 'time')
pbp_model <- pbp_full %>%
  # regular season, no tied states
  filter(lead != 0,
         game_id < 30000)
dtrain <- pbp_model %>%
  select(cols) %>%
  filter(lead != 0) %>%
  as.matrix()
dlabel <- pbp_model %>%
  filter(lead != 0) %>%
  pull(lead_won)

param <- list(objective = 'binary:logistic',
              eval_metric = 'logloss',
              max_depth = 1,
              eta = 0.01,
              monotone_constraints = c(1, 1))

model <- xgboost(data = dtrain, label = dlabel,
                 params = param, nrounds = 2000,
                 print_every_n = 100)

# create a test dataset to showcase results
test <- tibble(lead = 0:4) %>%
  group_by(lead) %>% 
  expand(time = 0:3600) %>%
  ungroup() %>%
  mutate(pred = predict(model, newdata=as.matrix(.[,cols])),
         pred = replace(pred, lead == 0, 0.5))

# graph results!
ggplot(test, aes(time, pred, group=as.factor(lead))) +
  geom_line(aes(col=as.factor(lead)), size=2.5, alpha=0.5) +
  geom_smooth(aes(col=as.factor(lead)), size=1.5, se=FALSE) +
  scale_color_viridis_d(begin=0.2, end=0.95) +
  scale_x_continuous(breaks=seq(0,3600,300)) +
  scale_y_continuous(breaks=seq(0,1,0.05),
                     limits=c(0.5,1), labels=scales::percent_format(1)) +
  theme_hc() +
  labs(x='Time (s)', y='P(Win)', col='Lead')

# get win probabilities for the playoffs
yoffs <- pbp_full %>%
  filter(game_id >= 30000) %>%
  mutate(pred = predict(model, newdata=as.matrix(.[,cols])),
         pred = replace(pred, lead == 0, 0.5)) %>%
  arrange(game_id, time) %>%
  group_by(game_id) %>%
  mutate(home_pred = ifelse(home_goals > away_goals, pred, 1-pred),
         change = abs(home_pred - lag(home_pred, default=0.5)),
         change = sum(change),
         spread = max(home_pred)-min(home_pred)) %>%
  ungroup()

# graph the biggest collapse...
yoffs %>%
  filter(spread == max(spread)) %>%
  ggplot(aes(time, home_pred)) +
  geom_line(col='deepskyblue', size=2) +
  scale_x_continuous(breaks=seq(0,3600,300)) +
  scale_y_continuous(breaks=seq(0,1,0.1),
                     limits=c(0,1), labels=scales::percent_format(1)) +
  annotate('text', x=150, y=0.05, label='CBJ @ TBL, Round 1, Game 1', size=8, hjust=0) +
  theme_hc() +
  labs(x='Time (s)', y='Home Win Probability', col='Lead')
