#rm(list=ls())
library(purrr)
library(tseries)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(scales)
library(rvest)
library(ggridges)
library(skimr)
library(dplyr)

# Calculate normalized entropy of a series.
norm_ent <- function(s){
  s1 <- which(s %in% 1)
  iet <- c(s1[1], diff(s1), length(s)+1-tail(s1,1))
  iet <- iet/(length(s)+1)
  h <- 1+((sum(log(iet)*iet))/log(sum(s)+1))
  return(h)
}

# Calculate streakiness percentile of series.
ent_test <- function(s){
  n <- 10000
  x <- replicate(n,sample(s,length(s),replace=F))
  x <- lapply(1:ncol(x), function(i) x[,i])
  y <- map_dbl(x,norm_ent)
  return(sum(y < norm_ent(s))/n)
}

# Scrape all game results from any year.
scrape_games <- function(year){
  url <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_",
                         year,"_games.html",sep=''))
  date <- url %>% 
    html_nodes("#games tbody th") %>%
    html_text()
  away <- url %>%
    html_nodes("#games .left+ .left a") %>%
    html_text()
  away_goals <- url %>%
    html_nodes("#games .right:nth-child(3)") %>%
    html_text()
  home <- url %>%
    html_nodes("#games .right+ .left a") %>%
    html_text()
  home_goals <- url %>%
    html_nodes("#games .right~ .left+ .right") %>%
    html_text()
  dat <- data.frame(cbind(date,away,away_goals,home,home_goals))
  dat$away_goals <- as.numeric(as.character(dat$away_goals))
  dat$home_goals <- as.numeric(as.character(dat$home_goals))
  dat$away_win <- ifelse(dat$away_goals > dat$home_goals,1,0)
  dat$home_win <- 1-dat$away_win
  dat$num <- rownames(dat)
  away <- dat[,c('num','away','away_win')]
  colnames(away) <- c('gamenum','team','win')
  home <- dat[,c('num','home','home_win')]
  colnames(home) <- c('gamenum','team','win')
  dat <- rbind(away,home)
  dat$gamenum <- as.numeric(as.character(dat$gamenum))
  dat <- dat[order(dat$gamenum),]
  rownames(dat) <- NULL
  dat <- na.omit(dat)
  dat$year <- year
  return(dat)
}

# Scrape all player game results from any year.
# Returns number of pages scraped while it runs.
scrape_gamelogs <- function(year){
  gamelogs <- data.frame()
  results <- data.frame(1)
  page <- 0
  while (nrow(results) > 0){
    url <- read_html(paste('https://www.hockey-reference.com/play-index/pgl_finder.cgi?c2stat=&team_game_min=1&game_type[]=R&game_type[]=OT&game_type[]=SO&c4stat=&c2comp=&game_month=0&player_game_max=9999&is_playoffs=N&order_by_asc=&on_birthday=&group_set=single&game_location=&c1comp=&year_min=',
                           year,'&year_max=',year,
                           '&request=1&match=game&team_id=&series_game_min=1&year_max=&c3comp=&report=pgl&season_end=-1&c3stat=&order_by=goals&season_start=1&c1val=&opp_id=&team_game_max=84&c3val=&c2val=&game_result=&rookie=N&pos=S&describe_only=&player_game_min=1&series_game_max=7&c1stat=&player=&series_game_num=&c4val=&playoff_round=&age_min=0&c4comp=&age_max=99&offset=',
                           page,sep=''))
    player <- url %>% 
      html_nodes(".right+ .left") %>%
      html_text()
    goals <- url %>%
      html_nodes(".right:nth-child(3)") %>%
      html_text()
    pos <- url %>%
      html_nodes(".center+ td.center") %>%
      html_text()
    date <- url %>%
      html_nodes(".center+ .left a") %>%
      html_text()
    team <- url %>%
      html_nodes(".left:nth-child(7) a") %>%
      html_text()
    assists <- url %>%
      html_nodes(".left~ .left+ .right") %>%
      html_text()
    results <- data.frame(cbind(player,goals,assists,pos,date,tm))
    gamelogs <- rbind(gamelogs,results)
    print(page/300)
    page <- page + 300
  }
  gamelogs$date <- ymd(gamelogs$date)
  gamelogs$goals <- as.numeric(as.character(gamelogs$goals))
  gamelogs$assists <- as.numeric(as.character(gamelogs$assists))
  gamelogs <- gamelogs %>% arrange(date)
  gamelogs$year <- year
  return(gamelogs)
}

# If you care to update my results (which include games through 2/22).
#scrape_games(2018)
#scrape_gamelogs(2018)

# Or load in my scraped data.
# In general, make sure the sequences are in chronological order.
# Otherwise, the formulas will lie to you.
load('gamelogs1418.RData')
skim(gamelogs)
load('games1018.RData')
skim(games)

# Update old team names.
games$team <- as.character(games$team)
games$team[games$team == 'Phoenix Coyotes'] <- 'Arizona Coyotes'
games$team[games$team == 'Atlanta Thrashers'] <- 'Winnipeg Jets'
games$team <- as.factor(games$team)

# Deal with duplicate player names as you see fit.
gamelogs$player <- as.character(gamelogs$player)
gamelogs$player[gamelogs$player == 'Sebastian Aho' &
                  gamelogs$team == 'NYI'] <- 'Lil Sebastian'
gamelogs$player[gamelogs$player == 'Erik Gustafsson' &
                  gamelogs$team == 'PHI'] <- 'Flyers Erik'
gamelogs$player <- as.factor(gamelogs$player)

# Test with the 2017-18 Flyers.
flyers <- subset(games, team == 'Philadelphia Flyers' & year == 2018)
series <- flyers$win
# Runs test.
runs.test(as.factor(series), alternative='less')
# p = 0.1259
# Normalized entropy percentile.
ent_test(series)
# 99th percentile.

# Get distribution of simulated normalized entropy values for the Flyers.
n <- 10000
x <- replicate(n,sample(series,length(series),replace=F))
x <- lapply(1:ncol(x), function(i) x[,i])
y <- map_dbl(x,norm_ent)
y <- data.frame(team = 'PHI',value = y)

ggplot(y, aes(value, fill = team)) + 
  geom_density(alpha=0.7) +
  scale_fill_manual(values = c("seagreen")) +
  labs(y=NULL,x='\nNormalized Entropy Distribution') +
  guides(fill=FALSE) +
  geom_vline(xintercept=norm_ent(flyers$win)) +
  annotate("text", x = .092, y = 25, label = "Actual\nValue",size = 5)

# Show that win-loss record affects distribution of probable entropy values.
n <- 10000
all_teams <- unique(levels(games$team))
ent_dist <- data.frame()
pb <- progress_estimated(length(all_teams))
for (i in 1:length(all_teams)){
  team <- subset(games,team==all_teams[i] & year == 2018)
  series <- team$win
  x <- replicate(n,sample(series,length(series),replace=F))
  x <- lapply(1:ncol(x), function(i) x[,i])
  y <- map_dbl(x,norm_ent)
  df <- data.frame(team = all_teams[i], ent_sample = y,
                   ent_actual = norm_ent(series), win_pct = mean(series),
                   ent_pct = ent_test(series))
  ent_dist <- rbind(ent_dist, df)
  pb$tick()$print()
}

ggplot(ent_dist, aes(x = ent_sample, y = reorder(team, win_pct),
                     fill = reorder(team, win_pct))) + 
  geom_density_ridges(scale=3,alpha=0.7,rel_min_height = 0.015) +
  scale_fill_cyclical(values = c("paleturquoise", "lightblue3")) +
  scale_x_continuous(limits=c(0.01,.17),breaks=seq(0.01,.17,0.02)) +
  expand_limits(y=35) +
  labs(y=NULL,x='\n2017-18 Normalized Entropy Distribution')

# 2017-18 streakiness percentiles by team.
palette <- c(rep(c('lightpink2','palevioletred'),15),'lightpink2') 
results_teams <- unique(ent_dist[c('team','ent_pct','win_pct')])
ggplot(results_teams,aes(reorder(team,ent_pct),ent_pct,fill=reorder(team,ent_pct))) + 
  geom_hline(yintercept=0.05) +
  geom_hline(yintercept=0.95) +
  geom_bar(stat='identity',col='black',alpha=0.65) +
  scale_y_continuous(breaks=seq(0,1,0.05),labels=percent) +
  labs(x=NULL,y='\n2017-18 Streakiness Percentile') +
  coord_flip() +
  guides(fill=FALSE) +
  scale_fill_manual(values = palette)

# Player streakiness.
# Limit to players who have scored at least 15 goals in at least 41 games
# in each of the last 5 seasons.
gamelogs41 <- gamelogs %>%
  group_by(player, year) %>% 
  filter(max(row_number()) >= 41 & sum(goals) >= 15) %>%
  ungroup() %>%
  group_by(player) %>%
  filter(n_distinct(year) == 5) %>%
  ungroup()
# Convert # of goals to binary scored or not in each game.
gamelogs41$goals <- ifelse(gamelogs41$goals >= 1,1,0)

# Compute streakiness percentile for each player season.
results_players <- gamelogs41 %>%
  group_by(player, year) %>%
  summarize(pct = ent_test(goals),goals_total = sum(goals))

# Get lags.
lags <- results_players %>%
  arrange(year) %>%
  group_by(player) %>%
  mutate(mean = mean(pct),lag = lag(pct)) %>%
  ungroup() %>%
  arrange(player)
lags_full <- subset(lags, year!=2014)
cor(lags_full$pct, lags_full$lag)
# No correlation from one year's streakiness to the next!

ggplot(lags,aes(x=reorder(player,mean),y=pct)) + 
  geom_hline(yintercept=0.05) +
  geom_hline(yintercept=0.95) +
  geom_line(color='gray',size=3,alpha=0.7) +
  geom_point(aes(fill=as.factor(year)),shape=21, size = 5,alpha=0.7) +
  scale_fill_brewer(palette='Spectral') +
  scale_y_continuous(breaks=seq(0,1,0.05),labels=percent) +
  expand_limits(x=c(0,36)) +
  labs(x=NULL,y='\nStreakiness Percentile') +
  guides(fill=guide_legend('Season')) +
  coord_flip()

summary(lm(I(pct-.5)~0+player,data=results_players))
# Backstrom & Wheeler somewhat streaky, Pavs & Tatar not so much.

cor(lags$pct,lags$goals_total)
# No correlation between streakiness and goals scored in a player season.

# Get team season streakiness going back to 2009-10.
n <- 10000
results_teams1018 <- games %>%
  group_by(team, year) %>%
  summarize(pct = ent_test(win), winpct = mean(win))
# Get lags.
lags1018 <- results_teams1018 %>%
  arrange(year) %>%
  group_by(team) %>%
  mutate(mean = mean(pct),lag = lag(pct)) %>%
  ungroup() %>%
  arrange(team)
lags1018_full <- na.omit(lags1018)
cor(lags1018_full$pct, lags1018_full$winpct)
# Once again, no repeatability.
summary(lm(I(pct-.5)~0+team,data=lags1018))

# Graphs.
ggplot(lags1018_full,aes(x=lag,y=pct)) +
  geom_point(col='indianred',size=2) + 
  labs(x='\nStreakiness Percentile in Season N-1',
       y='Streakiness Percentile in Season N\n',title='Teams\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  annotate("text", x = .1, y = .1, col = 'black',
           label = paste('r = ',round(cor(lags1018_full$lag,lags1018_full$pct),3),sep=' '),size = 5)

ggplot(lags_full,aes(x=lag,y=pct)) +
  geom_point(col='skyblue3',size=2) + 
  labs(x='\nStreakiness Percentile in Season N-1',
       y='Streakiness Percentile in Season N\n',title='Players\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  annotate("text", x = .1, y = .1, col = 'black',
           label = paste('r = ',round(cor(lags_full$lag,lags_full$pct),3),sep=' '),size = 5)

ggplot(lags1018,aes(x=pct,y=winpct)) +
  geom_point(col='seagreen',size=2) + 
  labs(x='\nStreakiness Percentile',
       y='Win Percentage\n',title='Teams\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  annotate("text", x = .1, y = .2, col = 'black',
           label = paste('r = ',round(cor(lags1018$pct,lags1018$winpct),3),sep=' '),size = 5)

ggplot(lags,aes(x=pct,y=goals_total)) +
  geom_point(col='goldenrod',size=2) + 
  labs(x='\nStreakiness Percentile',
       y='Total Games with a Goal\n',title='Players\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(5,45,5),limits=c(5,45)) +
  annotate("text", x = .1, y = 10, col = 'black',
           label = paste('r = ',round(cor(lags$pct,lags$goals_total),3),sep=' '),size = 5)

# See if streakiness is repeatable in season.
games_split <- games %>%
  group_by(team, year) %>%
  mutate(half = ifelse(row_number() <= median(row_number()),1,2)) %>%
  ungroup()
results_teams_split <- games_split %>%
  group_by(team, year, half) %>%
  summarize(pct = ent_test(win))
teams_splits <- dcast(results_teams_split, team + year ~ half)
colnames(teams_splits) <- c('team','year','first','second')
ggplot(teams_splits,aes(x=first,y=second)) +
  geom_point(col='purple',size=2) + 
  labs(x='\nStreakiness Percentile: First Half',
       y='Streakiness Percentile: Second Half\n',title='Teams\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  annotate("text", x = .1, y = .1, col = 'black',
           label = paste('r = ',round(cor(teams_splits$first,teams_splits$second),3),sep=' '),size = 5)
# Same for players.
gamelogs41_split <- gamelogs41 %>%
  group_by(player, year) %>%
  mutate(half = ifelse(row_number() <= median(row_number()),1,2)) %>%
  ungroup()
results_players_split <- gamelogs41_split %>%
  group_by(player, year, half) %>%
  summarize(pct = ent_test(goals))
players_split <- dcast(results_players_split, player + year ~ half)
colnames(players_split) <- c('player','year','first','second')
ggplot(players_split,aes(x=first,y=second)) +
  geom_point(col='palevioletred2',size=2) + 
  labs(x='\nStreakiness Percentile: First Half',
       y='Streakiness Percentile: Second Half\n',title='Players\n') +
  scale_x_continuous(breaks=seq(0,1,0.1),labels=percent) +
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent) +
  annotate("text", x = .1, y = .1, col = 'black',
           label = paste('r = ',round(cor(players_split$first,players_split$second),3),sep=' '),size = 5)
