library(tidyverse)
library(rvest)
library(ggthemes)
library(rstanarm)
theme_set(theme_gray())

# team abbreviations
abbr <- tibble(short = c('TBL', 'STL', 'WPG', 'TOR', 'CBJ', 'NJD', 'NSH', 'VEG', 'NYI',
                         'LAK', 'WSH', 'NYR', 'MIN', 'PIT', 'DAL', 'CGY', 'VAN', 'SJS',
                         'COL', 'BOS', 'DET', 'CHI', 'ANA', 'CAR', 'PHI', 'OTT', 'MTL',
                         'EDM', 'FLA', 'BUF', 'ARI', 'ATL', 'MDA', 'PHX', 'VGK'),
               long = c('Tampa Bay Lightning', 'St. Louis Blues', 'Winnipeg Jets',
                        'Toronto Maple Leafs', 'Columbus Blue Jackets', 'New Jersey Devils',
                        'Nashville Predators', 'Vegas Golden Knights', 'New York Islanders',
                        'Los Angeles Kings', 'Washington Capitals', 'New York Rangers',
                        'Minnesota Wild', 'Pittsburgh Penguins', 'Dallas Stars', 'Calgary Flames',
                        'Vancouver Canucks', 'San Jose Sharks', 'Colorado Avalanche',
                        'Boston Bruins', 'Detroit Red Wings', 'Chicago Blackhawks', 'Anaheim Ducks',
                        'Carolina Hurricanes', 'Philadelphia Flyers', 'Ottawa Senators',
                        'Montreal Canadiens', 'Edmonton Oilers', 'Florida Panthers',
                        'Buffalo Sabres', 'Arizona Coyotes', 'Atlanta Thrashers',
                        'Mighty Ducks of Anaheim', 'Phoenix Coyotes', 'Vegas Golden Knights'))

# draft data from NHL
nhl_draft_stats <- function(draftyear){
  url <- read_html(paste('http://www.nhl.com/ice/draftstats.htm?year=', draftyear,
                         '&team=&position=&round=#', sep=''))
  table <- url %>%
    html_table(fill=TRUE, header=TRUE) %>%
    .[[3]] %>%
    .[,1:11] %>%
    set_names(tolower(as.character(.[1,]))) %>%
    filter(str_detect(rnd, '^[[:digit:]]')) %>%
    select(-c(pick, `amateur league`, `amateur team`)) %>%
    separate(ht, sep='\'', into=c('feet', 'inches')) %>%
    mutate(inches = gsub('\\D','', inches)) %>%
    mutate_at(c('rnd', 'overall', 'feet', 'inches', 'wt'), as.numeric) %>%
    mutate(ht = 12*feet+inches,
           draftyear) %>%
    select(-c(feet, inches)) %>%
    rename(draftteam = team)
  # dealing with forfeited picks has taken years off of my life
  if (draftyear == 2011 & 69 %in% table$overall){
    table <- table %>%
      mutate(overall = ifelse(overall >= 69, overall+1, overall))
  }
  if (draftyear == 2009 & 118 %in% table$overall){
    table <- table %>%
      mutate(overall = ifelse(overall >= 118, overall+1, overall))
  }
  return(table)
}

# age data from Hockey Reference
hockeyref_draft_stats <- function(draftyear){
  url <- read_html(paste('https://www.hockey-reference.com/draft/NHL_',
                         draftyear, '_entry.html', sep=''))
  table <- url %>%
    html_table() %>%
    .[[1]] %>%
    .[,1:9] %>%
    set_names(tolower(as.character(.[1,]))) %>%
    filter(!overall %in% c('Overall', '')) %>%
    mutate(gp = replace(gp, gp=='', 0)) %>%
    mutate_at(c('overall', 'gp', 'age'), as.numeric) %>%
    select(overall, age, gp) %>%
    mutate(draftyear)
  return(table)
}

# scrape 2005-17 full draft data
drafts <- c()
for (year in 2005:2017){
  temp <- nhl_draft_stats(year) %>%
    left_join(hockeyref_draft_stats(year),
              by=c('draftyear', 'overall'))
  drafts <- rbind(drafts, temp)
  print(year)
  rm(temp, year)
}

# manually correct missing age
drafts <- drafts %>%
  mutate(age = replace(age, player == 'Trent Vogelhuber', 19))

# get ready to analyze overage prospects
picks <- drafts %>%
  select(draftyear, rnd, overall, draftteam, age) %>%
  mutate(overage = 1*(age > 18)) %>%
  # attach full team names
  left_join(abbr, by=c('draftteam' = 'short')) %>%
  rename(draftteamfull = long)

# use prior_summary() to see default weakly informative priors
model <- stan_glm(overage ~ overall + I(overall^0.5),
                  family = binomial(link = 'logit'),
                  data = picks, iter = 12500, chains = 4,
                  cores = parallel::detectCores())


# first graph: overager % by team
interval <- 2014:2017
overager_pcts <- picks %>%
  filter(draftyear %in% interval) %>%
  group_by(draftteamfull) %>%
  summarize(pct = mean(overage))
# graph
ggplot(overager_pcts, aes(x=reorder(draftteamfull, pct), y=pct)) +
  geom_bar(stat='identity', width=0.85, fill='skyblue3', alpha=0.75) +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,0.5,.05),
                     labels=scales::percent_format(1), expand=c(0.005, 0.01)) +
  labs(x=NULL, y=NULL,
       title='% of Picks Spent on Overagers',
       subtitle=paste0(interval[1],'-', str_sub(tail(interval,1),-2,-1),
                       ' NHL Drafts\n')) +
  theme_hc()

# second graph: fitted model values
fitted <- picks %>%
  mutate(preds = model$fitted.values) %>%
  select(overall, preds)
# overager % by pick
avgs <- picks %>%
  group_by(overall) %>%
  summarize(avg = mean(overage))
# graph
ggplot(avgs, aes(x=overall, y=avg)) +
  geom_point(size=2.5, alpha=0.75) +
  geom_line(data=fitted, aes(x=overall, y=preds),
            color='darkred', size=2, alpha=0.8) +
  scale_x_continuous(limits=c(1,211), breaks=seq(1,211,30)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.1),
                     labels=scales::percent) +
  labs(x='\nOverall Pick', y=NULL,
       title='% of Overage Selections at Every Pick',
       subtitle='2005-17 NHL Drafts\n') +
  theme_hc()

# third graph: overager pick % relative to expected
overager_pcts_adj <- picks %>%
  mutate(preds = model$fitted.values) %>%
  filter(draftyear %in% interval) %>%
  group_by(draftteamfull) %>%
  summarize(num = sum(overage),
            e_num = sum(preds),
            pct = mean(overage),
            e_pct = mean(preds),
            pct_adj = pct-e_pct) %>%
  mutate(posneg = 1*(pct_adj > 0))
# graph
ggplot(overager_pcts_adj, aes(x=reorder(draftteamfull, pct_adj), y=pct_adj)) +
  geom_bar(stat='identity', width=0.85, aes(fill=as.factor(posneg)), alpha=0.75) +
  coord_flip() +
  scale_fill_manual(values=c('palevioletred4', 'seagreen4')) +
  guides(fill = FALSE) +
  scale_y_continuous(breaks=seq(-.25,0.25,0.05),
                     labels=scales::percent_format(1), expand=c(0.005,0.01)) +
  labs(x=NULL, y=NULL,
       title='% of Picks Spent on Overagers > Expected',
       subtitle=paste0(interval[1],'-',str_sub(tail(interval,1),-2,-1),
                      ' NHL Drafts\n')) +
  theme_hc()

# fourth graph: posterior simulation of PIT picks
# posterior simulations
sims <- posterior_predict(model)
# pick team
check <- 'Pittsburgh Penguins'
# get team picks, expected vs. actual overagers
newdrafts <- sims[,which(picks$draftteamfull == check & picks$draftyear %in% interval)] %>%
  rowSums()
e_overage <- overager_pcts_adj %>%
  filter(draftteamfull == check) %>%
  pull(e_num)
actual <- overager_pcts_adj %>%
  filter(draftteamfull == check) %>%
  pull(num)
pct <- paste0(round(length(newdrafts[newdrafts < actual])/length(newdrafts)*100, 2), '%')
# graph
ggplot(data.frame(newdrafts), aes(newdrafts)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=1,
                 breaks=seq(-0.5,15.5,1),
                 col='gold3', size=0.5,
                 fill='gold', alpha=0.5) +
  geom_vline(xintercept=actual, size=1.5, col='indianred4') +
  geom_vline(xintercept=e_overage, size=1.5, col='indianred2') +
  scale_y_continuous(breaks=seq(0,1,0.05), limits=c(0,0.25),
                     labels=scales::percent_format(1)) +
  scale_x_continuous(breaks=seq(0,15,1), expand=c(0.005,0)) +
  annotate('text', x=ifelse(e_overage < actual, e_overage-1.3, e_overage+1.3),
           y=0.23, label='Expected', size=5) +
  annotate('text', x=ifelse(e_overage < actual, actual+1, actual-1),
           y=0.23, label='Actual', size=5) +
  annotate('text', x=12.5, y=0.1,
           label=paste('More overagers drafted\nthan in', pct, '\nof simulations.'), size=5) +
  labs(x='\n# of Overagers Selected', y='% of Simulations (25K Total)\n',
       title = 'Simulated Distribution of Overager Selections',
       subtitle = paste0(interval[1],'-',str_sub(tail(interval,1),-2,-1),' ',
                        check,' Picks\n')) +
  theme_hc()
