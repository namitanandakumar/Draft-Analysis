library(tidyverse)
library(rstanarm)
library(ggthemes)
source('scraping_functions.R')
theme_set(theme_gray())

### READ IN SCRAPED DATA
drafts <- read.csv('drafts.csv',
                   stringsAsFactors = FALSE)

### OVERAGER MODEL
picks <- drafts %>%
  select(draftyear,rnd,overall,draftteam,age) %>%
  mutate(overage = ifelse(age == 18,0,1)) %>%
  # attach full team names
  left_join(abbr,by=c('draftteam' = 'short')) %>%
  rename(draftteamfull = long)

model <- stan_glm(overage ~ overall+I(overall^0.5),
                  family = binomial(link = 'logit'),
                  data = picks, iter = 12500, chains = 4,
                  cores=parallel::detectCores())

### GRAPH 1
# overager % by team
interval <- 2014:2017
overager_pcts <- picks %>%
  filter(draftyear %in% interval) %>%
  group_by(draftteamfull) %>%
  summarize(pct = mean(overage))
# graph
ggplot(overager_pcts,aes(x=reorder(draftteamfull,pct),y=pct)) +
  geom_bar(stat='identity',width=0.85,fill='skyblue3',alpha=0.75) + coord_flip() +
  scale_y_continuous(breaks=seq(0,0.5,.05),labels = scales::percent,expand=c(0.005,0.01)) +
  labs(x = NULL, y = NULL,
       title = '% of Picks Spent on Overagers',
       subtitle = paste(interval[1],'-',str_sub(tail(interval,1),-2,-1),
                        ' NHL Drafts\n',sep='')) +
  theme_hc()

### GRAPH 2
# model fit
# fitted values
fitted <- picks %>%
  mutate(preds = model$fitted.values) %>%
  select(overall,preds)
# overager % by pick
avgs <- picks %>%
  group_by(overall) %>%
  summarize(avg = mean(overage))
# graph
ggplot(avgs,aes(x=overall,y=avg)) + geom_point(size=2.5,alpha=0.75) +
  geom_line(data=fitted,aes(x=overall,y=preds),
            color='darkred',size=2,alpha=0.8) +
  scale_x_continuous(limits=c(1,211), breaks=seq(1,211,30)) +
  scale_y_continuous(limits=c(0,.8),breaks=seq(0,.8,.1),labels = scales::percent) +
  labs(x = '\nOverall Pick', y = NULL,
       title = '% of Overage Selections at Every Pick',
       subtitle = '2005-17 NHL Drafts\n') +
  theme_hc()

### GRAPH 3
# relative to expected
overager_pcts_adj <- picks %>%
  mutate(preds = model$fitted.values) %>%
  filter(draftyear %in% interval) %>%
  group_by(draftteamfull) %>%
  summarize(num = sum(overage),
            e_num = sum(preds),
            pct = mean(overage),
            e_pct = mean(preds),
            pct_adj = pct-e_pct) %>%
  mutate(posneg = ifelse(pct_adj > 0,1,0))
# graph
ggplot(overager_pcts_adj,aes(x=reorder(draftteamfull,pct_adj),y=pct_adj)) +
  geom_bar(stat='identity',width=0.85,aes(fill=as.factor(posneg)),alpha=0.75) +
  coord_flip() +
  scale_fill_manual(values=c('palevioletred4','seagreen4')) +
  guides(fill = FALSE) +
  scale_y_continuous(breaks=seq(-.25,0.25,0.05),
                     labels = scales::percent,
                     expand=c(0.005,0.01)) +
  labs(x = NULL, y = NULL,
       title = '% of Picks Spent on Overagers > Expected',
       subtitle = paste(interval[1],'-',str_sub(tail(interval,1),-2,-1),
                        ' NHL Drafts\n',sep='')) +
  theme_hc()

### GRAPH 4
# posterior simulations
sims <- posterior_predict(model)
# pick team
check <- 'Pittsburgh Penguins'
# get team picks, expected vs. actual overagers
newdrafts <- sims[,which(picks$draftteamfull==check & picks$draftyear %in% interval)] %>%
  rowSums()
e_overage <- overager_pcts_adj %>%
  filter(draftteamfull == check) %>%
  select(e_num) %>% .[[1]]
actual <- overager_pcts_adj %>%
  filter(draftteamfull == check) %>%
  select(num) %>% .[[1]]
pct <- paste0(round(length(newdrafts[newdrafts<actual])/length(newdrafts)*100,2),'%')
# graph
ggplot(data.frame(newdrafts),aes(newdrafts)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 binwidth = 1,
                 breaks=seq(-0.5,15.5,1),
                 col='gold3',size=0.5,
                 fill='gold',alpha=0.5) +
  geom_vline(xintercept = actual,size=1.5,col='indianred4') +
  geom_vline(xintercept = e_overage,size=1.5,col='indianred2') +
  scale_y_continuous(breaks=seq(0,1,0.05),limits=c(0,0.25),
                     labels = scales::percent) +
  scale_x_continuous(breaks=seq(0,15,1),expand=c(0.005,0)) +
  annotate('text',x = ifelse(e_overage < actual,e_overage-1.3,e_overage+1.3),
           y = 0.23, label = 'Expected',size=5) +
  annotate('text',x = ifelse(e_overage < actual,actual+1,actual-1),
           y = 0.23, label = 'Actual',size=5) +
  annotate('text',x = 12.5,y = 0.1,
           label = paste('More overagers drafted\nthan in',pct,'\nof simulations.'),size=5) +
  labs(x = '\n# of Overagers Selected', y = '% of Simulations (25K Total)\n',
       title = 'Simulated Distribution of Overager Selections',
       subtitle = paste(interval[1],'-',str_sub(tail(interval,1),-2,-1),' ',
                        check,' Picks\n',sep='')) +
  theme_hc()

