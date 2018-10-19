library(tidyverse)
library(lubridate)
library(xgboost)
library(ggthemes)

### PREDICT PLAYER VALUE

drafts <- read.csv('drafts.csv',
                   stringsAsFactors = FALSE)
all <- read.csv('playergames.csv',
                stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
games <- read.csv('teamgames.csv',
                   stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))

# calculate games played in first 7 years after draft
# 2005-11 drafts
# define NA/Euro indicator etc.
all_7yrs <- all %>%
  left_join(games[,c('date','short','year')],
            by=c('team' = 'short','date' = 'date')) %>%
  rename(season = year) %>%
  mutate(careeryear = season - draftyear + 1) %>%
  filter(careeryear <= 7) %>%
  group_by(draftyear,overall) %>%
  mutate(gp_7yrs = n()) %>%
  filter(row_number() == 1) %>%
  right_join(drafts,by=colnames(drafts)) %>%
  filter(draftyear <= 2011) %>%
  select(-c(date,team,num,playerurl,season,careeryear)) %>%
  ungroup() %>%
  replace_na(list(gp_7yrs = 0,
                  gp = 0,
                  pointshares = 0)) %>%
  mutate(na = ifelse(country %in% c('USA','CAN'),1,0),
         pos = ifelse(pos %in% c('G','D'),pos,'F'),
         overage = ifelse(age > 18,1,0))

# need to create numeric matrix of predictors
all_7yrs_num <- all_7yrs %>%
  mutate(f = as.numeric(pos == 'F'),
         d = as.numeric(pos == 'D'),
         g = 1-f-d,
         pointshares = pointshares/gp,
         # zero out player impact per game if negative or < 10 games
         pointshares = replace(pointshares,
                               gp < 10 | pointshares < 0, 0))

# predictor columns
cols <- c('rnd','overall','f','d','g','age','ht','wt','na')
# model data
train_data_gp <- as.matrix(all_7yrs_num[,cols])
# model outcomes
label_gp <- all_7yrs_num$gp_7yrs

### PREDICT WITH XGBOOST: GAMES PLAYED
# I did lots of randomized parameter searching with 5-fold CV
best_param_gp <- list(objective = 'count:poisson',
                      max_depth = 3,
                      eta = 0.1048398,
                      gamma = 8.9412,
                      subsample = 0.6266583,
                      colsample_bytree = 0.989617,
                      min_child_weight = 1,
                      # make sure draft pick # and round are monotonically constrained
                      # to prevent overfitting
                      monotone_constraints=c(-1,-1,0,0,0,0,0,0,0))
best_seed_gp <- 9208
best_ll_round <- 137

set.seed(best_seed_gp)
xgb_reg_gp <- xgboost(data=train_data_gp, label=label_gp,
                      params=best_param_gp, nrounds=best_ll_round)

# variable importances
labels <- data.frame(Feature = cols,
                     Name = c('Draft Round','Pick #',
                              'Forward','Defenseman','Goalie',
                              'Age','Height','Weight','Nationality'),
                     stringsAsFactors = FALSE)
imp_gp <- xgb.importance(model = xgb_reg_gp) %>%
  as_data_frame() %>%
  left_join(labels,by='Feature')
ggplot(imp_gp,aes(reorder(Name,Gain),Gain)) +
  coord_flip() +
  geom_bar(stat='identity',fill='skyblue3',col='skyblue4') +
  scale_y_continuous(expand=c(0.01,0),limits=c(0,0.65),breaks=seq(0,1,.1)) +
  labs(x=NULL,y='\nRelative Gain in Accuracy',title='Games Played Model\n') +
  theme_hc()

### ONLY PLAYERS WHO PLAYED 10+ GP
all_7yrs_num_trim <- all_7yrs_num %>%
  filter(gp >= 10)

cols <- c('rnd','overall','f','d','g','age','ht','wt','na')
train_data_pspg10 <- as.matrix(all_7yrs_num_trim[,cols])
label_pspg10 <- all_7yrs_num_trim$pointshares

best_param_pspg10 <- list(objective = 'reg:linear',
                          max_depth = 3,
                          eta = 0.04508037,
                          gamma = 0,
                          subsample = 0.6129046,
                          colsample_bytree = 1,
                          min_child_weight = 1,
                          monotone_constraints=c(-1,-1,0,0,0,0,0,0,0))
best_seed_pspg10 <- 2106
best_rmse_round <- 124

set.seed(best_seed_pspg10)
xgb_reg_pspg10 <- xgboost(data=train_data_pspg10, label=label_pspg10,
                          params=best_param_pspg10, nrounds=best_rmse_round)

# variable importances
imp_pspg10 <- xgb.importance(model = xgb_reg_pspg10) %>%
  as_data_frame() %>%
  left_join(labels,by='Feature')
ggplot(imp_pspg10,aes(reorder(Name,Gain),Gain)) +
  coord_flip() +
  geom_bar(stat='identity',fill='palevioletred3',col='palevioletred4') +
  scale_y_continuous(expand=c(0.01,0),limits=c(0,0.65),breaks=seq(0,1,.1)) +
  labs(x=NULL,y='\nRelative Gain in Accuracy',title='Value Per Game Model\n') +
  theme_hc()

### COMPARE PREDICTIONS ACROSS AGES

all_7yrs_preds <- all_7yrs_num %>%
  expand(nesting(draftyear,rnd,overall,f,d,g,ht,wt,na), age) %>%
  filter(age < 22) %>%
  mutate(person = rep(1:(nrow(.)/4),each=4),
         xgb_pred_gp = predict(xgb_reg_gp,as.matrix(.[,cols])),
         xgb_pred_pspg10 = predict(xgb_reg_pspg10,as.matrix(.[,cols]))) %>%
  group_by(person) %>%
  arrange(age) %>%
  # I mean, is there an easier way to do this?
  # probably but who cares
  mutate(xgb_pred_gp = ifelse(age == 19,
                              xgb_pred_gp/lag(xgb_pred_gp)-1,
                              ifelse(age == 20,
                                     xgb_pred_gp/lag(xgb_pred_gp,2)-1,
                                     ifelse(age == 21,
                                            xgb_pred_gp/lag(xgb_pred_gp,3)-1,0))),
         xgb_pred_pspg10 = ifelse(age == 19,
                                  xgb_pred_pspg10/lag(xgb_pred_pspg10)-1,
                                  ifelse(age == 20,
                                         xgb_pred_pspg10/lag(xgb_pred_pspg10,2)-1,
                                         ifelse(age == 21,
                                                xgb_pred_pspg10/lag(xgb_pred_pspg10,3)-1,0)))) %>%
  ungroup()

library(LaCroixColoR)
ggplot(all_7yrs_preds,aes(as.factor(age),
                          # if you want to look at the value per game model
                          # switch `xgb_pred_gp` to `xgb_pred_pspg10`
                          xgb_pred_gp,
                          group=person)) +
  geom_hline(yintercept=0,col='darkred') +
  geom_line(alpha=0.05,aes(col=overall),size=0.8) +
  geom_point(alpha=0.05,aes(col=overall),size=1) +
  scale_color_gradientn(colours =lacroix_palette('Lemon',
                                                 n=10,
                                                 type = "continuous")[10:1],
                        breaks = seq(1,211,30),
                        trans='reverse',
                        name='Pick #') +
  labs(x='\nAge at Draft',y='% Increase in Prediction\nCompared to Age 18\n',
       title='Individual Conditional Expectation Plot: Age',
       subtitle='\n2005-11 Drafts, NHL Games Played 7 Seasons Since Draft\n') +
  annotate('text',x=1.5,y=3.75,hjust=0.5,
           label=' Holding pick #, nationality,\n position, size as constant.') +
  scale_y_continuous(labels=scales::percent,breaks=seq(-100,100,0.5),limits=c(-1,4)) +
  theme_hc() +
  guides(color = guide_colorbar(ticks = FALSE,barwidth=10,barheight=1))

### COMPARE PREDICTIONS

all_7yrs_preds %>%
  filter(age == 19) %>%
  group_by(rnd) %>%
  summarize(`median increase in GP prediction` =
              paste(round(median(xgb_pred_gp)*100,1),'%',sep=''),
            `median increase in PS prediction` =
              paste(round(median(xgb_pred_pspg10)*100,1),'%',sep=''))
all_7yrs_preds %>%
  filter(age == 19) %>%
  #group_by(rnd) %>%
  summarize(`% of prospects with decreased GP predictions` =
              paste(round(sum(xgb_pred_gp < 0)/n()*100,1),'%',sep=''),
            `% of prospects with decreased PS predictions` =
              paste(round(sum(xgb_pred_pspg10 < 0)/n()*100,1),'%',sep=''))
all_7yrs_preds %>%
  filter(age == 19) %>%
  #group_by(rnd) %>%
  summarize(`% of prospects with proportionally lower PS vs. GP predictions` =
              paste(round(sum(xgb_pred_pspg10 < xgb_pred_gp)/n()*100,1),'%',sep=''))
