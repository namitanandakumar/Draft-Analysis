library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)
library(ggthemes)
library(ggbeeswarm)

# Scrape article info.
articles <- c()
for (k in 1:281){
  url <- paste0('https://fivethirtyeight.com/sports/features/page/', k, '/')
  articles <- read_html(url) %>%
    html_nodes('#main .vertical-sports') %>%
    # Not every article has a tag.
    # So this fills in NAs rather than having mismatched #s of observations.
    map_df(~list(day = html_nodes(.x, '.updated') %>% 
                   html_text() %>% 
                   {if(length(.) == 0){NA} else {.}},
                 tag = html_nodes(.x, '.term') %>%
                   html_text() %>% 
                   {if(length(.) == 0){NA} else {.}})) %>%
    rbind(articles, .)
  #print(k)
}

# Do some basic cleaning.
articles_clean <- articles %>%
  # I wonder if there's a quicker way to do this.
  mutate(tag = tolower(tag),
         tag = replace(tag, grepl('super bowl|nfl|football', tag), 'football'),
         tag = replace(tag, grepl('world series|mlb|baseball', tag), 'baseball'),
         tag = replace(tag, grepl('march madness|nba|basketball', tag), 'basketball'),
         tag = replace(tag, grepl('stanley cup|nhl|hockey', tag), 'hockey')) %>%
  filter(tag %in% c('football', 'baseball', 'basketball', 'hockey')) %>%
  mutate(day = mdy(day),
         year = year(day)) %>%
  # Filter to recent years with more articles.
  filter(year >= 2014) %>%
  group_by(tag) %>%
  # Get total counts for ordering the sports on the graph.
  mutate(count = n()) %>%
  ungroup()

# Make a nice graph!
# My personal ggplot theme will remain a heavily guarded secret.
ggplot(articles_clean, aes(reorder(tag, count), day)) +
  geom_quasirandom(varwidth=TRUE, aes(col=tag), alpha=0.75) +
  scale_color_manual(values=c('violetred', 'seagreen', 'skyblue4', 'indianred')) +
  guides(col=FALSE) +
  scale_y_date(date_labels='%b-%y', date_breaks='3 months') +
  coord_flip() +
  labs(x=NULL, y=NULL, title='FiveThirtyEight Coverage of Major US Sports') +
  theme_hc() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
