# These are some useful scraping functions.

### DRAFT DATA FROM NHL.COM
nhl_draft_stats <- function(draftyear){
  url <- read_html(paste('http://www.nhl.com/ice/draftstats.htm?year=',draftyear,
                         '&team=&position=&round=#',sep=''))
  table <- url %>%
    html_table(fill=TRUE,header=TRUE) %>%
    .[[3]] %>%
    .[,1:11] %>%
    setNames(tolower(as.character(.[1,]))) %>%
    filter(str_detect(rnd,'^[[:digit:]]')) %>%
    select(-c(pick,`amateur league`,`amateur team`)) %>%
    separate(ht,sep='\'',into=c('feet', 'inches')) %>%
    mutate(inches = gsub('\\D','',inches)) %>%
    mutate_at(c('rnd','overall','feet','inches','wt'),funs(as.numeric)) %>%
    mutate(ht = 12*feet+inches,
           draftyear) %>%
    select(-c(feet,inches)) %>%
    rename(draftteam = team)
  # dealing with these forfeited picks has taken years off of my life
  if (draftyear == 2011 & 69 %in% table$overall){
    table <- table %>%
      mutate(overall = ifelse(overall >= 69,overall+1,overall))
  }
  if (draftyear == 2009 & 118 %in% table$overall){
    table <- table %>%
      mutate(overall = ifelse(overall >= 118,overall+1,overall))
  }
  return(table)
}

### DRAFT DATA FROM HOCKEY REF
hockeyref_draft_stats <- function(draftyear){
  url <- read_html(paste('https://www.hockey-reference.com/draft/NHL_',
                         draftyear,'_entry.html',sep=''))
  playerurl <- url %>%
    html_nodes('.left+ .left a') %>%
    html_attr('href') %>%
    paste('https://www.hockey-reference.com',.,sep='')
  table <- url %>%
    html_table %>%
    .[[1]] %>%
    .[,1:9] %>%
    setNames(tolower(as.character(.[1,]))) %>%
    filter(!overall %in% c('Overall','')) %>%
    mutate(gp = replace(gp, gp=='', 0)) %>%
    mutate_at(c('overall','gp','age'),funs(as.numeric)) %>%
    select(overall,age,gp) %>%
    mutate(playerurl,draftyear)
  return(table)
}

### CAREER NHL GAMES FROM HOCKEY REF
hockeyref_player_games <- function(url,verbose=TRUE){
  beepr::beep_on_error(playerpage <- read_html(url))
  ps <- playerpage %>%
    html_nodes('#info div:nth-child(6) p') %>%
    html_text() %>%
    as.numeric() %>%
    tail(.,1)
  seasons <- playerpage %>%
    html_nodes('#stats_basic_plus_nhl tbody th') %>%
    html_text() %>%
    str_sub(.,-2,-1) %>%
    paste('20',.,sep='') %>%
    as.numeric() %>%
    unique()
  results <- c()
  if(seasons[1] != 20){
    for (i in 1:length(seasons)){
      log <- read_html(gsub('.html','',paste(url,'/gamelog/',seasons[i],sep='')))
      date <- log %>%
        html_nodes('.right+ .left a') %>%
        html_text() %>%
        ymd()
      team <- log %>%
        html_nodes('.center+ .left a') %>%
        html_text()
      games <- data.frame(date,team,stringsAsFactors = FALSE)
      results <- rbind(results,games)
    }
    results <- results %>%
      mutate(num = row_number(),
             playerurl = url,
             pointshares = ps)
  }
  if (verbose){
    print((playerpage %>% html_nodes('h1') %>% html_text()))
  }
  return(results)
}

### TEAM ABBREVIATIONS
abbr <- data.frame(short = c('TBL', 'STL', 'WPG', 'TOR', 'CBJ', 'NJD', 'NSH', 'VEG', 'NYI',
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
                            'Mighty Ducks of Anaheim', 'Phoenix Coyotes', 'Vegas Golden Knights'),
                   stringsAsFactors = FALSE)

### SCHEDULES FOR EACH TEAM FROM HOCKEY REF
hockeyref_schedule <- function(year){
  url <- read_html(paste('https://www.hockey-reference.com/leagues/NHL_',
                         year,'_games.html',sep=''))
  teamgames <- url %>%
    html_table() %>%
    .[[1]] %>%
    setNames(make.names(names(.), unique = TRUE)) %>%
    filter_at(3, all_vars(. != ''))
  teamgames <- data.frame(date = ymd(rep(teamgames$Date,2)),
                          year = year - 1,
                          long = c(teamgames$Home,teamgames$Visitor),
                          stringsAsFactors = FALSE) %>%
    arrange(date) %>%
    left_join(abbr,by='long') %>%
    select(-long) %>%
    group_by(short) %>%
    mutate(teamnum = row_number())
  print(year)
  return(teamgames)
}
