library(tidyverse)
library(lubridate)
library(stringr)

dats = list()

# Data is scraped at calander year level, not season
for(f in list.files("data/box_scores/final/", pattern = "\\d")){
  year = str_extract(f, "\\d+")
  assign(paste("dat", year, sep = "_"), read.csv(paste0("data/box_scores/final/", f)))
  dats = c(dats, as.name(paste("dat", year, sep = "_")))
}

# Do not include playoff games, correctly assign regular season games to respective year
regular_season_dates = data.frame(year = c(2004, 2005, 2006, 2007, 2008),
                                  start = c("2004-11-02", "2005-11-01", "2006-10-31", "2007-10-30", "2008-10-28"),
                                  end = c("2005-04-20", "2006-04-19", "2007-04-18", "2008-04-16", "2009-04-16"))

# Return NA if first game of the year, otherwise days since game t-1
closest_game = function(x) ifelse(is.infinite(min(x[x > 0])), NA, min(x[x > 0]))

force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}

# Aggregate player-game data to team-game level, calculate days of rest of team and opp 
prep_data <- function(dat){
  df_year = str_extract(deparse(substitute(dat)), "\\d+") %>% as.integer

  ##### Aggregate player level data up to team level #####
  dat <- dat %>% mutate(points = (made_field_goals - made_three_point_field_goals)*2 + made_three_point_field_goals*3 + made_free_throws)
  
  team_dat <- dat %>% group_by(team, opponent, location, date) %>%
                      summarise(pts = sum(points), 
                                pf = sum(personal_fouls)) %>% 
                      ungroup()
  
  team_dat <- team_dat %>% inner_join(team_dat, by = c("opponent" = "team", "date")) %>% 
                           filter(location.x == "HOME") %>%
                           mutate(date = dmy(date))
  
  ##### Filter for only regular season games #####
  team_dat <- filter(team_dat, date >= ymd(filter(regular_season_dates, year == df_year)[,2]),
                               date <= ymd(filter(regular_season_dates, year == df_year)[,3]))
  
    
  team_dat <- team_dat %>% rename(home_team = team,
                                  away_team = opponent,
                                  home_points = pts.x,
                                  away_points = pts.y,
                                  home_fouls = pf.x,
                                  away_fouls = pf.y) %>%
                           select(-opponent.y, -location.x, -location.y) %>% 
                           arrange(date)
  
  ##### Find most recent game for home team #####  
  home_home <- team_dat %>% left_join(team_dat, by = c("home_team")) %>% 
                            select(home_team, away_team.x, date.x, away_team.y, date.y) %>%
                            mutate(home_prior_location = "HOME", 
                                   days_since_last = difftime(date.x, date.y, units = "days")) %>%
                            group_by(home_team, date.x) %>%
                            filter(days_since_last == closest_game(days_since_last)) %>%
                            ungroup()
                            
  home_away <- team_dat %>% left_join(team_dat, by = c("home_team" = "away_team")) %>% 
                            select(home_team, away_team, date.x, home_team.y, date.y) %>%
                            mutate(home_prior_location = "AWAY", 
                                   days_since_last = difftime(date.x, date.y, units = "days")) %>%
                            group_by(home_team, date.x) %>%
                            filter(days_since_last == closest_game(days_since_last)) %>%
                            ungroup()
  
  home_prior <- force_bind(home_home, home_away) %>% 
                group_by(home_team, date.x) %>%
                filter(days_since_last == min(days_since_last))
  
  ##### Find most recent game for home team ##### 
  away_away <- team_dat %>% left_join(team_dat, by = c("away_team")) %>% 
                            select(home_team.x, away_team, date.x, home_team.y, date.y) %>%
                            mutate(away_prior_location = "AWAY", 
                                   days_since_last = difftime(date.x, date.y, units = "days")) %>%
                            group_by(away_team, date.x) %>%
                            filter(days_since_last == closest_game(days_since_last)) %>%
                            ungroup()
  
  away_home <- team_dat %>% left_join(team_dat, by = c("away_team" = "home_team")) %>% 
                            select(home_team, away_team, date.x, away_team.y, date.y) %>%
                            mutate(away_prior_location = "HOME", 
                            days_since_last = difftime(date.x, date.y, units = "days")) %>%
                            group_by(away_team, date.x) %>%
                            filter(days_since_last == closest_game(days_since_last)) %>%
                            ungroup()
  
  away_prior <- force_bind(away_away, away_home) %>% 
                group_by(away_team, date.x) %>%
                filter(days_since_last == min(days_since_last))
  
  
  ##### Join prior game data back to original table #####
  team_dat <- team_dat %>% left_join(home_prior, by = c("home_team", "date" = "date.x")) %>%
                           left_join(away_prior, by = c("away_team", "date" = "date.x")) %>%
                           select(home_team, away_team, date, home_points, home_fouls, away_points, away_fouls, 
                                  home_prior_location, days_since_last.x, away_prior_location, days_since_last.y) %>%
                           rename(home_days_rest = days_since_last.x,
                                  away_days_rest = days_since_last.y)
  
  team_dat$year <- df_year
  
  return(team_dat)
}

# Five seasons worth of data 
team_dat = bind_rows(prep_data(dat_2004), 
                     prep_data(dat_2005), 
                     prep_data(dat_2006), 
                     prep_data(dat_2007), 
                     prep_data(dat_2008))

#################################################################################################################################################
# Drop where at least one team is playing first game
team_dat_final <- team_dat %>% filter(complete.cases(.)) %>%
                               mutate(home_days_rest = ifelse(home_days_rest - 1 > 2, "3+", home_days_rest - 1) %>% as.factor(),
                                      away_days_rest = ifelse(away_days_rest - 1 > 2, "3+", away_days_rest - 1) %>% as.factor()) 
                                      #point_margin = home_points - away_points) #%>%
                               #select(home_team, away_team, year, home_days_rest, away_days_rest, point_margin)


set.seed(123)

# Sample games 
home_teams <- sample(seq_len(nrow(team_dat_final)), size = nrow(team_dat_final)/2)

# Keep all games
#team_dat_final <- bind_rows(select(team_dat_final, -away_points, -away_days_rest) %>%
#                  lice(home_teams) %>% mutate(treatment = 1) %>% 
#                  rename(team = home_team, opponent = away_team, points = home_points, days_rest = home_days_rest), 
#                  select(team_dat_final, -home_points, -home_days_rest) %>% slice(-home_teams) %>% mutate(treatment = 0) %>% rename(team = away_team, opponent = home_team, points = away_points, days_rest = away_days_rest))

# Randomly sample to avoid home/away days rest mirroring each other 
team_dat_final <- bind_rows(select(team_dat_final, -away_points) %>% slice(home_teams) %>% mutate(treatment = 1) %>% 
                            rename(team = home_team, opp = away_team, team_days_rest = home_days_rest, opp_days_rest = away_days_rest, points = home_points), 
                            select(team_dat_final, -home_points) %>% slice(-home_teams) %>% mutate(treatment = 0) %>% 
                            rename(team = away_team, opp = home_team, team_days_rest = away_days_rest, opp_days_rest = home_days_rest, points = away_points))

# Relative to 0 days of rest                               
team_dat_final <- team_dat_final %>% within(team_days_rest <- relevel(team_days_rest, ref = "0"))
team_dat_final <- team_dat_final %>% within(opp_days_rest <- relevel(opp_days_rest, ref = "0"))

#  Dummify rest days variable
dmy <- dummyVars(~ team_days_rest + opp_days_rest, data = team_dat_final, fullRank = T)
dmy <- data.frame(predict(dmy, newdata = team_dat_final))
                 
team_dat_final <- bind_cols(select(team_dat_final, team, opp, treatment, year), dmy, select(team_dat_final, points))

team_dat_final <- team_dat_final %>% mutate(year = as.factor(year),
                                            #team = ifelse(treatment == 1, as.character(home_team), as.character(away_team)),
                                            #opp = ifelse(treatment == -1, as.character(away_team), as.character(home_team)),
                                            #point_margin = ifelse(treatment == 1, point_margin, point_margin*-1),
                                            rest3. = team_days_rest.3. - opp_days_rest.3.,
                                            rest1 = team_days_rest.1 - opp_days_rest.1, 
                                            rest2 = team_days_rest.2 - opp_days_rest.2)  %>% 
                                            #rest3 = team_days_rest.3. - opp_days_rest.3.) %>% 
                                     select(team, opp, treatment, year, rest3., rest1, rest2, points)
                                     #select(team, opp, year, treatment, rest0, rest1, rest2, point_margin)
                                    
write_csv(team_dat_final, "data/output_data/matchup_final.csv")
