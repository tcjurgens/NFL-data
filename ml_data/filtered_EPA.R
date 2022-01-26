library(tidyverse)
library(nflfastr)
library(glue)

pbp <- load_pbp(2011:2021)

games <- pbp %>% 
  select(game_id, season, week, home_team, away_team, posteam, defteam, play_type, epa, home_score, away_score, total, result)

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
scheduled_games <- load_sharpe_data("games") %>% filter(season %in% c(2011:2021)) %>%
  select(game_id, home_qb_name, away_qb_name, spread_line, total_line)

for(row in 1:nrow(scheduled_games)) {
  scheduled_games[row,'spread_line'] <- (scheduled_games[row,'spread_line'])*-1
}
for(row in 1:nrow(games)) {
  games[row,'result'] <- (games[row,'result'])*-1
}

ML_data <- full_join(scheduled_games, games, by = "game_id")


offensive_epa_data <- ML_data %>%
  filter(!is.na(epa), !is.na(posteam), play_type == "pass" | play_type == "run" ) %>%
  group_by(game_id, season, week, posteam, home_team, away_team, home_qb_name, away_qb_name) %>%
  summarize(off_epa = mean(epa),
            off_pass_epa = mean(epa[play_type == "pass"]),
            off_rush_epa = mean(epa[play_type == "run"]),
            .groups = "drop") 

home_games <- offensive_epa_data %>% 
  filter(home_team == posteam) %>% 
  select(game_id, season, week, home_team, home_qb_name, off_epa) %>%
  rename(home_off_epa = off_epa)
away_games <- offensive_epa_data %>%
  filter(away_team == posteam) %>%
  select(game_id, season, week, away_team, away_qb_name, off_epa) %>%
  rename(away_off_epa = off_epa)

# epa/play filtered by qbs
qbs_epa <- left_join(home_games, away_games, by = c('game_id','season','week'))

defensive_epa_data <- ML_data %>%
  filter(!is.na(epa), !is.na(posteam), play_type == "pass" | play_type == "run" ) %>%
  group_by(game_id, season, week, defteam, home_team, away_team) %>%
  summarize(def_epa = mean(epa),
            def_pass_epa = mean(epa[play_type == "pass"]),
            def_rush_epa = mean(epa[play_type == "run"]),
            .groups = "drop") 
home_defense <- defensive_epa_data %>%
  filter(home_team == defteam) %>%
  select(game_id, season, week, home_team, def_epa, def_pass_epa, def_rush_epa)
away_defense <- defensive_epa_data %>%
  filter(away_team == defteam) %>% 
  select(game_id, season, week, away_team, def_epa, def_pass_epa, def_rush_epa)
def_epa <- left_join(home_defense, away_defense, by = c('game_id','season','week')) %>%
  rename(
    def_epa_home = def_epa.x,
    def_pass_epa_home = def_pass_epa.x,
    def_rush_epa_home = def_rush_epa.x,
    def_epa_away = def_epa.y,
    def_pass_epa_away = def_pass_epa.y,
    def_rush_epa_away = def_rush_epa.y 
  )


# merge for the machine learning mmodel / preds
ml_epa_data <- left_join(qbs_epa,def_epa, by = c('game_id','home_team','away_team','season','week'))

write.csv(ml_epa_data,'/Users/tcjurgens/Documents/Personal/NFL-lines/ml_data/epa_data.csv')

