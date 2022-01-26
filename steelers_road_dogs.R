library(tidyverse)
library(glue)

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
gambling_dataf<-load_sharpe_data("games") %>%
  select(season, week, game_type, home_coach, away_coach, home_team, away_team, home_qb_name, away_qb_name,
         home_score, away_score, spread_line, result, home_moneyline, away_moneyline,
         total_line, total, overtime, home_rest, away_rest,
         weekday, gametime)

pit_games <- gambling_dataf %>%
  filter(away_team == 'PIT',
         away_coach == 'Mike Tomlin',
         away_qb_name == 'Ben Roethlisberger',
         game_type == 'REG')

#filter so favored home teams spread_line column will now be negative
for(row in 1:nrow(pit_games)) {
  pit_games[row,'spread_line'] <- (pit_games[row,'spread_line'])*-1
}
#result column will be negative if the home team wins outright
for(row in 1:nrow(pit_games)) {
  pit_games[row,'result'] <- (pit_games[row,'result'])*-1
}

pit_games <- pit_games %>%
  select(season, week, home_team, away_team, home_coach, home_qb_name, away_coach, away_qb_name,
         home_score, away_score, spread_line, result) %>%
  mutate(
    home_ATS_win = ifelse(result - spread_line < 0 , 1, 0),
    road_ATS_win = ifelse(result - spread_line > 0, 1, 0),
    home_ATS_loss = road_ATS_win,
    road_ATS_loss = home_ATS_win,
    ATS_push = ifelse(result - spread_line == 0, 1, 0),
    home_win = ifelse(result < 0, 1,0),
    road_win = ifelse(result > 0, 1,0),
    tie = ifelse(result == 0, 1,0),
    road_teaser_line = (spread_line - 6) * -1,
    home_teaser_line = spread_line + 6,
    road_teaser_win = case_when(
      spread_line < 0 & result < 0 ~ ifelse(road_teaser_line > (result * -1), 1, 0),
      spread_line > 0 & result > 0 ~ ifelse(road_teaser_line < result, 1, 0),
      spread_line < 0 & result > 0 ~ 1,
      spread_line > 0 & result < 0 ~ ifelse(road_teaser_line > (result * -1), 1, 0)
    ),
    home_teaser_win = case_when(
      spread_line < 0 & result < 0 ~ ifelse(home_teaser_line > result, 1, 0),
      spread_line > 0 & result > 0 ~ ifelse(home_teaser_line > result, 1, 0),
      spread_line < 0 & result > 0 ~ ifelse(home_teaser_line > result, 1, 0),
      spread_line > 0 & result < 0 ~ 1
    ),
    road_teaser_push = case_when(
      result < 0 ~ ifelse(result == (home_teaser_line * -1), 1, 0),
      result > 0 ~ ifelse(result == (home_teaser_line * -1), 1, 0),
      result == 0 ~ ifelse(result == (home_teaser_line * -1), 1, 0)
    ),
    home_teaser_push = case_when(
      result < 0 ~ ifelse(result == home_teaser_line, 1, 0),
      result > 0 ~ ifelse(result == home_teaser_line, 1, 0),
      result == 0 ~ ifelse(result == home_teaser_line, 1, 0)
    )
  )

tease <- pit_games %>%
  select(
    season, week, home_team, away_team, home_score, away_score, spread_line, result, 
    road_teaser_line, home_teaser_line, road_teaser_win, home_teaser_win, road_teaser_push, home_teaser_push
  )

pit_games <- pit_games %>% 
  filter(spread_line < 0)   # home team favored

road_ats_wins <- (sum(pit_games$road_ATS_win, na.rm = TRUE))
road_ats_losses <- (sum(pit_games$road_ATS_loss, na.rm = TRUE))
ats_push <- (sum(pit_games$ATS_push, na.rm = TRUE)) 

tomlin_teaser_wins <- sum(pit_games$road_teaser_win, na.rm = TRUE)



