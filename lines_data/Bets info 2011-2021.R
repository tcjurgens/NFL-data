library(tidyverse)
library(tidyquant)
#library(DataEditR)
library(openxlsx)
#library(tableHTML)
library(glue)
library(nflfastR)
#library(DT)
#library(shinydashboard)
#library(shinydashboardPlus)
#library(bs4Dash)
#library(shiny)
#### Use this start of code when filtering for specific team data

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
gambling_info <-load_sharpe_data("games") %>% 
  filter(season %in% c(2011:2021)) %>%
  select(game_id, season, game_type, week, home_coach, away_coach, home_team, away_team, home_qb_name, away_qb_name,
         home_score, away_score, spread_line, result, home_moneyline, away_moneyline,
         total_line, total, overtime, home_rest, away_rest, div_game,
         weekday, gametime, referee, stadium, roof, surface, temp, wind) 

pbp <- load_pbp(2011:2021) %>%
  select(game_id, weather) %>%
  distinct() 

#merge the data to get all the info we need
gambling_info<- full_join(gambling_info, pbp, by = "game_id")  

#change gametime to character type column so its easier to filter 
gambling_info$gametime <- as.character(gambling_info$gametime)

#filter so favored home teams spread_line column will now be negative
for(row in 1:nrow(gambling_info)) {
  gambling_info[row,'spread_line'] <- (gambling_info[row,'spread_line'])*-1
}
#result column will be negative if the home team wins outright
for(row in 1:nrow(gambling_info)) {
  gambling_info[row,'result'] <- (gambling_info[row,'result'])*-1
}

#add spread and total results
####  ATS_win signifies that the home team has covered
gambling_info <- gambling_info %>%
  mutate(
    home_ATS_win = ifelse(result - spread_line < 0 , 1, 0),
    road_ATS_win = ifelse(home_ATS_win == 1, 0, 1),
    home_ATS_loss = road_ATS_win,
    road_ATS_loss = home_ATS_win,
    ATS_push = ifelse(result - spread_line == 0, 1, 0)
  ) %>%
  mutate(
    over = ifelse(total - total_line > 0, 1, 0),
    under = ifelse(total - total_line < 0, 1, 0),
    push = ifelse(total - total_line == 0, 1, 0)
  )

#change team names to match current 
gambling_info <- gambling_info %>%
  mutate(
    home_team = case_when(
      home_team == 'SD' ~ 'LAC',
      home_team == 'STL' ~ 'LA',
      home_team == 'OAK' ~ 'LV',
      TRUE ~ home_team
    ),
    away_team = case_when(
      away_team == 'SD' ~ 'LAC',
      away_team == 'STL' ~ 'LA',
      away_team == 'OAK' ~ 'LV',
      TRUE ~ away_team
    )
  )

#conferences
NFC <- c('ARI','ATL','CAR','CHI','DAL','DET','GB','LA','MIN','NO','NYG','PHI','SF','SEA','TB','WAS')
AFC <- c('BAL','BUF','CIN','CLE','DEN','HOU','IND','JAX','KC','LV','LAC','MIA','NE','NYJ','PIT','TEN')

#divisions
NFC_N <- c('CHI','DET','GB','MIN')
NFC_S <- c('ATL','CAR','NO','TB')
NFC_E <- c('DAL','NYG','PHI','WAS')
NFC_W <- c('ARI','LA','SF','SEA')

AFC_N <- c('BAL','CIN','CLE','PIT')
AFC_S <- c('HOU','IND','JAX','TEN')
AFC_E <- c('BUF','MIA','NE','NYJ')
AFC_W <- c('DEN','KC','LV','LAC')

#adding columns to signify divisional, conference games
gambling_info <- gambling_info %>%
  mutate(
    conference = ifelse((home_team %in% NFC) & (away_team %in% NFC), 'NFC', 
                        ifelse((home_team %in% AFC) & (away_team %in% AFC), 'AFC', 'Inter Conf.')),
    division = ifelse(div_game == 1, ifelse(home_team %in% NFC_E, 'NFC East',
                                      ifelse(home_team %in% NFC_S, 'NFC South',
                                      ifelse(home_team %in% NFC_N, 'NFC North',
                                      ifelse(home_team %in% NFC_W, 'NFC West',
                                      ifelse(home_team %in% AFC_E, 'AFC East',
                                      ifelse(home_team %in% AFC_S, 'AFC South',
                                      ifelse(home_team %in% AFC_N, 'AFC North',
                                      'AFC West'))))))),
                      'Inter Div.'),
    matchup = paste(home_team, "v", away_team),
    qb_matchup = paste(home_qb_name, "v", away_qb_name),
    coaching_matchup = paste(home_coach, "v", away_coach),
    total_result = case_when(
      over == 1 ~ 'Over',
      under == 1 ~ 'Under',
      push == 1 ~ 'Push'
      ),
    spread_result = case_when(
      ATS_win == 1 ~ "Home Team Covered",
      ATS_loss == 1 ~ "Visiting Team Covered",
      ATS_push == 1 ~ "Push"
      )
    )
  

#change col order for aesthetic
reorder <- c("game_id","season","game_type","week","matchup","home_team","away_team","home_score","away_score","result","spread_line","spread_result",
             "total","total_line","total_result","overtime","qb_matchup","home_qb_name","away_qb_name","coaching_matchup","home_coach","away_coach",
             "conference","division","home_rest","away_rest","weekday","gametime", "referee", "stadium","roof","surface","temp","wind",
             "weather", "home_ATS_win","road_ATS_win","home_ATS_loss","road_ATS_loss", "ATS_push","over","under","push","div_game","home_moneyline","away_moneyline"
             )
gambling_info <- gambling_info[,reorder]

# Want to look at the data in tabular form so its easily filterable, below I export it as a Excel file and CSV


## EXCEL
  #initialize workbook for excel
wb <- createWorkbook()
  #add a worksheet
addWorksheet(wb, sheetName = 'NFL betting')
  #add data
writeDataTable(wb, sheet = 'NFL betting', x = gambling_info)
  #and finally,  save the excel file
saveWorkbook(wb, '/Users/tcjurgens/Documents/Personal/NFL-Analytics-Project copy/betting_info.xlsx', overwrite = TRUE)



## save as a CSV file
write.csv(gambling_info,'/Users/tcjurgens/Documents/Data Analytics/final_project/betting_info.csv')


