library(dplyr)
library(baseballr)

############# READ IN - 2021-2024, 100 PA min, Regular season, Group By: Player & Year #############

df_mlb <- read.csv("/Users/andrewsumner/Downloads/BB_MLB2.csv")
df_aaa <- read.csv("/Users/andrewsumner/Downloads/BB_AAA34.csv")

############# LIMIT DATA TO MLB DEBUTS 2021-2024 #############

players_21 <- try(mlb_sports_players(sport_id = 1, season = 2021))
players_22 <- try(mlb_sports_players(sport_id = 1, season = 2022))
players_23 <- try(mlb_sports_players(sport_id = 1, season = 2023))
players_24 <- try(mlb_sports_players(sport_id = 1, season = 2024))

common_cols_players <- intersect(colnames(players_23), colnames(players_24))


total_players <- rbind(
  subset(players_21, select = common_cols_players), 
  subset(players_22, select = common_cols_players),
  subset(players_23, select = common_cols_players), 
  subset(players_24, select = common_cols_players)
)

debuts <- total_players %>%
  filter(mlb_debut_date >= "2021-01-01" & mlb_debut_date <= "2024-12-31", primary_position_type != "Pitcher") %>%
  distinct(player_id,.keep_all = TRUE)


first_year <- df_mlb %>%  # First season in dataset for all hitters
  group_by(player_id) %>%
  filter(year == min(year))

limit_mlb <- inner_join(first_year,debuts,by=c("player_id")) # Limit to debut seasons (that meet min PA)

last_year <- df_aaa %>%
  mutate(year = as.numeric(year))

add_debut_year <- limit_mlb %>%
  mutate(debut_year = as.numeric(format(as.Date(mlb_debut_date),"%Y")))

total_df <- inner_join(add_debut_year,last_year,by=c("player_id"))

############# IDENTIFY, AND ADD CONDITIONS FOR, PLAYERS W/ MULTIPLE MnL SZNS #############

mult_seasons <- last_year %>%
  group_by(player_id) %>%
  summarise(tot = n(),
            mult_flag = 1) %>%
  filter(tot > 1)

add_flag <- left_join(total_df,mult_seasons,by=c("player_id"))

manip_mult <- add_flag %>%
  filter(mult_flag == 1, year.y <= year.x) %>%  # filter for players w/ multiple seasons, and limit to years that took place during or before their first MLB seasons
  group_by(player_id) %>%
  summarise(year.y = max(year.y)) # Take the max year of the options that met the criteria above

singles <- add_flag %>%
  filter(is.na(mult_flag), year.y <= year.x)

mults <- add_flag %>%
  filter(mult_flag == 1)

filt_mult <- inner_join(mults,manip_mult,by=c("player_id","year.y"))

final_df <- rbind(singles,filt_mult)

############# CALCULATE AVERAGE WALK DIFFERENTIAL BY YEAR #############

bb_diff <- final_df %>%
  mutate(diff = bb_percent.x-bb_percent.y) %>%
  group_by(year.x) %>%
  # group_by() %>% # use this group by for average differential overall
  summarise(tot = n(),
            avg_diff = mean(diff,na.rm = T))


############# PLOT SMOOTHED DISTRIBUTION OF BB% DIFFERENTIAL USING DENSITY PLOT #############

player_diff <- final_df %>%
  mutate(diff = bb_percent.x-bb_percent.y)

plot(density(player_diff$diff), 
     main = "Density Plot of BB% Diff", 
     xlab = "BB% (MLB - AAA)", 
     col = "blue", 
     lwd = 2)

