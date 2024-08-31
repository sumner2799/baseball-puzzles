library(baseballr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)



######## PCA MLB KPIS - FOR VALIDATION ########

words_to_remove <- c("bunt")

pca_23 <- sc_2023 %>%
  filter(game_type == "R", batter == 691718, description == "hit_into_play", !str_detect(des, paste(words_to_remove, 
                                                                                                    collapse = "|"))) %>%
  mutate(spray_angle = round(
    (atan(
      (hc_x-125.42)/(198.27-hc_y)
    )*180/pi*.75)
    ,1)) %>%
  mutate(sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & stand == "R", "oppo",
                          ifelse(spray_angle < -15 & stand == "R", "pull",
                                 ifelse(spray_angle > 15 & stand == "L", "pull",
                                        ifelse(spray_angle < -15 & stand == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(batter) %>%
  summarise(`BBE` = n(),
            `Avg LA` = round(mean(launch_angle, na.rm = T),1),
            `LA Std Dev` = round(sd(launch_angle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull")/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo")/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle")/n()),1),
            `FB%` = round(100*(sum(bb_type == "fly_ball")/n()),1),
            `LD%` = round(100*(sum(bb_type == "line_drive")/n()),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball")/n()),1),
            `HH LA` = round(mean(launch_angle[launch_speed >= 95], na.rm = T),1))


######## PCA MLB KPIS - FOR VALIDATION ########



pca_mnl_23 <- mnl_sc_23 %>%
  filter(matchup.batter.id == 691718, !is.na(hitData.trajectory), !str_detect(result.description, paste(words_to_remove,
                                                                                                        collapse = "|"))) %>%
  mutate(spray_angle = round(
    (atan(
      (hitData.coordinates.coordX-125.42)/(198.27-hitData.coordinates.coordY)
    )*180/pi*.75)
    ,1)) %>%
  mutate(sweet_spot = ifelse(hitData.launchAngle >= 8 & hitData.launchAngle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & matchup.batSide.code == "R", "oppo",
                          ifelse(spray_angle < -15 & matchup.batSide.code == "R", "pull",
                                 ifelse(spray_angle > 15 & matchup.batSide.code == "L", "pull",
                                        ifelse(spray_angle < -15 & matchup.batSide.code == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(matchup.batter.id) %>%
  summarise(`BBE` = n(),
            `Avg LA` = round(mean(hitData.launchAngle, na.rm = T),1),
            `LA Std Dev` = round(sd(hitData.launchAngle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull")/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo")/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle")/n()),1),
            `FB%` = round(100*(sum(hitData.trajectory == "fly_ball")/n()),1),
            `LD%` = round(100*(sum(hitData.trajectory == "line_drive")/n()),1),
            `GB%` = round(100*(sum(hitData.trajectory == "ground_ball")/n()),1),
            `HH LA` = round(mean(hitData.launchAngle[hitData.launchSpeed >= 95], na.rm = T),1))


######## SAMPLE GATHERING ########

mnl_sc_23 <- mnl_sc_23 %>%
  filter(home_level_name == "Triple-A", away_level_name == "Triple-A") %>%
  mutate(season = 2023)

mnl_sc_24 <- mnl_sc_24 %>%
  filter(home_level_name == "Triple-A", away_level_name == "Triple-A") %>%
  mutate(season = 2024)

players_23 <- try(mlb_sports_players(sport_id = 1, season = 2023))
players_24 <- try(mlb_sports_players(sport_id = 1, season = 2024))

common_cols_players <- intersect(colnames(players_23), colnames(players_24))


total_players <- rbind(
  subset(players_23, select = common_cols_players), 
  subset(players_24, select = common_cols_players)
)

debuts <- total_players %>%
  filter(mlb_debut_date >= "2023-01-01" & mlb_debut_date <= "2024-08-01") %>%
  distinct(player_id,.keep_all = TRUE)

common_cols_data <- intersect(colnames(mnl_sc_24), colnames(mnl_sc_23))


total_mnl <- rbind(
  subset(mnl_sc_23, select = common_cols_data), 
  subset(mnl_sc_24, select = common_cols_data)
)


mnl_sample <- inner_join(total_mnl,debuts,by=c("matchup.batter.id"="player_id"))

######## ADD LOGIC ########

mnl_profile <- mnl_sample %>%
  dplyr::filter(!is.na(hitData.trajectory), !str_detect(result.description, paste(words_to_remove,
                                                                           collapse = "|"))) %>%
  dplyr::mutate(spray_angle = round(
    (atan(
      (hitData.coordinates.coordX-125.42)/(198.27-hitData.coordinates.coordY)
    )*180/pi*.75)
    ,1)) %>%
  dplyr::mutate(sweet_spot = ifelse(hitData.launchAngle >= 8 & hitData.launchAngle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & matchup.batSide.code == "R", "oppo",
                          ifelse(spray_angle < -15 & matchup.batSide.code == "R", "pull",
                                 ifelse(spray_angle > 15 & matchup.batSide.code == "L", "pull",
                                        ifelse(spray_angle < -15 & matchup.batSide.code == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA))))),
         when_debut = ifelse(game_date <= mlb_debut_date,0,1)) %>%
  dplyr::group_by(matchup.batter.id, when_debut, mlb_debut_date) %>%
  dplyr::summarise(`BBE` = n(),
            `Avg LA` = round(mean(hitData.launchAngle, na.rm = T),1),
            `LA Std Dev` = round(sd(hitData.launchAngle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull", na.rm = T)/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo", na.rm = T)/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle", na.rm = T)/n()),1),
            `FB%` = round(100*(sum(hitData.trajectory == "fly_ball", na.rm = T)/n()),1),
            `LD%` = round(100*(sum(hitData.trajectory == "line_drive", na.rm = T)/n()),1),
            `GB%` = round(100*(sum(hitData.trajectory == "ground_ball", na.rm = T)/n()),1),
            `HH LA` = round(mean(hitData.launchAngle[hitData.launchSpeed >= 95], na.rm = T),1),
            `Stint Start` = min(game_date))

add_flag <- mnl_profile %>%
  group_by(matchup.batter.id) %>%
  summarise(flag = ifelse(n() <= 1,0,
                          ifelse(n()>1 & (`BBE`[when_debut == 0] < `BBE`[when_debut == 1]),1,0)))

bring_back <- left_join(mnl_profile,add_flag,by=c("matchup.batter.id")) %>%
  dplyr::group_by(matchup.batter.id) %>%
  dplyr::reframe(`BBE` = ifelse(n()<=1,`BBE`[when_debut == 0],
                                ifelse(n()>1,`BBE`[when_debut == flag],`BBE`[when_debut == 0])),
                   `Avg LA` = ifelse(n()<=1,`Avg LA`[when_debut == 0],
                                     ifelse(n()>1,`Avg LA`[when_debut == flag],`Avg LA`[when_debut == 0])),
                 `LA Std Dev` = ifelse(n()<=1,`LA Std Dev`[when_debut == 0],
                                   ifelse(n()>1,`LA Std Dev`[when_debut == flag],`LA Std Dev`[when_debut == 0])),
                 `Sweet Spot%` = ifelse(n()<=1,`Sweet Spot%`[when_debut == 0],
                                       ifelse(n()>1,`Sweet Spot%`[when_debut == flag],`Sweet Spot%`[when_debut == 0])),
                 `Pull%` = ifelse(n()<=1,`Pull%`[when_debut == 0],
                                        ifelse(n()>1,`Pull%`[when_debut == flag],`Pull%`[when_debut == 0])),
                 `Mid%` = ifelse(n()<=1,`Mid%`[when_debut == 0],
                                  ifelse(n()>1,`Mid%`[when_debut == flag],`Mid%`[when_debut == 0])),
                 `Stint End` = ifelse(n()<=1,"2026-10-12",
                                      ifelse(n()>1 & flag == 1,"2026-10-12",
                                             ifelse(n()>1 & flag == 0, `Stint Start`[when_debut != flag],NA))),
                 `Oppo%` = ifelse(n()<=1,`Oppo%`[when_debut == 0],
                                 ifelse(n()>1,`Oppo%`[when_debut == flag],`Oppo%`[when_debut == 0])),
                 `FB%` = ifelse(n()<=1,`FB%`[when_debut == 0],
                                  ifelse(n()>1,`FB%`[when_debut == flag],`FB%`[when_debut == 0])),
                 `LD%` = ifelse(n()<=1,`LD%`[when_debut == 0],
                                ifelse(n()>1,`LD%`[when_debut == flag],`LD%`[when_debut == 0])),
                 `GB%` = ifelse(n()<=1,`GB%`[when_debut == 0],
                                ifelse(n()>1,`GB%`[when_debut == flag],`GB%`[when_debut == 0])),
                 `HH LA` = ifelse(n()<=1,`HH LA`[when_debut == 0],
                                ifelse(n()>1,`HH LA`[when_debut == flag],`HH LA`[when_debut == 0])),
                 `Stint Start` = ifelse(n()<=1,`Stint Start`[when_debut == 0],
                                        ifelse(n()>1,`Stint Start`[when_debut == flag],`Stint Start`[when_debut == 0])))


total_mlb <- rbind(sc_2023,sc_2024)


filter_mlb <- inner_join(total_mlb,bring_back, by = c("batter" = "matchup.batter.id")) %>%
  filter(game_type == "R", description == "hit_into_play",!str_detect(des, paste(words_to_remove, collapse = "|"))) %>%
  mutate(spray_angle = round(
    (atan(
      (hc_x-125.42)/(198.27-hc_y)
    )*180/pi*.75)
    ,1),
    true_stint = ifelse(game_date >= `Stint Start` & game_date < `Stint End`,1,
                        ifelse(game_date >= `Stint End`,2,NA))) %>%
  mutate(sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & stand == "R", "oppo",
                          ifelse(spray_angle < -15 & stand == "R", "pull",
                                 ifelse(spray_angle > 15 & stand == "L", "pull",
                                        ifelse(spray_angle < -15 & stand == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(batter,true_stint) %>%
  summarise(`BBE` = n(),
            `Avg LA` = round(mean(launch_angle, na.rm = T),1),
            `LA Std Dev` = round(sd(launch_angle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull", na.rm = T)/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo", na.rm = T)/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle", na.rm = T)/n()),1),
            `FB%` = round(100*(sum(bb_type == "fly_ball", na.rm = T)/n()),1),
            `LD%` = round(100*(sum(bb_type == "line_drive", na.rm = T)/n()),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball", na.rm = T)/n()),1),
            `HH LA` = round(mean(launch_angle[launch_speed >= 95], na.rm = T),1)) 

# filter_mlb <- total_mlb %>%  ## CALCULATE KPIS FOR ALL MLB HITTERS (NOT REALLY USED IN THIS RESEARCH)
#   filter(game_year == 2023, game_type == "R", description == "hit_into_play",!str_detect(des, paste(words_to_remove, collapse = "|"))) %>%
#   # filter(game_date >= `Stint Start` & game_date < `Stint End`, game_type == "R", description == "hit_into_play",
#   #        !str_detect(des, paste(words_to_remove, collapse = "|"))) %>%
#   mutate(spray_angle = round(
#     (atan(
#       (hc_x-125.42)/(198.27-hc_y)
#     )*180/pi*.75)
#     ,1)) %>%
#   mutate(sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32,1,0),
#          bip_dir = ifelse(spray_angle > 15 & stand == "R", "oppo",
#                           ifelse(spray_angle < -15 & stand == "R", "pull",
#                                  ifelse(spray_angle > 15 & stand == "L", "pull",
#                                         ifelse(spray_angle < -15 & stand == "L", "oppo",
#                                                ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
#   # filter(batter == 676914) %>%
#   # select(des,hc_x,hc_y,spray_angle,bip_dir)
#   group_by(batter) %>%
#   summarise(`BBE` = n(),
#             `Avg LA` = round(mean(launch_angle, na.rm = T),1),
#             `LA Std Dev` = round(sd(launch_angle, na.rm = T),1),
#             `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
#             `Pull%` = round(100*(sum(bip_dir == "pull", na.rm = T)/n()),1),
#             `Oppo%` = round(100*(sum(bip_dir == "oppo", na.rm = T)/n()),1),
#             `Mid%` = round(100*(sum(bip_dir == "middle", na.rm = T)/n()),1),
#             `FB%` = round(100*(sum(bb_type == "fly_ball", na.rm = T)/n()),1),
#             `LD%` = round(100*(sum(bb_type == "line_drive", na.rm = T)/n()),1),
#             `GB%` = round(100*(sum(bb_type == "ground_ball", na.rm = T)/n()),1),
#             `HH LA` = round(mean(launch_angle[launch_speed >= 95], na.rm = T),1)) %>%
#   filter(`BBE` >= 80)

true_only <- filter_mlb %>%
  filter(true_stint == 1, BBE < 80)

final_true <- filter_mlb %>%
  filter(true_stint == 1, BBE >= 80)

small_sample <- as.list(true_only$batter)

after_guys <- filter_mlb %>%
  filter(true_stint == 2, batter %in% small_sample, BBE >= 80)

after_list <- as.list(after_guys$batter)

combined_guys <- inner_join(mnl_sample,after_guys,by=c("matchup.batter.id" = "batter")) %>%
  filter(!is.na(hitData.trajectory), !str_detect(result.description, paste(words_to_remove,
                                                                           collapse = "|"))) %>% 
  dplyr::mutate(spray_angle = round(
    (atan(
      (hitData.coordinates.coordX-125.42)/(198.27-hitData.coordinates.coordY)
    )*180/pi*.75)
    ,1)) %>%
  dplyr::mutate(sweet_spot = ifelse(hitData.launchAngle >= 8 & hitData.launchAngle <= 32,1,0),
                bip_dir = ifelse(spray_angle > 15 & matchup.batSide.code == "R", "oppo",
                                 ifelse(spray_angle < -15 & matchup.batSide.code == "R", "pull",
                                        ifelse(spray_angle > 15 & matchup.batSide.code == "L", "pull",
                                               ifelse(spray_angle < -15 & matchup.batSide.code == "L", "oppo",
                                                      ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(matchup.batter.id) %>%
  dplyr::summarise(`BBE` = n(),
                   `Avg LA` = round(mean(hitData.launchAngle, na.rm = T),1),
                   `LA Std Dev` = round(sd(hitData.launchAngle, na.rm = T),1),
                   `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
                   `Pull%` = round(100*(sum(bip_dir == "pull", na.rm = T)/n()),1),
                   `Oppo%` = round(100*(sum(bip_dir == "oppo", na.rm = T)/n()),1),
                   `Mid%` = round(100*(sum(bip_dir == "middle", na.rm = T)/n()),1),
                   `FB%` = round(100*(sum(hitData.trajectory == "fly_ball", na.rm = T)/n()),1),
                   `LD%` = round(100*(sum(hitData.trajectory == "line_drive", na.rm = T)/n()),1),
                   `GB%` = round(100*(sum(hitData.trajectory == "ground_ball", na.rm = T)/n()),1),
                   `HH LA` = round(mean(hitData.launchAngle[hitData.launchSpeed >= 95], na.rm = T),1))

final_list <- rbind(final_true,after_guys)

leave_extras <- bring_back %>%
  filter(!matchup.batter.id %in% after_list, !is.na(BBE)) %>%
  select(-c(`Stint Start`, `Stint End`))
  
final_mnl <- rbind(leave_extras,combined_guys)

profile_comp <- left_join(final_list,final_mnl,by=c("batter"="matchup.batter.id")) %>%
  filter(BBE.x >= 80, BBE.y >= 80)


######## SIMILARITY SCORE ########

sim_scores <- profile_comp %>%
  mutate(score = 1 - ((((((`Avg LA.x`/28.9)-(`Avg LA.y`/28.9))**2) +
           (((`LA Std Dev.x`/34.0)-(`LA Std Dev.y`/34.0))**2)+
           (((`Sweet Spot%.x`/48.4)-(`Sweet Spot%.y`/48.4))**2)+
           (((`Pull%.x`/59.6)-(`Pull%.y`/59.6))**2)+
           (((`Oppo%.x`/40.9)-(`Oppo%.y`/40.9))**2)+
           (((`Mid%.x`/51.7)-(`Mid%.y`/51.7))**2)+
           (((`FB%.x`/41.7)-(`FB%.y`/41.7))**2)+
           (((`LD%.x`/34.3)-(`LD%.y`/34.3))**2)+
           (((`GB%.x`/61.4)-(`GB%.y`/61.4))**2)+
           (((`HH LA.x`/36.6)-(`HH LA.y`/36.6))**2))/10)**0.5))


######## INVESTIGATE NA VALUES ########

nulls <- bring_back %>%
  filter(is.na(BBE))

list_nulls <- as.list(nulls$matchup.batter.id)

######## MnL %TILES ########

mnl_used <- as.list(final_mnl$matchup.batter.id)

other_mnl_kpis <- total_mnl %>%
  filter(!is.na(hitData.trajectory), !str_detect(result.description, paste(words_to_remove,
                                                                           collapse = "|")), home_level_name == "Triple-A", away_level_name == "Triple-A",
         !matchup.batter.id %in% mnl_used) %>%
  mutate(spray_angle = round(
    (atan(
      (hitData.coordinates.coordX-125.42)/(198.27-hitData.coordinates.coordY)
    )*180/pi*.75)
    ,1)) %>%
  mutate(sweet_spot = ifelse(hitData.launchAngle >= 8 & hitData.launchAngle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & matchup.batSide.code == "R", "oppo",
                          ifelse(spray_angle < -15 & matchup.batSide.code == "R", "pull",
                                 ifelse(spray_angle > 15 & matchup.batSide.code == "L", "pull",
                                        ifelse(spray_angle < -15 & matchup.batSide.code == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(matchup.batter.id, season) %>%
  summarise(`BBE` = n(),
            `Avg LA` = round(mean(hitData.launchAngle, na.rm = T),1),
            `LA Std Dev` = round(sd(hitData.launchAngle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull")/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo")/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle")/n()),1),
            `FB%` = round(100*(sum(hitData.trajectory == "fly_ball")/n()),1),
            `LD%` = round(100*(sum(hitData.trajectory == "line_drive")/n()),1),
            `GB%` = round(100*(sum(hitData.trajectory == "ground_ball")/n()),1),
            `HH LA` = round(mean(hitData.launchAngle[hitData.launchSpeed >= 95], na.rm = T),1)) %>%
  filter(BBE >= 80)


final_mnl <- final_mnl %>%
  mutate(season = 2324)


p_tiles_mnl <- rbind(final_mnl,other_mnl_kpis) %>%
  filter(BBE >= 80) %>%
  dplyr::arrange(desc(`Avg LA`)) %>% 
  dplyr::ungroup() %>%
  mutate(la_rank = round(percent_rank(`Avg LA`)*100,1)) %>%
  dplyr::arrange(`LA Std Dev`) %>% 
  dplyr::ungroup() %>%
  mutate(sdla_rank = round(percent_rank(desc(`LA Std Dev`))*100,1)) %>%
  dplyr::arrange(desc(`Sweet Spot%`)) %>% 
  dplyr::ungroup() %>%
  mutate(sweet_rank = round(percent_rank(`Sweet Spot%`)*100,1)) %>%
  dplyr::arrange(desc(`Pull%`)) %>% 
  dplyr::ungroup() %>%
  mutate(pull_rank = round(percent_rank(`Pull%`)*100,1)) %>%
  dplyr::arrange(desc(`Oppo%`)) %>% 
  dplyr::ungroup() %>%
  mutate(oppo_rank = round(percent_rank(`Oppo%`)*100,1)) %>%
  dplyr::arrange(desc(`Mid%`)) %>% 
  dplyr::ungroup() %>%
  mutate(mid_rank = round(percent_rank(`Mid%`)*100,1)) %>%
  dplyr::arrange(desc(`FB%`)) %>% 
  dplyr::ungroup() %>%
  mutate(fb_rank = round(percent_rank(`FB%`)*100,1)) %>%
  dplyr::arrange(desc(`LD%`)) %>% 
  dplyr::ungroup() %>%
  mutate(ld_rank = round(percent_rank(`LD%`)*100,1)) %>%
  dplyr::arrange(desc(`GB%`)) %>% 
  dplyr::ungroup() %>%
  mutate(gb_rank = round(percent_rank(`GB%`)*100,1)) %>%
  dplyr::arrange(desc(`HH LA`)) %>% 
  dplyr::ungroup() %>%
  mutate(hhla_rank = round(percent_rank(`HH LA`)*100,1)) %>%
  # select(matchup.batter.id,season,BBE,14:23) %>%
  mutate(level = "MiLB") %>%
  rename(game_year = season,
         batter = matchup.batter.id)
  

######## MLB %TILES ########

mlb_used <- as.list(final_list$batter)

other_mlb_kpis <- total_mlb %>%
  filter(game_type == "R", description == "hit_into_play",!str_detect(des, paste(words_to_remove, collapse = "|")), !batter %in% mlb_used) %>%
  mutate(spray_angle = round(
    (atan(
      (hc_x-125.42)/(198.27-hc_y)
    )*180/pi*.75)
    ,1)) %>%
  mutate(sweet_spot = ifelse(launch_angle >= 8 & launch_angle <= 32,1,0),
         bip_dir = ifelse(spray_angle > 15 & stand == "R", "oppo",
                          ifelse(spray_angle < -15 & stand == "R", "pull",
                                 ifelse(spray_angle > 15 & stand == "L", "pull",
                                        ifelse(spray_angle < -15 & stand == "L", "oppo",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(batter,game_year) %>%
  summarise(`BBE` = n(),
            `Avg LA` = round(mean(launch_angle, na.rm = T),1),
            `LA Std Dev` = round(sd(launch_angle, na.rm = T),1),
            `Sweet Spot%` = round(100*(sum(sweet_spot, na.rm = T)/n()),1),
            `Pull%` = round(100*(sum(bip_dir == "pull", na.rm = T)/n()),1),
            `Oppo%` = round(100*(sum(bip_dir == "oppo", na.rm = T)/n()),1),
            `Mid%` = round(100*(sum(bip_dir == "middle", na.rm = T)/n()),1),
            `FB%` = round(100*(sum(bb_type == "fly_ball", na.rm = T)/n()),1),
            `LD%` = round(100*(sum(bb_type == "line_drive", na.rm = T)/n()),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball", na.rm = T)/n()),1),
            `HH LA` = round(mean(launch_angle[launch_speed >= 95], na.rm = T),1)) 


final_list <- final_list %>%
  mutate(game_year = 2324)


p_tiles_mlb <- rbind(final_list,other_mlb_kpis) %>%
  filter(BBE >= 80) %>%
  dplyr::arrange(desc(`Avg LA`)) %>% 
  dplyr::ungroup() %>%
  mutate(la_rank = round(percent_rank(`Avg LA`)*100,1)) %>%
  dplyr::arrange(`LA Std Dev`) %>% 
  dplyr::ungroup() %>%
  mutate(sdla_rank = round(percent_rank(desc(`LA Std Dev`))*100,1)) %>%
  dplyr::arrange(desc(`Sweet Spot%`)) %>% 
  dplyr::ungroup() %>%
  mutate(sweet_rank = round(percent_rank(`Sweet Spot%`)*100,1)) %>%
  dplyr::arrange(desc(`Pull%`)) %>% 
  dplyr::ungroup() %>%
  mutate(pull_rank = round(percent_rank(`Pull%`)*100,1)) %>%
  dplyr::arrange(desc(`Oppo%`)) %>% 
  dplyr::ungroup() %>%
  mutate(oppo_rank = round(percent_rank(`Oppo%`)*100,1)) %>%
  dplyr::arrange(desc(`Mid%`)) %>% 
  dplyr::ungroup() %>%
  mutate(mid_rank = round(percent_rank(`Mid%`)*100,1)) %>%
  dplyr::arrange(desc(`FB%`)) %>% 
  dplyr::ungroup() %>%
  mutate(fb_rank = round(percent_rank(`FB%`)*100,1)) %>%
  dplyr::arrange(desc(`LD%`)) %>% 
  dplyr::ungroup() %>%
  mutate(ld_rank = round(percent_rank(`LD%`)*100,1)) %>%
  dplyr::arrange(desc(`GB%`)) %>% 
  dplyr::ungroup() %>%
  mutate(gb_rank = round(percent_rank(`GB%`)*100,1)) %>%
  dplyr::arrange(desc(`HH LA`)) %>% 
  dplyr::ungroup() %>%
  mutate(hhla_rank = round(percent_rank(`HH LA`)*100,1)) %>%
  select(-c(true_stint)) %>%
  # select(batter,game_year,BBE,14:24) %>%
  mutate(level = "MLB")

######## PLOT METRIC DISTRIBUTIONS ########

total_ptiles <- rbind(p_tiles_mlb,p_tiles_mnl)

data_long <- pivot_longer(total_ptiles, 
                          cols = c(`Avg LA`,`LA Std Dev`,`Sweet Spot%`,`Pull%`,`Mid%`,`Oppo%`,`FB%`,`GB%`,`LD%`,`HH LA`), 
                          names_to = "metric", values_to = "value")

ggplot(data_long, aes(x = value, fill = level)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ metric, scales = "free")
