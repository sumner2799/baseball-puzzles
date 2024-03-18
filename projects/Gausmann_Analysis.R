library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)
library(gtable)
library(cowplot)


rv <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", pitch_name == "4-Seam Fastball", game_type.x == "R", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(run_v = sum(delta_run_exp,na.rm = T))


fullBall <- 0.236
halfBall <- fullBall/2
posZone <- 0.708
negZone <- -0.708
topZone <- 3.5
botZone <- 1.5
vertRight <- posZone/3
vertLeft <- negZone/3
horzTop <- botZone + 2*(topZone-botZone)/3
horzBottom <- botZone + (topZone-botZone)/3

homePlatePitcher <- data.frame(x = c(0, posZone, posZone, negZone, negZone, 0), y = c(0.5, 0.25, 0, 0, 0.25, 0.5))
homePlateBatter <- data.frame(x = c(0, negZone, negZone, posZone, posZone, 0), y = c(0, 0.25, 0.5, 0.5, 0.25, 0))


################### USAGE & RV/100 BY BATTER HANDEDNESS ###################

platoon_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball",
         strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`Season` = "2023",
            `# vR` = sum(stand == "R",na.rm = T),
            `% vR` = 100*round(sum(stand == "R",na.rm = T)/n(),3),
            `RV/100 vR` = -1*round(sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),2),
            `# vL` = sum(stand == "L",na.rm = T),
            `% vL` = 100*round(sum(stand == "L",na.rm = T)/n(),3),
            `RV/100 vL` = -1*round(sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100),2))

platoon_22 <- sc_2022 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`Season` = "2022",
            `# vR` = sum(stand == "R",na.rm = T),
            `% vR` = 100*round(sum(stand == "R",na.rm = T)/n(),3),
            `RV/100 vR` = -1*round(sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),2),
            `# vL` = sum(stand == "L",na.rm = T),
            `% vL` = 100*round(sum(stand == "L",na.rm = T)/n(),3),
            `RV/100 vL` = -1*round(sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100),2))

platoon_21 <- sc_2021 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`Season` = "2021",
            `# vR` = sum(stand == "R",na.rm = T),
            `% vR` = 100*round(sum(stand == "R",na.rm = T)/n(),3),
            `RV/100 vR` = -1*round(sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),2),
            `# vL` = sum(stand == "L",na.rm = T),
            `% vL` = 100*round(sum(stand == "L",na.rm = T)/n(),3),
            `RV/100 vL` = -1*round(sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100),2))

platoon_tot <- rbind(platoon_21,platoon_22,platoon_23) %>%
  select(-c(player_name))

table_theme <- ttheme_default(
  core = list(bg_params = list(fill = c("gray95", "gray90"))),
  colhead = list(fg_params = list(col = "gray95"),
                 bg_params = list(fill = "black"))
)



table_platoon <- tableGrob(platoon_tot, rows = NULL, theme = table_theme)

table_platoon <- gtable_add_grob(table_platoon, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_platoon), l = 1, r = ncol(table_platoon))

table_platoon <- gtable_add_grob(table_platoon, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_platoon))

format_table_platoon <- plot_grid(NULL, table_platoon, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))


################### GENERAL 2K COUNT DISTRIBUTIONS ###################

two_21 <- sc_2021 %>%
  filter(strikes == 2, game_type.x == "R")

two_22 <- sc_2022 %>%
  filter(strikes == 2, game_type.x == "R")

two_23 <- sc_2023 %>%
  filter(strikes == 2, game_type.x == "R")

two_tot <- rbind(two_21,two_22,two_23)

two_stats <- two_tot %>%
  group_by(balls,strikes) %>%
  summarise(tot = n(),
            rv = sum(delta_run_exp,na.rm = T),
            rv_100 = sum(delta_run_exp,na.rm = T)/(n()/100),
            avg_rv = mean(abs(delta_run_exp),na.rm=T),
            std_dev = sd(abs(delta_run_exp),na.rm = T))


################### GAUSMAN 2K COUNT DISTRIBUTIONS ###################

counts_21 <- sc_2021 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`0-2` = sum(balls == 0 & strikes == 2,na.rm = T)/n(),
            `1-2` = sum(balls == 1 & strikes == 2,na.rm = T)/n(),
            `2-2` = sum(balls == 2 & strikes == 2,na.rm = T)/n(),
            `3-2` = sum(balls == 3 & strikes == 2,na.rm = T)/n())

counts_22 <- sc_2022 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`0-2` = sum(balls == 0 & strikes == 2,na.rm = T)/n(),
            `1-2` = sum(balls == 1 & strikes == 2,na.rm = T)/n(),
            `2-2` = sum(balls == 2 & strikes == 2,na.rm = T)/n(),
            `3-2` = sum(balls == 3 & strikes == 2,na.rm = T)/n())

counts_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  group_by(player_name) %>%
  summarise(`0-2` = sum(balls == 0 & strikes == 2,na.rm = T)/n(),
            `1-2` = sum(balls == 1 & strikes == 2,na.rm = T)/n(),
            `2-2` = sum(balls == 2 & strikes == 2,na.rm = T)/n(),
            `3-2` = sum(balls == 3 & strikes == 2,na.rm = T)/n())

################### PERFORMANCE SPLITS ###################

# perf_21 <- sc_2021
# perf_22 <- sc_2022
perf_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
  mutate(bip_dir = ifelse(stand == "L" & spray_angle > 15,"pull",
                          ifelse(stand == "L" & spray_angle < -15, "oppo",
                                 ifelse(stand == "R" & spray_angle > 15, "oppo",
                                        ifelse(stand == "R" & spray_angle < -15, "pull",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(player_name,stand) %>%
  summarise(`Season` = "2023",
            `Pitches`= n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Strike%` = round(100*(sum(description %in% c("called_strike","foul","hit_into_play","swinging_strike",
                                                          "swinging_strike_blocked","foul_bunt"))/n()),1),
            `Zone%` = round(100*(sum(isZone == TRUE, na.rm = T)/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")))/
                                    sum(isZone == FALSE)),1),
            `CS%` = round(100*(sum(description == "called_strike",na.rm = T)/n()),1),
            `Foul%` = round(100*(sum(description == "foul",na.rm = T)/sum(description %in% c("foul","hit_into_play",
                                                                                             "swinging_strike",
                                                                                             "swinging_strike_blocked",
                                                                                             "foul_tip","foul_bunt"))),1),
            `Whiff%` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                         "foul_tip"),na.rm = T)/
                                    sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                           "swinging_strike_blocked","foul_tip",
                                                           "bunt_foul_tip","missed_bunt","foul_bunt"),
                                        na.rm = T)),1),
            `IZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip") & isZone == TRUE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt")
                                          & isZone == TRUE,na.rm = T)),1),
            `OZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip")& isZone == FALSE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt") &
                                            isZone == FALSE,na.rm = T)),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball" & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play",na.rm = T)),1),
            `HH%` = round(100*(sum(launch_speed >= 95 & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play", na.rm = T)),1),
            `xwOBAcon` = round(sum(estimated_woba_using_speedangle[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `wOBAcon` = round(sum(woba_value[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `pull` = round(100*(sum(bip_dir == "pull" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `oppo` = round(100*(sum(bip_dir == "oppo" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `middle` = round(100*(sum(bip_dir == "middle" & description=="hit_into_play",na.rm = T)/
                                    sum(description == "hit_into_play",na.rm = T)),1))

perf_tot <- rbind(perf_21,perf_22,perf_23) %>%
  filter(stand == "R") %>%
  ungroup() %>%
  select(-c(player_name,stand,`CS%`,`Foul%`,`IZ Whiff`,`OZ Whiff`,pull,oppo,middle))

table_perf <- tableGrob(perf_tot, rows = NULL, theme = table_theme)

table_perf <- gtable_add_grob(table_perf, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_perf), l = 1, r = ncol(table_perf))

table_perf <- gtable_add_grob(table_perf, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_perf))

format_table_perf <- plot_grid(NULL, table_perf, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))

################### LOCATION SPLITS ###################

# location_21 <- sc_2021
# location_22 <- sc_2022
location_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         vert_class = ifelse(zone %in% c(1,2,3),"high",
                             ifelse(zone %in% c(4,5,6),"middle",
                                    ifelse(zone %in% c(7,8,9),"low",
                                           ifelse(zone %in% c(11,12),"too high",
                                                  ifelse(zone %in% c(13,14),"too low",NA))))),
         horz_class = ifelse(zone %in% c(1,4,7) & stand == "L","inside",
                             ifelse(zone %in% c(2,5,8),"middle",
                                    ifelse(zone %in% c(3,6,9) & stand == "L","outside",
                                           ifelse(zone %in% c(11,13) & stand == "L","too inside",
                                                  ifelse(zone %in% c(12,14) & stand == "L","too outside",
                                                         ifelse(zone %in% c(1,4,7) & stand == "R","outside",
                                                                ifelse(zone %in% c(3,6,9) & stand == "R","inside",
                                                                       ifelse(zone %in% c(11,13) & stand == "R","too outside",
                                                                              ifelse(zone %in% c(12,14) & stand == "R","too inside",NA)))))))))) %>%
  group_by(stand)%>%
  summarise(`Season` = "2023",
            `Pitches` = n(),
            `High %` = 100*round(sum(vert_class == "high",na.rm = T)/n(),3),
            `Middle %` = 100*round(sum(vert_class == "middle",na.rm = T)/n(),3),
            `Low %` = 100*round(sum(vert_class == "low",na.rm = T)/n(),3),
            `Too High %` = 100*round(sum(vert_class == "too high",na.rm = T)/n(),3),
            `Too Low %` = 100*round(sum(vert_class == "too low",na.rm = T)/n(),3),
            `inside%` = 100*round(sum(horz_class == "inside",na.rm = T)/n(),3),
            `h middle%` = 100*round(sum(horz_class == "middle",na.rm = T)/n(),3),
            `outside%` = 100*round(sum(horz_class == "outside",na.rm = T)/n(),3),
            `too inside%` = 100*round(sum(horz_class == "too inside",na.rm = T)/n(),3),
            `too outside%` = 100*round(sum(horz_class == "too outside",na.rm = T)/n(),3))

location_tot <- rbind(location_21,location_22,location_23) %>%
  filter(stand == "R") %>%
  ungroup() %>%
  select(-c(stand,`inside%`,`h middle%`,`outside%`,`too inside%`,`too outside%`))

table_loc <- tableGrob(location_tot, rows = NULL, theme = table_theme)

table_loc <- gtable_add_grob(table_loc, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_loc), l = 1, r = ncol(table_loc))

table_loc <- gtable_add_grob(table_loc, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_loc))

format_table_loc <- plot_grid(NULL, table_loc, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))


################### PERFORMANCE BY LOCATION ###################
loc_perf_21 <- sc_2021
loc_perf_22 <- sc_2022
loc_perf_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         vert_class = ifelse(zone %in% c(1,2,3),"high",
                             ifelse(zone %in% c(4,5,6),"middle",
                                    ifelse(zone %in% c(7,8,9),"low",
                                           ifelse(zone %in% c(11,12),"too high",
                                                  ifelse(zone %in% c(13,14),"too low",NA))))),
         horz_class = ifelse(zone %in% c(1,4,7) & stand == "L","inside",
                             ifelse(zone %in% c(2,5,8),"middle",
                                    ifelse(zone %in% c(3,6,9) & stand == "L","outside",
                                           ifelse(zone %in% c(11,13) & stand == "L","too inside",
                                                  ifelse(zone %in% c(12,14) & stand == "L","too outside",
                                                         ifelse(zone %in% c(1,4,7) & stand == "R","outside",
                                                                ifelse(zone %in% c(3,6,9) & stand == "R","inside",
                                                                       ifelse(zone %in% c(11,13) & stand == "R","too outside",
                                                                              ifelse(zone %in% c(12,14) & stand == "R","too inside",NA))))))))),
         spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
  mutate(bip_dir = ifelse(stand == "L" & spray_angle > 15,"pull",
                          ifelse(stand == "L" & spray_angle < -15, "oppo",
                                 ifelse(stand == "R" & spray_angle > 15, "oppo",
                                        ifelse(stand == "R" & spray_angle < -15, "pull",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(player_name,stand,vert_class) %>%
  summarise(`Season` = "2023",
            `Pitches`= n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Strike%` = round(100*(sum(description %in% c("called_strike","foul","hit_into_play","swinging_strike",
                                                          "swinging_strike_blocked","foul_bunt"))/n()),1),
            `Zone%` = round(100*(sum(isZone == TRUE, na.rm = T)/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")))/
                                    sum(isZone == FALSE)),1),
            `CS%` = round(100*(sum(description == "called_strike",na.rm = T)/n()),1),
            `Foul%` = round(100*(sum(description == "foul",na.rm = T)/sum(description %in% c("foul","hit_into_play",
                                                                                             "swinging_strike",
                                                                                             "swinging_strike_blocked",
                                                                                             "foul_tip","foul_bunt"))),1),
            `Whiff%` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                         "foul_tip"),na.rm = T)/
                                    sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                           "swinging_strike_blocked","foul_tip",
                                                           "bunt_foul_tip","missed_bunt","foul_bunt"),
                                        na.rm = T)),1),
            `IZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip") & isZone == TRUE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt")
                                          & isZone == TRUE,na.rm = T)),1),
            `OZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip")& isZone == FALSE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt") &
                                            isZone == FALSE,na.rm = T)),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball" & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play",na.rm = T)),1),
            `HH%` = round(100*(sum(launch_speed >= 95 & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play", na.rm = T)),1),
            `xwOBAcon` = round(sum(estimated_woba_using_speedangle[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `wOBAcon` = round(sum(woba_value[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `pull` = round(100*(sum(bip_dir == "pull" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `oppo` = round(100*(sum(bip_dir == "oppo" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `middle` = round(100*(sum(bip_dir == "middle" & description=="hit_into_play",na.rm = T)/
                                    sum(description == "hit_into_play",na.rm = T)),1))

################### PERFORMANCE BY LOCATION (HIGHS AND LOWS GROUPED) ###################

# loc_perf_21_grp <- sc_2021
# loc_perf_22_grp <- sc_2022
loc_perf_23_grp <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         vert_class = ifelse(zone %in% c(1,2,3),"high",
                             ifelse(zone %in% c(4,5,6),"middle",
                                    ifelse(zone %in% c(7,8,9),"low",
                                           ifelse(zone %in% c(11,12),"high",
                                                  ifelse(zone %in% c(13,14),"low",NA))))),
         horz_class = ifelse(zone %in% c(1,4,7) & stand == "L","inside",
                             ifelse(zone %in% c(2,5,8),"middle",
                                    ifelse(zone %in% c(3,6,9) & stand == "L","outside",
                                           ifelse(zone %in% c(11,13) & stand == "L","too inside",
                                                  ifelse(zone %in% c(12,14) & stand == "L","too outside",
                                                         ifelse(zone %in% c(1,4,7) & stand == "R","outside",
                                                                ifelse(zone %in% c(3,6,9) & stand == "R","inside",
                                                                       ifelse(zone %in% c(11,13) & stand == "R","too outside",
                                                                              ifelse(zone %in% c(12,14) & stand == "R","too inside",NA))))))))),
         spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
  mutate(bip_dir = ifelse(stand == "L" & spray_angle > 15,"pull",
                          ifelse(stand == "L" & spray_angle < -15, "oppo",
                                 ifelse(stand == "R" & spray_angle > 15, "oppo",
                                        ifelse(stand == "R" & spray_angle < -15, "pull",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(player_name,stand,vert_class) %>%
  summarise(`Season` = "2023",
            `Pitches`= n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Strike%` = round(100*(sum(description %in% c("called_strike","foul","hit_into_play","swinging_strike",
                                                          "swinging_strike_blocked","foul_bunt"))/n()),1),
            `Zone%` = round(100*(sum(isZone == TRUE, na.rm = T)/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")))/
                                    sum(isZone == FALSE)),1),
            `CS%` = round(100*(sum(description == "called_strike",na.rm = T)/n()),1),
            `Foul%` = round(100*(sum(description == "foul",na.rm = T)/sum(description %in% c("foul","hit_into_play",
                                                                                             "swinging_strike",
                                                                                             "swinging_strike_blocked",
                                                                                             "foul_tip","foul_bunt"))),1),
            `Whiff%` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                         "foul_tip"),na.rm = T)/
                                    sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                           "swinging_strike_blocked","foul_tip",
                                                           "bunt_foul_tip","missed_bunt","foul_bunt"),
                                        na.rm = T)),1),
            `IZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip") & isZone == TRUE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt")
                                          & isZone == TRUE,na.rm = T)),1),
            `OZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip")& isZone == FALSE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt") &
                                            isZone == FALSE,na.rm = T)),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball" & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play",na.rm = T)),1),
            `HH%` = round(100*(sum(launch_speed >= 95 & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play", na.rm = T)),1),
            `xwOBAcon` = round(sum(estimated_woba_using_speedangle[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `wOBAcon` = round(sum(woba_value[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `pull` = round(100*(sum(bip_dir == "pull" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `oppo` = round(100*(sum(bip_dir == "oppo" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `middle` = round(100*(sum(bip_dir == "middle" & description=="hit_into_play",na.rm = T)/
                                    sum(description == "hit_into_play",na.rm = T)),1))

loc_perf_tot <- rbind(loc_perf_21_grp,loc_perf_22_grp,loc_perf_23_grp) %>%
  filter(stand == "R") %>%
  ungroup() %>%
  select(-c(player_name,stand,`CS%`,`Foul%`,`IZ Whiff`,`OZ Whiff`,pull,oppo,middle)) %>%
  rename(`Location` = vert_class) %>%
  arrange(`Location`,`Season`)

table_loc_perf <- tableGrob(loc_perf_tot, rows = NULL, theme = table_theme)

table_loc_perf <- gtable_add_grob(table_loc_perf, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_loc_perf), l = 1, r = ncol(table_loc_perf))

table_loc_perf <- gtable_add_grob(table_loc_perf, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_loc_perf))

format_table_loc_perf <- plot_grid(NULL, table_loc_perf, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))

################### PLOT 2K PITCHES ###################

# fb2k_21_l <- sc_2021
# fb2k_22_l/fb2k_22_r <- sc_2022
fb2k_23_l <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R", pitch_name == "4-Seam Fastball", strikes == 2,
         stand == "L")

ggplot2::ggplot(fb2k_22_l, aes(x = plate_x, y = plate_z)) + 
  scale_x_continuous(limits = c(-2.5,2.5)) + scale_y_continuous(limits = c(0,5)) +
  geom_path(data = homePlateBatter, aes(x = x, y = y), size = 1) + 
  geom_rect(aes(xmin = negZone , xmax = posZone, ymin = horzBottom, ymax = horzTop), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = vertLeft, xmax = vertRight, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = negZone, xmax = posZone, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "black") +
  geom_point(color = "black", pch = 21, alpha = 0.9, size = 4, na.rm = TRUE) + 
  theme_bw() + guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 12), panel.grid = element_blank()) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))

ggplot2::ggplot(fb2k_23_l, aes(x = plate_x, y = plate_z)) + 
  scale_x_continuous(limits = c(-2.5,2.5)) + scale_y_continuous(limits = c(0,5)) +
  geom_path(data = homePlateBatter, aes(x = x, y = y), size = 1) + 
  geom_rect(aes(xmin = negZone , xmax = posZone, ymin = horzBottom, ymax = horzTop), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = vertLeft, xmax = vertRight, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "grey55") +
  geom_rect(aes(xmin = negZone, xmax = posZone, ymin = botZone, ymax = topZone), alpha = 0, size = 1, color = "black") +
  geom_point(color = "black", pch = 21, alpha = 0.9, size = 4, na.rm = TRUE) + 
  theme_bw() + guides(fill = guide_legend(nrow = 1)) +
  theme(text = element_text(size = 12), panel.grid = element_blank()) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) + 
  theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12))

################### SEQUENCING USAGE/PERFORMANCE ###################

seq_21 <- sc_2021
seq_22 <- sc_2022
seq_23 <- sc_2023 %>%
  filter(player_name == "Gausman, Kevin", game_type.x == "R") %>%
  arrange(game_date.x,inning,at_bat_number,pitch_number) %>%
  mutate(prev_pitch = lag(pitch_name,n=1)) %>%
  filter(pitch_name == "4-Seam Fastball", strikes == 2) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         vert_class = ifelse(zone %in% c(1,2,3),"high",
                             ifelse(zone %in% c(4,5,6),"middle",
                                    ifelse(zone %in% c(7,8,9),"low",
                                           ifelse(zone %in% c(11,12),"high",
                                                  ifelse(zone %in% c(13,14),"low",NA))))),
         horz_class = ifelse(zone %in% c(1,4,7) & stand == "L","inside",
                             ifelse(zone %in% c(2,5,8),"middle",
                                    ifelse(zone %in% c(3,6,9) & stand == "L","outside",
                                           ifelse(zone %in% c(11,13) & stand == "L","too inside",
                                                  ifelse(zone %in% c(12,14) & stand == "L","too outside",
                                                         ifelse(zone %in% c(1,4,7) & stand == "R","outside",
                                                                ifelse(zone %in% c(3,6,9) & stand == "R","inside",
                                                                       ifelse(zone %in% c(11,13) & stand == "R","too outside",
                                                                              ifelse(zone %in% c(12,14) & stand == "R","too inside",NA))))))))),
         spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
  mutate(bip_dir = ifelse(stand == "L" & spray_angle > 15,"pull",
                          ifelse(stand == "L" & spray_angle < -15, "oppo",
                                 ifelse(stand == "R" & spray_angle > 15, "oppo",
                                        ifelse(stand == "R" & spray_angle < -15, "pull",
                                               ifelse(spray_angle >= -15 & spray_angle <= 15, "middle",NA)))))) %>%
  group_by(player_name,stand,pitch_name,prev_pitch) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Strike%` = round(100*(sum(description %in% c("called_strike","foul","hit_into_play","swinging_strike",
                                                          "swinging_strike_blocked","foul_bunt"))/n()),1),
            `Zone%` = round(100*(sum(isZone == TRUE, na.rm = T)/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")))/
                                    sum(isZone == FALSE)),1),
            `CS%` = round(100*(sum(description == "called_strike",na.rm = T)/n()),1),
            `Foul%` = round(100*(sum(description == "foul",na.rm = T)/sum(description %in% c("foul","hit_into_play",
                                                                                             "swinging_strike",
                                                                                             "swinging_strike_blocked",
                                                                                             "foul_tip","foul_bunt"))),1),
            `Whiff%` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                         "foul_tip"),na.rm = T)/
                                    sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                           "swinging_strike_blocked","foul_tip",
                                                           "bunt_foul_tip","missed_bunt","foul_bunt"),
                                        na.rm = T)),1),
            `IZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip") & isZone == TRUE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt")
                                          & isZone == TRUE,na.rm = T)),1),
            `OZ Whiff` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                           "foul_tip")& isZone == FALSE,na.rm = T)/
                                      sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                             "swinging_strike_blocked","foul_tip",
                                                             "bunt_foul_tip","missed_bunt","foul_bunt") &
                                            isZone == FALSE,na.rm = T)),1),
            `GB%` = round(100*(sum(bb_type == "ground_ball" & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play",na.rm = T)),1),
            `HH%` = round(100*(sum(launch_speed >= 95 & description == "hit_into_play", na.rm = T)/sum(description == "hit_into_play", na.rm = T)),1),
            `xwOBA` = round(sum(estimated_woba_using_speedangle[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `wOBA` = round(sum(woba_value[description=="hit_into_play"], na.rm = T)/sum(woba_denom[description=="hit_into_play"], na.rm = T),3),
            `pull` = round(100*(sum(bip_dir == "pull" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `oppo` = round(100*(sum(bip_dir == "oppo" & description=="hit_into_play",na.rm = T)/
                                  sum(description == "hit_into_play",na.rm = T)),1),
            `middle` = round(100*(sum(bip_dir == "middle" & description=="hit_into_play",na.rm = T)/
                                    sum(description == "hit_into_play",na.rm = T)),1)) %>%
  filter(`Pitches`>=10)


################### YEAR-OVER-YEAR RV/100 CORRELATIONS ###################

rv_23 <- sc_2023 %>%
  filter(strikes == 2, game_type.x == "R", pitch_name == "4-Seam Fastball") %>%
  group_by(player_name) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100),
            `RV/100 vR` = sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),
            `RV/100 vL` = sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100)) %>%
  filter(`Pitches`>=100)
  
rv_22 <- sc_2022 %>%
  filter(strikes == 2, game_type.x == "R", pitch_name == "4-Seam Fastball") %>%
  group_by(player_name) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100),
            `RV/100 vR` = sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),
            `RV/100 vL` = sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100)) %>%
  filter(`Pitches`>=100)

rv_21 <- sc_2021 %>%
  filter(strikes == 2, game_type.x == "R", pitch_name == "4-Seam Fastball") %>%
  group_by(player_name) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100),
            `RV/100 vR` = sum(delta_run_exp[stand == "R"],na.rm = T)/(sum(stand == "R",na.rm = T)/100),
            `RV/100 vL` = sum(delta_run_exp[stand == "L"],na.rm = T)/(sum(stand == "L",na.rm = T)/100)) %>%
  filter(`Pitches`>=100)


comb_1 <- inner_join(rv_22,rv_23,by=c("player_name"))
comb_2 <- inner_join(rv_21,rv_22,by=c("player_name"))

total <- rbind(comb_1,comb_2)

cor(total$`RV/100.x`,total$`RV/100.y`)
cor(total$`RV/100 vR.x`,total$`RV/100 vR.y`)
cor(total$`RV/100 vL.x`,total$`RV/100 vL.y`)
plot(total$`RV/100.x`,total$`RV/100.y`)
