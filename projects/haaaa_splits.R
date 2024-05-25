library(baseballr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(cowplot)



################### CALCULATE OPS BY HANDEDNESS FOR HITTERS 2021-2023 ###################

ops_23 <- sc_2023 %>%
  filter(game_type == "R", events != "") %>%
  group_by(batter, stand, p_throws) %>%
  summarise(season = 2023,
            woba = round(sum(woba_value,na.rm=T)/sum(woba_denom,na.rm = T),3),
            ab = sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                   "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                   "strikeout_double_play")),
            obp = sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")),
            slg = (sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play")),
            ops = (sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")))+
              ((sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play")))) %>%
  filter(ab >= 100)

ops_22 <- sc_2022 %>%
  filter(game_type == "R", events != "") %>%
  group_by(batter, stand, p_throws) %>%
  summarise(season = 2022,
            woba = round(sum(woba_value,na.rm=T)/sum(woba_denom,na.rm = T),3),
            ab = sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                   "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                   "strikeout_double_play")),
            obp = sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")),
            slg = (sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play")),
            ops = (sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
                     sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                       "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                       "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")))+
              ((sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
                 sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                   "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                   "strikeout_double_play")))) %>%
  filter(ab >= 100)

ops_21 <- sc_2021 %>%
  filter(game_type == "R", events != "") %>%
  group_by(batter, stand, p_throws) %>%
  summarise(season = 2021,
            woba = round(sum(woba_value,na.rm=T)/sum(woba_denom,na.rm = T),3),
            ab = sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                   "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                   "strikeout_double_play")),
            obp = sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")),
            slg = (sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
              sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                "strikeout_double_play")),
            ops = (sum(events %in% c("single","double","triple","home_run","walk","hit_by_pitch"))/
                     sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                       "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                       "strikeout_double_play","walk","hit_by_pitch","sac_fly_double_play","sac_fly")))+
              ((sum(events == "single")+(sum(events=="double")*2)+(sum(events=="triple")*3)+(sum(events=="home_run")*4))/
                 sum(events %in% c("single","double","triple","home_run","double_play","field_out","field_error","force_out",
                                   "fielders_choice_out","grounded_into_double_play","fielders_choice","triple_play","strikeout",
                                   "strikeout_double_play")))) %>%
  filter(ab >= 100)

ops_total <- rbind(ops_21,ops_22,ops_23)  


ops_wide <- ops_total %>%
  pivot_wider(names_from = p_throws, values_from = ops) %>%
  replace_na(list(R = 0, L = 0))

ops_diff <- ops_wide %>%
  group_by(batter, stand, season) %>%
  summarise(
    tot = n(),
    ab = sum(ab),
    R = sum(R),
    L = sum(L),
    ops_diff = abs(sum(R) - sum(L))
  ) %>%
  filter(tot == 2) %>%
  mutate(reverse_split = ifelse(stand == "R" & (L>R),1,
                                ifelse(stand == "L" & (R>L),1,0)))

################### FIND HITTERS WITH "SIGNIFICANT" SPLITS ###################

avg_ops_diff <- mean(ops_diff$ops_diff)
sd_ops_diff <- sd(ops_diff$ops_diff)

splits_hitters <- ops_diff %>%
  filter(ops_diff >= (avg_ops_diff+(sd_ops_diff)), reverse_split == 1)

non_splits <- ops_diff %>%
  filter(ops_diff < (avg_ops_diff+(sd_ops_diff)))

################### COMPILE DATAFRAME OF SPLITS HITTERS AND SAME HANDED PITCHERS ###################

split_21 <- inner_join(sc_2021,splits_hitters,by=c("batter","game_year"="season"))
split_22 <- inner_join(sc_2022,splits_hitters,by=c("batter","game_year"="season"))
split_23 <- inner_join(sc_2023,splits_hitters,by=c("batter","game_year"="season"))

split_total <- rbind(split_21,split_22,split_23) %>%
  filter(stand.x == p_throws, game_type == "R")

################### COMPILE DATAFRAME OF NON-SPLITS HITTERS AND SAME HANDED PITCHERS ###################

even_21 <- inner_join(sc_2021,non_splits,by=c("batter","game_year"="season"))
even_22 <- inner_join(sc_2022,non_splits,by=c("batter","game_year"="season"))
even_23 <- inner_join(sc_2023,non_splits,by=c("batter","game_year"="season"))

even_total <- rbind(even_21,even_22,even_23) %>%
  filter(stand.x == p_throws, game_type == "R")

################### COMPARE PERFORMANCE BY PITCH TYPE ###################

splits_perf <- split_total %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  group_by(pitch_name) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

even_perf <- even_total %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  group_by(pitch_name) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100)) 

# rv_compare <- inner_join(splits_perf,even_perf,by=c("pitch_name")) %>%
#   mutate(`RV diff` = `RV/100 even` - `RV/100 splits`)


################### SLIDER BUCKETS ###################

# Q <- quantile(sliders$pfx_z_adj, probs=c(.25, .75), na.rm = T)
# iqr <- IQR(sliders$pfx_z_adj, na.rm = T)
# up <-  Q[2]+1.5*iqr 
# low<- Q[1]-1.5*iqr

# group <- split(sliders$pfx_z_adj,cut_number(sliders$pfx_z_adj,n= 23))


sliders_even <- even_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 21, pfx_z_adj >= -9, pfx_z_adj <= 13) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 23),
         hb_bucket = cut(pfx_x_adj,breaks = 22)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 100)

sliders_splits <- split_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 21, pfx_z_adj >= -9, pfx_z_adj <= 13) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 23),
         hb_bucket = cut(pfx_x_adj,breaks = 22)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 100)
  

ggplot(sliders_even, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

ggplot(sliders_splits, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

################### CURVEBALL BUCKETS ###################

# Q <- quantile(curveball_splits$pfx_z_adj, probs=c(.25, .75), na.rm = T)
# iqr <- IQR(curveball_splits$pfx_z_adj, na.rm = T)
# up <-  Q[2]+1.5*iqr
# low<- Q[1]-1.5*iqr
# boxplot(curveball_splits$pfx_z_adj)
# 
# group <- split(curveball_splits$pfx_z_adj,cut_number(curveball_splits$pfx_z_adj,n= 33))


curveballs_even <- even_total %>%
  filter(pitch_name %in% c("Knuckle Curve","Curveball")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 24, pfx_z_adj >= -25, pfx_z_adj <= 7) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 16),
         hb_bucket = cut(pfx_x_adj,breaks = 13)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 50)

curveballs_splits <- split_total %>%
  filter(pitch_name %in% c("Knuckle Curve","Curveball")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 24, pfx_z_adj >= -25, pfx_z_adj <= 7) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 16),
         hb_bucket = cut(pfx_x_adj,breaks = 13)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 50)


ggplot(curveballs_even, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

ggplot(curveballs_splits, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

################### FASTBALL BUCKETS ###################

# Q <- quantile(fastballs_splits$pfx_z_adj, probs=c(.25, .75), na.rm = T)
# iqr <- IQR(fastballs_splits$pfx_z_adj, na.rm = T)
# up <-  Q[2]+1.5*iqr
# low<- Q[1]-1.5*iqr
# boxplot(fastballs_splits$pfx_z_adj)
# 
# group <- split(fastballs_splits$pfx_z_adj,cut_number(fastballs_splits$pfx_z_adj,n= 30))


fastballs_even <- even_total %>%
  filter(pitch_name %in% c("4-Seam Fastball","Sinker")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 26, pfx_z_adj >= -1, pfx_z_adj <= 28) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 30),
         hb_bucket = cut(pfx_x_adj,breaks = 27)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 100)

fastballs_splits <- split_total %>%
  filter(pitch_name %in% c("4-Seam Fastball","Sinker")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj <= 26, pfx_z_adj >= -1, pfx_z_adj <= 28) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 30),
         hb_bucket = cut(pfx_x_adj,breaks = 27)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 100)


ggplot(fastballs_even, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

ggplot(fastballs_splits, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

################### OFFSPEED BUCKETS ###################

# Q <- quantile(offspeed_splits$pfx_z_adj, probs=c(.25, .75), na.rm = T)
# iqr <- IQR(offspeed_splits$pfx_z_adj, na.rm = T)
# up <-  Q[2]+1.5*iqr
# low<- Q[1]-1.5*iqr
# boxplot(offspeed_splits$pfx_z_adj)
# 
# group <- split(offspeed_splits$pfx_z_adj,cut_number(offspeed_splits$pfx_z_adj,n= 13))


offspeed_even <- even_total %>%
  filter(pitch_name %in% c("Changeup","Split-Finger")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj >= 3, pfx_x_adj <= 23, pfx_z_adj >= -7, pfx_z_adj <= 18) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 13),
         hb_bucket = cut(pfx_x_adj,breaks = 10)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 50)

offspeed_splits <- split_total %>%
  filter(pitch_name %in% c("Changeup","Split-Finger")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  filter(pfx_x_adj >= 3, pfx_x_adj <= 23, pfx_z_adj >= -7, pfx_z_adj <= 18) %>%
  mutate(ivb_bucket = cut(pfx_z_adj,breaks = 13),
         hb_bucket = cut(pfx_x_adj,breaks = 10)) %>%
  group_by(ivb_bucket,hb_bucket) %>%
  summarise(`Pitches` = n(),
            `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
  filter(`Pitches` >= 50)


ggplot(offspeed_even, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()

ggplot(offspeed_splits, aes(x = hb_bucket, y = ivb_bucket, fill = `RV/100`)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 5) +
  labs(x = "Horizontal Break Bucket",
       y = "Vertical Break Bucket",
       color = "RV/100",
       title = "RV/100 by Horizontal Break and Vertical Break Buckets") +
  theme_minimal()


################### SPECIFIC PITCH TYPE GROUPS - SLIDERS ###################

split_perf_sl <- split_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         slider_type = ifelse(pfx_x_adj < 5, "short",
                              ifelse(pfx_x_adj >=5 & pfx_x_adj < 10, "middle",
                                     ifelse(pfx_x_adj >= 10, "long",NA)))) %>%
  group_by(slider_type) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

even_perf_sl <- even_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         slider_type = ifelse(pfx_x_adj < 5, "short",
                              ifelse(pfx_x_adj >=5 & pfx_x_adj < 10, "middle",
                                     ifelse(pfx_x_adj >= 10, "long",NA)))) %>%
  group_by(slider_type) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100)) 

################### SPECIFIC PITCH TYPE GROUPS - CURVEBALLS ###################

split_perf_cb <- split_total %>%
  filter(pitch_name %in% c("Knuckle Curve","Curveball")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         curve_type = ifelse(pfx_x_adj < 7, "true",
                              ifelse(pfx_x_adj >= 7, "slurve",NA))) %>%
  group_by(curve_type) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

even_perf_cb <- even_total %>%
  filter(pitch_name %in% c("Knuckle Curve","Curveball")) %>%
  mutate(pfx_x_adj = abs(pfx_x)*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE),
         curve_type = ifelse(pfx_x_adj < 7, "true",
                             ifelse(pfx_x_adj >= 7, "slurve",NA))) %>%
  group_by(curve_type) %>%
  summarise(`BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100))


################### HAA-AA 4-SEAM FASTBALLS ###################

ff_21 <- sc_2021 %>%
  filter(pitch_name == "4-Seam Fastball", game_type == "R")
ff_22 <- sc_2022 %>%
  filter(pitch_name == "4-Seam Fastball", game_type == "R")
ff_23 <- sc_2023 %>%
  filter(pitch_name == "4-Seam Fastball", game_type == "R")

ff_all <- rbind(ff_21,ff_22,ff_23)

haa_ff <- ff_all %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi)) %>%
  group_by(p_throws,release_pos_x,plate_x) %>%
  summarise(tot = n(),
            avg_haa = mean(haa))

splits_ff_haa <- split_total %>%
  filter(pitch_name == "4-Seam Fastball") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

even_ff_haa <- even_total %>%
  filter(pitch_name == "4-Seam Fastball") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

splits_ff_perf <- left_join(splits_ff_haa,haa_ff,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.801,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

# min(splits_ff_perf$haa_aa)
# max(splits_ff_perf$haa_aa)
# 
# group <- split(splits_ff_perf$haa_aa,cut_number(splits_ff_perf$haa_aa,n= 12))
# group <- cut(splits_ff_perf$haa_aa, breaks = 12)

even_ff_perf <- left_join(even_ff_haa,haa_ff,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.802,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100))

################### HAA-AA SINKERS ###################

si_21 <- sc_2021 %>%
  filter(pitch_name == "Sinker", game_type == "R")
si_22 <- sc_2022 %>%
  filter(pitch_name == "Sinker", game_type == "R")
si_23 <- sc_2023 %>%
  filter(pitch_name == "Sinker", game_type == "R")

si_all <- rbind(si_21,si_22,si_23)

haa_si <- si_all %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi)) %>%
  group_by(p_throws,release_pos_x,plate_x) %>%
  summarise(tot = n(),
            avg_haa = mean(haa))

splits_si_haa <- split_total %>%
  filter(pitch_name == "Sinker") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

even_si_haa <- even_total %>%
  filter(pitch_name == "Sinker") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

splits_si_perf <- left_join(splits_si_haa,haa_si,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.483,-0.17]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

# min(splits_si_perf$haa_aa)
# max(splits_si_perf$haa_aa)
# 
# group <- split(splits_si_perf$haa_aa,cut_number(splits_si_perf$haa_aa,n= 12))
# group <- cut(splits_si_perf$haa_aa, breaks = 5)

even_si_perf <- left_join(even_si_haa,haa_si,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.802,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100))

################### HAA-AA CHANGEUPS ###################

ch_21 <- sc_2021 %>%
  filter(pitch_name == "Changeup", game_type == "R")
ch_22 <- sc_2022 %>%
  filter(pitch_name == "Changeup", game_type == "R")
ch_23 <- sc_2023 %>%
  filter(pitch_name == "Changeup", game_type == "R")

ch_all <- rbind(ch_21,ch_22,ch_23)

haa_ch <- ch_all %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi)) %>%
  group_by(p_throws,release_pos_x,plate_x) %>%
  summarise(tot = n(),
            avg_haa = mean(haa))

splits_ch_haa <- split_total %>%
  filter(pitch_name == "Changeup") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

even_ch_haa <- even_total %>%
  filter(pitch_name == "Changeup") %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = -atan((vx_f/vy_f))*(180/pi))

splits_ch_perf <- left_join(splits_ch_haa,haa_ch,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.483,-0.17]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

# min(splits_ch_perf$haa_aa)
# max(splits_ch_perf$haa_aa)
# 
# group <- split(splits_ch_perf$haa_aa,cut_number(splits_ch_perf$haa_aa,n= 12))
# group <- cut(splits_ch_perf$haa_aa, breaks = 5)

even_ch_perf <- left_join(even_ch_haa,haa_ch,by=c("p_throws","release_pos_x","plate_x")) %>%
  mutate(haa_aa = ifelse(p_throws == "R",-1*(haa-avg_haa),haa-avg_haa),
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.802,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100))

################### HAA-AA SLIDERS/SWEEPERS ###################

sl_21 <- sc_2021 %>%
  filter(pitch_name %in% c("Slider","Sweeper"), game_type == "R")
sl_22 <- sc_2022 %>%
  filter(pitch_name %in% c("Slider","Sweeper"), game_type == "R")
sl_23 <- sc_2023 %>%
  filter(pitch_name %in% c("Slider","Sweeper"), game_type == "R")

sl_all <- rbind(sl_21,sl_22,sl_23)

haa_sl <- sl_all %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x)) %>%
  group_by(release_pos_x,plate_x) %>%
  summarise(tot = n(),
            avg_haa = mean(haa))

splits_sl_haa <- split_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x))

even_sl_haa <- even_total %>%
  filter(pitch_name %in% c("Slider","Sweeper")) %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x))

splits_sl_perf <- left_join(splits_sl_haa,haa_sl,by=c("release_pos_x","plate_x")) %>%
  mutate(haa_aa = haa-avg_haa,
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.801,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n(),
  #           hb = round(mean(pfx_x)*12,3))
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = round(sum(delta_run_exp,na.rm = T)/(n()/100),2)) %>%
  rename(`HAAAA` = haa_bucket) %>%
  select(`HAAAA`,`Pitches`,`BIP`,`Chase%`,`Whiff%`,`GB%`,`HH%`,`wOBAcon`,`xwOBAcon`,`RV/100 splits`)

table_theme <- ttheme_default(
  core = list(bg_params = list(fill = c("gray95", "gray90"))),
  colhead = list(fg_params = list(col = "gray95"),
                 bg_params = list(fill = "black"))
)

table_splits <- tableGrob(splits_sl_perf, rows = NULL, theme = table_theme)

table_splits <- gtable_add_grob(table_splits, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_splits), l = 1, r = ncol(table_splits))

table_splits <- gtable_add_grob(table_splits, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_splits))

format_table_splits <- plot_grid(NULL, table_splits, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))

# min(splits_sl_perf$haa_aa)
# max(splits_sl_perf$haa_aa)
# 
# group <- split(splits_sl_perf$haa_aa,cut_number(splits_sl_perf$haa_aa,n= 12))
# group <- cut(splits_sl_perf$haa_aa, breaks = 5)

even_sl_perf <- left_join(even_sl_haa,haa_sl,by=c("release_pos_x","plate_x")) %>%
  mutate(haa_aa = haa-avg_haa,
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.802,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = round(sum(delta_run_exp,na.rm = T)/(n()/100),2)) %>%
  rename(`HAAAA` = haa_bucket) %>%
  select(`HAAAA`,`Pitches`,`BIP`,`Chase%`,`Whiff%`,`GB%`,`HH%`,`wOBAcon`,`xwOBAcon`,`RV/100 even`)

table_even <- tableGrob(even_sl_perf, rows = NULL, theme = table_theme)

table_even <- gtable_add_grob(table_even, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_even), l = 1, r = ncol(table_even))

table_even <- gtable_add_grob(table_even, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_even))

format_table_even <- plot_grid(NULL, table_even, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))

################### HAA-AA CURVEBALLS/SLURVE/KNUCKLE CURVE ###################

cb_21 <- sc_2021 %>%
  filter(pitch_name %in% c("Curveball","Knuckle Curve","Slurve"), game_type == "R")
cb_22 <- sc_2022 %>%
  filter(pitch_name %in% c("Curveball","Knuckle Curve","Slurve"), game_type == "R")
cb_23 <- sc_2023 %>%
  filter(pitch_name %in% c("Curveball","Knuckle Curve","Slurve"), game_type == "R")

cb_all <- rbind(cb_21,cb_22,cb_23)

haa_cb <- cb_all %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x)) %>%
  group_by(release_pos_x,plate_x) %>%
  summarise(tot = n(),
            avg_haa = mean(haa))

splits_cb_haa <- split_total %>%
  filter(pitch_name %in% c("Curveball","Knuckle Curve","Slurve")) %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x))

even_cb_haa <- even_total %>%
  filter(pitch_name %in% c("Curveball","Knuckle Curve","Slurve")) %>%
  mutate(vy_f = -sqrt((vy0**2)-(2*ay*(50-(17/12))))) %>%
  mutate(t = (vy_f-vy0)/ay) %>%
  mutate(vx_f = vx0+(ax*t)) %>%
  mutate(haa = ifelse(p_throws == "L",-1*(-atan((vx_f/vy_f))*(180/pi)),-atan((vx_f/vy_f))*(180/pi)),
         release_pos_x = ifelse(p_throws == "L",release_pos_x*-1,release_pos_x),
         plate_x = ifelse(p_throws == "L",plate_x*-1,plate_x))

splits_cb_perf <- left_join(splits_cb_haa,haa_cb,by=c("release_pos_x","plate_x")) %>%
  mutate(haa_aa = haa-avg_haa,
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(0.479,0.801]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n(),
  #           hb = round(mean(pfx_x)*12,3))
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 splits` = sum(delta_run_exp,na.rm = T)/(n()/100))

# min(splits_cb_perf$haa_aa)
# max(splits_cb_perf$haa_aa)
# 
# group <- split(splits_cb_perf$haa_aa,cut_number(splits_cb_perf$haa_aa,n= 12))
# group <- cut(splits_cb_perf$haa_aa, breaks = 5)

even_cb_perf <- left_join(even_cb_haa,haa_cb,by=c("release_pos_x","plate_x")) %>%
  mutate(haa_aa = haa-avg_haa,
         isZone = ifelse(plate_z >= sz_bot & 
                           plate_z <= sz_top & 
                           plate_x >= -0.708 & plate_x <= 0.708,TRUE,FALSE)) %>%
  filter(haa_aa >= -0.8, haa_aa <= 0.8) %>%
  mutate(haa_bucket = cut(haa_aa,breaks = 5)) %>%
  # filter(haa_bucket == "(-0.802,-0.48]") %>%
  # group_by(player_name) %>%
  # summarise(tot = n())
  group_by(haa_bucket) %>%
  summarise(`Pitches` = n(),
            `BIP` = sum(description == "hit_into_play"),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Chase%` = round(100*(sum(isZone == FALSE & 
                                        (description %in% c("foul","hit_into_play","swinging_strike",
                                                            "swinging_strike_blocked","foul_tip",
                                                            "bunt_foul_tip","missed_bunt","foul_bunt")), na.rm=T)/
                                    sum(isZone == FALSE,na.rm=T)),1),
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
            `RV/100 even` = sum(delta_run_exp,na.rm = T)/(n()/100))


################### IDENTIFYING SPLITS HITTERS W/ PLUS PERF ###################

# plus_hitters <- split_total %>%
#   group_by(batter,game_year,pitch_name) %>%
#   summarise(`#` = n(),
#             `RV/100` = sum(delta_run_exp,na.rm = T)/(n()/100)) %>%
#   filter(`RV/100` > 0, `#` >= 50)
# 
# multi_plus <- plus_hitters %>%
#   group_by(batter,game_year) %>%
#   summarise(tot = n()) %>% 
#   filter(tot >= 2)
