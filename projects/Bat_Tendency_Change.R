library(baseballr)
library(dplyr)

################## LOAD DATA - SWITCHING 'STARTSEASON', 'ENDSEASON', TABLE NAME, & 'QUAL' (2020 DIFF) ####################

df_15 <- fg_bat_leaders(
  age = "",
  pos = "all",
  stats = "bat",
  lg = "all",
  qual = 150,
  startseason = "2015",
  endseason = "2015",
  month = "0",
  hand = "",
  team = "0",
  pageitems = "10000",
  pagenum = "1",
  ind = "0",
  rost = "0",
  players = "",
  type = "8",
  postseason = "",
  sortdir = "default",
  sortstat = "WAR"
)

common_cols <- intersect(colnames(df_all), colnames(df_23))

df_all <- rbind(
  subset(df_all, select = common_cols), 
  subset(df_23, select = common_cols)
)



########################### ADD FLAGS FOR SEASONS THAT MEET CRITERIA ###########################

# strictly for profile changes
sig_seasons <- df_all %>%
  mutate(ground_flag = ifelse(`GB_pct` >= 0.5,1,0),
         fly_flag = ifelse(`GB_pct` < 0.45,1,0)) 

# evaluating impact quality as well
sig_seasons_impq <- df_all %>%
  mutate(ground_flag = ifelse(`GB_pct` >= 0.5 & EV >= 90.0,1,0),
         fly_flag = ifelse(`GB_pct` < 0.45 & EV >= 90.0,1,0))

########################### CREATE LIST OF PLAYERS THAT MEET CRITERIA ###########################


gb_prof <- sig_seasons %>%
  group_by(xMLBAMID,PlayerName) %>%
  summarise(gb = sum(ground_flag),
            fb = sum(fly_flag)) %>%
  filter(gb >= 1)

players <- as.list(gb_prof$xMLBAMID)

gb_impq <- sig_seasons_impq %>%
  group_by(xMLBAMID,PlayerName) %>%
  summarise(gb = sum(ground_flag),
            fb = sum(fly_flag)) %>%
  filter(gb >= 1)

players_gb_impq <- as.list(gb_impq$xMLBAMID)

prof_change <- sig_seasons_impq %>%
  group_by(xMLBAMID,PlayerName) %>%
  summarise(gb = sum(ground_flag),
            fb = sum(fly_flag)) %>%
  filter(gb >= 1, fb >= 1)

players_prof_change <- as.list(prof_change$xMLBAMID)
  

collect_szns <- sig_seasons %>%
  filter(xMLBAMID %in% gb_prof) %>%
  group_by(xMLBAMID,PlayerName) %>%
  reframe(tot = n(),
            gb = list(Season[ground_flag == 1]),
            fb = list(Season[fly_flag == 1]),
            plus_fb = list(Season[`FB_pct+` >= 110]),
            # all_after = list(Season[Season>=Season[`FB_pct+` >= 110][1]]),
            # both_year = list(Season[fly_flag == 1 & ground_flag == 1]),
            years_no_change = list(Season[Season <= tail(Season[GB_pct >= 0.45],1)]),
            years_sig_change = list(Season[Season > tail(Season[GB_pct >= 0.45],1)]),
            lad = list(Season[team_name == "LAD" & fly_flag == 1]),
            last_45 = ifelse(length(Season[GB_pct >= 0.45]) > 0, tail(Season[GB_pct >= 0.45], 1),NA),
            last_50 = ifelse(length(Season[GB_pct >= 0.50]) > 0, tail(Season[GB_pct >= 0.50], 1),NA),
            under_45_first = Season[`GB_pct` < 0.45][1],
            under_50_first = Season[`GB_pct` < 0.50][1],
            first_year_gb = ifelse(length(Season[ground_flag == 1]) > 0, Season[ground_flag == 1][1], NA),
            first_year_fb = ifelse(length(Season[fly_flag == 1]) > 0, Season[fly_flag == 1][1], NA)
            # last_year_A = ifelse(length(Season[ground_flag == 1]) > 0, tail(Season[ground_flag == 1], 1), NA),
            # first_year_b = Season[GB_pct >= 0.45 & (Season > Season[fly_flag == 1][1])][1],
            # no_back = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
            #                  & (ifelse(length(Season[ground_flag == 1]) > 0 ,tail(Season[ground_flag == 1], 1),2045)) < Season[fly_flag == 1][1], 1, 0),
            # no_back_2 = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
            #                    & (ifelse(length(Season[GB_pct >= 0.45]) > 0 ,tail(Season[GB_pct >= 0.45], 1),2045)) < Season[fly_flag == 1][1], 1, 0),
            # heavy_fb = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
            #                    & (ifelse(length(Season[GB_pct >= 0.45]) > 0 ,tail(Season[GB_pct >= 0.45], 1),2045)) < Season[`FB_pct+` >= 110][1], 1, 0),
          ) %>%
  filter(is.na(under_45_first) | (first_year_gb < under_45_first), tot >= 2)

# HAD AT LEAST ONE SEASON OF < 45% AFTER HAVING > 50%
# !is.na(under_45_first), first_year_gb < under_45_first

# NEVER WENT BACK TO 50%
# !is.na(under_45_first), first_year_gb < under_45_first, last_50 < under_45_first

# NEVER WENT BACK TO 45%
# !is.na(under_45_first), first_year_gb < under_45_first, last_45 < under_45_first

collect_szns_impq <- sig_seasons %>%
  filter(xMLBAMID %in% gb_impq) %>%
  group_by(xMLBAMID,PlayerName) %>%
  reframe(tot = n(),
          gb = list(Season[ground_flag == 1]),
          fb = list(Season[fly_flag == 1]),
          plus_fb = list(Season[`FB_pct+` >= 110]),
          # all_after = list(Season[Season>=Season[`FB_pct+` >= 110][1]]),
          # both_year = list(Season[fly_flag == 1 & ground_flag == 1]),
          years_no_change = list(Season[Season <= tail(Season[GB_pct >= 0.45],1)]),
          years_sig_change = list(Season[Season > tail(Season[GB_pct >= 0.45],1)]),
          lad = list(Season[team_name == "LAD" & fly_flag == 1]),
          last_45 = ifelse(length(Season[GB_pct >= 0.45]) > 0, tail(Season[GB_pct >= 0.45], 1),NA),
          last_50 = ifelse(length(Season[GB_pct >= 0.50]) > 0, tail(Season[GB_pct >= 0.50], 1),NA),
          under_45_first = Season[`GB_pct` < 0.45][1],
          under_50_first = Season[`GB_pct` < 0.50][1],
          first_year_gb = ifelse(length(Season[ground_flag == 1]) > 0, Season[ground_flag == 1][1], NA),
          first_year_fb = ifelse(length(Season[fly_flag == 1]) > 0, Season[fly_flag == 1][1], NA)
          # last_year_A = ifelse(length(Season[ground_flag == 1]) > 0, tail(Season[ground_flag == 1], 1), NA),
          # first_year_b = Season[GB_pct >= 0.45 & (Season > Season[fly_flag == 1][1])][1],
          # no_back = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
          #                  & (ifelse(length(Season[ground_flag == 1]) > 0 ,tail(Season[ground_flag == 1], 1),2045)) < Season[fly_flag == 1][1], 1, 0),
          # no_back_2 = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
          #                    & (ifelse(length(Season[GB_pct >= 0.45]) > 0 ,tail(Season[GB_pct >= 0.45], 1),2045)) < Season[fly_flag == 1][1], 1, 0),
          # heavy_fb = ifelse(length(Season[ground_flag == 1]) > 0 & length(Season[fly_flag == 1]) > 0
          #                    & (ifelse(length(Season[GB_pct >= 0.45]) > 0 ,tail(Season[GB_pct >= 0.45], 1),2045)) < Season[`FB_pct+` >= 110][1], 1, 0),
  ) %>%
  filter(is.na(under_45_first) | (first_year_gb < under_45_first), tot >= 2)

# ALL HITTERS THAT STARTED WITH A SEASON OF >= 50% GB AND 90 EV BEFORE (OR NEVER) GOING BELOW 45% GB
# is.na(under_45_first) | (first_year_gb < under_45_first)

# HAD AT LEAST ONE SEASON OF < 45% AND 90 EV AFTER HAVING > 50% AND 90 EV,
# !is.na(under_45_first), first_year_gb < under_45_first

# NEVER WENT BACK TO 50% - IMPQ
# !is.na(first_year_fb), first_year_gb < first_year_fb, last_50 < under_45_first, tot >= 2

# NEVER WENT BACK TO 45% - IMPQ
# !is.na(first_year_fb), first_year_gb < first_year_fb, last_45 < under_45_first, tot >= 2


# indv_eval <- sig_seasons %>%
#   filter(PlayerName == "Carlos Correa") %>%
#   select(Season,team_name,ground_flag,fly_flag,GB_pct,`GB_pct+`,LD_pct,`LD_pct+`,FB_pct,`FB_pct+`,EV,HardHit_pct,wRC_plus,
#          wOBA,xwOBA)