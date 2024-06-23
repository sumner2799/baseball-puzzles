library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(cowplot)

############################ LOAD ARM SLOT DATA AND JOIN TO STATCAST ############################

arm_slots <- read.csv("MLB Arm Angles - Pitch Level Arm Angles.csv")

sc_all <- rbind(sc_2024,sc_2023,sc_2022,sc_2021)

df <- left_join(sc_all,arm_slots,by=c("pitcher","game_year","pitch_type"))


################### COMPARE AVG HRA AND SLOT DIFFERENTIALS BETWEEN 3/4 SLOTS (HIGH V LOW) ####################


rel_angle_diff <- df %>%
  filter(game_type == "R",pitch_name %in% c("4-Seam Fastball","Sweeper")) %>%
  mutate(vy_s = -sqrt(vy0**2 - 2 * ay * (60.5 - release_extension - 50)),
         pfx_x_adj = pfx_x*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(t_s = (vy_s - vy0) / ay) %>%
  mutate(vx_s = vx0 - ax * t_s,
         vz_s = vz0 - az * t_s) %>%
  mutate(HRA = -atan(vx_s / vy_s) * (180 / pi),
         VRA = -atan(vz_s / vy_s) * (180 / pi)) %>%
  group_by(player_name,game_year,pitch_name,arm_slot) %>%
  summarise(tot = n(),
            avg_hra = mean(HRA,na.rm = T),
            avg_vra = mean(VRA,na.rm = T),
            slot = mean(arm_angle,na.rm = T),
            hb = mean(pfx_x_adj,na.rm = T),
            ivb = mean(pfx_z_adj,na.rm = T)) %>%
  filter(tot >= 50) %>%
  group_by(player_name,game_year,arm_slot) %>%
  summarise(tot = n(),
            fb_slot = slot[pitch_name == "4-Seam Fastball"],
            st_slot = slot[pitch_name == "Sweeper"],
            slot_diff = abs(slot[pitch_name == "4-Seam Fastball"] - slot[pitch_name == "Sweeper"]),
            hra_diff = abs(avg_hra[pitch_name == "4-Seam Fastball"] - avg_hra[pitch_name == "Sweeper"]),
            vra_diff = abs(avg_vra[pitch_name == "4-Seam Fastball"] - avg_vra[pitch_name == "Sweeper"]),
            sl_hb = hb[pitch_name == "Sweeper"]) %>%
  filter(tot == 2,!is.na(arm_slot)) %>%
  group_by(arm_slot) %>%
  summarise(tot = n(),
            avg_hra = mean(hra_diff),
            avg_vra = mean(vra_diff))


slot_diff <- df %>%
  filter(game_type == "R",pitch_name %in% c("4-Seam Fastball","Sweeper")) %>%
  mutate(vy_s = -sqrt(vy0**2 - 2 * ay * (60.5 - release_extension - 50)),
         pfx_x_adj = pfx_x*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(t_s = (vy_s - vy0) / ay) %>%
  mutate(vx_s = vx0 - ax * t_s,
         vz_s = vz0 - az * t_s) %>%
  mutate(HRA = -atan(vx_s / vy_s) * (180 / pi),
         VRA = -atan(vz_s / vy_s) * (180 / pi)) %>%
  group_by(player_name,game_year,pitch_name,arm_slot) %>%
  summarise(tot = n(),
            avg_hra = mean(HRA,na.rm = T),
            avg_vra = mean(VRA,na.rm = T),
            slot = mean(arm_angle,na.rm = T),
            hb = mean(pfx_x_adj,na.rm = T),
            ivb = mean(pfx_z_adj,na.rm = T)) %>%
  filter(tot >= 50) %>%
  group_by(player_name,game_year,arm_slot) %>%
  summarise(tot = n(),
            fb_slot = slot[pitch_name == "4-Seam Fastball"],
            st_slot = slot[pitch_name == "Sweeper"],
            slot_diff = abs(slot[pitch_name == "4-Seam Fastball"] - slot[pitch_name == "Sweeper"]),
            hra_diff = abs(avg_hra[pitch_name == "4-Seam Fastball"] - avg_hra[pitch_name == "Sweeper"]),
            vra_diff = abs(avg_vra[pitch_name == "4-Seam Fastball"] - avg_vra[pitch_name == "Sweeper"]),
            sl_hb = hb[pitch_name == "Sweeper"]) %>%
  filter(tot == 2,!is.na(arm_slot)) %>%
  mutate(flag = ifelse(slot_diff <= 5,1,0)) %>%
  group_by(flag) %>%
  summarise(avg_hra = mean(hra_diff),
            avg_vra = mean(vra_diff))


################### COMPLETE DF W/ ST PERFORMANCE ####################

fb_v_st <- df %>%
  filter(game_type == "R",pitch_name %in% c("4-Seam Fastball","Sweeper")) %>%
  mutate(vy_s = -sqrt(vy0**2 - 2 * ay * (60.5 - release_extension - 50)),
         pfx_x_adj = pfx_x*12,
         pfx_z_adj = pfx_z*12) %>%
  mutate(t_s = (vy_s - vy0) / ay) %>%
  mutate(vx_s = vx0 - ax * t_s,
         vz_s = vz0 - az * t_s) %>%
  mutate(HRA = -atan(vx_s / vy_s) * (180 / pi),
         VRA = -atan(vz_s / vy_s) * (180 / pi)) %>%
  group_by(player_name,game_year,pitch_name,arm_slot) %>%
  summarise(tot = n(),
            velo = mean(release_speed,na.rm = T),
            avg_hra = mean(HRA,na.rm = T),
            avg_vra = mean(VRA,na.rm = T),
            slot = mean(arm_angle,na.rm = T),
            hb = mean(pfx_x_adj,na.rm = T),
            ivb = mean(pfx_z_adj,na.rm = T),
            `Swing%` = round(100*(sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                         "swinging_strike_blocked","foul_tip","foul_bunt"))/n()),1),
            `Whiff%` = round(100*(sum(description %in% c("swinging_strike","swinging_strike_blocked",
                                                         "foul_tip"),na.rm = T)/
                                    sum(description %in% c("foul","hit_into_play","swinging_strike",
                                                           "swinging_strike_blocked","foul_tip",
                                                           "bunt_foul_tip","missed_bunt","foul_bunt"),
                                        na.rm = T)),1)) %>%
  filter(tot >= 50) %>%
  group_by(player_name,game_year,arm_slot) %>%
  summarise(tot = n(),
            fb_slot = slot[pitch_name == "4-Seam Fastball"],
            st_slot = slot[pitch_name == "Sweeper"],
            slot_diff = abs(slot[pitch_name == "4-Seam Fastball"] - slot[pitch_name == "Sweeper"]),
            hra_diff = abs(avg_hra[pitch_name == "4-Seam Fastball"] - avg_hra[pitch_name == "Sweeper"]),
            vra_diff = abs(avg_vra[pitch_name == "4-Seam Fastball"] - avg_vra[pitch_name == "Sweeper"]),
            sl_velo = velo[pitch_name == "Sweeper"],
            sl_hb = abs(hb[pitch_name == "Sweeper"]),
            sl_swing = `Swing%`[pitch_name == "Sweeper"],
            sl_whiff = `Whiff%`[pitch_name == "Sweeper"]) %>%
  filter(tot == 2,!is.na(arm_slot))

# cor(fb_v_st$fb_slot,fb_v_st$vra_diff)
# plot(fb_v_st$fb_slot,fb_v_st$vra_diff)

################### BUCKET HRA/VRA BY SLOT & ANALYZE PERF BY VRA BUCKET ####################

slot_breaks <- seq(min(fb_v_st$fb_slot), max(fb_v_st$fb_slot), length.out = 10)

slot_buckets <- fb_v_st %>%
  filter(fb_slot < 90, !is.na(fb_slot)) %>%
  mutate(slot_bucket = cut(fb_slot,breaks = slot_breaks)) %>%
  group_by(slot_bucket) %>%
  summarise(tot = n(),
            hra = mean(hra_diff),
            vra = mean(vra_diff))

vra_breaks <- seq(min(fb_v_st$vra_diff), max(fb_v_st$vra_diff), length.out = 10)

vra_buckets <- fb_v_st %>%
  filter(fb_slot < 90, !is.na(fb_slot)) %>%
  mutate(vra_bucket = cut(vra_diff,breaks = vra_breaks)) %>%
  group_by(vra_bucket) %>%
  summarise(tot = n(),
            hra = mean(hra_diff),
            hb = mean(sl_hb),
            swing = mean(sl_swing),
            whiff = mean(sl_whiff))


################### CREATE THIELBAR VISUALS ####################

ggplot(data = fb_v_st, aes(x = fb_slot,y = vra_diff)) + 
  geom_point() +
  geom_text(aes(label = ifelse(player_name == "Thielbar, Caleb", paste("Thielbar",game_year), "")), vjust = -1) +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "VRA Diff. vs 4-Seam Arm Angle", x = "FB Arm Angle", y = "VRA Diff.") + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90"))

thielbar_2 <- fb_v_st %>%
  filter(player_name == "Thielbar, Caleb") %>%
  mutate(`FG ST Location+` = ifelse(game_year %in% c(2021,2022),106,
                                    ifelse(game_year == 2023, 113,101)),
         `VRA Diff.` = round(vra_diff,2),
         `ST Velo` = round(sl_velo,1),
         `ST HB` = round(sl_hb,1),
         `ST Whiff%` = round(sl_whiff,1),
         `Season` = game_year) %>%
  ungroup() %>%
  select(`Season`,`VRA Diff.`,`ST Velo`,`ST HB`,`ST Whiff%`,`FG ST Location+`)

table_theme <- ttheme_default(
  core = list(bg_params = list(fill = c("gray95", "gray90"))),
  colhead = list(fg_params = list(col = "gray95"),
                 bg_params = list(fill = "black"))
)

table_thielbar <- tableGrob(thielbar_2, rows = NULL, theme = table_theme)

table_thielbar <- gtable_add_grob(table_thielbar, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(table_thielbar), l = 1, r = ncol(table_thielbar))

table_thielbar <- gtable_add_grob(table_thielbar, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(table_thielbar))

format_table_thielbar <- plot_grid(NULL, table_thielbar, NULL, ncol = 3, rel_widths = c(1/10, 8/10, 1/10))

