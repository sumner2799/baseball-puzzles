library(baseballr)
library(dplyr)

fg_grades <- read.csv("/Users/andrewsumner/Downloads/stuff_exp.csv")


################### FORMAT PLAYER NAMES ###################

fg_grades$Name <- format(as.person(fg_grades$Name), include=c("family","given"), braces=list(family=c("",",")))


################### RV/100 BY PITCH TYPE ###################

pitch_rv_21 <- sc_2021 %>%
  filter(game_type == "R") %>%
  group_by(pitcher,player_name, pitch_name) %>%
  summarise(`#` = n(),
            `Season` = "2021",
            `RV/100` = -1*round(sum(delta_run_exp,na.rm = T)/(n()/100),2)) %>%
  filter(`#` >= 100) %>%
  arrange(desc(`RV/100`))

pitch_rv_22 <- sc_2022 %>%
  filter(game_type == "R") %>%
  group_by(pitcher,player_name, pitch_name) %>%
  summarise(`#` = n(),
            `Season` = "2022",
            `RV/100` = -1*round(sum(delta_run_exp,na.rm = T)/(n()/100),2)) %>%
  filter(`#` >= 100) %>%
  arrange(desc(`RV/100`))

pitch_rv_23 <- sc_2023 %>%
  filter(game_type == "R") %>%
  group_by(pitcher,player_name, pitch_name) %>%
  summarise(`#` = n(),
            `Season` = "2023",
            `RV/100` = -1*round(sum(delta_run_exp,na.rm = T)/(n()/100),2)) %>%
  filter(`#` >= 100) %>%
  arrange(desc(`RV/100`))

pitch_rv <- rbind(pitch_rv_23,pitch_rv_22,pitch_rv_21)

confuse_sl <- pitch_rv %>%
  mutate(is_sl = ifelse(pitch_name %in% c("Sweeper","Slider"),1,0)) %>%
  group_by(pitcher,player_name,`Season`) %>%
  summarise(`# SL/SW` = sum(is_sl)) %>%
  filter(`# SL/SW` == 2)


pitch_rv$Season <- as.integer(pitch_rv$Season)
confuse_sl$Season <- as.integer(confuse_sl$Season)

add_model <- left_join(pitch_rv,fg_grades,by=c("player_name"="Name","Season"="season")) %>%
  # filter(!player_name %in% c(remove_sl)) %>%
  mutate(`Stuff+` = ifelse(pitch_name == "4-Seam Fastball",`Stf..FA`,
                           ifelse(pitch_name == "Sinker",`Stf..SI`,
                                  ifelse(pitch_name == "Changeup",`Stf..CH`,
                                         ifelse(pitch_name %in% c("Slider","Sweeper"),`Stf..SL`,
                                                ifelse(pitch_name == "Curveball",`Stf..CU`,
                                                       ifelse(pitch_name == "Splitter",`Stf..FS`,
                                                              ifelse(pitch_name == "Cutter",`Stf..FC`,
                                                                     ifelse(pitch_name == "Knuckle Curve",`Stf..KC`,NA)))))))),
         `Pitching+` = ifelse(pitch_name == "4-Seam Fastball",`Pit..FA`,
                           ifelse(pitch_name == "Sinker",`Pit..SI`,
                                  ifelse(pitch_name == "Changeup",`Pit..CH`,
                                         ifelse(pitch_name %in% c("Slider","Sweeper"),`Pit..SL`,
                                                ifelse(pitch_name == "Curveball",`Pit..CU`,
                                                       ifelse(pitch_name == "Splitter",`Pit..FS`,
                                                              ifelse(pitch_name == "Cutter",`Pit..FC`,
                                                                     ifelse(pitch_name == "Knuckle Curve",`Pit..KC`,NA))))))))) %>%
  select(Season,pitcher,player_name,pitch_name,`#`,`Stuff+`,`Pitching+`,`RV/100`)

remove_sl <- left_join(add_model,confuse_sl,by=c("player_name","Season")) %>%
  filter(is.na(`# SL/SW`), !is.na(`Stuff+`), !is.na(`Pitching+`)) %>%
  select(-c(`# SL/SW`,`pitcher.y`))

################### CALCULATE AVG AND STD DEV STUFF+ BY PITCH TYPE ###################

avg_sd_stf <- remove_sl %>%
  group_by(pitch_name) %>%
  summarise(`#` = n(),
            avg_stf = mean(`Stuff+`),
            sd_stf = sd(`Stuff+`))

avg_sd_pit <- remove_sl %>%
  group_by(pitch_name) %>%
  summarise(`#` = n(),
            avg_pit = mean(`Pitching+`),
            sd_pit = sd(`Pitching+`))


################### FLAG FOR SIGNATRUE PITCH & OVERPERFORMING PITCHES - STUFF+ ###################

combined_arsenal_stf <- left_join(remove_sl,avg_sd_stf,by=c("pitch_name")) %>%
  mutate(signature_pitch = ifelse(`Stuff+` > 100 & `RV/100` > 0,1,0),
         overperform_pitch = ifelse(`Stuff+` < 100 & `RV/100` > 0,1,0),
         bavg_pitch = ifelse(`Stuff+` < 100,1,0),
         aavg_pitch = ifelse(`Stuff+` > 100,1,0),
         waavg_pitch = ifelse(`Stuff+`>=(avg_stf+(sd_stf)),1,0),
         signature_waavg = ifelse((`Stuff+` >= (avg_stf+(sd_stf))) & (`RV/100` > 0),1,0)
         # waavg_pitch = ifelse(`Stuff+`>=(avg_stf+(sd_stf*2)),1,0),
         # signature_waavg = ifelse((`Stuff+` >= (avg_stf+(sd_stf*2))) & (`RV/100` > 0),1,0)
         ) %>%
  group_by(Season,player_name) %>%
  summarise(`#-Pitches` = n(),
            `#-BAVG` = sum(bavg_pitch,na.rm = T),
            `#-AAVG` = sum(aavg_pitch,na.rm = T),
            `#-WAAVG` = sum(waavg_pitch,na.rm = T),
            `#-Signature` = sum(signature_pitch,na.rm=T),
            `#-Signature/WAAVG` = sum(signature_waavg,na.rm=T),
            `%-Overperforming` = round(sum(overperform_pitch,na.rm=T)/sum(bavg_pitch,na.rm = T),2))


# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-AAVG` == 1, `#-Signature` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-AAVG` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature` == 1)

# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-WAAVG` == 1, `#-Signature/WAAVG` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-WAAVG` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature/WAAVG` == 1)
limit_arsenal_stf <- combined_arsenal_stf %>%
  filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature/WAAVG` < 1)



################### FLAG FOR SIGNATRUE PITCH & OVERPERFORMING PITCHES - PITCHING+ ###################

combined_arsenal_pit <- left_join(remove_sl,avg_sd_pit,by=c("pitch_name")) %>%
  mutate(signature_pitch = ifelse(`Pitching+` > 100 & `RV/100` > 0,1,0),
         overperform_pitch = ifelse(`Pitching+` < 100 & `RV/100` > 0,1,0),
         bavg_pitch = ifelse(`Pitching+` < 100,1,0),
         aavg_pitch = ifelse(`Pitching+` > 100,1,0),
         waavg_pitch = ifelse(`Pitching+`>=(avg_pit+(sd_pit)),1,0),
         signature_waavg = ifelse((`Pitching+` >= (avg_pit+(sd_pit))) & (`RV/100` > 0),1,0)
         # waavg_pitch = ifelse(`Pitching+`>=(avg_pit+(sd_pit*2)),1,0),
         # signature_waavg = ifelse((`Pitching+` >= (avg_pit+(sd_pit*2))) & (`RV/100` > 0),1,0)
  ) %>%
  group_by(Season,player_name) %>%
  summarise(`#-Pitches` = n(),
            `#-BAVG` = sum(bavg_pitch,na.rm = T),
            `#-AAVG` = sum(aavg_pitch,na.rm = T),
            `#-WAAVG` = sum(waavg_pitch,na.rm = T),
            `#-Signature` = sum(signature_pitch,na.rm=T),
            `#-Signature/WAAVG` = sum(signature_waavg,na.rm=T),
            `%-Overperforming` = round(sum(overperform_pitch,na.rm=T)/sum(bavg_pitch,na.rm = T),2))



# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-AAVG` == 1, `#-Signature` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-AAVG` == 1)

# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature/WAAVG` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-WAAVG` == 1, `#-Signature/WAAVG` == 1)
# filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-WAAVG` == 1)
limit_arsenal_pit <- combined_arsenal_pit %>%
  filter(`#-Pitches` > 2, `#-BAVG` >= 1, `#-Signature` < 1)

################### COMBINING SEASONS FOR YEAR-OVER-YEAR PITCH DEVELOPMENTS ###################

comb_1 <- inner_join(pitch_rv_22,pitch_rv_23,by=c("pitcher","player_name","pitch_name"))
comb_2 <- inner_join(pitch_rv_21,pitch_rv_22,by=c("pitcher","player_name","pitch_name"))

total <- rbind(comb_1,comb_2)

total$Season.x <- as.integer(total$Season.x)
total$Season.y <- as.integer(total$Season.y)

first_model <- left_join(total,fg_grades,by=c("player_name"="Name","Season.x"="season")) %>%
  # filter(!player_name %in% c(remove_sl)) %>%
  mutate(`Stuff+` = ifelse(pitch_name == "4-Seam Fastball",`Stf..FA`,
                           ifelse(pitch_name == "Sinker",`Stf..SI`,
                                  ifelse(pitch_name == "Changeup",`Stf..CH`,
                                         ifelse(pitch_name %in% c("Slider","Sweeper"),`Stf..SL`,
                                                ifelse(pitch_name == "Curveball",`Stf..CU`,
                                                       ifelse(pitch_name == "Splitter",`Stf..FS`,
                                                              ifelse(pitch_name == "Cutter",`Stf..FC`,
                                                                     ifelse(pitch_name == "Knuckle Curve",`Stf..KC`,NA)))))))),
         `Pitching+` = ifelse(pitch_name == "4-Seam Fastball",`Pit..FA`,
                              ifelse(pitch_name == "Sinker",`Pit..SI`,
                                     ifelse(pitch_name == "Changeup",`Pit..CH`,
                                            ifelse(pitch_name %in% c("Slider","Sweeper"),`Pit..SL`,
                                                   ifelse(pitch_name == "Curveball",`Pit..CU`,
                                                          ifelse(pitch_name == "Splitter",`Pit..FS`,
                                                                 ifelse(pitch_name == "Cutter",`Pit..FC`,
                                                                        ifelse(pitch_name == "Knuckle Curve",`Pit..KC`,NA))))))))) %>%
  select(1:9,`Stuff+`,`Pitching+`)

second_model <- left_join(first_model,fg_grades,by=c("player_name"="Name","Season.y"="season")) %>%
  # filter(!player_name %in% c(remove_sl)) %>%
  mutate(`Stuff+ y2` = ifelse(pitch_name == "4-Seam Fastball",`Stf..FA`,
                           ifelse(pitch_name == "Sinker",`Stf..SI`,
                                  ifelse(pitch_name == "Changeup",`Stf..CH`,
                                         ifelse(pitch_name %in% c("Slider","Sweeper"),`Stf..SL`,
                                                ifelse(pitch_name == "Curveball",`Stf..CU`,
                                                       ifelse(pitch_name == "Splitter",`Stf..FS`,
                                                              ifelse(pitch_name == "Cutter",`Stf..FC`,
                                                                     ifelse(pitch_name == "Knuckle Curve",`Stf..KC`,NA)))))))),
         `Pitching+ y2` = ifelse(pitch_name == "4-Seam Fastball",`Pit..FA`,
                              ifelse(pitch_name == "Sinker",`Pit..SI`,
                                     ifelse(pitch_name == "Changeup",`Pit..CH`,
                                            ifelse(pitch_name %in% c("Slider","Sweeper"),`Pit..SL`,
                                                   ifelse(pitch_name == "Curveball",`Pit..CU`,
                                                          ifelse(pitch_name == "Splitter",`Pit..FS`,
                                                                 ifelse(pitch_name == "Cutter",`Pit..FC`,
                                                                        ifelse(pitch_name == "Knuckle Curve",`Pit..KC`,NA))))))))) %>%
  select(1:11,`Stuff+ y2`,`Pitching+ y2`)

first_remove <- left_join(second_model,confuse_sl,by=c("player_name"="player_name","Season.x"="Season")) %>%
  filter(is.na(`# SL/SW`), !is.na(`Stuff+`), !is.na(`Pitching+`), !is.na(`Stuff+ y2`), 
         !is.na(`Pitching+ y2`)) %>%
  select(-c(`# SL/SW`,`pitcher.y`))

second_remove <- left_join(first_remove,confuse_sl,by=c("player_name"="player_name","Season.y"="Season")) %>%
  filter(is.na(`# SL/SW`)) %>%
  select(-c(`# SL/SW`,`pitcher`))


################### ADD FLAGS FOR YEAR-OVER-YEAR IMPROVEMENTS - STUFF+ ###################

add_flags_stf <- left_join(second_remove,avg_sd_stf,by=c("pitch_name")) %>%
  mutate(`BAVG both` = ifelse(`Stuff+` < 100 & `Stuff+ y2` < 100 & `RV/100.x` < 0,1,0),
         `BAVG Improved PERF` = ifelse(`Stuff+` < 100 & `Stuff+ y2` < 100 & `RV/100.x` < 0 & `RV/100.y` > 0,1,0),
         `AAVG y1` = ifelse(`Pitching+` > 100 & `RV/100.x` > 0,1,0),
         `AAVG y2` = ifelse(`Stuff+` < 100 & `Stuff+ y2` > 100 & `RV/100.y` > 0,1,0),
         `WAAVG y2` = ifelse((`Stuff+` < (avg_stf+(sd_stf))) & (`Stuff+ y2` >= (avg_stf+(sd_stf))) & `RV/100.y` > 0,1,0)) %>%
  group_by(pitcher.x,player_name,Season.x) %>%
  summarise(`Pitches` = n(),
            `AAVG y1` = sum(`AAVG y1`),
            `Total BAVG` = sum(`BAVG both`),
            `Total Improved` = sum(`BAVG Improved PERF`),
            `% of improved BAVG` = round(sum(`BAVG Improved PERF`)/sum(`BAVG both`),2),
            `Total AAVG` = sum(`AAVG y2`),
            `Total WAAVG` = sum(`WAAVG y2`))

improved_pitchers_stf <- add_flags_stf %>%
  filter(`Total WAAVG` >= 1, `Total BAVG` >= 1, `Pitches` >= 3)


################### ADD FLAGS FOR YEAR-OVER-YEAR IMPROVEMENTS - PITCHING+ ###################

add_flags_pit <- left_join(second_remove,avg_sd_pit,by=c("pitch_name")) %>%
  mutate(`BAVG both` = ifelse(`Pitching+` < 100 & `Pitching+ y2` < 100 & `RV/100.x` < 0,1,0),
         `BAVG Improved PERF` = ifelse(`Pitching+` < 100 & `Pitching+ y2` < 100 & `RV/100.x` < 0 & `RV/100.y` > 0,1,0),
         `AAVG y1` = ifelse(`Pitching+` > 100 & `RV/100.x` > 0,1,0),
         `WAAVG y1` = ifelse((`Pitching+` > (avg_pit+(sd_pit))) & `RV/100.x` > 0,1,0),
         `AAVG y2` = ifelse(`Pitching+` < 100 & `Pitching+ y2` > 100 & `RV/100.y` > 0,1,0),
         `WAAVG y2` = ifelse((`Pitching+` < (avg_pit+(sd_pit))) & (`Pitching+ y2` >= (avg_pit+(sd_pit))) & `RV/100.y` > 0,1,0)) %>%
  group_by(pitcher.x,player_name,Season.x) %>%
  summarise(`Pitches` = n(),
            `AAVG y1` = sum(`AAVG y1`),
            `WAAVG y1` = sum(`WAAVG y1`),
            `Total BAVG` = sum(`BAVG both`),
            `Total Improved` = sum(`BAVG Improved PERF`),
            `% of improved BAVG` = round(sum(`BAVG Improved PERF`)/sum(`BAVG both`),2),
            `Total AAVG` = sum(`AAVG y2`),
            `Total WAAVG` = sum(`WAAVG y2`))

improved_pitchers_pit <- add_flags_pit %>%
  filter(`Total AAVG` >= 1, `Total BAVG` >= 1, `Pitches` >= 3, `AAVG y1` == 0)
