library(dplyr)
library(baseballr)
library(purrr)
library(RPostgres)
library(tidyverse)
library(furrr)

start <- "2024-04-20" # NOT FOR MNL DATA
end <- "2024-04-21"

for (i in 1:10000){

  dates <- data.frame(day = rep(seq(as.Date(start), as.Date(end), by = 'days'),
                                times = 1))
  
  minor_league_game_pk_list <- 1:nrow(dates) %>% purrr::map(function(x) mlb_game_pks(dates$day[x],
                                                                                     level_ids = c(11:14,16)))
  
  ml_game_pks <- minor_league_game_pk_list %>% bind_rows() %>% dplyr::filter(status.codedGameState == "F",
                                                                             !is.na(game_pk)) %>%
    pull(game_pk)
  
  safe_pbp <- safely(mlb_pbp)
  
  ml_pbp <- 1:length(ml_game_pks) %>% furrr::future_map(function(x) safe_pbp(ml_game_pks[x]), .progress = T) %>%
    map('result') %>% bind_rows()
  
  ml_pbp <- ml_pbp %>% as.data.frame() 
  
  mnl_scrape <- ml_pbp %>%
    filter(type == "pitch", !is.na(pitchData.coordinates.pfxX),!is.na(pitchData.coordinates.pfxZ))
  
  common_cols <- intersect(colnames(mnl_sc_24), colnames(mnl_scrape))
  
  mnl_sc_24 <- rbind(
    subset(mnl_sc_24, select = common_cols), 
    subset(mnl_scrape, select = common_cols)
  )
  
  
  scrape <- scrape_statcast_savant(start_date = start, end_date = end, player_type = "pitcher")
  
  sc_2024 <- rbind(sc_2024,scrape)
  
  start = as.Date(end) + 1
  end = as.Date(start) + 1
  
  if (end >= as.Date("2024-04-20")){
    break
  } else {
    start = as.character(start)
    end = as.character(end)
  }

}

colnames(mnl_scrape[, !names(mnl_scrape) %in% names(mnl_sc_24)])
# xl2 <- rbind(xl,mnl_scrape)
