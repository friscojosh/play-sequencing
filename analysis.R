###############################
### Play sequencing analysis
##############################

library(tidyverse)

plays <- read_csv("data/play.csv") %>%
   select(-phase)
drives <- read_csv("data/drive.csv")
games <- read_csv("data/game.csv")
agg_play <- read_csv("data/agg_play.csv")
all_epa <- read_csv("data/all_epa.csv")

run_pass <- agg_play %>%
   select(gsis_id, play_id, passing_att, rushing_att, passing_cmp_air_yds, passing_incmp_air_yds, passing_yds, rushing_yds, passing_sk)

play_sequencing <- plays %>%
   left_join(games, by = "gsis_id") %>%
   left_join(run_pass, by = c("gsis_id", "play_id")) %>%
   left_join(all_epa, by = c("gsis_id" = "game_id", "play_id")) %>%
   select(-time_inserted.x, -time_inserted.y, -time_updated.x, -time_updated.y) %>%
   arrange(gsis_id, play_id) %>%
   mutate(success = ifelse(epa > 0, 1, 0)) %>%
   mutate(shotgun = ifelse(str_detect(description, "(Shotgun)"), 1, 0)) %>%
   mutate(two_min_warning = ifelse(str_detect(description, "Two-Minute Warning"), 1, 0)) %>%
   mutate(end_quarter = ifelse(str_detect(description, "END QUARTER"), 1, 0)) %>%
   mutate(penalty = ifelse(penalty == 1 | str_detect(description, "Penalty"), 1, 0)) %>%
   mutate(timeout = ifelse(str_detect(description, "Timeout"), 1, 0)) %>%
   ### we're calling this pass attempts but they're dropbacks since we include sacks
   mutate(passing_att = ifelse(passing_att == 1 | passing_sk == 1, 1, 0)) %>%
   mutate(down_na_check = ifelse(is.na(down) == FALSE, 1, 0)) %>%
   mutate(second_down = ifelse(down == 2 & down_na_check == 1, 1, 0)) %>%
   mutate(third_down = ifelse(down == 3 & down_na_check == 1, 1, 0)) %>%
   mutate(airyards = passing_cmp_air_yds + passing_incmp_air_yds) %>%
   ### fix instances where rushing atempts are > 1
   mutate(rushing_att = ifelse(rushing_att >= 1, 1, 0)) %>%
   mutate(passing_att = ifelse(passing_att >= 1, 1, 0)) %>%
   mutate(time = str_remove_all(time, "[()]")) %>%
   filter(season_type == "Regular",
          penalty == 0,
          two_min_warning == 0,
          end_quarter == 0,
          timeout == 0,
          down %in% 1:3) %>%
   ### should use a case when here but nevetheless
   mutate(sequence = ifelse(lag(rushing_att, 2) == 1 & lag(rushing_att, 1) == 1 & rushing_att == 1 & down == 3, "rush/rush/rush",
                     ifelse(lag(rushing_att, 2) == 1 & lag(rushing_att, 1) == 1 & passing_att == 1 & down == 3, "rush/rush/pass",
                     ifelse(lag(rushing_att, 2) == 1 & lag(passing_att, 1) == 1 & passing_att == 1 & down == 3, "rush/pass/pass",
                     ifelse(lag(passing_att, 2) == 1 & lag(passing_att, 1) == 1 & passing_att == 1 & down == 3, "pass/pass/pass",
                     ifelse(lag(passing_att, 2) == 1 & lag(rushing_att, 1) == 1 & passing_att == 1 & down == 3, "pass/rush/pass",
                     ifelse(lag(rushing_att, 2) == 1 & lag(passing_att, 1) == 1 & rushing_att == 1 & down == 3, "rush/pass/rush",
                     ifelse(lag(passing_att, 2) == 1 & lag(passing_att, 1) == 1 & rushing_att == 1 & down == 3, "pass/pass/rush",
                     ifelse(lag(passing_att, 2) == 1 & lag(rushing_att, 1) == 1 & rushing_att == 1 & down == 3, "pass/rush/rush",
                     ifelse(lag(rushing_att, 1) == 1 & rushing_att == 1 & down == 2, "rush/rush",
                     ifelse(lag(rushing_att, 1) == 1 & passing_att == 1 & down == 2, "rush/pass",
                     ifelse(lag(passing_att, 1) == 1 & rushing_att == 1 & down == 2, "pass/rush",
                     ifelse(lag(passing_att, 1) == 1 & passing_att == 1 & down == 2, "pass/pass",
                     ifelse(passing_att == 1 & down == 1, "pass",
                     ifelse(rushing_att == 1 & down == 1, "rush",
                     0
                     )))))))))))))))

play_sequencing[is.na(play_sequencing)] <- 0

### quick spot check
play_sequencing %>% select(down, rushing_att, passing_att, sequence, description)

yards_by_sequence <- play_sequencing %>%
   filter(sequence != 0,
          rushing_att != passing_att) %>%
   group_by(sequence, down) %>%
   summarize(to_go = round(mean(yards_to_go), 1),
             epa = round(mean(epa), 2),
             success = 100 * round(mean(success), 3),
             wpa = 100 * round(mean(wpa), 4),
             count = n()) %>%
   group_by(down) %>%
   mutate(freq = round(count / sum(count), 2))
