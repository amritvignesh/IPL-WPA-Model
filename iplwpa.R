library(cricketdata)
library(dplyr)
library(xgboost)
library(vip)
library(caret)
library(gt)

og_data <- fetch_cricsheet("bbb", "male", "ipl")
matches <- fetch_cricsheet("match", "male", "ipl")

result_matches <- matches %>%
  filter((outcome == "tie" | is.na(outcome)) & is.na(method))

filt_data <- og_data %>%
  filter(match_id %in% result_matches$match_id) %>%
  mutate(data_num = row_number(), win = as.factor(ifelse((innings == 1 & innings1_total > innings2_total) | (innings == 2 & innings2_total > innings1_total), 1, 0)))

filt_data$season[which(filt_data$season == "2007/08")] <- 2008
filt_data$season[which(filt_data$season == "2009/10")] <- 2010
filt_data$season[which(filt_data$season == "2020/21")] <- 2020

inning1 <- filt_data %>%
  filter(innings == 1) %>%
  select(data_num, season, win, balls_remaining, runs_scored_yet, wickets_lost_yet)

inning2 <- filt_data %>%
  filter(innings == 2) %>%
  mutate(runs_left = target - runs_scored_yet) %>%
  select(data_num, season, win, balls_remaining, runs_scored_yet, wickets_lost_yet, runs_left)

xgboost_train_inning1 <- inning1 %>%
  filter(season < 2021)

xgboost_test_inning1 <- inning1 %>%
  filter(season >= 2021)

labels_train_inning1 <- as.matrix(xgboost_train_inning1[,3])
xgboost_trainfinal_inning1 <- as.matrix(xgboost_train_inning1[,c(4:6)])
xgboost_testfinal_inning1 <- as.matrix(xgboost_test_inning1[,c(4:6)])

wp_model_inning1 <- xgboost(data = xgboost_trainfinal_inning1, label = labels_train_inning1, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(wp_model_inning1)

wp_inning1_predict <- predict(wp_model_inning1, xgboost_testfinal_inning1)
wp_inning1 <- as.matrix(xgboost_test_inning1[,3])

wp_inning1_predictions <- as.data.frame(
  matrix(predict(wp_model_inning1, as.matrix(inning1[,c(4:6)])))
)

inning1_wp <- cbind(inning1, wp_inning1_predictions) %>%
  select(data_num, wp_1 = V1)

xgboost_train_inning2 <- inning2 %>%
  filter(season < 2021)

xgboost_test_inning2 <- inning2 %>%
  filter(season >= 2021)

labels_train_inning2 <- as.matrix(xgboost_train_inning2[,3])
xgboost_trainfinal_inning2 <- as.matrix(xgboost_train_inning2[,c(4:7)])
xgboost_testfinal_inning2 <- as.matrix(xgboost_test_inning2[,c(4:7)])

wp_model_inning2 <- xgboost(data = xgboost_trainfinal_inning2, label = labels_train_inning2, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(wp_model_inning2)

wp_inning2_predict <- predict(wp_model_inning2, xgboost_testfinal_inning2)
wp_inning2 <- as.matrix(xgboost_test_inning2[,3])

wp_inning2_predictions <- as.data.frame(
  matrix(predict(wp_model_inning2, as.matrix(inning2[,c(4:7)])))
)

inning2_wp <- cbind(inning2, wp_inning2_predictions) %>%
  select(data_num, wp_2 = V1)

data <- left_join(filt_data, inning1_wp, by = "data_num")
data <- left_join(data, inning2_wp, by = "data_num")

data <- data %>%
  mutate(wp = ifelse(innings == 1, wp_1, wp_2)) %>%
  filter(!is.na(wp)) # super over innings

full_data <- data %>%
  group_by(match_id, innings) %>%
  mutate(pre_wp = lag(wp)) %>%
  ungroup() %>%
  mutate(pre_wp = ifelse(innings == 2 & is.na(pre_wp), 1 - lag(wp), ifelse(innings == 1 & is.na(pre_wp), 0.5, pre_wp)), bat_wpa = wp - pre_wp, bowl_wpa = -bat_wpa)

stats_2023 <- full_data %>% filter(season == 2023)

batting_wpa <- stats_2023 %>%
  group_by(batsman = striker, team = batting_team) %>%
  summarize(total_wpa = sum(bat_wpa, na.rm = TRUE), wpa_per_play = mean(bat_wpa, na.rm = TRUE), runs = sum(runs_off_bat, na.rm = TRUE), outs = sum(wicket, na.rm = TRUE), balls = n(), strike_rate = runs/balls * 100) %>%
  ungroup() %>%
  mutate(total_wpa = round(total_wpa, digits = 4), wpa_per_play = round(wpa_per_play, digits = 4)) %>%
  arrange(-total_wpa)

bowling_wpa <- stats_2023 %>%
  mutate(runs_gained = runs_off_bat + extras) %>%
  group_by(bowler, team = bowling_team) %>%
  summarize(total_wpa = sum(bowl_wpa, na.rm = TRUE), wpa_per_ball = mean(bowl_wpa, na.rm = TRUE), wickets = sum(wicket, na.rm = TRUE), runs_given = sum(runs_gained, na.rm = TRUE), balls = n(), economy = runs_given/balls * 6) %>%
  ungroup() %>%
  mutate(total_wpa = round(total_wpa, digits = 4), wpa_per_ball = round(wpa_per_ball, 4)) %>%
  arrange(-total_wpa)

batting_wpa_over <- stats_2023 %>%
  group_by(batsman = striker, team = batting_team, over) %>%
  summarize(total_wpa = sum(bat_wpa, na.rm = TRUE), wpa_per_play = mean(bat_wpa, na.rm = TRUE), runs = sum(runs_off_bat, na.rm = TRUE), outs = sum(wicket, na.rm = TRUE), balls = n(), strike_rate = runs/balls * 100) %>%
  ungroup() %>%
  mutate(total_wpa = round(total_wpa, digits = 4), wpa_per_play = round(wpa_per_play, digits = 4)) %>%
  group_by(over) %>%
  arrange(-total_wpa) %>%
  filter(row_number() <= 1) %>%
  arrange(over) %>%
  ungroup()

bowling_wpa_over <- stats_2023 %>%
  mutate(runs_gained = runs_off_bat + extras) %>%
  group_by(bowler, team = bowling_team, over) %>%
  summarize(total_wpa = sum(bowl_wpa, na.rm = TRUE), wpa_per_ball = mean(bowl_wpa, na.rm = TRUE), wickets = sum(wicket, na.rm = TRUE), runs_given = sum(runs_gained, na.rm = TRUE), balls = n(), economy = runs_given/balls * 6) %>%
  ungroup() %>%
  mutate(total_wpa = round(total_wpa, digits = 4), wpa_per_ball = round(wpa_per_ball, 4)) %>%
  group_by(over) %>%
  arrange(-total_wpa) %>%
  filter(row_number() <= 1) %>%
  arrange(over) %>%
  ungroup()

top15_batting_gt <- batting_wpa %>% filter(row_number() <= 15)
top15_bowling_gt <- bowling_wpa %>% filter(row_number() <= 15)

bat_gt_1 <- top15_batting_gt %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(batsman, team, total_wpa, wpa_per_play, runs, outs, balls, strike_rate)
  ) %>%
  data_color(
    columns = c(total_wpa, wpa_per_play, strike_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    batsman = md("**Batsman**"),
    team = md("**Team**"),
    total_wpa = md("**Total WPA**"),
    wpa_per_play = md("**WPA Per Ball**"),
    runs = md("**Runs**"),
    outs = md("**Outs**"),
    balls = md("**Balls**"),
    strike_rate = md("**Strike Rate**"),
  ) %>%
  tab_header(
    title = md("**2023 IPL Batting Win Probability Added Leaders**"),
    subtitle = "Based on IPL Ball-By-Ball Data from 2008 - 2020 Trained on 2021 - 2023 With No Result and D/L Games Excluded"
  )
gtsave(bat_gt_1, "bat_gt_1.png")

bowl_gt_1 <- top15_bowling_gt %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(bowler, team, total_wpa, wpa_per_ball, wickets, runs_given, balls, economy)
  ) %>%
  data_color(
    columns = c(total_wpa, wpa_per_ball),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = economy,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE  
    ) 
  ) %>%
  cols_label(
    bowler = md("**Bowler**"),
    team = md("**Team**"),
    total_wpa = md("**Total WPA**"),
    wpa_per_ball = md("**WPA Per Ball**"),
    wickets = md("**Wickets**"),
    runs_given = md("**Runs Given**"),
    balls = md("**Balls**"),
    economy = md("**Economy**"),
  ) %>%
  tab_header(
    title = md("**2023 IPL Bowling Win Probability Added Leaders**"),
    subtitle = "Based on IPL Ball-By-Ball Data from 2008 - 2020 Trained on 2021 - 2023 With No Result and D/L Games Excluded"
  )
gtsave(bowl_gt_1, "bowl_gt_1.png")

bat_over_gt <- batting_wpa_over %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(over, batsman, team, total_wpa, wpa_per_play, runs, outs, balls, strike_rate)
  ) %>%
  data_color(
    columns = c(total_wpa, wpa_per_play, strike_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    over = md("**Over**"),
    batsman = md("**Batsman**"),
    team = md("**Team**"),
    total_wpa = md("**Total WPA**"),
    wpa_per_play = md("**WPA Per Ball**"),
    runs = md("**Runs**"),
    outs = md("**Outs**"),
    balls = md("**Balls**"),
    strike_rate = md("**Strike Rate**"),
  ) %>%
  tab_header(
    title = md("**2023 IPL Batting Win Probability Added Leaders By Over**"),
    subtitle = "Based on IPL Ball-By-Ball Data from 2008 - 2020 Trained on 2021 - 2023 With No Result and D/L Games Excluded"
  )
gtsave(bat_over_gt, "bat_over_gt.png")

bat_over_gt <- batting_wpa_over %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(over, batsman, team, total_wpa, wpa_per_play, runs, outs, balls, strike_rate)
  ) %>%
  data_color(
    columns = c(total_wpa, wpa_per_play, strike_rate),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    over = md("**Over**"),
    batsman = md("**Batsman**"),
    team = md("**Team**"),
    total_wpa = md("**Total WPA**"),
    wpa_per_play = md("**WPA Per Ball**"),
    runs = md("**Runs**"),
    outs = md("**Outs**"),
    balls = md("**Balls**"),
    strike_rate = md("**Strike Rate**"),
  ) %>%
  tab_header(
    title = md("**2023 IPL Batting Win Probability Added Leaders By Over**"),
    subtitle = "Based on IPL Ball-By-Ball Data from 2008 - 2020 Trained on 2021 - 2023 With No Result and D/L Games Excluded"
  )
gtsave(bat_over_gt, "bat_over_gt.png")

bowl_over_gt <- bowling_wpa_over %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(over, bowler, team, total_wpa, wpa_per_ball, wickets, runs_given, balls, economy)
  ) %>%
  data_color(
    columns = c(total_wpa, wpa_per_ball),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = economy,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE  
    ) 
  ) %>%
  cols_label(
    over = md("**Over**"),
    bowler = md("**Bowler**"),
    team = md("**Team**"),
    total_wpa = md("**Total WPA**"),
    wpa_per_ball = md("**WPA Per Ball**"),
    wickets = md("**Wickets**"),
    runs_given = md("**Runs Given**"),
    balls = md("**Balls**"),
    economy = md("**Economy**"),
  ) %>%
  tab_header(
    title = md("**2023 IPL Bowling Win Probability Added Leaders by Over**"),
    subtitle = "Based on IPL Ball-By-Ball Data from 2008 - 2020 Trained on 2021 - 2023 With No Result and D/L Games Excluded"
  )
gtsave(bowl_over_gt, "bowl_over_gt.png")