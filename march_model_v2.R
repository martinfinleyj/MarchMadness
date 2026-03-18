library(xgboost)
library(dplyr)
library(tidyverse)

kenbart <- read.csv("KenPom Barttorvik Updated.csv")
kenpre <- read.csv("KenPom Preseason Updated.csv")
matchups <- read.csv("Tournament Matchups.csv")
matchups_25 <- read.csv("2025_games.csv")
evanmiya <- read.csv("EvanMiya Updated.csv")
shooting <- read.csv("Shooting Splits Updated.csv")
matchups_26 <- read.csv("2026_Potential_Matchups.csv")
rppf <- read.csv("RPPF Ratings.csv")
height <- read.csv("INT _ KenPom _ Height.csv")
resumes <- read.csv("Resumes Updated.csv")

matchups_25_clean <- matchups_25 %>% mutate(ID = 945 + row_number()) %>% mutate(A_TEAM = str_to_upper(str_trim(A_TEAM)), B_TEAM = str_to_upper(str_trim(B_TEAM))) %>% relocate(ID) %>% rename(TEAM_A_WIN = "A_TEAM_WIN")
matchups_26_clean <- matchups_26 %>% mutate(ID = 1008 + row_number()) %>% rename(A_TEAM = "HigherSeed", TEAM_A_SEED  = "HigherSeedNum", B_TEAM = "LowerSeed", TEAM_B_SEED = "LowerSeedNum") %>% 
  select(-HigherSeedID, -LowerSeedID, -Predictions) %>%
  mutate(A_TEAM = str_to_upper(str_trim(A_TEAM)), B_TEAM = str_to_upper(str_trim(B_TEAM))) %>%
  mutate("YEAR" = 2026) %>%
  mutate(across(c(A_TEAM, B_TEAM), ~recode(.x,
        "ST JOHN'S" = "ST. JOHN'S", "MICHIGAN ST" = "MICHIGAN ST.",
        "OHIO ST" = "OHIO ST.", "ST MARY'S CA" = "SAINT MARY'S",
        "IOWA ST" = "IOWA ST.", "KENNESAW" = "KENNESAW ST.",
        "MCNEESE ST" = "MCNEESE ST.", "N DAKOTA ST" = "NORTH DAKOTA ST.", "NC STATE" = "NORTH CAROLINA ST.",
        "PRAIRIE VIEW" = "PRAIRIE VIEW A&M", "QUEENS NC" = "QUEENS", "ST LOUIS" = "SAINT LOUIS",
        "TENNESSEE ST" = "TENNESSEE ST.", "UTAH ST" = "UTAH ST.", "WRIGHT ST" = "WRIGHT ST."
    )
  ))

team_ref <- tibble("HigherSeed" = matchups_26_clean$A_TEAM,
                   "LowerSeed" = matchups_26_clean$B_TEAM,
                   "id_a" = matchups_26$HigherSeed,
                   "id_b" = matchups_26$LowerSeed)

matchups_clean <- matchups %>% filter(!(YEAR %in% c(2008, 2009, 2010, 2011, 2021, 2025))) %>% 
  mutate(ID = ceiling(row_number() / 2)) %>% mutate(TEAM = str_to_upper(str_trim(TEAM)))

flat_matchups <- matchups_clean %>%
  group_by(ID) %>% 
  summarise("YEAR" = first(YEAR), "ROUND" = first(CURRENT.ROUND), 
            "A_TEAM" = first(TEAM), "B_TEAM" = last(TEAM),
            "TEAM_A_SEED" = first(SEED), "TEAM_B_SEED" = last(SEED),
            "TEAM_A_SCORE" = first(SCORE), "TEAM_B_SCORE" = last(SCORE),
            "TEAM_A_WIN" = as.integer(first(SCORE) > last(SCORE)))

flat_matchups <- flat_matchups %>% bind_rows(matchups_25_clean)
flat_matchups <- flat_matchups %>% bind_rows(matchups_26_clean)

height_clean <- height %>% rename(TEAM = "TeamName", YEAR = "Season") %>% mutate(TEAM = str_to_upper(str_trim(TEAM)))
rppf_clean <- rppf %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(YEAR, TEAM, RPPF.RATING.RANK:STREM.RANK.2)
shooting_clean <- shooting %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(YEAR, TEAM, DUNKS.FG.:THREES.D.SHARE.RANK)
evanmiya_clean <- evanmiya %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(YEAR, TEAM, O.RATE:TOTAL.KILL.SHOTS.CONCEDED.RANK)
resumes_clean <- resumes %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(YEAR, TEAM, NET.RPI:R.SCORE)
kenbart_clean <- kenbart %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(YEAR, TEAM, K.TEMPO:AST.)
kenpre <- kenpre %>% mutate(TEAM = str_to_upper(str_trim(TEAM))) %>% select(-TEAM.NO, -SEED, -ROUND)

kenpom <- list(kenbart_clean, kenpre, evanmiya_clean, shooting_clean, rppf_clean, height_clean, resumes_clean)
stats_combined <- flat_matchups

for (df in kenpom) {
  
  df_A <- df %>%
    rename_with(.cols = c(-YEAR, -TEAM), .fn = ~ paste0(.x, "_A"))
  
  df_B <- df %>%
    rename_with(.cols = c(-YEAR, -TEAM), .fn = ~ paste0(.x, "_B"))
  
  stats_combined <- stats_combined %>%
    left_join(df_A, by = c("YEAR", "A_TEAM" = "TEAM")) %>%
    left_join(df_B, by = c("YEAR", "B_TEAM" = "TEAM"))

}

stats <- c("BADJ.EM", "KADJ.EM.CHANGE", "KO.RANK", "RADJ.EM", "RADJ.D", "RPPF.RATING.RANK", "R.PACE.RANK", "CenterPts", "Q2.W", "B.POWER",
              "KADJ.T.CHANGE", "RADJ.D.RANK", "CenterOR", "KADJ.EM", "SFPts", "STROE.RANK", "NPB.RATING", "NPB.RATING.RANK",
           "PRESEASON.KADJ.EM", "THREES.FG.D", "STRDE.RANK", "D.RATE", "BLK.")
stats_rat <- c("RADJ.O", "KADJ.O", "PRESEASON.KADJ.EM", "ELO", "SGPts", 'R.PACE', "RPPF.RATING")
net_stats <- stats_combined

for (stat in stats) {

  net_stats <- net_stats %>% mutate(!!paste0(stat, "_NET") := stats_combined[[paste0(stat, "_A")]] - stats_combined[[paste0(stat, "_B")]]) %>%
    select(-ends_with("_A"), -ends_with("_B"))

}

for (stat in stats_rat) {
  
  net_stats <- net_stats %>% mutate(!!paste0(stat, "_RAT") := stats_combined[[paste0(stat, "_A")]] / stats_combined[[paste0(stat, "_B")]]) %>%
    select(-ends_with("_A"), -ends_with("_B"))
  
}

#net_stats <- net_stats %>% mutate("SEED_NET" = TEAM_B_SEED - TEAM_A_SEED)

#testing years since 2012. gets rid of past styles not relevant#
test_years <- c(2012, 2013, 2014, 2015, 2016, 2017,
                2018, 2019, 2022, 2023, 2024, 2025)

importance_list = list()

stats_26 <- net_stats %>% filter(YEAR == 2026)
net_stats <- net_stats %>% filter(YEAR != 2026)

df_prob <- net_stats

df_prob$pred_prob <- NA

for (test_year in test_years) {

  set.seed(1000 + test_year)
  
  train_data <- net_stats %>% filter(YEAR != test_year) %>% select(-ID:-TEAM_B_SCORE)
  test_data  <- net_stats %>% filter(YEAR == test_year) %>% select(-ID:-TEAM_B_SCORE)
  
  y_train <- train_data$TEAM_A_WIN

  X_train <- train_data %>% select(-TEAM_A_WIN)
  X_test  <- test_data %>% select(-TEAM_A_WIN)
  
  dtrain <- xgb.DMatrix(as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(as.matrix(X_test))
  
  params <- list(
    objective = "binary:logistic",
    booster = 'gblinear',
    eta = 0.35
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 2000,
    evals = list(train=dtrain),
    print_every_n = 250
  )
  
  probs <- predict(model, dtest)
  
  test_rows <- net_stats$YEAR == test_year
  
  importance_list[[as.character(test_year)]] <- xgb.importance(feature_names = colnames(X_train), model = model)
  
  df_prob$pred_prob[test_rows] <- probs

  df_prob <- df_prob %>% relocate(pred_prob, TEAM_A_WIN)
  

}

year.to.pred <- c()
brier_scores <- c()

df_prob <- df_prob %>% mutate("PRED" = if_else(pred_prob > 0.5, 1, 0)) %>% relocate(PRED)
                                                       
df_prob <- df_prob %>% mutate("BRIER" = (pred_prob - TEAM_A_WIN)^2) %>% relocate(BRIER)

for (year in test_years) {

  df_year <- df_prob %>% filter(YEAR == year)
  pred_mean <- mean(df_year$PRED == df_year$TEAM_A_WIN)
  year.to.pred <- append(year.to.pred, pred_mean)
  brier_score <- mean(df_year$BRIER)
  brier_scores <- append(brier_scores, brier_score)

}

all_importance <- bind_rows(importance_list, .id = "year")

#re-training data with all years, testing on 2026 #

train_full <- net_stats %>% select(-ID:-TEAM_B_SCORE)

y_full <- train_full$TEAM_A_WIN
X_full <- train_full %>% select(-TEAM_A_WIN)

dtrain_full <- xgb.DMatrix(as.matrix(X_full), label = y_full)

final_model <- xgb.train(
  params = params,
  data = dtrain_full,
  evals = list(train=dtrain_full),
  nrounds = 2000
)

final_26 <- stats_26 %>% select(-ID:-TEAM_A_WIN)
final_26 <- as.matrix(final_26)
test.2026 <- xgb.DMatrix(data = final_26)
preds.2026 <- predict(final_model, test.2026)
predictions <- cbind(stats_26, preds.2026)
predictions <- predictions %>% relocate(preds.2026)

og_submission_df <- predictions %>% select(A_TEAM, B_TEAM, preds.2026) %>% rename(HigherSeed = "A_TEAM", LowerSeed = "B_TEAM", Predictions = "preds.2026")
submission_df <- og_submission_df %>% left_join(team_ref, by = c("HigherSeed", "LowerSeed")) %>% select(-HigherSeed, -LowerSeed) %>% 
  rename(HigherSeed = "id_a", LowerSeed = "id_b") %>% relocate(c(HigherSeed, LowerSeed))

matchups_26_new <- matchups_26 %>% left_join(submission_df, by = c("HigherSeed", "LowerSeed")) %>% select(-Predictions.x) %>% rename(Predictions = "Predictions.y")
write.csv(matchups_26_new, "2026_Potential_Matchups_Martin.csv")

colnames(stats_combined)

print(year.to.pred)
mean(year.to.pred)
print(brier_scores)
mean(brier_scores)