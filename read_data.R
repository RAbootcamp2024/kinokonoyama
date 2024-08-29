install.packages("tidyverse")

library(tidyverse)

  data <- read_csv("C:/Users/Owner/Desktop/kinokonoyama/dataset.csv")


# make table2
library(dplyr)

data1 <- data %>%
  mutate(pre_reg_s = case_when(
    state == "California" & year >= 2009 ~ 1,
    state == "Colorado" & year >= 2013 ~ 1,
    state == "Delaware" & year >= 2010 ~ 1,
    state == "Florida" & year >= 2007 ~ 1,
    state == "Louisiana" & year >= 2014 ~ 1,
    state == "Maine" & year >= 2011 ~ 1,
    state == "Maryland" & year >= 2010 ~ 1,
    state == "Massachusetts" & year >= 2014 ~ 1,
    state == "North Carolina" & year >= 2009 & year <= 2013 ~ 1,
    state == "Oregon" & year >= 2007 ~ 1,
    state == "Rhode Island" & year >= 2010 ~ 1,
    state == "Utah" & year >= 2015 ~ 1,
    state == "Hawaii" & year >= 1993 ~ 1,
    state == "New Jersey" & year >= 2015 ~ 1,
    TRUE ~ 0  # Default case if none of the conditions match
  )) |>
  arrange(stid, year) |>
  mutate(pre_reg = 0) |>
  mutate(pre_reg_s_lag = lag(pre_reg_s, default = 0),  # 前の行の 'pre_reg_s' を取得
         pre_reg = if_else(pre_reg_s_lag == 0 & pre_reg_s == 1, 1, pre_reg), .by = stid)


# Create F0_pre, F1_pre, ..., F10_pre
for (i in 0:10) {
  data1 <- data1 |> 
    mutate(!!paste0("F", i, "_pre"):= 0)
}


# データの処理
data1 <- data1 |>
  # stateとyearでデータをソートします
  arrange(state, year) |>
  # treated_statesという新しい変数を0で初期化します
  mutate(treated_states = 0) |>
  # stateごとにpre_regの最大値を計算して、その値をtreated_statesにセットします
  mutate(treated_states = max(pre_reg), .by = state)

data1 <- data1 |>
  mutate(law_year = case_when(
    state == "California" ~ 2009,
    state == "Colorado" ~ 2013,
    state == "Delaware" ~ 2010,
    state == "Florida" ~ 2007,
    state == "Louisiana" ~ 2014,
    state == "Maine" ~ 2011,
    state == "Maryland" ~ 2010,
    state == "Massachusetts" ~ 2014,
    state == "North Carolina" ~ 2009,
    state == "Oregon" ~ 2007,
    state == "Rhode Island" ~ 2010,
    state == "Utah" ~ 2015,
    state == "Hawaii" ~ 1993,
    state == "New Jersey" ~ 2015,
    TRUE ~ NA))  # Default case if none of the conditions match)

data1 <- data1 |>
  mutate(dif_Y = year - law_year) |>
  mutate(F10_pre = if_else(dif_Y <= -10, 1, 0), 
         F9_pre = if_else(dif_Y == -9, 1, 0),
         F8_pre = if_else(dif_Y == -8, 1, 0),
         F7_pre = if_else(dif_Y == -7, 1, 0),
         F6_pre = if_else(dif_Y == -6, 1, 0),
         F5_pre = if_else(dif_Y == -5, 1, 0),
         F4_pre = if_else(dif_Y == -4, 1, 0),
         F3_pre = if_else(dif_Y == -3, 1, 0),
         F2_pre = if_else(dif_Y == -2, 1, 0),
         F1_pre = if_else(dif_Y == -1, 1, 0),
         F0_pre = if_else(dif_Y == 0, 1, 0))


data1 <- data1 |>
  mutate(L4_pre = if_else(dif_Y >= 4, 1, 0), 
         L3_pre = if_else(dif_Y == 3, 1, 0),
         L2_pre = if_else(dif_Y == 2, 1, 0),
         L1_pre = if_else(dif_Y == 1, 1, 0))


# for or fastdummy 後でやろう

# North Carolinaのみ操作
data1 <- data1 |>
  mutate(L4_pre = if_else(state == "North Carolina" & dif_Y >= 5, 0, L4_pre))
  
# 確認
data1 |>
  filter(state == "North Carolina")|>
  select(state, year, dif_Y, L4_pre)|>
  view()

names(data1)

# income変数操作
data1 <- data1 |>
  mutate(lnp_income = log(p_income),
         lne031 = log(e031))
  
# uno = F1_pre = 0
## dif_Y = -1のデータ消去

data2 <- data1 |>
  mutate(F1_pre = 0) 

# regression----
## baseline----
install.packages("fixest")
  library(fixest)
model1 <- feols(lne031 ~ F10_pre + F9_pre + F8_pre + F7_pre + F6_pre +
               F5_pre + F4_pre + F3_pre + F2_pre + F1_pre + F0_pre +
               L1_pre + L2_pre + L3_pre + L4_pre + lnp_income + factor(year) +
                 factor(state)| stid,  data = data2)
model1 <- feols(lne031 ~ F10_pre + F9_pre + F8_pre + F7_pre + F6_pre +
                  F5_pre + F4_pre + F3_pre + F2_pre + F1_pre + F0_pre +
                  L1_pre + L2_pre + L3_pre + L4_pre + lnp_income + factor(year) +
                  factor(stid)|state + year,  data = data2)

summary(model1)




 





library(dplyr)

# データの処理
data1 <- data1 |>
  # 年差を計算
  mutate(dif_Y = year - law_year) |>
  # F-9_pre から F0_pre までの変数を作成
  mutate(across(
    .cols = paste0("F", -9:0, "_pre"), # 作成する列名のパターン
    .fns = ~ case_when(
      # 条件に基づいて1または0を設定
      dif_Y == as.integer(sub("F", "", sub("_pre", "", cur_column()))) ~ 1,
      TRUE ~ 0
    ),
    .names = "{col}"  # 列名をそのまま使う
  ))

# データを確認
print(data1)


# Create L0_pre, L1_pre, ..., L10_pre
for (i in 1:4) {
  data1 <- data1 |> 
    mutate(!!paste0("L", i, "_pre"):= 0)
}





  #select(state, year, e001, e031, stid, p_income, )

#*************** BASELINE ***************
#  reg  F10_last_pre F9_pre F8_pre F7_pre F6_pre F5_pre F4_pre F3_pre F2_pre uno F0_pre L1_pre L2_pre L3_pre L4_last_pre	
# eststo no_trend

data |>
  filter()